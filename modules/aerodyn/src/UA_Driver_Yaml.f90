!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of AeroDyn.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!**********************************************************************************************************************************
!> Reader for the YAML form of the standalone UnsteadyAero driver input file. Fills the
!! same Dvr_Parameters (UA_Dvr_Subs.f90) fields ReadDriverInputFile does (UA_Dvr_Subs.f90:
!! 182-330), key-for-key for whichever schema branch simulation_control:SimMod selects, so
!! everything downstream (Dvr_SetParameters, the driver's own time-marching loop in
!! UnsteadyAero_Driver.f90) is shared between the two formats.
!!
!! This module is used ONLY by this driver, not by UA_Dvr_Subs itself (a `use` the other
!! way would be circular, since Dvr_Parameters is defined inside UA_Dvr_Subs): the funnel
!! lives at the top of the driver program (UnsteadyAero_Driver.f90), exactly mirroring
!! AeroDisk_Driver.f90's IsYamlExt dispatch at its own top-level call site.
!!
!! Schema: sections mirror the text file's banners --
!!   general:                 Echo
!!   environmental_conditions: FldDens, KinVisc, SpdSound
!!   unsteady_aero:            UAMod, Flookup
!!   airfoil_properties:       AirFoil, Chord, Vec_AQ, Vec_AT, UseCm
!!   simulation_control:       SimMod -- the schema-branch selector (see below)
!!   output_control:           SumPrint, WrAFITables
!!
!! TWO schema branches, guarded on simulation_control:SimMod exactly as the driver program
!! itself branches (UnsteadyAero_Driver.f90:78,124,153 all test `SimMod==3`; the internal
!! SimMod==1 vs SimMod==2 split lives in Dvr_SetParameters/setUAinputsAlphaSim,
!! UA_Dvr_Subs.f90:352-381,680). Only the fields consumed by the selected branch are
!! read; the other branch's keys are never emitted by the converter and never looked up
!! here:
!!   SimMod=1 -> reduced_frequency: InflowVel, NCycles, StepsPerCycle, Frequency,
!!               Amplitude, Mean, Phase ("periodic-motion": the reduced-frequency/
!!               oscillating-AoA model). Exercised by r-test case ua_redfreq (UA2.dvr/
!!               UA3.dvr).
!!   SimMod=3 -> aeroelastic: TMax, DT, ActiveDOF, InitPos, InitVel, GFScaling (3x3),
!!               MassMatrix (3x3), DampMatrix (3x3), StifMatrix (3x3), Twist, InflowMod
!!               (+ Inflow when constant / InflowTSFile when file), MotionMod (+
!!               MotionTSFile when prescribed -- the literal "prescribed-time-series"
!!               sub-case of this branch's MotionMod={1:dynamic,2:prescribed} selector,
!!               UA_Dvr_Subs.f90:53). Exercised by r-test case ua_elast (UA4.dvr, which
!!               itself uses InflowMod=1/MotionMod=1 -- the constant/dynamic sub-case;
!!               the file/prescribed sub-case is implemented and reachable but has no
!!               r-test baseline to self-verify against).
!! SimMod=2 (prescribed-aero time series, TMax_PA/DT_PA/AeroTSFile) has no r-test driver
!! case at all and is NOT implemented here -- an unsupported SimMod is a fatal YAML parse
!! error with an explicit message, rather than silently mis-parsing.
!!
!! AeroTSFile (SimMod=2, unimplemented) and InflowTSFile/MotionTSFile (SimMod=3) are all
!! second-order files: path-valued only, never inlined. Airfoil/InflowTSFile/MotionTSFile
!! relative-path resolution (like the text path's PriPath handling) is left to the shared
!! trailing block in UA_Dvr_Subs.f90's ReadDriverInputFile is NOT reused here (this driver
!! calls this module directly, not through ReadDriverInputFile) -- so this module performs
!! its own PriPath resolution, mirroring that trailing block exactly.
module UA_Driver_Yaml

   use NWTC_Library
   use YamlInput
   use UA_Dvr_Subs, only: Dvr_Parameters, InflowMod_Cst, InflowMod_File, MotionMod_Cst, MotionMod_File, &
                           InflowMod_Valid, MotionMod_Valid

   implicit none
   private

   public :: UADvr_ParseYamlFile

contains

!> Load and parse a YAML-format UnsteadyAero driver input file into the same
!! Dvr_Parameters structure ReadDriverInputFile fills from text.
subroutine UADvr_ParseYamlFile(FileName, InitInp, ErrStat, ErrMsg)
   character(1024),       intent(in   ) :: FileName
   type(Dvr_Parameters),  intent(inout) :: InitInp
   integer(IntKi),        intent(  out) :: ErrStat
   character(*),          intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'UADvr_ParseYamlFile'
   type(YamlDoc)           :: Doc
   character(1024)         :: PriPath
   integer(IntKi)          :: UnEcho
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ''
   UnEcho  = -1

   call WrScr(' Opening UnsteadyAero Driver input file: '//trim(FileName) )
   call GetPath( FileName, PriPath )    ! Input files will be relative to the path where the primary input file is located.

   call Yaml_LoadFile(FileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'general:Echo', InitInp%Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (InitInp%Echo) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included) -- same convention as AeroDisk_Driver_Yaml.f90's reader,
      ! which supersedes the text path's line-by-line echo.
      call OpenEcho(UnEcho, trim(FileName)//'.ech', TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEcho, '(A)') 'Echo file for UnsteadyAero driver input file: '//trim(FileName)
      call Yaml_LoadFile(FileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEcho)
      if (Failed()) return
   end if

   call ParseYamlDoc(Doc, InitInp, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! --- Path resolution (mirrors ReadDriverInputFile's own trailing block,
   ! UA_Dvr_Subs.f90:294-297 -- this driver never goes through ReadDriverInputFile itself)
   if (PathIsRelative(InitInp%Airfoil1))    InitInp%Airfoil1     = trim(PriPath)//trim(InitInp%Airfoil1)
   if (PathIsRelative(InitInp%AeroTSFile))  InitInp%AeroTSFile   = trim(PriPath)//trim(InitInp%AeroTSFile)
   if (PathIsRelative(InitInp%InflowTSFile)) InitInp%InflowTSFile = trim(PriPath)//trim(InitInp%InflowTSFile)
   if (PathIsRelative(InitInp%MotionTSFile)) InitInp%MotionTSFile = trim(PriPath)//trim(InitInp%MotionTSFile)

   ! --- Checks (mirrors ReadDriverInputFile's own Check() calls, UA_Dvr_Subs.f90:301-302)
   if (.not. any(InitInp%InflowMod == InflowMod_Valid)) then
      call SetErrStat(ErrID_Fatal, 'InflowMod not implemented: '//trim(Num2LStr(InitInp%InflowMod)), &
         ErrStat, ErrMsg, RoutineName)
      call Cleanup()
      return
   end if
   if (.not. any(InitInp%MotionMod == MotionMod_Valid)) then
      call SetErrStat(ErrID_Fatal, 'MotionMod not implemented: '//trim(Num2LStr(InitInp%MotionMod)), &
         ErrStat, ErrMsg, RoutineName)
      call Cleanup()
      return
   end if
   if (InitInp%SimMod == 3) then
      if (InitInp%Vec_AT(2) < 0) call WrScr('[WARN] Vec_AT(2) is negative, but this value is usually positive (for A between T and Q)')
      if (InitInp%Vec_AQ(2) > 0) call WrScr('[WARN] Vec_AQ(2) is positive, but this value is usually negative (for A between T and Q)')
   end if

   ! --- OutRootName is inferred from the current filename (matches ReadDriverInputFile)
   call GetRoot(FileName, InitInp%OutRootName)

   ! typo guard: anything never looked up is reported (warning severity)
   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

   call Cleanup()

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
      if (Failed) call Cleanup()
   end function Failed

   subroutine Cleanup()
      if (InitInp%Echo .and. UnEcho > 0_IntKi) close(UnEcho)
   end subroutine Cleanup

end subroutine UADvr_ParseYamlFile

!> Fill InitInp (a Dvr_Parameters) from a parsed document. Split from the file wrapper for
!! symmetry with the other Wave-4 driver readers.
subroutine ParseYamlDoc(Doc, InitInp, ErrStat, ErrMsg)
   type(YamlDoc),         intent(inout) :: Doc
   type(Dvr_Parameters),  intent(inout) :: InitInp
   integer(IntKi),        intent(  out) :: ErrStat
   character(*),          intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ParseYamlDoc'
   logical                 :: EchoTmp
   real(ReKi), allocatable :: TmpReAry(:)
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ''

   ! general:Echo was already consulted (and, if true, acted on) by the caller before any
   ! possible echo-reload; touch it again here so it is marked Used on whichever Doc
   ! instance this routine actually receives (mirrors AeroDisk_Driver_Yaml.f90's
   ! ParseYamlDoc, which re-reads it for the same reason).
   call YamlGet(Doc, 'general:Echo', EchoTmp, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   !-------------------------------------------------------------------------------------
   ! environmental_conditions (required)
   !-------------------------------------------------------------------------------------
   call YamlGet(Doc, 'environmental_conditions:FldDens',  InitInp%FldDens,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:KinVisc',  InitInp%KinVisc,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:SpdSound', InitInp%SpdSound, TmpErrStat, TmpErrMsg); if (Failed()) return

   !-------------------------------------------------------------------------------------
   ! unsteady_aero (required)
   !-------------------------------------------------------------------------------------
   call YamlGet(Doc, 'unsteady_aero:UAMod',   InitInp%UAMod,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'unsteady_aero:Flookup', InitInp%Flookup, TmpErrStat, TmpErrMsg); if (Failed()) return

   !-------------------------------------------------------------------------------------
   ! airfoil_properties (required)
   !-------------------------------------------------------------------------------------
   call YamlGet(Doc, 'airfoil_properties:AirFoil', InitInp%AirFoil1, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'airfoil_properties:Chord',   InitInp%Chord,    TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGet(Doc, 'airfoil_properties:Vec_AQ', TmpReAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (BadSize(size(TmpReAry), 2, 'airfoil_properties:Vec_AQ')) return
   InitInp%Vec_AQ = TmpReAry

   call YamlGet(Doc, 'airfoil_properties:Vec_AT', TmpReAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (BadSize(size(TmpReAry), 2, 'airfoil_properties:Vec_AT')) return
   InitInp%Vec_AT = TmpReAry

   call YamlGet(Doc, 'airfoil_properties:UseCm', InitInp%UseCm, TmpErrStat, TmpErrMsg); if (Failed()) return

   !-------------------------------------------------------------------------------------
   ! simulation_control (required) -- SimMod selects the schema branch below
   !-------------------------------------------------------------------------------------
   call YamlGet(Doc, 'simulation_control:SimMod', InitInp%SimMod, TmpErrStat, TmpErrMsg); if (Failed()) return

   select case (InitInp%SimMod)
   case (1)
      call ParseReducedFrequency(Doc, InitInp, TmpErrStat, TmpErrMsg); if (Failed()) return
   case (3)
      call ParseAeroelastic(Doc, InitInp, TmpErrStat, TmpErrMsg); if (Failed()) return
   case default
      call SetErrStat(ErrID_Fatal, '"simulation_control:SimMod" = '//trim(Num2LStr(InitInp%SimMod))// &
         ' is not supported by the YAML UnsteadyAero driver schema (supported: 1=reduced-frequency/'// &
         'periodic-motion, 3=aeroelastic). SimMod=2 (prescribed-aero time series) has no YAML schema yet; '// &
         'use the text (.dvr) driver format for that case.', ErrStat, ErrMsg, RoutineName)
      return
   end select

   !-------------------------------------------------------------------------------------
   ! output_control (required)
   !-------------------------------------------------------------------------------------
   call YamlGet(Doc, 'output_control:SumPrint',    InitInp%SumPrint,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output_control:WrAFITables', InitInp%WrAFITables, TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

   logical function BadSize(NGiven, NExpect, Path)
      integer(IntKi), intent(in) :: NGiven
      integer(IntKi), intent(in) :: NExpect
      character(*),   intent(in) :: Path
      BadSize = (NGiven /= NExpect)
      if (BadSize) call SetErrStat(ErrID_Fatal, '"'//Path//'" must list exactly '//trim(Num2LStr(NExpect))// &
         ' value(s); found '//trim(Num2LStr(NGiven))//'.', ErrStat, ErrMsg, RoutineName)
   end function BadSize

end subroutine ParseYamlDoc

!> simulation_control:SimMod == 1 branch ("periodic-motion"): reduced-frequency /
!! oscillating-AoA model. Fields match UA_Dvr_Subs.f90:245-251 exactly (the only fields
!! Dvr_SetParameters/setUAinputsAlphaSim consume for this SimMod, UA_Dvr_Subs.f90:352-361,
!! 680-720).
subroutine ParseReducedFrequency(Doc, InitInp, ErrStat, ErrMsg)
   type(YamlDoc),         intent(inout) :: Doc
   type(Dvr_Parameters),  intent(inout) :: InitInp
   integer(IntKi),        intent(  out) :: ErrStat
   character(*),          intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ParseReducedFrequency'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ''

   call YamlGet(Doc, 'reduced_frequency:InflowVel',     InitInp%InflowVel,     TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'reduced_frequency:NCycles',        InitInp%NCycles,       TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'reduced_frequency:StepsPerCycle',  InitInp%StepsPerCycle, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'reduced_frequency:Frequency',      InitInp%Frequency,     TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'reduced_frequency:Amplitude',      InitInp%Amplitude,     TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'reduced_frequency:Mean',           InitInp%Mean,          TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'reduced_frequency:Phase',          InitInp%Phase,         TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseReducedFrequency

!> simulation_control:SimMod == 3 branch ("aeroelastic"): fields match UA_Dvr_Subs.f90:
!! 260-283 exactly (the only fields the driver's SimMod==3 path consumes,
!! UnsteadyAero_Driver.f90:78-101,124-141,153-222). InflowMod/MotionMod are themselves
!! nested selectors (UA_Dvr_Subs.f90:15-20): InflowMod={1:constant (Inflow),2:file
!! (InflowTSFile)}, MotionMod={1:dynamic (no extra field -- computed by LinDyn),
!! 2:prescribed (MotionTSFile)}. Only the field the selected sub-mode needs is emitted/
!! read -- e.g. a constant-inflow, dynamic-motion case (InflowMod=1, MotionMod=1, the
!! ua_elast r-test case) never has InflowTSFile or MotionTSFile keys at all.
subroutine ParseAeroelastic(Doc, InitInp, ErrStat, ErrMsg)
   type(YamlDoc),         intent(inout) :: Doc
   type(Dvr_Parameters),  intent(inout) :: InitInp
   integer(IntKi),        intent(  out) :: ErrStat
   character(*),          intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'ParseAeroelastic'
   real(ReKi), allocatable   :: TmpReAry(:)
   real(ReKi), allocatable   :: TmpMat(:,:)
   logical,    allocatable   :: TmpLoAry(:)
   integer(IntKi)            :: TmpErrStat
   character(ErrMsgLen)      :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ''

   call YamlGet(Doc, 'aeroelastic:TMax', InitInp%TMax, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'aeroelastic:DT',   InitInp%dt,   TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGetLoAryLocal(Doc, 'aeroelastic:ActiveDOF', TmpLoAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (BadSize(size(TmpLoAry), 3, 'aeroelastic:ActiveDOF')) return
   InitInp%activeDOFs = TmpLoAry

   call YamlGet(Doc, 'aeroelastic:InitPos', TmpReAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (BadSize(size(TmpReAry), 3, 'aeroelastic:InitPos')) return
   InitInp%initPos = TmpReAry

   call YamlGet(Doc, 'aeroelastic:InitVel', TmpReAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (BadSize(size(TmpReAry), 3, 'aeroelastic:InitVel')) return
   InitInp%initVel = TmpReAry

   call YamlGet(Doc, 'aeroelastic:GFScaling', TmpMat, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (BadMatSize(TmpMat, 3, 3, 'aeroelastic:GFScaling')) return
   InitInp%GFScaling = TmpMat

   call YamlGet(Doc, 'aeroelastic:MassMatrix', TmpMat, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (BadMatSize(TmpMat, 3, 3, 'aeroelastic:MassMatrix')) return
   InitInp%MM = TmpMat

   call YamlGet(Doc, 'aeroelastic:DampMatrix', TmpMat, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (BadMatSize(TmpMat, 3, 3, 'aeroelastic:DampMatrix')) return
   InitInp%CC = TmpMat

   call YamlGet(Doc, 'aeroelastic:StifMatrix', TmpMat, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (BadMatSize(TmpMat, 3, 3, 'aeroelastic:StifMatrix')) return
   InitInp%KK = TmpMat

   call YamlGet(Doc, 'aeroelastic:Twist', InitInp%Twist, TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGet(Doc, 'aeroelastic:InflowMod', InitInp%InflowMod, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (InitInp%InflowMod == InflowMod_File) then
      call YamlGet(Doc, 'aeroelastic:InflowTSFile', InitInp%InflowTSFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   else
      call YamlGet(Doc, 'aeroelastic:Inflow', TmpReAry, TmpErrStat, TmpErrMsg); if (Failed()) return
      if (BadSize(size(TmpReAry), 2, 'aeroelastic:Inflow')) return
      InitInp%Inflow = TmpReAry
   end if

   call YamlGet(Doc, 'aeroelastic:MotionMod', InitInp%MotionMod, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (InitInp%MotionMod == MotionMod_File) then
      call YamlGet(Doc, 'aeroelastic:MotionTSFile', InitInp%MotionTSFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

   logical function BadSize(NGiven, NExpect, Path)
      integer(IntKi), intent(in) :: NGiven
      integer(IntKi), intent(in) :: NExpect
      character(*),   intent(in) :: Path
      BadSize = (NGiven /= NExpect)
      if (BadSize) call SetErrStat(ErrID_Fatal, '"'//Path//'" must list exactly '//trim(Num2LStr(NExpect))// &
         ' value(s); found '//trim(Num2LStr(NGiven))//'.', ErrStat, ErrMsg, RoutineName)
   end function BadSize

   logical function BadMatSize(Mat, NRows, NCols, Path)
      real(ReKi), allocatable, intent(in) :: Mat(:,:)
      integer(IntKi),          intent(in) :: NRows
      integer(IntKi),          intent(in) :: NCols
      character(*),            intent(in) :: Path
      BadMatSize = (.not. allocated(Mat))
      if (.not. BadMatSize) BadMatSize = (size(Mat,1) /= NRows) .or. (size(Mat,2) /= NCols)
      if (BadMatSize) call SetErrStat(ErrID_Fatal, '"'//Path//'" must be a '//trim(Num2LStr(NRows))//'x'// &
         trim(Num2LStr(NCols))//' matrix (one flow-sequence row per line).', ErrStat, ErrMsg, RoutineName)
   end function BadMatSize

end subroutine ParseAeroelastic

!> Logical-array lookup (no logical-array specific getter exists in YamlGet yet -- same
!! local-helper pattern FAST_Yaml.f90's YamlGetLoAryLocal uses, reproduced here rather
!! than depending on openfast-library, which this driver does not link against).
subroutine YamlGetLoAryLocal(Doc, Path, Ary, ErrStat, ErrMsg)
   type(YamlDoc),         intent(inout) :: Doc
   character(*),          intent(in   ) :: Path
   logical,   allocatable, intent(  out) :: Ary(:)
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg

   character(:), allocatable :: Toks(:)
   character(8)               :: UTok
   integer                    :: k

   call YamlGet(Doc, Path, Toks, ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return
   allocate(Ary(size(Toks)))
   do k = 1, size(Toks)
      UTok = Toks(k)
      call Conv2UC(UTok)
      select case (trim(UTok))
      case ('TRUE', 'T', '.TRUE.')
         Ary(k) = .true.
      case ('FALSE', 'F', '.FALSE.')
         Ary(k) = .false.
      case default
         ErrStat = ErrID_Fatal
         ErrMsg  = '>> "'//trim(Path)//'" entries must be true/false; found "'//trim(Toks(k))//'".'
         return
      end select
   end do
end subroutine YamlGetLoAryLocal

end module UA_Driver_Yaml
