!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of HydroDyn.
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
!> Reader for the YAML form of the HydroDyn *driver* input file. Fills the same fields
!! as ReadDriverInputFile's sequential ReadVar/ReadAry/ReadStr body
!! (HydroDyn_DriverSubs.f90:110-317, a class-B/sequential-reader driver), so everything
!! downstream (ReadPRPInputsFile, the driver's own time-marching loop) is shared
!! between the two formats.
!!
!! HD_Drvr_Data is a proper derived type, but it is declared inside MODULE
!! HydroDynDriverSubs -- the very module whose ReadDriverInputFile hosts the funnel
!! call site -- rather than in a dedicated *_Driver_Types module. Taking it as a dummy
!! argument here would require this module to USE HydroDynDriverSubs for the type
!! definition while HydroDynDriverSubs USEs this module for the parser call, a
!! circular dependency Fortran does not allow. So, exactly like SeaState's and
!! SubDyn's drivers (their InitInp types are program-local for the same underlying
!! reason -- no separate type-only module to break the cycle), this reader takes each
!! HD_Drvr_Data field as its own dummy argument; the funnel call site copies
!! InitInp%<field> in and back out.
!!
!! Schema: sections mirror the text file's banners --
!!   general:                     Echo, FTitle (the free-text description on the
!!                                 driver file's second line, read via ReadStr; both
!!                                 precede the "ENVIRONMENTAL CONDITIONS" banner in the
!!                                 text format, so both land in "general" here)
!!   environmental_conditions:    Gravity, WtrDens, WtrDpth, MSL2SWL
!!   hydrodyn:                    HDInputFile, SeaStateInputFile, OutRootName,
!!                                 Linearize, NSteps, TimeInterval
!!   prp_inputs:                  PRPInputsMod, NAddDOF, PtfmRefzt, PRPInputsFile
!!   prp_steady_state_inputs:     uPRPInSteady, uDotPRPInSteady, uDotDotPRPInSteady
!!
!! Table-free (~15 scalars); no key accepts the "default" keyword (every field is a
!! plain ReadVar/ReadAry/ReadStr read in the text path). HDInputFile/SeaStateInputFile/
!! OutRootName/PRPInputsFile are externally-referenced files (second-order rule: stay
!! path-valued), each resolved relative to the driver YAML file's own directory here,
!! exactly as the text path resolves them relative to PriPath
!! (HydroDyn_DriverSubs.f90:221/226/231/269). PRPInputsFile's own referenced
!! time-series table is read later by ReadPRPInputsFile (HydroDyn_DriverSubs.f90:319),
!! a separate driver subroutine the main program calls unconditionally after
!! ReadDriverInputFile returns -- it operates purely off the PRPInputsMod/
!! PRPInputsFile/NAddDOF fields this parser fills, so this parser never needs to open
!! PRPInputsFile itself (unlike SubDyn's InputsFile, whose SDin table the text path
!! reads inline inside ReadDriverInputFile itself).
!!
!! prp_steady_state_inputs: the text path's three ReadAry calls
!! (HydroDyn_DriverSubs.f90:281-290) are unconditional -- unlike SubDyn's
!! steady_state_inputs (whose IF (InputsMod==1) branch skips the *reads* entirely when
!! not steady-state), HydroDyn always physically reads all three 6-vectors from the
!! file, and only *afterward* zeroes them out when PRPInputsMod /= 1
!! (HydroDyn_DriverSubs.f90:292-296). This parser mirrors that literally: the section
!! is always required (the reader always reads it), and the same post-read
!! PRPInputsMod-guarded zeroing is applied here too.
module HydroDyn_Driver_Yaml

   use NWTC_Library
   use YamlInput

   implicit none
   private

   public :: HDDvr_ParseYamlFile

contains

!> Load and parse a YAML-format HydroDyn driver input file, filling each HD_Drvr_Data
!! field passed in as its own dummy argument (see the module header for why).
subroutine HDDvr_ParseYamlFile(DvrFileName, Echo, FTitle, Gravity, WtrDens, WtrDpth, MSL2SWL, &
                                HDInputFile, SeaStateInputFile, OutRootName, Linearize, NSteps, TimeInterval, &
                                PRPInputsMod, NAddDOF, PtfmRefzt, PRPInputsFile, &
                                uPRPInSteady, uDotPRPInSteady, uDotDotPRPInSteady, &
                                ErrStat, ErrMsg)
   character(*),   intent(in   ) :: DvrFileName        !< the .yaml driver input file
   logical,        intent(  out) :: Echo
   character(*),   intent(  out) :: FTitle
   real(ReKi),     intent(  out) :: Gravity
   real(ReKi),     intent(  out) :: WtrDens
   real(ReKi),     intent(  out) :: WtrDpth
   real(ReKi),     intent(  out) :: MSL2SWL
   character(*),   intent(  out) :: HDInputFile
   character(*),   intent(  out) :: SeaStateInputFile
   character(*),   intent(  out) :: OutRootName
   logical,        intent(  out) :: Linearize
   integer,        intent(  out) :: NSteps
   real(DbKi),     intent(  out) :: TimeInterval
   integer,        intent(  out) :: PRPInputsMod
   integer,        intent(  out) :: NAddDOF
   real(ReKi),     intent(  out) :: PtfmRefzt
   character(*),   intent(  out) :: PRPInputsFile
   real(R8Ki),     intent(  out) :: uPRPInSteady(6)
   real(R8Ki),     intent(  out) :: uDotPRPInSteady(6)
   real(R8Ki),     intent(  out) :: uDotDotPRPInSteady(6)
   integer,        intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'HDDvr_ParseYamlFile'
   type(YamlDoc)            :: Doc
   character(1024)          :: PriPath
   integer(IntKi)           :: UnEc
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1
   Echo    = .false.   ! initialize for error handling (cleanup path)

   call WrScr( 'Opening HydroDyn Driver input file:  '//trim(DvrFileName) )
   call GetPath( trim(DvrFileName), PriPath )   ! Input files will be relative to the path where the driver input file is located.

   call Yaml_LoadFile(DvrFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'general:Echo', Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (Echo) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included), which supersedes the text path's line-by-line echo
      call OpenEcho(UnEc, trim(DvrFileName)//'.ech', TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for HydroDyn driver input file: '//trim(DvrFileName)
      call Yaml_LoadFile(DvrFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   call YamlGet(Doc, 'general:FTitle', FTitle, TmpErrStat, TmpErrMsg, Default='')
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! environmental_conditions (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'environmental_conditions:Gravity', Gravity, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:WtrDens', WtrDens, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:WtrDpth', WtrDpth, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:MSL2SWL', MSL2SWL, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! hydrodyn (required) -- HDInputFile/SeaStateInputFile/OutRootName are
   ! externally-referenced files (second-order rule: stay path-valued)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'hydrodyn:HDInputFile', HDInputFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if ( PathIsRelative( HDInputFile ) ) HDInputFile = trim(PriPath)//trim(HDInputFile)

   call YamlGet(Doc, 'hydrodyn:SeaStateInputFile', SeaStateInputFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if ( PathIsRelative( SeaStateInputFile ) ) SeaStateInputFile = trim(PriPath)//trim(SeaStateInputFile)

   call YamlGet(Doc, 'hydrodyn:OutRootName', OutRootName, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if ( PathIsRelative( OutRootName ) ) OutRootName = trim(PriPath)//trim(OutRootName)

   call YamlGet(Doc, 'hydrodyn:Linearize', Linearize, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'hydrodyn:NSteps', NSteps, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'hydrodyn:TimeInterval', TimeInterval, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! prp_inputs (required) -- PRPInputsFile is an externally-referenced file
   ! (second-order rule: stays path-valued; its own referenced time-series table is
   ! read later by ReadPRPInputsFile, not here -- see the module header)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'prp_inputs:PRPInputsMod', PRPInputsMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'prp_inputs:NAddDOF', NAddDOF, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'prp_inputs:PtfmRefzt', PtfmRefzt, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'prp_inputs:PRPInputsFile', PRPInputsFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if ( PathIsRelative( PRPInputsFile ) ) PRPInputsFile = trim(PriPath)//trim(PRPInputsFile)

   !----------------------------------------------------------------------------------
   ! prp_steady_state_inputs (required -- the text path always reads these three
   ! 6-vectors unconditionally; see the module header). Zeroed afterward when
   ! PRPInputsMod /= 1, exactly mirroring HydroDyn_DriverSubs.f90:292-296.
   !----------------------------------------------------------------------------------
   call ParseFixedR8Ary6(Doc, 'prp_steady_state_inputs:uPRPInSteady', uPRPInSteady, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call ParseFixedR8Ary6(Doc, 'prp_steady_state_inputs:uDotPRPInSteady', uDotPRPInSteady, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call ParseFixedR8Ary6(Doc, 'prp_steady_state_inputs:uDotDotPRPInSteady', uDotDotPRPInSteady, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   if (PRPInputsMod /= 1) then
      uPRPInSteady       = 0.0_R8Ki
      uDotPRPInSteady    = 0.0_R8Ki
      uDotDotPRPInSteady = 0.0_R8Ki
   end if

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
      if (UnEc > 0_IntKi) close(UnEc)
   end subroutine Cleanup

end subroutine HDDvr_ParseYamlFile

!> Read a fixed-size 6-element real(R8Ki) array key into a (non-allocatable) dummy
!! array -- YamlGet's array form requires an allocatable actual argument, so this
!! reads into a local allocatable first and copies over, checking the length matches.
subroutine ParseFixedR8Ary6(Doc, Path, Ary, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   character(*),   intent(in   ) :: Path
   real(R8Ki),     intent(  out) :: Ary(6)
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'HDDvr_ParseFixedR8Ary6'
   real(R8Ki), allocatable  :: Tmp(:)
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, Path, Tmp, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
   if (ErrStat >= AbortErrLev) return

   if (size(Tmp) /= 6) then
      call SetErrStat(ErrID_Fatal, trim(Path)//' must list exactly 6 values.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   Ary = Tmp

end subroutine ParseFixedR8Ary6

end module HydroDyn_Driver_Yaml
