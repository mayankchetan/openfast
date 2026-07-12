!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of SubDyn.
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
!> Reader for the YAML form of the SubDyn *driver* input file. Fills the same fields as
!! ReadDriverInputFile's sequential ReadVar/ReadAry body (SubDyn_Driver.f90:359-479, a
!! class-B/sequential-reader driver, mirroring SeaState's precedent), so everything
!! downstream (the driver's own time-marching loop) is shared between the two formats.
!!
!! SD_dvr_InitInput is a program-local (host-associated) type declared inside PROGRAM
!! SubDyn_Driver, so this reader cannot take it as a dummy argument (an external/module
!! procedure cannot type-match a program-internal derived type) -- it instead takes
!! each InitInp field as its own dummy argument, exactly like SeaState_Driver_Yaml.
!! The same restriction applies one level deeper to InitInp%AppliedLoads: its element
!! type ALoadType is *also* program-local, so this reader cannot return an array of it
!! either. Instead it returns the applied-loads table as parallel plain-intrinsic
!! arrays (JointID/SteadyLoad/UnsteadyFile); the funnel call site (an internal
!! subroutine of PROGRAM SubDyn_Driver, so it *does* have host access to ALoadType)
!! assembles InitInp%AppliedLoads(:) from them, calling ReadDelimFile itself for any
!! row with a non-empty UnsteadyFile -- the same NWTC_Library call the text path's
!! readAppliedForce makes, just relocated to the one place that can build the array.
!!
!! Schema: sections mirror the text file's banners --
!!   general:                  Echo
!!   environmental_conditions: Gravity, WtrDpth
!!   subdyn:                   SDInputFile, OutRootName, NSteps, TimeInterval,
!!                              tp_ref_points (list of {x,y,z}; its length is nTP --
!!                              counts derive from list lengths, never a separate key),
!!                              SubRotateZ
!!   inputs:                   InputsMod, InputsFile
!!   steady_state_inputs:      uTPInSteady, uDotTPInSteady, uDotDotTPInSteady -- present
!!                              (and read) ONLY when InputsMod == 1, exactly mirroring
!!                              the text path's IF (InitInp%InputsMod == 1) branch
!!                              (SubDyn_Driver.f90:429-440); when InputsMod /= 1 the
!!                              three arrays are set to zero here without looking the
!!                              section up at all (the text path's ELSE branch just
!!                              skips the lines as comments -- there is no discarded
!!                              value to preserve either way).
!!   loads:                    applied_loads (optional list of row mappings, default
!!                              empty; no legacy "missing nAppliedLoads line" warning --
!!                              a modern-format concession, same convention as
!!                              SubDyn_Yaml.f90's guyan_damping/cable_properties)
!!
!! SDInputFile/OutRootName/InputsFile are externally-referenced files (second-order
!! rule: stay path-valued); each is resolved relative to the driver YAML file's own
!! directory here, exactly as the text path resolves them relative to PriPath
!! (SubDyn_Driver.f90:469-477). InputsFile's referenced time-series file (InputsMod==2)
!! and each applied-load row's UnsteadyFile are also externally-referenced files and
!! stay path-valued; InputsFile's own referenced SDin table is read here (a plain
!! REAL array, no program-local type involved) via the same ReadDelimFile call the
!! text path makes (SubDyn_Driver.f90:442). If OutRootName is blank, it defaults to
!! SDInputFile's root, exactly as the text path (SubDyn_Driver.f90:466-468).
module SubDyn_Driver_Yaml

   use NWTC_Library
   use YamlInput

   implicit none
   private

   public :: SDDvr_ParseYamlFile

contains

!> Load and parse a YAML-format SubDyn driver input file, filling each InitInp field
!! passed in as its own dummy argument (see the module header for why), plus the
!! applied-loads table as parallel plain arrays (see the module header for why).
subroutine SDDvr_ParseYamlFile(DvrFileName, Echo, Gravity, WtrDpth, SDInputFile, OutRootName, &
                                NSteps, TimeInterval, nTP, TP_RefPoint, SubRotateZ, &
                                InputsMod, InputsFile, SDin, &
                                uTPInSteady, uDotTPInSteady, uDotDotTPInSteady, &
                                nAppliedLoads, ALJointID, ALSteadyLoad, ALUnsteadyFile, &
                                ErrStat, ErrMsg)
   character(*),             intent(in   ) :: DvrFileName        !< the .yaml driver input file
   logical,                  intent(  out) :: Echo
   real(ReKi),               intent(  out) :: Gravity
   real(ReKi),               intent(  out) :: WtrDpth
   character(*),             intent(  out) :: SDInputFile
   character(*),             intent(  out) :: OutRootName
   integer(IntKi),           intent(  out) :: NSteps
   real(DbKi),               intent(  out) :: TimeInterval
   integer(IntKi),           intent(  out) :: nTP
   real(ReKi), allocatable,  intent(  out) :: TP_RefPoint(:,:)   !< (3,nTP): x/y/z rows
   real(ReKi),               intent(  out) :: SubRotateZ
   integer(IntKi),           intent(  out) :: InputsMod
   character(*),             intent(  out) :: InputsFile
   real(ReKi), allocatable,  intent(  out) :: SDin(:,:)          !< only filled when InputsMod==2
   real(ReKi), allocatable,  intent(  out) :: uTPInSteady(:)
   real(ReKi), allocatable,  intent(  out) :: uDotTPInSteady(:)
   real(ReKi), allocatable,  intent(  out) :: uDotDotTPInSteady(:)
   integer(IntKi),           intent(  out) :: nAppliedLoads
   integer(IntKi), allocatable, intent(  out) :: ALJointID(:)
   real(ReKi), allocatable,  intent(  out) :: ALSteadyLoad(:,:)  !< (6,nAppliedLoads): Fx,Fy,Fz,Mx,My,Mz
   character(1024), allocatable, intent(out) :: ALUnsteadyFile(:) !< '' when a row has none
   integer(IntKi),           intent(  out) :: ErrStat
   character(*),             intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SDDvr_ParseYamlFile'
   type(YamlDoc)            :: Doc
   character(1024)          :: PriPath
   integer(IntKi)           :: UnEc
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1
   Echo    = .false.   ! initialize for error handling (cleanup path)

   call WrScr( 'Opening SubDyn Driver input file:  '//trim(DvrFileName) )
   call GetPath( DvrFileName, PriPath )   ! Input files will be relative to the path where the driver input file is located.

   call Yaml_LoadFile(DvrFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'general:Echo', Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (Echo) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included), which supersedes the text path's line-by-line echo
      call OpenEcho(UnEc, trim(DvrFileName)//'.ech', TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for SubDyn driver input file: '//trim(DvrFileName)
      call Yaml_LoadFile(DvrFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   !----------------------------------------------------------------------------------
   ! environmental_conditions (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'environmental_conditions:Gravity', Gravity, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:WtrDpth', WtrDpth, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! subdyn (required) -- SDInputFile is an externally-referenced file (second-order
   ! rule: stays path-valued); tp_ref_points' length is nTP.
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'subdyn:SDInputFile', SDInputFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if ( PathIsRelative( SDInputFile ) ) SDInputFile = trim(PriPath)//trim(SDInputFile)

   call YamlGet(Doc, 'subdyn:OutRootName', OutRootName, TmpErrStat, TmpErrMsg, Default='')
   if (Failed()) return

   call YamlGet(Doc, 'subdyn:NSteps', NSteps, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'subdyn:TimeInterval', TimeInterval, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call ParseTPRefPoints(Doc, nTP, TP_RefPoint, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'subdyn:SubRotateZ', SubRotateZ, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! If no root is provided, use the SDInputFile -- mirrors SubDyn_Driver.f90:466-468
   if (len_trim(OutRootName) == 0) then
      call GetRoot(SDInputFile, OutRootName)
   end if
   if ( PathIsRelative( OutRootName ) ) OutRootName = trim(PriPath)//trim(OutRootName)

   !----------------------------------------------------------------------------------
   ! inputs (required) -- InputsFile is an externally-referenced file (second-order
   ! rule: stays path-valued); its own referenced time-series table (SDin) is read
   ! here, exactly as the text path (SubDyn_Driver.f90:441-443), when InputsMod==2.
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'inputs:InputsMod', InputsMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'inputs:InputsFile', InputsFile, TmpErrStat, TmpErrMsg, Default='')
   if (Failed()) return
   if ( PathIsRelative( InputsFile ) ) InputsFile = trim(PriPath)//trim(InputsFile)

   if (InputsMod == 2) then
      call ReadDelimFile(InputsFile, 1_IntKi+18_IntKi*nTP, SDin, TmpErrStat, TmpErrMsg, 0_IntKi, PriPath)
      if (Failed()) return
   end if

   !----------------------------------------------------------------------------------
   ! steady_state_inputs -- present (and read) only when InputsMod == 1, exactly
   ! mirroring the text path's IF/ELSE branch (SubDyn_Driver.f90:429-440).
   !----------------------------------------------------------------------------------
   call ParseSteadyStateInputs(Doc, InputsMod, nTP, uTPInSteady, uDotTPInSteady, uDotDotTPInSteady, &
                                TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! loads (optional; default empty -- a modern-format concession, no legacy warning)
   !----------------------------------------------------------------------------------
   call ParseAppliedLoads(Doc, nAppliedLoads, ALJointID, ALSteadyLoad, ALUnsteadyFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return

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

end subroutine SDDvr_ParseYamlFile

!> subdyn:tp_ref_points -- a list of {x,y,z} row mappings; its length is nTP (counts
!! derive from list lengths). Stored (3,nTP): row 1 = x, row 2 = y, row 3 = z, matching
!! the text path's three separate ReadAry calls into TP_RefPoint(1,:)/(2,:)/(3,:)
!! (SubDyn_Driver.f90:416-418).
subroutine ParseTPRefPoints(Doc, nTP, TP_RefPoint, ErrStat, ErrMsg)
   type(YamlDoc),           intent(inout) :: Doc
   integer(IntKi),          intent(  out) :: nTP
   real(ReKi), allocatable, intent(  out) :: TP_RefPoint(:,:)
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SDDvr_ParseTPRefPoints'
   integer(IntKi)           :: iSeq, iRow, i
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'subdyn:tp_ref_points', iSeq, TmpErrStat, TmpErrMsg); if (Failed()) return

   nTP = int(Yaml_NumChildren(Doc, iSeq))
   if (nTP < 1) then
      call SetErrStat(ErrID_Fatal, 'subdyn:tp_ref_points must list at least 1 transition piece.', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if

   call AllocAry(TP_RefPoint, 3, nTP, 'TP_RefPoint', TmpErrStat, TmpErrMsg); if (Failed()) return

   do i = 1, nTP
      iRow = Yaml_Child(Doc, iSeq, i)
      call YamlGet(Doc, 'x', TP_RefPoint(1,i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'y', TP_RefPoint(2,i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'z', TP_RefPoint(3,i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseTPRefPoints

!> steady_state_inputs:uTPInSteady/uDotTPInSteady/uDotDotTPInSteady -- each a flat list
!! of 6*nTP values (TP1's 6 components, then TP2's, ...), required only when
!! InputsMod==1 (SubDyn_Driver.f90:429-440); zeroed without a lookup otherwise.
subroutine ParseSteadyStateInputs(Doc, InputsMod, nTP, uTPInSteady, uDotTPInSteady, uDotDotTPInSteady, ErrStat, ErrMsg)
   type(YamlDoc),           intent(inout) :: Doc
   integer(IntKi),          intent(in   ) :: InputsMod
   integer(IntKi),          intent(in   ) :: nTP
   real(ReKi), allocatable, intent(  out) :: uTPInSteady(:)
   real(ReKi), allocatable, intent(  out) :: uDotTPInSteady(:)
   real(ReKi), allocatable, intent(  out) :: uDotDotTPInSteady(:)
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SDDvr_ParseSteadyStateInputs'
   integer(IntKi)           :: nVal
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   nVal = 6_IntKi * nTP

   if (InputsMod == 1) then
      call YamlGet(Doc, 'steady_state_inputs:uTPInSteady', uTPInSteady, TmpErrStat, TmpErrMsg); if (Failed()) return
      call YamlGet(Doc, 'steady_state_inputs:uDotTPInSteady', uDotTPInSteady, TmpErrStat, TmpErrMsg); if (Failed()) return
      call YamlGet(Doc, 'steady_state_inputs:uDotDotTPInSteady', uDotDotTPInSteady, TmpErrStat, TmpErrMsg); if (Failed()) return
      if (size(uTPInSteady) /= nVal .or. size(uDotTPInSteady) /= nVal .or. size(uDotDotTPInSteady) /= nVal) then
         call SetErrStat(ErrID_Fatal, 'steady_state_inputs entries must each list exactly 6*nTP ('// &
            trim(Num2LStr(nVal))//') values.', ErrStat, ErrMsg, RoutineName)
         return
      end if
   else
      call AllocAry(uTPInSteady,       nVal, 'uTPInSteady',       TmpErrStat, TmpErrMsg); if (Failed()) return
      call AllocAry(uDotTPInSteady,    nVal, 'uDotTPInSteady',    TmpErrStat, TmpErrMsg); if (Failed()) return
      call AllocAry(uDotDotTPInSteady, nVal, 'uDotDotTPInSteady', TmpErrStat, TmpErrMsg); if (Failed()) return
      uTPInSteady       = 0.0_ReKi
      uDotTPInSteady    = 0.0_ReKi
      uDotDotTPInSteady = 0.0_ReKi
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseSteadyStateInputs

!> loads:applied_loads -- a list of row mappings {JointID, Fx, Fy, Fz, Mx, My, Mz,
!! UnsteadyFile (optional, default '')}, returned as parallel plain arrays (see the
!! module header for why: ALoadType, their eventual element type, is program-local).
!! Missing section -> zero rows (a modern-format concession, no legacy warning, same
!! convention as SubDyn_Yaml.f90's cable_properties/guyan_damping).
subroutine ParseAppliedLoads(Doc, nAppliedLoads, ALJointID, ALSteadyLoad, ALUnsteadyFile, ErrStat, ErrMsg)
   type(YamlDoc),                intent(inout) :: Doc
   integer(IntKi),                intent(  out) :: nAppliedLoads
   integer(IntKi), allocatable,   intent(  out) :: ALJointID(:)
   real(ReKi), allocatable,       intent(  out) :: ALSteadyLoad(:,:)
   character(1024), allocatable,  intent(  out) :: ALUnsteadyFile(:)
   integer(IntKi),                intent(  out) :: ErrStat
   character(*),                  intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SDDvr_ParseAppliedLoads'
   integer(IntKi)           :: iSeq, iRow, i
   logical                  :: SecFound
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'loads:applied_loads', iSeq, TmpErrStat, TmpErrMsg, Found=SecFound); if (Failed()) return

   if (.not. SecFound) then
      nAppliedLoads = 0
      allocate(ALJointID(0), ALUnsteadyFile(0))
      allocate(ALSteadyLoad(6,0))
      return
   end if

   nAppliedLoads = int(Yaml_NumChildren(Doc, iSeq))
   allocate(ALJointID(nAppliedLoads), ALUnsteadyFile(nAppliedLoads))
   allocate(ALSteadyLoad(6,nAppliedLoads))

   do i = 1, nAppliedLoads
      iRow = Yaml_Child(Doc, iSeq, i)
      call YamlGet(Doc, 'JointID', ALJointID(i),      TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Fx',      ALSteadyLoad(1,i),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Fy',      ALSteadyLoad(2,i),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Fz',      ALSteadyLoad(3,i),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Mx',      ALSteadyLoad(4,i),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'My',      ALSteadyLoad(5,i),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Mz',      ALSteadyLoad(6,i),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'UnsteadyFile', ALUnsteadyFile(i), TmpErrStat, TmpErrMsg, Default='', From=iRow); if (Failed()) return
      call WrScr('    Applied Load: '//trim(Num2LStr(ALJointID(i))))
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseAppliedLoads

end module SubDyn_Driver_Yaml
