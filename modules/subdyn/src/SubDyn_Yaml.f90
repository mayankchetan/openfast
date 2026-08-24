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
!> Reader for the YAML form of the SubDyn primary input file. Fills the same Init (SD_InitType)
!! and p (SD_ParameterType) fields that SD_Input (the text path, in SubDyn.f90) fills, up
!! through the interface-joints table, the members/property-set/cosine-matrix/concentrated-
!! mass tables, and the output section -- everything except the small set of cross-cutting
!! post-processing steps (SubRotate, CheckBCs, the on-the-fly SSI-file reads, isFloating,
!! CheckIntf) that SD_Input itself performs identically for both formats, once, after the
!! format-specific reading branch (SubRotate and ReadSSIfile live in SubDyn.f90, which this
!! module is USEd BY at the funnel call site, so they cannot be called back into here without
!! creating a circular module dependency -- the same reasoning documented in BeamDyn_Yaml).
!!
!! Schema: top-level keys mirror the text file's section banners: simulation_control,
!! fea_and_craig_bampton, guyan_damping (optional), initial_rigid_body_position,
!! structure_joints, base_reaction_joints, interface_joints, members,
!! member_cross_section_properties, cable_properties, rigid_link_properties,
!! spring_element_properties, member_direction_cosine_matrices, concentrated_masses, output,
!! member_output_list, outputs.
!!
!! Counts derive from list lengths per the project-wide rule: NJoints, nNodes_C (reactions),
!! nNodes_I (interfaces), NMembers, NPropSetsBC/BR/X/C/R/S, NCOSMs, nCMass, NMOutputs, NumOuts
!! are never separate YAML keys.
!!
!! Pure-numeric fixed-column tables (joints, the three beam-section-property tables, the
!! rigid-link and spring-element property tables, and the cosine-matrix table) are written as
!! a plain list-of-lists ("rows"), one row per YAML_Get call -- the same shape BeamDyn's
!! key_points table uses -- since their column layout never varies. Tables whose rows carry
!! optional/typed fields (reactions with an optional SSIfile path, members whose last column's
!! meaning depends on MType, cable properties with an optional CtrlChannel, concentrated
!! masses with optional off-diagonal terms) are written as a list of row mappings keyed by
!! column name, mirroring MoorDyn/HydroDyn's table handling.
!!
!! Deliberate simplifications versus the text format (documented, not silent):
!!  - structure_joints:joints is always the current 9-column format; the legacy 4-column
!!    all-cantilever form has no YAML equivalent (LegacyFormat is always .false. for YAML).
!!  - interface_joints entries never expose per-DOF free/fixed flags: the text reader already
!!    fatally rejects anything other than all six DOF being "locked to the TP" (the only
!!    configuration SubDyn currently implements), so the YAML schema only exposes JointID and
!!    the optional TPIdx (transition-piece index, for floating multi-TP models); all six DOF
!!    flags are always written as 1 (leader) internally, exactly the only value that survives
!!    validation on the text path.
!!  - the "OutCBModes/OutFEMModes missing -> legacy fallback" and "GuyanDampMod block missing
!!    -> legacy fallback" tolerances are text-format-only concessions to old decks; the YAML
!!    schema requires OutCBModes/OutFEMModes explicitly, and treats a missing guyan_damping
!!    section as the modern "no Guyan damping" case (GuyanDampMod=0), without a warning.
!!
!! Second-order files: none (SubDyn's primary file has no second-order file references other
!! than each reaction joint's optional SSIfile, which stays a path, resolved relative to the
!! primary file exactly like the text path -- resolution happens in SD_Input's shared
!! post-processing, not here).
module SubDyn_Yaml

   use NWTC_Library
   use SubDyn_Types
   use SD_FEM
   use FEM, only: Determinant, FINDLOCI
   use YamlInput

   implicit none
   private

   public :: SD_ParseYamlFile
   public :: SD_ParseYamlFileInfo

   ! MaxOutPts mirrors the parameter defined in SubDyn_Output (this module is USEd BY
   ! SubDyn.f90 at the funnel call site in SD_Input, so it cannot in turn USE anything from
   ! SubDyn_Output without creating a circular module dependency; duplicated here instead,
   ! matching BeamDyn_Yaml's precedent for MaxOutPts/BeamDyn_Ver).
   integer(IntKi), parameter :: MaxOutPts = 21921

contains

!> Load and parse a YAML-format SubDyn primary input file.
subroutine SD_ParseYamlFile(SDInputFile, Init, p, TPIdxInput, ErrStat, ErrMsg)
   character(*),            intent(in   ) :: SDInputFile !< the .yaml primary input file
   type(SD_InitType),       intent(inout) :: Init
   type(SD_ParameterType),  intent(inout) :: p
   integer(IntKi), allocatable, intent(  out) :: TPIdxInput(:) !< raw (pre-offset) TP index per interface joint
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseYamlFile'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call Yaml_LoadFile(SDInputFile, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call ParseYamlDoc(Doc, Init, p, TPIdxInput, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! typo guard: anything never looked up is reported (warning severity)
   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine SD_ParseYamlFile

!> Parse YAML-format SubDyn input arriving as a FileInfoType -- the passed-data channel used
!! for inline module input from a YAML primary file (the glue code's inline SubFile handover
!! when CompSub selects SubDyn).
subroutine SD_ParseYamlFileInfo(FileInfoIn, Init, p, TPIdxInput, ErrStat, ErrMsg)
   type(FileInfoType),     intent(in   ) :: FileInfoIn !< YAML text lines with provenance
   type(SD_InitType),       intent(inout) :: Init
   type(SD_ParameterType),  intent(inout) :: p
   integer(IntKi), allocatable, intent(  out) :: TPIdxInput(:)
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseYamlFileInfo'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call Yaml_LoadFileInfo(FileInfoIn, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call ParseYamlDoc(Doc, Init, p, TPIdxInput, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine SD_ParseYamlFileInfo

!> Fill Init/p from a parsed document. Split from the two file-entry wrappers, mirroring the
!! project-wide ParseYamlDoc convention.
subroutine ParseYamlDoc(Doc, Init, p, TPIdxInput, ErrStat, ErrMsg)
   type(YamlDoc),           intent(inout) :: Doc
   type(SD_InitType),       intent(inout) :: Init
   type(SD_ParameterType),  intent(inout) :: p
   integer(IntKi), allocatable, intent(  out) :: TPIdxInput(:)
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseYamlDoc'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call ParseSimulationControl(Doc, Init, p, TmpErrStat, TmpErrMsg);       if (Failed()) return
   call ParseCraigBampton(Doc, Init, p, TmpErrStat, TmpErrMsg);            if (Failed()) return
   call ParseGuyanDamping(Doc, Init, TmpErrStat, TmpErrMsg);               if (Failed()) return
   call ParseRigidBodyPosition(Doc, Init, TmpErrStat, TmpErrMsg);          if (Failed()) return
   call ParseJoints(Doc, Init, TmpErrStat, TmpErrMsg);                    if (Failed()) return
   call ParseReactions(Doc, Init, p, TmpErrStat, TmpErrMsg);               if (Failed()) return
   call ParseInterfaces(Doc, p, TPIdxInput, TmpErrStat, TmpErrMsg);        if (Failed()) return
   call ParseMembers(Doc, Init, p, TmpErrStat, TmpErrMsg);                 if (Failed()) return
   call ParsePropSets(Doc, Init, TmpErrStat, TmpErrMsg);                   if (Failed()) return
   call ParseCosineMatrices(Doc, Init, TmpErrStat, TmpErrMsg);             if (Failed()) return
   call ParseConcentratedMasses(Doc, Init, TmpErrStat, TmpErrMsg);         if (Failed()) return
   call ParseOutput(Doc, Init, p, TmpErrStat, TmpErrMsg);                  if (Failed()) return
   call ParseMemberOutputList(Doc, Init, p, TmpErrStat, TmpErrMsg);        if (Failed()) return
   call ParseOutList(Doc, Init, p, TmpErrStat, TmpErrMsg);                 if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseYamlDoc

!----------------------------------------------------------------------------------------
! section parsers
!----------------------------------------------------------------------------------------

!> simulation_control: Echo (consumed by the caller before this is reached; re-read here so
!! it is marked "used" and not flagged by the typo guard), SDdeltaT ("default" or a positive
!! number), IntMethod, SttcSolve (a number or a boolean, exactly like the text format's
!! is_numeric/is_logical dispatch).
subroutine ParseSimulationControl(Doc, Init, p, ErrStat, ErrMsg)
   type(YamlDoc),          intent(inout) :: Doc
   type(SD_InitType),      intent(inout) :: Init
   type(SD_ParameterType), intent(inout) :: p
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseSimulationControl'
   character(64)           :: ChStr
   logical                 :: Echo, DummyBool
   real(ReKi)               :: DummyFloat
   integer(IntKi)           :: IOS
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'simulation_control:Echo', Echo, TmpErrStat, TmpErrMsg, Default=.false.); if (Failed()) return

   ! Default='DEFAULT' lets YamlGet's generic "default" keyword handling return the
   ! literal text back (rather than fatally erroring for lack of a registered
   ! default), exactly like BeamDyn_Yaml.f90's DTBeam -- the manual
   ! Conv2UC/'DEFAULT' check below then dispatches on that text as usual.
   call YamlGet(Doc, 'simulation_control:SDdeltaT', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if (trim(ChStr) == 'DEFAULT') then
      p%SDdeltaT = Init%DT
   else
      read(ChStr, *, iostat=IOS) p%SDdeltaT
      call CheckIOS(IOS, '', 'SDdeltaT', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
      if (p%SDdeltaT <= 0) then
         call SetErrStat(ErrID_Fatal, 'simulation_control:SDdeltaT must be greater than or equal to 0.', ErrStat, ErrMsg, RoutineName)
         return
      end if
   end if

   call YamlGet(Doc, 'simulation_control:IntMethod', p%IntMethod, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (p%IntMethod < 1 .or. p%IntMethod > 4) then
      call SetErrStat(ErrID_Fatal, 'simulation_control:IntMethod must be 1 through 4.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call YamlGet(Doc, 'simulation_control:SttcSolve', ChStr, TmpErrStat, TmpErrMsg); if (Failed()) return
   p%SttcSolve = idSIM_None
   if (is_numeric(trim(ChStr), DummyFloat)) then
      p%SttcSolve = int(DummyFloat)
   else if (is_logical(trim(ChStr), DummyBool)) then
      if (DummyBool) p%SttcSolve = idSIM_Full
   else
      call SetErrStat(ErrID_Fatal, 'simulation_control:SttcSolve should be an integer or a logical, received: '// &
         trim(ChStr), ErrStat, ErrMsg, RoutineName)
      return
   end if
   if (.not. any(idSIM_Valid == p%SttcSolve)) then
      call SetErrStat(ErrID_Fatal, 'Invalid value entered for simulation_control:SttcSolve', ErrStat, ErrMsg, RoutineName)
      return
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseSimulationControl

!> fea_and_craig_bampton: FEMMod, NDiv, Nmodes (-> p%nDOFM/Init%CBMod), JDampings (a list; its
!! length must be 1 (broadcast to every retained mode) or exactly Nmodes when Nmodes>0, and
!! must be absent/empty when Nmodes==0 -- a deliberate tightening of the text path's "fewer
!! values than Nmodes -> repeat the last one" tolerance).
subroutine ParseCraigBampton(Doc, Init, p, ErrStat, ErrMsg)
   type(YamlDoc),          intent(inout) :: Doc
   type(SD_InitType),      intent(inout) :: Init
   type(SD_ParameterType), intent(inout) :: p
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'SD_ParseCraigBampton'
   real(ReKi), allocatable     :: TmpAry(:)
   integer(IntKi)              :: TmpErrStat
   character(ErrMsgLen)        :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'fea_and_craig_bampton:FEMMod', Init%FEMMod, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'fea_and_craig_bampton:NDiv',   Init%NDiv,   TmpErrStat, TmpErrMsg); if (Failed()) return

   if (Init%FEMMod < 0 .or. Init%FEMMod > 4) then
      call SetErrStat(ErrID_Fatal, 'fea_and_craig_bampton:FEMMod must be 0, 1, 2, or 3.', ErrStat, ErrMsg, RoutineName); return
   end if
   if (Init%NDiv < 1) then
      call SetErrStat(ErrID_Fatal, 'fea_and_craig_bampton:NDiv must be a positive integer', ErrStat, ErrMsg, RoutineName); return
   end if
   if (Init%FEMMod == 2) then
      call SetErrStat(ErrID_Fatal, 'FEMMod = 2 (tapered Euler-Bernoulli) not implemented', ErrStat, ErrMsg, RoutineName); return
   end if
   if (Init%FEMMod == 4) then
      call SetErrStat(ErrID_Fatal, 'FEMMod = 4 (tapered Timoshenko) not implemented', ErrStat, ErrMsg, RoutineName); return
   end if

   call YamlGet(Doc, 'fea_and_craig_bampton:Nmodes', p%nDOFM, TmpErrStat, TmpErrMsg); if (Failed()) return
   Init%CBMod = (p%nDOFM >= 0)

   call YamlGetJDampings(Doc, TmpAry, TmpErrStat, TmpErrMsg); if (Failed()) return

   if (Init%CBMod) then
      if (p%nDOFM > 0) then
         call AllocAry(Init%JDampings, p%nDOFM, 'JDamping', TmpErrStat, TmpErrMsg); if (Failed()) return
         if (size(TmpAry) == 1) then
            Init%JDampings = TmpAry(1)
         else if (size(TmpAry) == p%nDOFM) then
            Init%JDampings = TmpAry
         else
            call SetErrStat(ErrID_Fatal, 'fea_and_craig_bampton:JDampings must list either 1 value (applied to all '// &
               'retained modes) or exactly Nmodes ('//trim(Num2LStr(p%nDOFM))//') values; found '// &
               trim(Num2LStr(size(TmpAry)))//'.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      else
         call AllocAry(Init%JDampings, 1, 'JDamping', TmpErrStat, TmpErrMsg); if (Failed()) return
         Init%JDampings = 0.0_ReKi
      end if
   else
      call AllocAry(Init%JDampings, 1, 'JDamping', TmpErrStat, TmpErrMsg); if (Failed()) return
      if (size(TmpAry) /= 1) then
         call SetErrStat(ErrID_Fatal, 'fea_and_craig_bampton:JDampings must list exactly 1 value when Nmodes < 0 '// &
            '(all modes retained).', ErrStat, ErrMsg, RoutineName)
         return
      end if
      Init%JDampings(1) = TmpAry(1)
   end if

   if ((p%nDOFM > 0) .or. (.not. Init%CBMod)) then
      Init%JDampings = Init%JDampings / 100.0_ReKi
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseCraigBampton

!> Fetch fea_and_craig_bampton:JDampings tolerating a missing/empty key (Nmodes==0 case,
!! where the text path skips the line entirely).
subroutine YamlGetJDampings(Doc, TmpAry, ErrStat, ErrMsg)
   type(YamlDoc),           intent(inout) :: Doc
   real(ReKi), allocatable, intent(  out) :: TmpAry(:)
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   logical :: WasFound

   call YamlGet(Doc, 'fea_and_craig_bampton:JDampings', TmpAry, ErrStat, ErrMsg, Found=WasFound)
   if (ErrStat >= AbortErrLev) return
   if (.not. WasFound) allocate(TmpAry(0))
end subroutine YamlGetJDampings

!> guyan_damping (optional): absent -> GuyanDampMod=idGuyanDamp_None (the modern "no Guyan
!! damping" case; no legacy warning, unlike the text path's tolerate-missing-lines fallback).
subroutine ParseGuyanDamping(Doc, Init, ErrStat, ErrMsg)
   type(YamlDoc),      intent(inout) :: Doc
   type(SD_InitType),  intent(inout) :: Init
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseGuyanDamping'
   real(ReKi), allocatable  :: TmpMat(:,:)
   real(ReKi), allocatable  :: TmpAry(:)
   logical                  :: SecFound
   integer(IntKi)           :: iSec
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'guyan_damping', iSec, TmpErrStat, TmpErrMsg, Found=SecFound); if (Failed()) return
   if (.not. SecFound) then
      Init%GuyanDampMod = idGuyanDamp_None
      Init%RayleighDamp = 0.0_ReKi
      Init%GuyanDampSize = 0
      call AllocAry(Init%GuyanDampMat, 0, 0, 'GuyanDampMat', TmpErrStat, TmpErrMsg); if (Failed()) return
      return
   end if

   call YamlGet(Doc, 'guyan_damping:GuyanDampMod', Init%GuyanDampMod, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'guyan_damping:RayleighDamp', TmpAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (size(TmpAry) /= 2) then
      call SetErrStat(ErrID_Fatal, 'guyan_damping:RayleighDamp must list exactly 2 values.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   Init%RayleighDamp = TmpAry
   call YamlGet(Doc, 'guyan_damping:GuyanDampSize', Init%GuyanDampSize, TmpErrStat, TmpErrMsg); if (Failed()) return

   if (.not. any(idGuyanDamp_Valid == Init%GuyanDampMod)) then
      call SetErrStat(ErrID_Fatal, 'Invalid value entered for guyan_damping:GuyanDampMod', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call YamlGet(Doc, 'guyan_damping:GuyanDampMat', TmpMat, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (size(TmpMat,1) /= Init%GuyanDampSize .or. size(TmpMat,2) /= Init%GuyanDampSize) then
      call SetErrStat(ErrID_Fatal, 'guyan_damping:GuyanDampMat must be a '//trim(Num2LStr(Init%GuyanDampSize))//'x'// &
         trim(Num2LStr(Init%GuyanDampSize))//' matrix (GuyanDampSize x GuyanDampSize).', ErrStat, ErrMsg, RoutineName)
      return
   end if
   call AllocAry(Init%GuyanDampMat, Init%GuyanDampSize, Init%GuyanDampSize, 'GuyanDampMat', TmpErrStat, TmpErrMsg); if (Failed()) return
   Init%GuyanDampMat = TmpMat

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseGuyanDamping

!> initial_rigid_body_position:qR0 -- 6 values (3 translations, 3 rotations in degrees,
!! converted to radians on read exactly like the text path).
subroutine ParseRigidBodyPosition(Doc, Init, ErrStat, ErrMsg)
   type(YamlDoc),      intent(inout) :: Doc
   type(SD_InitType),  intent(inout) :: Init
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseRigidBodyPosition'
   real(R8Ki), allocatable  :: TmpAry(:)
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'initial_rigid_body_position:qR0', TmpAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (size(TmpAry) /= 6) then
      call SetErrStat(ErrID_Fatal, 'initial_rigid_body_position:qR0 must list exactly 6 values '// &
         '(3 translations, 3 rotations in degrees).', ErrStat, ErrMsg, RoutineName)
      return
   end if
   Init%qR0 = TmpAry
   Init%qR0(4:6) = Init%qR0(4:6) * D2R

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseRigidBodyPosition

!> structure_joints:joints -- a plain NJoints x 9 numeric table (JointID, JointXss, JointYss,
!! JointZss, JointType, JointDirX, JointDirY, JointDirZ, JointStiff). Always the modern
!! 9-column format; SubRotate is applied later, in SD_Input's shared post-processing.
subroutine ParseJoints(Doc, Init, ErrStat, ErrMsg)
   type(YamlDoc),      intent(inout) :: Doc
   type(SD_InitType),  intent(inout) :: Init
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseJoints'
   real(ReKi), allocatable  :: TmpMat(:,:)
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'structure_joints:joints', TmpMat, TmpErrStat, TmpErrMsg); if (Failed()) return

   Init%NJoints = size(TmpMat, 1)
   if (Init%NJoints < 2) then
      call SetErrStat(ErrID_Fatal, 'structure_joints:joints must list at least 2 joints (NJoints must be greater than 1)', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if
   if (size(TmpMat, 2) /= JointsCol) then
      call SetErrStat(ErrID_Fatal, 'structure_joints:joints rows must each have '//trim(Num2LStr(JointsCol))// &
         ' entries (JointID, JointXss, JointYss, JointZss, JointType, JointDirX, JointDirY, JointDirZ, JointStiff).', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if

   call AllocAry(Init%Joints, Init%NJoints, JointsCol, 'Joints', TmpErrStat, TmpErrMsg); if (Failed()) return
   Init%Joints = TmpMat

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseJoints

!> base_reaction_joints:reactions -- a list of row mappings {JointID, Rctx, Rcty, Rctz,
!! Rctxss, Rctyss, Rctzss, SSIfile (optional)}. The DOF flags stay raw 0/1/2 values, exactly
!! as the text path's ReadIAryFromStrSD leaves them for the shared CheckBCs call to remap.
!! SSIfile stays a path string; it is resolved and read in SD_Input's shared post-processing.
subroutine ParseReactions(Doc, Init, p, ErrStat, ErrMsg)
   type(YamlDoc),          intent(inout) :: Doc
   type(SD_InitType),      intent(inout) :: Init
   type(SD_ParameterType), intent(inout) :: p
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseReactions'
   integer(IntKi)           :: iSeq, iRow, i, k
   logical                  :: HasSSIfile
   integer(IntKi)           :: DOF(6)
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'base_reaction_joints:reactions', iSeq, TmpErrStat, TmpErrMsg); if (Failed()) return

   p%nNodes_C = int(Yaml_NumChildren(Doc, iSeq))

   call AllocAry(p%Nodes_C, p%nNodes_C, ReactCol, 'Reacts', TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(Init%SSIfile, p%nNodes_C, 'SSIFile', TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(Init%SSIK, 21, p%nNodes_C, 'SSIK', TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(Init%SSIM, 21, p%nNodes_C, 'SSIM', TmpErrStat, TmpErrMsg); if (Failed()) return
   Init%SSIfile(:) = ''
   Init%SSIK = 0.0_ReKi
   Init%SSIM = 0.0_ReKi

   do i = 1, p%nNodes_C
      iRow = Yaml_Child(Doc, iSeq, i)
      call YamlGet(Doc, 'JointID', p%Nodes_C(i,1), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Rctx',   DOF(1), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Rcty',   DOF(2), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Rctz',   DOF(3), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Rctxss', DOF(4), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Rctyss', DOF(5), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Rctzss', DOF(6), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      do k = 1, 6
         p%Nodes_C(i, k+1) = DOF(k)
      end do

      call YamlGet(Doc, 'SSIfile', Init%SSIfile(i), TmpErrStat, TmpErrMsg, Found=HasSSIfile, From=iRow); if (Failed()) return
      if (.not. HasSSIfile) Init%SSIfile(i) = ''
   end do

   if (p%nNodes_C > Init%NJoints) then
      call SetErrStat(ErrID_Fatal, 'base_reaction_joints:reactions must list fewer entries than the number of joints '// &
         '(NReact must be less than number of joints)', ErrStat, ErrMsg, RoutineName)
      return
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseReactions

!> interface_joints:interfaces -- a list of row mappings {JointID, TPIdx (optional, default
!! 1)}. All six DOF flags are always written as 1 (leader/locked-to-TP): the only
!! configuration the text path accepts (see the module header).
subroutine ParseInterfaces(Doc, p, TPIdxInput, ErrStat, ErrMsg)
   type(YamlDoc),          intent(inout) :: Doc
   type(SD_ParameterType), intent(inout) :: p
   integer(IntKi), allocatable, intent(  out) :: TPIdxInput(:)
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseInterfaces'
   integer(IntKi)           :: iSeq, iRow, i, k
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'interface_joints:interfaces', iSeq, TmpErrStat, TmpErrMsg); if (Failed()) return

   p%nNodes_I = int(Yaml_NumChildren(Doc, iSeq))

   call AllocAry(p%Nodes_I,  p%nNodes_I, InterfCol, 'Interf', TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(p%TPIdx,    p%nNodes_I,            'TPIdx',  TmpErrStat, TmpErrMsg); if (Failed()) return
   allocate(TPIdxInput(p%nNodes_I))
   p%Nodes_I(:,:) = 1
   p%TPIdx(:)     = -1

   do i = 1, p%nNodes_I
      iRow = Yaml_Child(Doc, iSeq, i)
      call YamlGet(Doc, 'JointID', p%Nodes_I(i,1), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      do k = 2, InterfCol
         p%Nodes_I(i,k) = 1
      end do
      call YamlGet(Doc, 'TPIdx', TPIdxInput(i), TmpErrStat, TmpErrMsg, Default=1_IntKi, From=iRow); if (Failed()) return
   end do

   if (p%nNodes_I < 0) then
      call SetErrStat(ErrID_Fatal, 'interface_joints:interfaces: NInterf must be non-negative.', ErrStat, ErrMsg, RoutineName)
      return
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseInterfaces

!> members:members -- a list of row mappings {MemberID, MJointID1, MJointID2, MPropSetID1,
!! MPropSetID2, MType, MSpin (required for beam-type members; the member's spin about its own
!! axis, degrees), COSMID (required for any other/spring-type member), MDivSize (optional,
!! beam-type members only: maximum element length in m, overrides NDiv)}. MType accepts an
!! integer (idMemberBeamCirc=1, idMemberBeamRect=-1, idMemberCable=2, idMemberRigid=3,
!! idMemberBeamArb=4, or any other integer for a spring member referencing COSMID) or the
!! text format's "1c"/"1r" spellings for the circular/rectangular beam types.
subroutine ParseMembers(Doc, Init, p, ErrStat, ErrMsg)
   type(YamlDoc),          intent(inout) :: Doc
   type(SD_InitType),      intent(inout) :: Init
   type(SD_ParameterType), intent(inout) :: p
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseMembers'
   integer(IntKi)           :: iSeq, iRow, i
   character(16)            :: MTypeStr
   logical                  :: HasSpin, HasCOSMID, HasDivSize, bInteger
   real(ReKi)               :: MTypeFloat
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'members:members', iSeq, TmpErrStat, TmpErrMsg); if (Failed()) return

   p%NMembers = int(Yaml_NumChildren(Doc, iSeq))
   if (p%NMembers == 0) then
      call SetErrStat(ErrID_Fatal, 'members:members must list at least one SubDyn member.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call AllocAry(Init%Members,    p%NMembers, MembersCol, 'Members',    TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(Init%MemberSpin, p%NMembers,             'MemberSpin', TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(Init%MemberDivSize, p%NMembers,          'MemberDivSize', TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(Init%MemberNDiv,    p%NMembers,          'MemberNDiv', TmpErrStat, TmpErrMsg); if (Failed()) return
   Init%Members(:,:)     = 0
   Init%MemberSpin(:)    = 0.0_ReKi
   Init%MemberDivSize(:) = 0.0_ReKi
   Init%MemberNDiv(:)    = 0_IntKi

   do i = 1, p%NMembers
      iRow = Yaml_Child(Doc, iSeq, i)
      call YamlGet(Doc, 'MemberID',     Init%Members(i,1), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'MJointID1',    Init%Members(i,2), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'MJointID2',    Init%Members(i,3), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'MPropSetID1',  Init%Members(i,4), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'MPropSetID2',  Init%Members(i,5), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return

      call YamlGet(Doc, 'MType', MTypeStr, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call Conv2UC(MTypeStr)
      select case (trim(MTypeStr))
      case ('1C')
         Init%Members(i,6) = idMemberBeamCirc
      case ('1R')
         Init%Members(i,6) = idMemberBeamRect
      case default
         bInteger = is_integer(trim(MTypeStr), Init%Members(i,6))
         if (.not. bInteger) then
            call SetErrStat(ErrID_Fatal, 'members:members entry '//trim(Num2LStr(i))// &
               ': MType must be an integer or "1c"/"1r". Received: "'//trim(MTypeStr)//'"', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end select

      if (Init%Members(i,6) == idMemberBeamCirc .or. Init%Members(i,6) == idMemberBeamRect .or. &
          Init%Members(i,6) == idMemberBeamArb) then
         call YamlGet(Doc, 'MSpin', Init%MemberSpin(i), TmpErrStat, TmpErrMsg, Found=HasSpin, From=iRow); if (Failed()) return
         if (.not. HasSpin) then
            call SetErrStat(ErrID_Fatal, 'members:members entry '//trim(Num2LStr(i))// &
               ': MSpin is required for beam-type members.', ErrStat, ErrMsg, RoutineName)
            return
         end if
         Init%MemberSpin(i) = Init%MemberSpin(i) * D2R
         Init%Members(i,7) = -1
         Init%MemberNDiv(i) = Init%NDiv
         ! Optional per-member maximum element length (overrides NDiv for this beam member).
         call YamlGet(Doc, 'MDivSize', Init%MemberDivSize(i), TmpErrStat, TmpErrMsg, Found=HasDivSize, From=iRow); if (Failed()) return
         if (HasDivSize .and. Init%MemberDivSize(i) <= 0.0_ReKi) then
            call SetErrStat(ErrID_Fatal, 'members:members entry '//trim(Num2LStr(i))// &
               ': MDivSize must be greater than zero.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      else if (Init%Members(i,6) == idMemberCable .or. Init%Members(i,6) == idMemberRigid) then
         Init%MemberSpin(i) = 0.0_ReKi
         Init%Members(i,7) = -1
         Init%MemberNDiv(i) = 1
      else
         Init%MemberSpin(i) = 0.0_ReKi
         call YamlGet(Doc, 'COSMID', Init%Members(i,7), TmpErrStat, TmpErrMsg, Found=HasCOSMID, From=iRow); if (Failed()) return
         if (.not. HasCOSMID) then
            call SetErrStat(ErrID_Fatal, 'members:members entry '//trim(Num2LStr(i))// &
               ': COSMID is required for spring-type members (MType not a beam/cable/rigid type).', ErrStat, ErrMsg, RoutineName)
            return
         end if
         Init%MemberNDiv(i) = 1
      end if
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseMembers

!> member_cross_section_properties:circular_beam_props/rectangular_beam_props/
!! arbitrary_beam_props (plain numeric tables), cable_properties:cables (row mappings, with
!! optional CtrlChannel), rigid_link_properties:rigid_props and
!! spring_element_properties:spring_props (plain numeric tables).
subroutine ParsePropSets(Doc, Init, ErrStat, ErrMsg)
   type(YamlDoc),      intent(inout) :: Doc
   type(SD_InitType),  intent(inout) :: Init
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParsePropSets'
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call ParseNumericTable(Doc, 'member_cross_section_properties:circular_beam_props', PropSetsBCCol, &
      Init%PropSetsBC, Init%NPropSetsBC, TmpErrStat, TmpErrMsg); if (Failed()) return
   call ParseNumericTable(Doc, 'member_cross_section_properties:rectangular_beam_props', PropSetsBRCol, &
      Init%PropSetsBR, Init%NPropSetsBR, TmpErrStat, TmpErrMsg); if (Failed()) return
   call ParseNumericTable(Doc, 'member_cross_section_properties:arbitrary_beam_props', PropSetsXCol, &
      Init%PropSetsX, Init%NPropSetsX, TmpErrStat, TmpErrMsg); if (Failed()) return
   call ParseCableProperties(Doc, Init, TmpErrStat, TmpErrMsg); if (Failed()) return
   call ParseNumericTable(Doc, 'rigid_link_properties:rigid_props', PropSetsRCol, &
      Init%PropSetsR, Init%NPropSetsR, TmpErrStat, TmpErrMsg); if (Failed()) return
   call ParseNumericTable(Doc, 'spring_element_properties:spring_props', PropSetsSCol, &
      Init%PropSetsS, Init%NPropSetsS, TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParsePropSets

!> Shared plain-numeric-table reader: fetch Path (an optional key; absent -> zero rows) as an
!! N x NCols matrix and allocate/store it, deriving the row count from the matrix shape.
subroutine ParseNumericTable(Doc, Path, NCols, Mat, NRows, ErrStat, ErrMsg)
   type(YamlDoc),           intent(inout) :: Doc
   character(*),            intent(in   ) :: Path
   integer(IntKi),          intent(in   ) :: NCols
   real(ReKi), allocatable, intent(  out) :: Mat(:,:)
   integer(IntKi),          intent(  out) :: NRows
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseNumericTable'
   real(ReKi), allocatable  :: TmpMat(:,:)
   logical                  :: WasFound
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, Path, TmpMat, TmpErrStat, TmpErrMsg, Found=WasFound); if (Failed()) return

   if (.not. WasFound) then
      NRows = 0
      call AllocAry(Mat, 0, NCols, 'PropSet', TmpErrStat, TmpErrMsg); if (Failed()) return
      return
   end if

   NRows = size(TmpMat, 1)
   if (NRows < 0) then
      call SetErrStat(ErrID_Fatal, '"'//trim(Path)//'" must have a non-negative number of rows.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   if (NRows > 0 .and. size(TmpMat,2) /= NCols) then
      call SetErrStat(ErrID_Fatal, '"'//trim(Path)//'" rows must each have '//trim(Num2LStr(NCols))// &
         ' entries; found '//trim(Num2LStr(size(TmpMat,2)))//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   call AllocAry(Mat, NRows, NCols, 'PropSet', TmpErrStat, TmpErrMsg); if (Failed()) return
   if (NRows > 0) Mat = TmpMat

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseNumericTable

!> cable_properties:cables -- a list of row mappings {PropSetID, EA, MatDens, T0, CtrlChannel
!! (optional, default 0)}, with the same non-zero-pretension warning the text path prints.
subroutine ParseCableProperties(Doc, Init, ErrStat, ErrMsg)
   type(YamlDoc),      intent(inout) :: Doc
   type(SD_InitType),  intent(inout) :: Init
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseCableProperties'
   integer(IntKi)           :: iSeq, iRow, i
   logical                  :: SecFound, bCableHasPretension
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'cable_properties:cables', iSeq, TmpErrStat, TmpErrMsg, Found=SecFound); if (Failed()) return

   if (.not. SecFound) then
      Init%NPropSetsC = 0
      call AllocAry(Init%PropSetsC, 0, PropSetsCCol, 'PropSetsC', TmpErrStat, TmpErrMsg); if (Failed()) return
      return
   end if

   Init%NPropSetsC = int(Yaml_NumChildren(Doc, iSeq))
   call AllocAry(Init%PropSetsC, Init%NPropSetsC, PropSetsCCol, 'PropSetsC', TmpErrStat, TmpErrMsg); if (Failed()) return

   bCableHasPretension = .false.
   do i = 1, Init%NPropSetsC
      iRow = Yaml_Child(Doc, iSeq, i)
      call YamlGet(Doc, 'PropSetID', Init%PropSetsC(i,1), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'EA',        Init%PropSetsC(i,2), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'MatDens',   Init%PropSetsC(i,3), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'T0',        Init%PropSetsC(i,4), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'CtrlChannel', Init%PropSetsC(i,5), TmpErrStat, TmpErrMsg, Default=0.0_ReKi, From=iRow); if (Failed()) return
      if (Init%PropSetsC(i,4) > 0.0_ReKi) bCableHasPretension = .true.
   end do

   if (bCableHasPretension) then
      call WrScr('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
      call WrScr('Warning: Cable with non-zero pretension specified.')
      call WrScr('         SubDyn currently does not account for geometric stiffness from pretension.' )
      call WrScr('         Avoid non-zero cable pretension if possible.' )
      call WrScr('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseCableProperties

!> member_direction_cosine_matrices:cosm -- a plain N x 10 numeric table (COSMID, COSM11,
!! COSM12, COSM13, COSM21, COSM22, COSM23, COSM31, COSM32, COSM33), with the same
!! orthogonality/right-handedness checks the text path performs.
subroutine ParseCosineMatrices(Doc, Init, ErrStat, ErrMsg)
   type(YamlDoc),      intent(inout) :: Doc
   type(SD_InitType),  intent(inout) :: Init
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseCosineMatrices'
   real(R8Ki), allocatable  :: TmpMat(:,:)
   real(R8Ki)               :: tmpMat33(3,3)
   integer(IntKi)           :: i, j, k
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call ParseCosmTable(Doc, TmpMat, TmpErrStat, TmpErrMsg); if (Failed()) return

   Init%NCOSMs = size(TmpMat, 1)
   call AllocAry(Init%COSMs, Init%NCOSMs, COSMsCol, 'COSMs', TmpErrStat, TmpErrMsg); if (Failed()) return
   Init%COSMs = TmpMat

   do i = 1, Init%NCOSMs
      tmpMat33 = reshape(Init%COSMs(i,2:COSMsCol), shape(tmpMat33))
      tmpMat33 = matmul(tmpMat33, transpose(tmpMat33))
      tmpMat33(1,1) = tmpMat33(1,1) - 1.0_R8Ki
      tmpMat33(2,2) = tmpMat33(2,2) - 1.0_R8Ki
      tmpMat33(3,3) = tmpMat33(3,3) - 1.0_R8Ki
      tmpMat33 = abs(tmpMat33)
      do j = 1,3
         do k = 1,3
            if (tmpMat33(j,k) > 0.0001_R8Ki) then
               call SetErrStat(ErrID_Fatal, 'member_direction_cosine_matrices:cosm entry '//trim(Num2LStr(i))// &
                  ' with COSMID '//trim(Num2LStr(int(Init%COSMs(i,1))))// &
                  ' is not orthogonal to the required precision and is therefore not a valid rotation matrix.', &
                  ErrStat, ErrMsg, RoutineName)
               return
            end if
         end do
      end do
      tmpMat33 = reshape(Init%COSMs(i,2:COSMsCol), shape(tmpMat33))
      if (Determinant(tmpMat33, TmpErrStat, TmpErrMsg) < 0.0_R8Ki) then
         call SetErrStat(ErrID_Fatal, 'member_direction_cosine_matrices:cosm entry '//trim(Num2LStr(i))// &
            ' with COSMID '//trim(Num2LStr(int(Init%COSMs(i,1))))// &
            ' has a negative determinant and is therefore not a valid rotation matrix.', ErrStat, ErrMsg, RoutineName)
         return
      end if
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseCosineMatrices

!> Fetch member_direction_cosine_matrices:cosm tolerating a missing key (zero matrices).
subroutine ParseCosmTable(Doc, TmpMat, ErrStat, ErrMsg)
   type(YamlDoc),           intent(inout) :: Doc
   real(R8Ki), allocatable, intent(  out) :: TmpMat(:,:)
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   logical :: WasFound

   call YamlGet(Doc, 'member_direction_cosine_matrices:cosm', TmpMat, ErrStat, ErrMsg, Found=WasFound)
   if (ErrStat >= AbortErrLev) return
   if (.not. WasFound) allocate(TmpMat(0, COSMsCol))
end subroutine ParseCosmTable

!> concentrated_masses:masses -- a list of row mappings {JointID, JMass, JMXX, JMYY, JMZZ,
!! JMXY/JMXZ/JMYZ/CGX/CGY/CGZ (all optional, default 0 -- the text format's 5-vs-11-column
!! legacy tolerance, made explicit as per-field defaults instead of a column-count check).
subroutine ParseConcentratedMasses(Doc, Init, ErrStat, ErrMsg)
   type(YamlDoc),      intent(inout) :: Doc
   type(SD_InitType),  intent(inout) :: Init
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseConcentratedMasses'
   integer(IntKi)           :: iSeq, iRow, i
   logical                  :: SecFound
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'concentrated_masses:masses', iSeq, TmpErrStat, TmpErrMsg, Found=SecFound); if (Failed()) return

   if (.not. SecFound) then
      Init%nCMass = 0
      call AllocAry(Init%CMass, 0, CMassCol, 'CMass', TmpErrStat, TmpErrMsg); if (Failed()) return
      return
   end if

   Init%nCMass = int(Yaml_NumChildren(Doc, iSeq))
   call AllocAry(Init%CMass, Init%nCMass, CMassCol, 'CMass', TmpErrStat, TmpErrMsg); if (Failed()) return
   Init%CMass = 0.0_ReKi

   do i = 1, Init%nCMass
      iRow = Yaml_Child(Doc, iSeq, i)
      call YamlGet(Doc, 'JointID', Init%CMass(i,1),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'JMass',   Init%CMass(i,2),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'JMXX',    Init%CMass(i,3),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'JMYY',    Init%CMass(i,4),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'JMZZ',    Init%CMass(i,5),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'JMXY',    Init%CMass(i,6),  TmpErrStat, TmpErrMsg, Default=0.0_ReKi, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'JMXZ',    Init%CMass(i,7),  TmpErrStat, TmpErrMsg, Default=0.0_ReKi, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'JMYZ',    Init%CMass(i,8),  TmpErrStat, TmpErrMsg, Default=0.0_ReKi, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'CGX',     Init%CMass(i,9),  TmpErrStat, TmpErrMsg, Default=0.0_ReKi, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'CGY',     Init%CMass(i,10), TmpErrStat, TmpErrMsg, Default=0.0_ReKi, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'CGZ',     Init%CMass(i,11), TmpErrStat, TmpErrMsg, Default=0.0_ReKi, From=iRow); if (Failed()) return
      if (Init%CMass(i,1) <= 0.0_ReKi) then
         call SetErrStat(ErrID_Fatal, 'concentrated_masses:masses entry '//trim(Num2LStr(i))// &
            ': invalid JointID.', ErrStat, ErrMsg, RoutineName)
         return
      end if
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseConcentratedMasses

!> output: SumPrint, OutCBModes, OutFEMModes, OutCOSM, OutAll, OutSwtch, TabDelim, OutDec,
!! OutFmt, OutSFmt. Unlike the text path, OutCBModes/OutFEMModes/OutCOSM are always required
!! (no legacy-missing-lines fallback in the YAML schema).
subroutine ParseOutput(Doc, Init, p, ErrStat, ErrMsg)
   type(YamlDoc),          intent(inout) :: Doc
   type(SD_InitType),      intent(inout) :: Init
   type(SD_ParameterType), intent(inout) :: p
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseOutput'
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'output:SumPrint',   Init%SSSum,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:OutCBModes', p%OutCBModes,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:OutFEMModes',p%OutFEMModes, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:OutCOSM',    Init%OutCOSM,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:OutAll',     p%OutAll,      TmpErrStat, TmpErrMsg); if (Failed()) return
   p%OutAllInt = 1
   if (.not. p%OutAll) p%OutAllInt = 0

   call YamlGet(Doc, 'output:OutSwtch', p%OutSwtch, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (p%OutSwtch < 1 .or. p%OutSwtch > 3) then
      call SetErrStat(ErrID_Fatal, 'output:OutSwtch must be >0 and <4', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call YamlGet(Doc, 'output:TabDelim', Init%TabDelim, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (Init%TabDelim) then
      p%Delim = TAB
   else
      p%Delim = ' '
   end if

   call YamlGet(Doc, 'output:OutDec',  p%OutDec,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:OutFmt',  p%OutFmt,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:OutSFmt', p%OutSFmt, TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseOutput

!> member_output_list:members -- a list of row mappings {MemberID, NodeCnt: [ints]}.
!! NMOutputs derives from the list length; NOutCnt derives from each row's NodeCnt length.
subroutine ParseMemberOutputList(Doc, Init, p, ErrStat, ErrMsg)
   type(YamlDoc),          intent(inout) :: Doc
   type(SD_InitType),      intent(inout) :: Init
   type(SD_ParameterType), intent(inout) :: p
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SD_ParseMemberOutputList'
   integer(IntKi)           :: MemberDivCount, MemberNodeMax
   real(ReKi)               :: MemberLen
   integer(IntKi), allocatable :: NodeCnt(:)
   integer(IntKi)           :: iSeq, iRow, i, j, k, flg
   logical                  :: SecFound
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'member_output_list:members', iSeq, TmpErrStat, TmpErrMsg, Found=SecFound); if (Failed()) return

   if (.not. SecFound) then
      p%NMOutputs = 0
      return
   end if

   p%NMOutputs = int(Yaml_NumChildren(Doc, iSeq))
   if (p%NMOutputs < 0 .or. p%NMOutputs > p%NMembers .or. p%NMOutputs > 99) then
      call SetErrStat(ErrID_Fatal, 'member_output_list:members must have >=0 and <= minimum(NMembers,99) entries', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if

   if (p%NMOutputs > 0) then
      allocate(p%MOutLst(p%NMOutputs), STAT=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating MOutLst arrays', ErrStat, ErrMsg, RoutineName)
         return
      end if

      do i = 1, p%NMOutputs
         iRow = Yaml_Child(Doc, iSeq, i)
         call YamlGet(Doc, 'MemberID', p%MOutLst(i)%MemberID, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'NodeCnt', NodeCnt, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return

         p%MOutLst(i)%NOutCnt = size(NodeCnt)
         if (p%MOutLst(i)%NOutCnt < 1 .or. p%MOutLst(i)%NOutCnt > 9) then
            call SetErrStat(ErrID_Fatal, 'member_output_list:members entry '//trim(Num2LStr(i))// &
               ': NodeCnt must list >= 1 and <= 9 entries', ErrStat, ErrMsg, RoutineName)
            return
         end if

         call AllocAry(p%MOutLst(i)%NodeCnt, p%MOutLst(i)%NOutCnt, 'NodeCnt', TmpErrStat, TmpErrMsg); if (Failed()) return
         p%MOutLst(i)%NodeCnt = NodeCnt

         flg = 0
         do j = 1, p%NMembers
            if (p%MOutLst(i)%MemberID == Init%Members(j,1)) then
               flg = flg + 1
               ! per-member node count: NDiv, or ceil(L/MDivSize) for beams with MDivSize (text-path parity)
               MemberDivCount = Init%MemberNDiv(j)
               if (Init%MemberDivSize(j) > 0.0_ReKi .and. (Init%Members(j,6) == idMemberBeamCirc .or. &
                   Init%Members(j,6) == idMemberBeamRect .or. Init%Members(j,6) == idMemberBeamArb)) then
                  MemberLen = SD_YamlMemberLength(j, Init, TmpErrStat, TmpErrMsg); if (Failed()) return
                  MemberDivCount = max(1_IntKi, int(ceiling(MemberLen/Init%MemberDivSize(j)), IntKi))
               end if
               MemberNodeMax = MemberDivCount + 1
               if (p%MOutLst(i)%NOutCnt > min(MemberNodeMax, 9_IntKi)) then
                  call SetErrStat(ErrID_Fatal, 'member_output_list:members entry '//trim(Num2LStr(i))// &
                     ': NOutCnt should be less than or equal to min(number of nodes on the requested member, 9).', ErrStat, ErrMsg, RoutineName)
                  return
               end if
               do k = 1, p%MOutLst(i)%NOutCnt
                  if (p%MOutLst(i)%NodeCnt(k) > MemberNodeMax .or. p%MOutLst(i)%NodeCnt(k) < 1) then
                     call SetErrStat(ErrID_Fatal, 'member_output_list:members entry '//trim(Num2LStr(i))// &
                        ': NodeCnt should be less than or equal to the number of nodes on the requested member and greater than 0.', ErrStat, ErrMsg, RoutineName)
                     return
                  end if
               end do
            end if
         end do
         if (flg == 0) then
            call SetErrStat(ErrID_Fatal, 'member_output_list:members entry '//trim(Num2LStr(i))// &
               ': MemberID '//trim(Num2LStr(p%MOutLst(i)%MemberID))//' requested for output is not in the list of Members.', &
               ErrStat, ErrMsg, RoutineName)
            return
         end if
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseMemberOutputList

!> outputs:OutList -- a sequence of channel names (no terminating END, per project convention).
subroutine ParseOutList(Doc, Init, p, ErrStat, ErrMsg)
   type(YamlDoc),          intent(inout) :: Doc
   type(SD_InitType),      intent(inout) :: Init
   type(SD_ParameterType), intent(inout) :: p
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'SD_ParseOutList'
   character(:), allocatable :: TmpList(:)
   integer(IntKi)             :: i
   integer(IntKi)             :: TmpErrStat
   character(ErrMsgLen)       :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'outputs:OutList', TmpList, TmpErrStat, TmpErrMsg); if (Failed()) return

   allocate(Init%SSOutList(MaxOutPts + p%OutAllInt*p%OutAllDims), STAT=TmpErrStat)
   if (TmpErrStat /= 0) then
      call SetErrStat(ErrID_Fatal, 'Error allocating SSOutList arrays', ErrStat, ErrMsg, RoutineName)
      return
   end if
   Init%SSOutList = ''

   p%NumOuts = size(TmpList)
   do i = 1, p%NumOuts
      if (len_trim(TmpList(i)) > ChanLen) then
         call SetErrStat(ErrID_Fatal, 'outputs:OutList entry "'//trim(TmpList(i))//'" is longer than the '// &
            trim(Num2LStr(ChanLen))//'-character channel-name limit.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      Init%SSOutList(i) = TmpList(i)
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseOutList


!> Straight-line length of member iMember from its two joints (mirrors SubDyn's MemberLength,
!! which lives in module SubDyn and cannot be USEd here without a circular dependency).
function SD_YamlMemberLength(iMember, Init, ErrStat, ErrMsg) result(L)
   integer(IntKi),    intent(in   ) :: iMember
   type(SD_InitType), intent(in   ) :: Init
   integer(IntKi),    intent(  out) :: ErrStat
   character(*),      intent(  out) :: ErrMsg
   real(ReKi)                       :: L
   integer(IntKi)                   :: Joint1, Joint2
   character(*), parameter          :: RoutineName = 'SD_YamlMemberLength'
   ErrStat = ErrID_None; ErrMsg = ''; L = 0.0_ReKi
   Joint1 = FINDLOCI(Init%Joints(:,1), Init%Members(iMember,2))
   Joint2 = FINDLOCI(Init%Joints(:,1), Init%Members(iMember,3))
   if (Joint1 <= 0 .or. Joint2 <= 0) then
      call SetErrStat(ErrID_Fatal, ' Member with ID '//trim(Num2LStr(Init%Members(iMember,1)))// &
         ' references a joint that is not in the joint list.', ErrStat, ErrMsg, RoutineName); return
   end if
   L = sqrt(sum((Init%Joints(Joint2,2:4) - Init%Joints(Joint1,2:4))**2))
   if (EqualRealNos(L, 0.0_ReKi)) then
      call SetErrStat(ErrID_Fatal, ' Member with ID '//trim(Num2LStr(Init%Members(iMember,1)))// &
         ' has zero length!', ErrStat, ErrMsg, RoutineName); return
   end if
end function SD_YamlMemberLength

end module SubDyn_Yaml
