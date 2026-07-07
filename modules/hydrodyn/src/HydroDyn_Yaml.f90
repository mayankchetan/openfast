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
!> Reader for the YAML form of the HydroDyn primary input file. Fills the same
!! HydroDyn_InputFile structure as HydroDyn_ParseInput (the text path, HydroDyn_Input.f90),
!! so validation (HydroDynInput_ProcessInitData) and everything downstream is shared
!! between the two formats -- including relative-path resolution of PotFile/GeoFile
!! against InitInp%InputFile, which is set to the primary file's (possibly pseudo, for
!! inline input) path regardless of format.
!!
!! Schema: sections mirror the text file's banners (general, floating_platform,
!! second_order_wamit_forces, additional_stiffness_damping, strip_theory,
!! axial_coefficients, member_joints, cylindrical_member_cross_section,
!! rectangular_member_cross_section, simple_hydrodynamic_coefficients_cylindrical,
!! simple_hydrodynamic_coefficients_rectangular,
!! depth_based_hydrodynamic_coefficients_cylindrical,
!! depth_based_hydrodynamic_coefficients_rectangular,
!! member_based_hydrodynamic_coefficients_cylindrical,
!! member_based_hydrodynamic_coefficients_rectangular, members, filled_members,
!! marine_growth_by_depth, member_output_list, joint_output_list, output,
!! output_channels).
!!
!! Counted tables (axial_coefficients:AxialCoefs, member_joints:Joints,
!! cylindrical_member_cross_section:MPropSetsCyl,
!! rectangular_member_cross_section:MPropSetsRec,
!! depth_based_hydrodynamic_coefficients_cylindrical/rectangular:CoefDpths{Cyl,Rec},
!! member_based_hydrodynamic_coefficients_cylindrical/rectangular:CoefMembers{Cyl,Rec},
!! members:Members, filled_members:FilledGroups, marine_growth_by_depth:MGDepths,
!! member_output_list:MOutLst) are YAML lists of mappings -- one mapping per table
!! row, with the documented column names as keys -- rather than positional lists, so
!! the N* counts (NAxCoef, NJoints, NPropSetsCyl, ..., NFillGroups, NMGDepths,
!! NMOutputs) are all derived from list lengths and are never themselves YAML keys.
!! joint_output_list:JOutLst (a flat list of joint IDs) is the one counted table that
!! stays a plain list, since it is not itself tabular.
!!
!! Optional per-row keys apply the same fallback defaults as the text path's
!! variant-length reads: axial_coefficients rows may omit AxFDMod/AxVnCOff/AxFDLoFSc
!! (defaults 0, -1.0, 1.0, matching HydroDyn_ParseInput's 5- and 4-entry fallback
!! reads); members rows may omit FDMod/VnCOffA/VnCOffB/FDLoFScA/FDLoFScB (defaults 0,
!! -1.0, -1.0, 1.0, 1.0, matching the 11-entry fallback read; the same "only
!! applicable to rectangular members"/"only applicable with FDMod>0" WrScr warnings
!! are emitted here too).
!!
!! The MacCamy-Fuchs "MCF" keyword substitution (ParseRAryWKywrd in the text path) is
!! expressed here as named cells that accept either a real number or the literal
!! string "MCF": SimplCp/SimplCpMG, SimplRecCp/SimplRecCpMG, DpthCp/DpthCpMG (both
!! depth-based tables), and MemberCp1/MemberCp2/MemberCpMG1/MemberCpMG2 (both
!! member-based tables). Within one such group every cell must say "MCF" or none may
!! -- mixing is fatal, exactly as the text path's KywrdEntry check enforces; across
!! all rows of a depth-based table the MCF flag must also agree (checked after the
!! per-row loop, exactly as the text path does for CoefDpthsCyl/CoefDpthsRec).
!!
!! RdtnDT (additional_stiffness_damping... actually floating_platform:RdtnDT) and
!! filled_members row key FillDens accept the literal scalar "default"/"DEFAULT"
!! exactly like the text format (resolved later, once other values are known, in
!! HydroDynInput_ProcessInitData) -- like SeaState's CurrSSDir, they are NOT read
!! through the "default" scalar mechanism in the usual sense (there is no single
!! fallback value to substitute at parse time). Default='DEFAULT' is passed to
!! YamlGet so the literal text passes through unmolested instead of tripping
!! YamlGet's built-in "is set to default but no default exists" fatal.
!!
!! PotFile/GeoFile (potential-flow data root names / geometry files) stay path
!! strings -- resolved relative to the primary input file later, in
!! HydroDynInput_ProcessInitData, never converted or inlined.
module HydroDyn_Yaml

   use NWTC_Library
   use YamlInput
   use HydroDyn_Types
   use Morison_Types, only: MSecGeom_Rec

   implicit none
   private

   public :: HD_ParseYamlFile
   public :: HD_ParseYamlFileInfo

contains

!> Load and parse a YAML-format HydroDyn primary input file. Mirrors the contract of
!! ProcessComFile + HydroDyn_ParseInput (the text path), including echo handling.
subroutine HD_ParseYamlFile(InputFileName, OutRootName, InputFileData, ErrStat, ErrMsg)
   character(*),               intent(in   ) :: InputFileName !< the .yaml primary input file
   character(*),               intent(in   ) :: OutRootName   !< root name for the echo file
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData !< the shared input-file structure
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'HD_ParseYamlFile'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: UnEc
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1

   InputFileData%Echo = .false.  ! initialize for error handling (cleanup path)

   call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'general:Echo', InputFileData%Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (InputFileData%Echo) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included), which supersedes the text path's line-by-line echo
      call OpenEcho(UnEc, trim(OutRootName)//'.ech', TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for HydroDyn primary input file: '//trim(InputFileName)
      call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   call ParseYamlDoc(Doc, InputFileData, TmpErrStat, TmpErrMsg)
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

end subroutine HD_ParseYamlFile

!> Parse YAML-format HydroDyn input arriving as a FileInfoType -- the passed-data
!! channel used for inline module input from a YAML primary file (the glue code's
!! inline HydroFile handover when CompHydro selects HydroDyn). Per-line provenance in
!! the FileInfoType keeps error messages pointing at the original file and line.
subroutine HD_ParseYamlFileInfo(FileInfo, OutRootName, InputFileData, ErrStat, ErrMsg)
   type(FileInfoType),         intent(in   ) :: FileInfo
   character(*),               intent(in   ) :: OutRootName
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'HD_ParseYamlFileInfo'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: UnEc
   integer(IntKi)          :: i
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1

   InputFileData%Echo = .false.

   call Yaml_LoadFileInfo(FileInfo, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'general:Echo', InputFileData%Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (InputFileData%Echo) then
      call OpenEcho(UnEc, trim(OutRootName)//'.ech', TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for HydroDyn primary input (passed YAML data)'
      do i = 1, FileInfo%NumLines
         write(UnEc, '(A)') trim(FileInfo%Lines(i))
      end do
   end if

   call ParseYamlDoc(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

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

end subroutine HD_ParseYamlFileInfo

!> Fill InputFileData from a parsed document. Split from the file/FileInfo wrappers so
!! both entry points share it (mirrors the other modules' ParseYamlDoc split).
subroutine ParseYamlDoc(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'HD_ParseYamlDoc'
   integer(IntKi)              :: NDOF
   integer(IntKi)              :: TmpErrStat
   character(ErrMsgLen)        :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   !----------------------------------------------------------------------------------
   ! general (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'general:Echo', InputFileData%Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! floating_platform (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'floating_platform:PotMod', InputFileData%PotMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'floating_platform:ExctnMod', InputFileData%WAMIT%ExctnMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'floating_platform:ExctnDisp', InputFileData%WAMIT%ExctnDisp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'floating_platform:ExctnCutOff', InputFileData%WAMIT%ExctnCutOff, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'floating_platform:PtfmYMod', InputFileData%PtfmYMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'floating_platform:PtfmRefY', InputFileData%PtfmRefY, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%PtfmRefY = InputFileData%PtfmRefY * D2R
   call YamlGet(Doc, 'floating_platform:PtfmYCutOff', InputFileData%PtfmYCutOff, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'floating_platform:NExctnHdg', InputFileData%WAMIT%NExctnHdg, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%WAMIT2%NExctnHdg = InputFileData%WAMIT%NExctnHdg
   call YamlGet(Doc, 'floating_platform:RdtnMod', InputFileData%WAMIT%RdtnMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'floating_platform:RdtnTMax', InputFileData%WAMIT%RdtnTMax, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'floating_platform:RdtnDT', InputFileData%WAMIT%Conv_Rdtn%RdtnDTChr, TmpErrStat, TmpErrMsg, &
                Default='DEFAULT')
   if (Failed()) return

   call YamlGet(Doc, 'floating_platform:NBody', InputFileData%NBody, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'floating_platform:NBodyMod', InputFileData%NBodyMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   if (InputFileData%NBodyMod == 1) then
      ! Special case where all data in a single WAMIT input file as opposed to
      ! InputFileData%NBody number of separate input files.
      InputFileData%nWAMITObj     = 1
      InputFileData%vecMultiplier = InputFileData%NBody
   else
      InputFileData%nWAMITObj     = InputFileData%NBody
      InputFileData%vecMultiplier = 1
   end if

   call ReadFloatingPlatformArrays(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! second_order_wamit_forces (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'second_order_wamit_forces:MnDrift', InputFileData%WAMIT2%MnDrift, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'second_order_wamit_forces:NewmanApp', InputFileData%WAMIT2%NewmanApp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'second_order_wamit_forces:DiffQTF', InputFileData%WAMIT2%DiffQTF, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'second_order_wamit_forces:SumQTF', InputFileData%WAMIT2%SumQTF, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! additional_stiffness_damping (required) -- NDOF depends on NBody/NAddDOF(1) when
   ! NBody==1, else 6*vecMultiplier, exactly as the text path computes it
   !----------------------------------------------------------------------------------
   if (InputFileData%NBody == 1_IntKi) then
      NDOF = 6 + InputFileData%NAddDOF(1)
   else
      NDOF = 6 * InputFileData%vecMultiplier
   end if

   call ReadAddStiffnessDamping(Doc, InputFileData, NDOF, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! strip_theory (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'strip_theory:WaveDisp', InputFileData%Morison%WaveDisp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'strip_theory:AMMod', InputFileData%Morison%AMMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'strip_theory:HstMod', InputFileData%Morison%HstMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! axial_coefficients (required) -- AxFDMod/AxVnCOff/AxFDLoFSc are optional per row
   !----------------------------------------------------------------------------------
   call ReadAxialCoefs(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! member_joints (required)
   !----------------------------------------------------------------------------------
   call ReadJoints(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! cylindrical_member_cross_section (required)
   !----------------------------------------------------------------------------------
   call ReadMPropSetsCyl(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! rectangular_member_cross_section (required)
   !----------------------------------------------------------------------------------
   call ReadMPropSetsRec(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! simple_hydrodynamic_coefficients_cylindrical (required) -- SimplCp/SimplCpMG may
   ! both say "MCF" instead of a number (SimplMCF)
   !----------------------------------------------------------------------------------
   call ReadSimplCyl(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! simple_hydrodynamic_coefficients_rectangular (required) -- SimplRecCp/SimplRecCpMG
   ! may both say "MCF" instead of a number (SimplRecMCF)
   !----------------------------------------------------------------------------------
   call ReadSimplRec(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! depth_based_hydrodynamic_coefficients_cylindrical (required)
   !----------------------------------------------------------------------------------
   call ReadCoefDpthsCyl(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! depth_based_hydrodynamic_coefficients_rectangular (required)
   !----------------------------------------------------------------------------------
   call ReadCoefDpthsRec(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! member_based_hydrodynamic_coefficients_cylindrical (required)
   !----------------------------------------------------------------------------------
   call ReadCoefMembersCyl(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! member_based_hydrodynamic_coefficients_rectangular (required)
   !----------------------------------------------------------------------------------
   call ReadCoefMembersRec(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! members (required)
   !----------------------------------------------------------------------------------
   call ReadMembers(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! filled_members (required)
   !----------------------------------------------------------------------------------
   call ReadFilledGroups(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! marine_growth_by_depth (required)
   !----------------------------------------------------------------------------------
   call ReadMGDepths(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! member_output_list (required)
   !----------------------------------------------------------------------------------
   call ReadMOutLst(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! joint_output_list (required) -- a flat list of joint IDs (not itself tabular)
   !----------------------------------------------------------------------------------
   call ReadJOutLst(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! output (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'output:HDSum', InputFileData%HDSum, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'output:OutAll', InputFileData%Morison%OutAll, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'output:OutSwtch', InputFileData%OutSwtch, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'output:OutFmt', InputFileData%OutFmt, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'output:OutSFmt', InputFileData%OutSFmt, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! output_channels (required) -- NUserOutputs derives from the OutList length
   !----------------------------------------------------------------------------------
   call ReadOutputChannels(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseYamlDoc

!> The floating_platform section's WAMIT-object/body-sized arrays: PotFile/WAMITULEN
!! (nWAMITObj entries), PtfmRefxt/PtfmRefyt/PtfmRefzt/PtfmRefztRot/PtfmVol0/PtfmCOBxt/
!! PtfmCOByt/NAddDOF/GeoFile (NBody entries), and FKMod (nWAMITObj entries in the file,
!! broadcast to all NBody entries when NBodyMod==1 -- exactly as the text path does).
subroutine ReadFloatingPlatformArrays(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'ReadFloatingPlatformArrays'
   integer(IntKi), allocatable :: TmpFKMod(:)
   character(:), allocatable   :: TmpFileList(:)
   integer(IntKi)             :: k
   integer(IntKi)             :: TmpErrStat
   character(ErrMsgLen)       :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   ! PotFile/GeoFile are fixed-length CHARACTER(1024) arrays -- YamlGetChAry's
   ! deferred-length target doesn't match that directly, so read into a temporary
   ! deferred-length list first, then copy element-wise.
   call YamlGet(Doc, 'floating_platform:PotFile', TmpFileList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpFileList), InputFileData%nWAMITObj, 'floating_platform:PotFile')) return
   call AllocAry(InputFileData%PotFile, InputFileData%nWAMITObj, 'InputFileData%PotFile', TmpErrStat, TmpErrMsg)
   if (Failed()) return
   do k = 1, InputFileData%nWAMITObj
      InputFileData%PotFile(k) = TmpFileList(k)
   end do

   call YamlGet(Doc, 'floating_platform:WAMITULEN', InputFileData%WAMITULEN, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(InputFileData%WAMITULEN), InputFileData%nWAMITObj, 'floating_platform:WAMITULEN')) return

   call YamlGet(Doc, 'floating_platform:PtfmRefxt', InputFileData%PtfmRefxt, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(InputFileData%PtfmRefxt), InputFileData%NBody, 'floating_platform:PtfmRefxt')) return

   call YamlGet(Doc, 'floating_platform:PtfmRefyt', InputFileData%PtfmRefyt, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(InputFileData%PtfmRefyt), InputFileData%NBody, 'floating_platform:PtfmRefyt')) return

   call YamlGet(Doc, 'floating_platform:PtfmRefzt', InputFileData%PtfmRefzt, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(InputFileData%PtfmRefzt), InputFileData%NBody, 'floating_platform:PtfmRefzt')) return

   call YamlGet(Doc, 'floating_platform:PtfmRefztRot', InputFileData%PtfmRefztRot, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(InputFileData%PtfmRefztRot), InputFileData%NBody, 'floating_platform:PtfmRefztRot')) return
   InputFileData%PtfmRefztRot = InputFileData%PtfmRefztRot * D2R_D

   call YamlGet(Doc, 'floating_platform:PtfmVol0', InputFileData%PtfmVol0, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(InputFileData%PtfmVol0), InputFileData%NBody, 'floating_platform:PtfmVol0')) return

   call YamlGet(Doc, 'floating_platform:PtfmCOBxt', InputFileData%PtfmCOBxt, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(InputFileData%PtfmCOBxt), InputFileData%NBody, 'floating_platform:PtfmCOBxt')) return

   call YamlGet(Doc, 'floating_platform:PtfmCOByt', InputFileData%PtfmCOByt, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(InputFileData%PtfmCOByt), InputFileData%NBody, 'floating_platform:PtfmCOByt')) return

   call YamlGet(Doc, 'floating_platform:NAddDOF', InputFileData%NAddDOF, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(InputFileData%NAddDOF), InputFileData%NBody, 'floating_platform:NAddDOF')) return

   ! FKMod: nWAMITObj entries in the file; broadcast to every body when NBodyMod==1
   call YamlGet(Doc, 'floating_platform:FKMod', TmpFKMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpFKMod), InputFileData%nWAMITObj, 'floating_platform:FKMod')) return
   call AllocAry(InputFileData%FKMod, InputFileData%NBody, 'InputFileData%FKMod', TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%FKMod(1:InputFileData%nWAMITObj) = TmpFKMod
   if (InputFileData%NBodyMod == 1) then
      InputFileData%FKMod(2:InputFileData%NBody) = InputFileData%FKMod(1)
   end if

   call YamlGet(Doc, 'floating_platform:GeoFile', TmpFileList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpFileList), InputFileData%NBody, 'floating_platform:GeoFile')) return
   call AllocAry(InputFileData%GeoFile, InputFileData%NBody, 'InputFileData%GeoFile', TmpErrStat, TmpErrMsg)
   if (Failed()) return
   do k = 1, InputFileData%NBody
      InputFileData%GeoFile(k) = TmpFileList(k)
   end do

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
      if (BadSize) call SetErrStat(ErrID_Fatal, '"'//Path//'" must have exactly '//trim(Num2LStr(NExpect))// &
         ' entries; found '//trim(Num2LStr(NGiven))//'.', ErrStat, ErrMsg, RoutineName)
   end function BadSize

end subroutine ReadFloatingPlatformArrays

!> AddF0 (NDOF x nWAMITObj), AddCLin/AddBLin/AddBQuad (nWAMITObj matrices of NDOF x
!! NDOF each -- a YAML list of matrices, one per WAMIT object, rather than the text
!! path's single NDOF x (nWAMITObj*NDOF) flattened block).
subroutine ReadAddStiffnessDamping(Doc, InputFileData, NDOF, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(in   ) :: NDOF
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'ReadAddStiffnessDamping'
   real(ReKi), allocatable    :: TmpMat(:,:)
   integer(IntKi)             :: TmpErrStat
   character(ErrMsgLen)       :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call AllocAry(InputFileData%AddF0, NDOF, InputFileData%nWAMITObj, 'InputFileData%AddF0', TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call AllocAry(InputFileData%AddCLin, NDOF, NDOF, InputFileData%nWAMITObj, 'InputFileData%AddCLin', TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call AllocAry(InputFileData%AddBLin, NDOF, NDOF, InputFileData%nWAMITObj, 'InputFileData%AddBLin', TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call AllocAry(InputFileData%AddBQuad, NDOF, NDOF, InputFileData%nWAMITObj, 'InputFileData%AddBQuad', TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'additional_stiffness_damping:AddF0', TmpMat, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadMatShape(TmpMat, NDOF, InputFileData%nWAMITObj, 'additional_stiffness_damping:AddF0')) return
   InputFileData%AddF0 = TmpMat

   call ReadMatrixList('additional_stiffness_damping:AddCLin', InputFileData%AddCLin)
   if (Failed()) return
   call ReadMatrixList('additional_stiffness_damping:AddBLin', InputFileData%AddBLin)
   if (Failed()) return
   call ReadMatrixList('additional_stiffness_damping:AddBQuad', InputFileData%AddBQuad)
   if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

   logical function BadMatShape(Mat, NRowsExpect, NColsExpect, Path)
      real(ReKi),     intent(in) :: Mat(:,:)
      integer(IntKi), intent(in) :: NRowsExpect
      integer(IntKi), intent(in) :: NColsExpect
      character(*),   intent(in) :: Path
      BadMatShape = (size(Mat,1) /= NRowsExpect .or. size(Mat,2) /= NColsExpect)
      if (BadMatShape) call SetErrStat(ErrID_Fatal, '"'//Path//'" must be a '//trim(Num2LStr(NRowsExpect))//'x'// &
         trim(Num2LStr(NColsExpect))//' matrix; found '//trim(Num2LStr(size(Mat,1)))//'x'// &
         trim(Num2LStr(size(Mat,2)))//'.', ErrStat, ErrMsg, RoutineName)
   end function BadMatShape

   !> One nWAMITObj-long list of NDOF x NDOF matrices.
   subroutine ReadMatrixList(Path, Dest)
      character(*), intent(in   ) :: Path
      real(ReKi),   intent(inout) :: Dest(:,:,:)
      integer(IntKi) :: iOuter, iMat, j

      call YamlGetNode(Doc, Path, iOuter, TmpErrStat, TmpErrMsg)
      if (Failed()) return
      if (Yaml_NumChildren(Doc, iOuter) /= InputFileData%nWAMITObj) then
         call SetErrStat(ErrID_Fatal, '"'//Path//'" must list exactly '//trim(Num2LStr(InputFileData%nWAMITObj))// &
            ' matri(x/ces) (one per WAMIT object); found '// &
            trim(Num2LStr(int(Yaml_NumChildren(Doc, iOuter))))//'.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      do j = 1, InputFileData%nWAMITObj
         iMat = Yaml_Child(Doc, iOuter, j)
         call YamlGet(Doc, '', TmpMat, TmpErrStat, TmpErrMsg, From=iMat)
         if (Failed()) return
         if (BadMatShape(TmpMat, NDOF, NDOF, Path)) return
         Dest(:,:,j) = TmpMat
      end do
   end subroutine ReadMatrixList

end subroutine ReadAddStiffnessDamping

!> axial_coefficients:AxialCoefs -- AxFDMod/AxVnCOff/AxFDLoFSc are optional per row
!! (defaults 0, -1.0, 1.0), matching the text path's 5- and 4-entry fallback reads.
subroutine ReadAxialCoefs(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadAxialCoefs'
   integer(IntKi) :: iSeq, iRow, i, N
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'axial_coefficients:AxialCoefs', iSeq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   N = int(Yaml_NumChildren(Doc, iSeq))
   InputFileData%Morison%NAxCoefs = N
   if (N > 0) then
      allocate(InputFileData%Morison%AxialCoefs(N), stat=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating space for AxialCoefs array.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      do i = 1, N
         iRow = Yaml_Child(Doc, iSeq, i)
         call YamlGet(Doc, 'AxCoefID', InputFileData%Morison%AxialCoefs(i)%AxCoefID, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'AxCd', InputFileData%Morison%AxialCoefs(i)%AxCd, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'AxCa', InputFileData%Morison%AxialCoefs(i)%AxCa, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'AxCp', InputFileData%Morison%AxialCoefs(i)%AxCp, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'AxFDMod', InputFileData%Morison%AxialCoefs(i)%AxFDMod, TmpErrStat, TmpErrMsg, &
                      Default=0_IntKi, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'AxVnCOff', InputFileData%Morison%AxialCoefs(i)%AxVnCOff, TmpErrStat, TmpErrMsg, &
                      Default=-1.0_ReKi, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'AxFDLoFSc', InputFileData%Morison%AxialCoefs(i)%AxFDLoFSc, TmpErrStat, TmpErrMsg, &
                      Default=1.0_ReKi, From=iRow)
         if (Failed()) return
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadAxialCoefs

!> member_joints:Joints
subroutine ReadJoints(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadJoints'
   integer(IntKi) :: iSeq, iRow, i, N
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'member_joints:Joints', iSeq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   N = int(Yaml_NumChildren(Doc, iSeq))
   InputFileData%Morison%NJoints = N
   if (N > 0) then
      allocate(InputFileData%Morison%InpJoints(N), stat=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating space for InpJoints array.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      do i = 1, N
         iRow = Yaml_Child(Doc, iSeq, i)
         call YamlGet(Doc, 'JointID', InputFileData%Morison%InpJoints(i)%JointID, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'Jointxi', InputFileData%Morison%InpJoints(i)%Position(1), TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'Jointyi', InputFileData%Morison%InpJoints(i)%Position(2), TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'Jointzi', InputFileData%Morison%InpJoints(i)%Position(3), TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'JointAxID', InputFileData%Morison%InpJoints(i)%JointAxID, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'JointOvrlp', InputFileData%Morison%InpJoints(i)%JointOvrlp, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadJoints

!> cylindrical_member_cross_section:MPropSetsCyl
subroutine ReadMPropSetsCyl(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadMPropSetsCyl'
   integer(IntKi) :: iSeq, iRow, i, N
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'cylindrical_member_cross_section:MPropSetsCyl', iSeq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   N = int(Yaml_NumChildren(Doc, iSeq))
   InputFileData%Morison%NPropSetsCyl = N
   if (N > 0) then
      allocate(InputFileData%Morison%MPropSetsCyl(N), stat=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating space for MPropSetsCyl array.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      do i = 1, N
         iRow = Yaml_Child(Doc, iSeq, i)
         call YamlGet(Doc, 'PropSetID', InputFileData%Morison%MPropSetsCyl(i)%PropSetID, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'PropD', InputFileData%Morison%MPropSetsCyl(i)%PropD, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'PropThck', InputFileData%Morison%MPropSetsCyl(i)%PropThck, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadMPropSetsCyl

!> rectangular_member_cross_section:MPropSetsRec
subroutine ReadMPropSetsRec(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadMPropSetsRec'
   integer(IntKi) :: iSeq, iRow, i, N
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'rectangular_member_cross_section:MPropSetsRec', iSeq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   N = int(Yaml_NumChildren(Doc, iSeq))
   InputFileData%Morison%NPropSetsRec = N
   if (N > 0) then
      allocate(InputFileData%Morison%MPropSetsRec(N), stat=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating space for MPropSetsRec array.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      do i = 1, N
         iRow = Yaml_Child(Doc, iSeq, i)
         call YamlGet(Doc, 'PropSetID', InputFileData%Morison%MPropSetsRec(i)%PropSetID, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'PropA', InputFileData%Morison%MPropSetsRec(i)%PropA, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'PropB', InputFileData%Morison%MPropSetsRec(i)%PropB, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'PropThck', InputFileData%Morison%MPropSetsRec(i)%PropThck, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadMPropSetsRec

!> Read a coefficient cell that may be either a real number or the literal keyword
!! "MCF" (case-sensitive, exact match, mirroring ParseRAryWKywrd's HasKywrd
!! semantics). A group of related cells (2 for the simple/depth-based sections, 4 for
!! the member-based sections) must ALL say "MCF" or NONE may -- mixing is fatal,
!! exactly as the text path's KywrdEntry check enforces.
subroutine GetCoefGroupMCF(Doc, From, Keys, Vals, HasMCF, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   integer(IntKi), intent(in   ), optional :: From !< absent = look up from the document root
   character(*),   intent(in   ) :: Keys(:)
   real(ReKi),     intent(  out) :: Vals(:)
   logical,        intent(  out) :: HasMCF
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'GetCoefGroupMCF'
   character(25) :: Text
   integer(IntKi) :: i, nMCF, IOS
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   nMCF    = 0

   do i = 1, size(Keys)
      if (present(From)) then
         call YamlGet(Doc, trim(Keys(i)), Text, TmpErrStat, TmpErrMsg, From=From)
      else
         call YamlGet(Doc, trim(Keys(i)), Text, TmpErrStat, TmpErrMsg)
      end if
      if (Failed()) return
      if (trim(Text) == 'MCF') then
         nMCF = nMCF + 1
         Vals(i) = 1.0_ReKi
      else
         read(Text, *, IOSTAT=IOS) Vals(i)
         if (IOS /= 0) then
            call SetErrStat(ErrID_Fatal, '"'//trim(Keys(i))//'" must be a number or "MCF"; found "'// &
               trim(Text)//'".', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
   end do

   HasMCF = (nMCF == size(Keys))
   if (nMCF > 0 .and. .not. HasMCF) then
      call SetErrStat(ErrID_Fatal, '"MCF" is used at some but not all of '//trim(Num2LStr(size(Keys)))// &
         ' related keys ('//trim(JoinKeys(Keys))//').', ErrStat, ErrMsg, RoutineName)
      return
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

   function JoinKeys(K) result(S)
      character(*),  intent(in) :: K(:)
      character(:), allocatable :: S
      integer :: j
      S = ''
      do j = 1, size(K)
         if (j > 1) S = S // ', '
         S = S // trim(K(j))
      end do
   end function JoinKeys

end subroutine GetCoefGroupMCF

!> simple_hydrodynamic_coefficients_cylindrical (a single row -- there is no count key)
subroutine ReadSimplCyl(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadSimplCyl'
   character(*), parameter :: SectionName = 'simple_hydrodynamic_coefficients_cylindrical'
   real(ReKi)     :: MCFVals(2)
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, SectionName//':SimplCd', InputFileData%Morison%SimplCd, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplCdMG', InputFileData%Morison%SimplCdMG, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplCa', InputFileData%Morison%SimplCa, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplCaMG', InputFileData%Morison%SimplCaMG, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call GetCoefGroupMCF(Doc, Keys=[character(80) :: SectionName//':SimplCp', SectionName//':SimplCpMG'], Vals=MCFVals, &
                        HasMCF=InputFileData%Morison%SimplMCF, ErrStat=TmpErrStat, ErrMsg=TmpErrMsg)
   if (Failed()) return
   InputFileData%Morison%SimplCp   = MCFVals(1)
   InputFileData%Morison%SimplCpMG = MCFVals(2)

   call YamlGet(Doc, SectionName//':SimplAxCd', InputFileData%Morison%SimplAxCd, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplAxCdMG', InputFileData%Morison%SimplAxCdMG, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplAxCa', InputFileData%Morison%SimplAxCa, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplAxCaMG', InputFileData%Morison%SimplAxCaMG, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplAxCp', InputFileData%Morison%SimplAxCp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplAxCpMG', InputFileData%Morison%SimplAxCpMG, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplCb', InputFileData%Morison%SimplCb, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplCbMG', InputFileData%Morison%SimplCbMG, TmpErrStat, TmpErrMsg)
   if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadSimplCyl

!> simple_hydrodynamic_coefficients_rectangular (a single row -- there is no count key)
subroutine ReadSimplRec(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadSimplRec'
   character(*), parameter :: SectionName = 'simple_hydrodynamic_coefficients_rectangular'
   real(ReKi)     :: MCFVals(2)
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, SectionName//':SimplRecCdA', InputFileData%Morison%SimplRecCdA, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplRecCdAMG', InputFileData%Morison%SimplRecCdAMG, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplRecCdB', InputFileData%Morison%SimplRecCdB, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplRecCdBMG', InputFileData%Morison%SimplRecCdBMG, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplRecCaA', InputFileData%Morison%SimplRecCaA, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplRecCaAMG', InputFileData%Morison%SimplRecCaAMG, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplRecCaB', InputFileData%Morison%SimplRecCaB, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplRecCaBMG', InputFileData%Morison%SimplRecCaBMG, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call GetCoefGroupMCF(Doc, Keys=[character(80) :: SectionName//':SimplRecCp', SectionName//':SimplRecCpMG'], Vals=MCFVals, &
                        HasMCF=InputFileData%Morison%SimplRecMCF, ErrStat=TmpErrStat, ErrMsg=TmpErrMsg)
   if (Failed()) return
   InputFileData%Morison%SimplRecCp   = MCFVals(1)
   InputFileData%Morison%SimplRecCpMG = MCFVals(2)

   call YamlGet(Doc, SectionName//':SimplRecAxCd', InputFileData%Morison%SimplRecAxCd, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplRecAxCdMG', InputFileData%Morison%SimplRecAxCdMG, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplRecAxCa', InputFileData%Morison%SimplRecAxCa, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplRecAxCaMG', InputFileData%Morison%SimplRecAxCaMG, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplRecAxCp', InputFileData%Morison%SimplRecAxCp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplRecAxCpMG', InputFileData%Morison%SimplRecAxCpMG, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplRecCb', InputFileData%Morison%SimplRecCb, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, SectionName//':SimplRecCbMG', InputFileData%Morison%SimplRecCbMG, TmpErrStat, TmpErrMsg)
   if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadSimplRec

!> depth_based_hydrodynamic_coefficients_cylindrical:CoefDpthsCyl -- DpthCp/DpthCpMG
!! may both say "MCF"; the MCF flag must agree across every row (checked after the
!! loop, exactly as the text path does).
subroutine ReadCoefDpthsCyl(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadCoefDpthsCyl'
   integer(IntKi) :: iSeq, iRow, i, N
   real(ReKi)     :: MCFVals(2)
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'depth_based_hydrodynamic_coefficients_cylindrical:CoefDpthsCyl', iSeq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   N = int(Yaml_NumChildren(Doc, iSeq))
   InputFileData%Morison%NCoefDpthCyl = N
   if (N > 0) then
      allocate(InputFileData%Morison%CoefDpthsCyl(N), stat=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating space for CoefDpthsCyl array.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      do i = 1, N
         iRow = Yaml_Child(Doc, iSeq, i)
         call YamlGet(Doc, 'Dpth', InputFileData%Morison%CoefDpthsCyl(i)%Dpth, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCd', InputFileData%Morison%CoefDpthsCyl(i)%DpthCd, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCdMG', InputFileData%Morison%CoefDpthsCyl(i)%DpthCdMG, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCa', InputFileData%Morison%CoefDpthsCyl(i)%DpthCa, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCaMG', InputFileData%Morison%CoefDpthsCyl(i)%DpthCaMG, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return

         call GetCoefGroupMCF(Doc, iRow, ['DpthCp  ', 'DpthCpMG'], MCFVals, &
                              InputFileData%Morison%CoefDpthsCyl(i)%DpthMCF, TmpErrStat, TmpErrMsg)
         if (Failed()) return
         InputFileData%Morison%CoefDpthsCyl(i)%DpthCp   = MCFVals(1)
         InputFileData%Morison%CoefDpthsCyl(i)%DpthCpMG = MCFVals(2)

         call YamlGet(Doc, 'DpthAxCd', InputFileData%Morison%CoefDpthsCyl(i)%DpthAxCd, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthAxCdMG', InputFileData%Morison%CoefDpthsCyl(i)%DpthAxCdMG, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthAxCa', InputFileData%Morison%CoefDpthsCyl(i)%DpthAxCa, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthAxCaMG', InputFileData%Morison%CoefDpthsCyl(i)%DpthAxCaMG, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthAxCp', InputFileData%Morison%CoefDpthsCyl(i)%DpthAxCp, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthAxCpMG', InputFileData%Morison%CoefDpthsCyl(i)%DpthAxCpMG, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCb', InputFileData%Morison%CoefDpthsCyl(i)%DpthCb, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCbMG', InputFileData%Morison%CoefDpthsCyl(i)%DpthCbMg, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
      end do

      do i = 2, N
         if (InputFileData%Morison%CoefDpthsCyl(i)%DpthMCF .neqv. InputFileData%Morison%CoefDpthsCyl(1)%DpthMCF) then
            call SetErrStat(ErrID_Fatal, 'In depth_based_hydrodynamic_coefficients_cylindrical:CoefDpthsCyl, MCF is '// &
               'specified for some depth but not others.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadCoefDpthsCyl

!> depth_based_hydrodynamic_coefficients_rectangular:CoefDpthsRec -- DpthCp/DpthCpMG
!! may both say "MCF"; the MCF flag must agree across every row.
subroutine ReadCoefDpthsRec(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadCoefDpthsRec'
   integer(IntKi) :: iSeq, iRow, i, N
   real(ReKi)     :: MCFVals(2)
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'depth_based_hydrodynamic_coefficients_rectangular:CoefDpthsRec', iSeq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   N = int(Yaml_NumChildren(Doc, iSeq))
   InputFileData%Morison%NCoefDpthRec = N
   if (N > 0) then
      allocate(InputFileData%Morison%CoefDpthsRec(N), stat=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating space for CoefDpthsRec array.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      do i = 1, N
         iRow = Yaml_Child(Doc, iSeq, i)
         call YamlGet(Doc, 'Dpth', InputFileData%Morison%CoefDpthsRec(i)%Dpth, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCdA', InputFileData%Morison%CoefDpthsRec(i)%DpthCdA, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCdAMG', InputFileData%Morison%CoefDpthsRec(i)%DpthCdAMG, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCdB', InputFileData%Morison%CoefDpthsRec(i)%DpthCdB, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCdBMG', InputFileData%Morison%CoefDpthsRec(i)%DpthCdBMG, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCaA', InputFileData%Morison%CoefDpthsRec(i)%DpthCaA, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCaAMG', InputFileData%Morison%CoefDpthsRec(i)%DpthCaAMG, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCaB', InputFileData%Morison%CoefDpthsRec(i)%DpthCaB, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCaBMG', InputFileData%Morison%CoefDpthsRec(i)%DpthCaBMG, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return

         call GetCoefGroupMCF(Doc, iRow, ['DpthCp  ', 'DpthCpMG'], MCFVals, &
                              InputFileData%Morison%CoefDpthsRec(i)%DpthMCF, TmpErrStat, TmpErrMsg)
         if (Failed()) return
         InputFileData%Morison%CoefDpthsRec(i)%DpthCp   = MCFVals(1)
         InputFileData%Morison%CoefDpthsRec(i)%DpthCpMG = MCFVals(2)

         call YamlGet(Doc, 'DpthAxCd', InputFileData%Morison%CoefDpthsRec(i)%DpthAxCd, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthAxCdMG', InputFileData%Morison%CoefDpthsRec(i)%DpthAxCdMG, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthAxCa', InputFileData%Morison%CoefDpthsRec(i)%DpthAxCa, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthAxCaMG', InputFileData%Morison%CoefDpthsRec(i)%DpthAxCaMG, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthAxCp', InputFileData%Morison%CoefDpthsRec(i)%DpthAxCp, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthAxCpMG', InputFileData%Morison%CoefDpthsRec(i)%DpthAxCpMG, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCb', InputFileData%Morison%CoefDpthsRec(i)%DpthCb, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'DpthCbMG', InputFileData%Morison%CoefDpthsRec(i)%DpthCbMg, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
      end do

      do i = 2, N
         if (InputFileData%Morison%CoefDpthsRec(i)%DpthMCF .neqv. InputFileData%Morison%CoefDpthsRec(1)%DpthMCF) then
            call SetErrStat(ErrID_Fatal, 'In depth_based_hydrodynamic_coefficients_rectangular:CoefDpthsRec, MCF is '// &
               'specified for some depth but not others.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadCoefDpthsRec

!> member_based_hydrodynamic_coefficients_cylindrical:CoefMembersCyl -- MemberCp1/
!! MemberCp2/MemberCpMG1/MemberCpMG2 may all say "MCF" (or none may).
subroutine ReadCoefMembersCyl(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadCoefMembersCyl'
   integer(IntKi) :: iSeq, iRow, i, N
   real(ReKi)     :: MCFVals(4)
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'member_based_hydrodynamic_coefficients_cylindrical:CoefMembersCyl', iSeq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   N = int(Yaml_NumChildren(Doc, iSeq))
   InputFileData%Morison%NCoefMembersCyl = N
   if (N > 0) then
      allocate(InputFileData%Morison%CoefMembersCyl(N), stat=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating space for CoefMembersCyl array.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      do i = 1, N
         iRow = Yaml_Child(Doc, iSeq, i)
         call YamlGet(Doc, 'MemberID', InputFileData%Morison%CoefMembersCyl(i)%MemberID, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCd1', InputFileData%Morison%CoefMembersCyl(i)%MemberCd1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCd2', InputFileData%Morison%CoefMembersCyl(i)%MemberCd2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCdMG1', InputFileData%Morison%CoefMembersCyl(i)%MemberCdMG1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCdMG2', InputFileData%Morison%CoefMembersCyl(i)%MemberCdMG2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCa1', InputFileData%Morison%CoefMembersCyl(i)%MemberCa1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCa2', InputFileData%Morison%CoefMembersCyl(i)%MemberCa2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCaMG1', InputFileData%Morison%CoefMembersCyl(i)%MemberCaMG1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCaMG2', InputFileData%Morison%CoefMembersCyl(i)%MemberCaMG2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return

         call GetCoefGroupMCF(Doc, iRow, ['MemberCp1  ', 'MemberCp2  ', 'MemberCpMG1', 'MemberCpMG2'], MCFVals, &
                              InputFileData%Morison%CoefMembersCyl(i)%MemberMCF, TmpErrStat, TmpErrMsg)
         if (Failed()) return
         InputFileData%Morison%CoefMembersCyl(i)%MemberCp1   = MCFVals(1)
         InputFileData%Morison%CoefMembersCyl(i)%MemberCp2   = MCFVals(2)
         InputFileData%Morison%CoefMembersCyl(i)%MemberCpMG1 = MCFVals(3)
         InputFileData%Morison%CoefMembersCyl(i)%MemberCpMG2 = MCFVals(4)

         call YamlGet(Doc, 'MemberAxCd1', InputFileData%Morison%CoefMembersCyl(i)%MemberAxCd1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCd2', InputFileData%Morison%CoefMembersCyl(i)%MemberAxCd2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCdMG1', InputFileData%Morison%CoefMembersCyl(i)%MemberAxCdMG1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCdMG2', InputFileData%Morison%CoefMembersCyl(i)%MemberAxCdMG2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCa1', InputFileData%Morison%CoefMembersCyl(i)%MemberAxCa1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCa2', InputFileData%Morison%CoefMembersCyl(i)%MemberAxCa2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCaMG1', InputFileData%Morison%CoefMembersCyl(i)%MemberAxCaMG1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCaMG2', InputFileData%Morison%CoefMembersCyl(i)%MemberAxCaMG2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCp1', InputFileData%Morison%CoefMembersCyl(i)%MemberAxCp1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCp2', InputFileData%Morison%CoefMembersCyl(i)%MemberAxCp2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCpMG1', InputFileData%Morison%CoefMembersCyl(i)%MemberAxCpMG1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCpMG2', InputFileData%Morison%CoefMembersCyl(i)%MemberAxCpMG2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCb1', InputFileData%Morison%CoefMembersCyl(i)%MemberCb1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCb2', InputFileData%Morison%CoefMembersCyl(i)%MemberCb2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCbMG1', InputFileData%Morison%CoefMembersCyl(i)%MemberCbMG1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCbMG2', InputFileData%Morison%CoefMembersCyl(i)%MemberCbMG2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadCoefMembersCyl

!> member_based_hydrodynamic_coefficients_rectangular:CoefMembersRec -- MemberCp1/
!! MemberCp2/MemberCpMG1/MemberCpMG2 may all say "MCF" (or none may).
subroutine ReadCoefMembersRec(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadCoefMembersRec'
   integer(IntKi) :: iSeq, iRow, i, N
   real(ReKi)     :: MCFVals(4)
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'member_based_hydrodynamic_coefficients_rectangular:CoefMembersRec', iSeq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   N = int(Yaml_NumChildren(Doc, iSeq))
   InputFileData%Morison%NCoefMembersRec = N
   if (N > 0) then
      allocate(InputFileData%Morison%CoefMembersRec(N), stat=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating space for CoefMembersRec array.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      do i = 1, N
         iRow = Yaml_Child(Doc, iSeq, i)
         call YamlGet(Doc, 'MemberID', InputFileData%Morison%CoefMembersRec(i)%MemberID, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCdA1', InputFileData%Morison%CoefMembersRec(i)%MemberCdA1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCdA2', InputFileData%Morison%CoefMembersRec(i)%MemberCdA2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCdAMG1', InputFileData%Morison%CoefMembersRec(i)%MemberCdAMG1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCdAMG2', InputFileData%Morison%CoefMembersRec(i)%MemberCdAMG2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCdB1', InputFileData%Morison%CoefMembersRec(i)%MemberCdB1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCdB2', InputFileData%Morison%CoefMembersRec(i)%MemberCdB2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCdBMG1', InputFileData%Morison%CoefMembersRec(i)%MemberCdBMG1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCdBMG2', InputFileData%Morison%CoefMembersRec(i)%MemberCdBMG2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCaA1', InputFileData%Morison%CoefMembersRec(i)%MemberCaA1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCaA2', InputFileData%Morison%CoefMembersRec(i)%MemberCaA2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCaAMG1', InputFileData%Morison%CoefMembersRec(i)%MemberCaAMG1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCaAMG2', InputFileData%Morison%CoefMembersRec(i)%MemberCaAMG2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCaB1', InputFileData%Morison%CoefMembersRec(i)%MemberCaB1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCaB2', InputFileData%Morison%CoefMembersRec(i)%MemberCaB2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCaBMG1', InputFileData%Morison%CoefMembersRec(i)%MemberCaBMG1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCaBMG2', InputFileData%Morison%CoefMembersRec(i)%MemberCaBMG2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return

         call GetCoefGroupMCF(Doc, iRow, ['MemberCp1  ', 'MemberCp2  ', 'MemberCpMG1', 'MemberCpMG2'], MCFVals, &
                              InputFileData%Morison%CoefMembersRec(i)%MemberMCF, TmpErrStat, TmpErrMsg)
         if (Failed()) return
         InputFileData%Morison%CoefMembersRec(i)%MemberCp1   = MCFVals(1)
         InputFileData%Morison%CoefMembersRec(i)%MemberCp2   = MCFVals(2)
         InputFileData%Morison%CoefMembersRec(i)%MemberCpMG1 = MCFVals(3)
         InputFileData%Morison%CoefMembersRec(i)%MemberCpMG2 = MCFVals(4)

         call YamlGet(Doc, 'MemberAxCd1', InputFileData%Morison%CoefMembersRec(i)%MemberAxCd1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCd2', InputFileData%Morison%CoefMembersRec(i)%MemberAxCd2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCdMG1', InputFileData%Morison%CoefMembersRec(i)%MemberAxCdMG1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCdMG2', InputFileData%Morison%CoefMembersRec(i)%MemberAxCdMG2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCa1', InputFileData%Morison%CoefMembersRec(i)%MemberAxCa1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCa2', InputFileData%Morison%CoefMembersRec(i)%MemberAxCa2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCaMG1', InputFileData%Morison%CoefMembersRec(i)%MemberAxCaMG1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCaMG2', InputFileData%Morison%CoefMembersRec(i)%MemberAxCaMG2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCp1', InputFileData%Morison%CoefMembersRec(i)%MemberAxCp1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCp2', InputFileData%Morison%CoefMembersRec(i)%MemberAxCp2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCpMG1', InputFileData%Morison%CoefMembersRec(i)%MemberAxCpMG1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberAxCpMG2', InputFileData%Morison%CoefMembersRec(i)%MemberAxCpMG2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCb1', InputFileData%Morison%CoefMembersRec(i)%MemberCb1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCb2', InputFileData%Morison%CoefMembersRec(i)%MemberCb2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCbMG1', InputFileData%Morison%CoefMembersRec(i)%MemberCbMG1, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MemberCbMG2', InputFileData%Morison%CoefMembersRec(i)%MemberCbMG2, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadCoefMembersRec

!> members:Members -- FDMod/VnCOffA/VnCOffB/FDLoFScA/FDLoFScB are optional per row
!! (defaults 0, -1.0, -1.0, 1.0, 1.0, matching the text path's 11-entry fallback
!! read); when any of them IS given, the same "only applicable to rectangular
!! members"/"only applicable with FDMod>0" diagnostics as the text path's full
!! 16-entry read are emitted (a row that omits all five stays silent, exactly like
!! the text path's short-row fallback).
subroutine ReadMembers(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadMembers'
   integer(IntKi) :: iSeq, iRow, i, N
   logical        :: OptFound(5)
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'members:Members', iSeq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   N = int(Yaml_NumChildren(Doc, iSeq))
   InputFileData%Morison%NMembers = N
   if (N > 0) then
      allocate(InputFileData%Morison%InpMembers(N), stat=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating space for InpMembers array.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      do i = 1, N
         iRow = Yaml_Child(Doc, iSeq, i)
         associate (Mbr => InputFileData%Morison%InpMembers(i))
            call YamlGet(Doc, 'MemberID', Mbr%MemberID, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'MJointID1', Mbr%MJointID1, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'MJointID2', Mbr%MJointID2, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'MPropSetID1', Mbr%MPropSetID1, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'MPropSetID2', Mbr%MPropSetID2, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'MSecGeom', Mbr%MSecGeom, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'MSpinOrient', Mbr%MSpinOrient, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return
            Mbr%MSpinOrient = Mbr%MSpinOrient * D2R
            call YamlGet(Doc, 'MDivSize', Mbr%MDivSize, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'MCoefMod', Mbr%MCoefMod, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'MHstLMod', Mbr%MHstLMod, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'PropPot', Mbr%PropPot, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return

            call YamlGet(Doc, 'FDMod', Mbr%FDMod, TmpErrStat, TmpErrMsg, Default=0_IntKi, Found=OptFound(1), From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'VnCOffA', Mbr%VnCOffA, TmpErrStat, TmpErrMsg, Default=-1.0_ReKi, Found=OptFound(2), From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'VnCOffB', Mbr%VnCOffB, TmpErrStat, TmpErrMsg, Default=-1.0_ReKi, Found=OptFound(3), From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'FDLoFScA', Mbr%FDLoFScA, TmpErrStat, TmpErrMsg, Default=1.0_ReKi, Found=OptFound(4), From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'FDLoFScB', Mbr%FDLoFScB, TmpErrStat, TmpErrMsg, Default=1.0_ReKi, Found=OptFound(5), From=iRow)
            if (Failed()) return

            if (.not. any(OptFound)) then
               ! none of the optional entries given: the text path's silent short-row
               ! fallback (defaults already applied above; no diagnostics)
            else if (Mbr%MSecGeom /= MSecGeom_Rec) then
               call WrScr('HydroDyn Warning: The optional member inputs FDMod, VnCOffA, VnCOffB, FDLoFScA, and '// &
                  'FDLoFScB are only applicable to members with rectangular sections. These will be ignored for '// &
                  'Member ID '//trim(Num2LStr(Mbr%MemberID))//'. ')
               Mbr%FDMod    = 0_IntKi
               Mbr%VnCOffA  = -1.0_ReKi
               Mbr%VnCOffB  = -1.0_ReKi
               Mbr%FDLoFScA = 1.0_ReKi
               Mbr%FDLoFScB = 1.0_ReKi
            else if (Mbr%FDMod == 0_IntKi) then
               call WrScr('HydroDyn Warning: Velocity filtering for rectangular-member transverse drag force is '// &
                  'only available with FDMod = 1 or 2. The optional member inputs VnCOffA, VnCOffB, FDLoFScA, and '// &
                  'FDLoFScB will be ignored for Member ID '//trim(Num2LStr(Mbr%MemberID))//'. ')
               Mbr%VnCOffA  = -1.0_ReKi
               Mbr%VnCOffB  = -1.0_ReKi
               Mbr%FDLoFScA = 1.0_ReKi
               Mbr%FDLoFScB = 1.0_ReKi
            end if
         end associate
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadMembers

!> filled_members:FilledGroups -- FillNumM derives from the FillMList length;
!! FillDens accepts the literal scalar "default"/"DEFAULT" (resolved later, once
!! WtrDens is known, in HydroDynInput_ProcessInitData).
subroutine ReadFilledGroups(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadFilledGroups'
   integer(IntKi) :: iSeq, iRow, i, N
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'filled_members:FilledGroups', iSeq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   N = int(Yaml_NumChildren(Doc, iSeq))
   InputFileData%Morison%NFillGroups = N
   if (N > 0) then
      allocate(InputFileData%Morison%FilledGroups(N), stat=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating space for FilledGroups array.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      do i = 1, N
         iRow = Yaml_Child(Doc, iSeq, i)
         associate (Grp => InputFileData%Morison%FilledGroups(i))
            call YamlGet(Doc, 'FillMList', Grp%FillMList, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return
            Grp%FillNumM = size(Grp%FillMList)
            call YamlGet(Doc, 'FillFSLoc', Grp%FillFSLoc, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'FillDens', Grp%FillDensChr, TmpErrStat, TmpErrMsg, Default='DEFAULT', From=iRow)
            if (Failed()) return
         end associate
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadFilledGroups

!> marine_growth_by_depth:MGDepths
subroutine ReadMGDepths(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadMGDepths'
   integer(IntKi) :: iSeq, iRow, i, N
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'marine_growth_by_depth:MGDepths', iSeq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   N = int(Yaml_NumChildren(Doc, iSeq))
   InputFileData%Morison%NMGDepths = N
   if (N > 0) then
      allocate(InputFileData%Morison%MGDepths(N), stat=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating space for MGDepths array.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      do i = 1, N
         iRow = Yaml_Child(Doc, iSeq, i)
         call YamlGet(Doc, 'MGDpth', InputFileData%Morison%MGDepths(i)%MGDpth, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MGThck', InputFileData%Morison%MGDepths(i)%MGThck, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
         call YamlGet(Doc, 'MGDens', InputFileData%Morison%MGDepths(i)%MGDens, TmpErrStat, TmpErrMsg, From=iRow)
         if (Failed()) return
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadMGDepths

!> member_output_list:MOutLst -- NOutLoc derives from the NodeLocs length
subroutine ReadMOutLst(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadMOutLst'
   integer(IntKi) :: iSeq, iRow, i, N
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'member_output_list:MOutLst', iSeq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   N = int(Yaml_NumChildren(Doc, iSeq))
   InputFileData%Morison%NMOutputs = N
   if (N > 0) then
      allocate(InputFileData%Morison%MOutLst(N), stat=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating space for MOutLst array.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      do i = 1, N
         iRow = Yaml_Child(Doc, iSeq, i)
         associate (Out => InputFileData%Morison%MOutLst(i))
            call YamlGet(Doc, 'MemberID', Out%MemberID, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return
            call YamlGet(Doc, 'NodeLocs', Out%NodeLocs, TmpErrStat, TmpErrMsg, From=iRow)
            if (Failed()) return
            Out%NOutLoc = size(Out%NodeLocs)
            call AllocAry(Out%MeshIndx1, Out%NOutLoc, 'MeshIndx1', TmpErrStat, TmpErrMsg)
            if (Failed()) return
            call AllocAry(Out%MemberIndx1, Out%NOutLoc, 'MemberIndx1', TmpErrStat, TmpErrMsg)
            if (Failed()) return
            call AllocAry(Out%MeshIndx2, Out%NOutLoc, 'MeshIndx2', TmpErrStat, TmpErrMsg)
            if (Failed()) return
            call AllocAry(Out%MemberIndx2, Out%NOutLoc, 'MemberIndx2', TmpErrStat, TmpErrMsg)
            if (Failed()) return
            call AllocAry(Out%s, Out%NOutLoc, 's', TmpErrStat, TmpErrMsg)
            if (Failed()) return
         end associate
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadMOutLst

!> joint_output_list:JOutLst -- a flat list of joint IDs (the one counted table that
!! is not itself tabular)
subroutine ReadJOutLst(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadJOutLst'
   integer(IntKi), allocatable :: TmpAry(:)
   integer(IntKi) :: i, N
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'joint_output_list:JOutLst', TmpAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   N = size(TmpAry)
   InputFileData%Morison%NJOutputs = N
   if (N > 0) then
      allocate(InputFileData%Morison%JOutLst(N), stat=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating space for JOutLst data structures.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      do i = 1, N
         InputFileData%Morison%JOutLst(i)%JointID = TmpAry(i)
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadJOutLst

!> output_channels:OutList -- NUserOutputs derives from the OutList length
subroutine ReadOutputChannels(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(HydroDyn_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'ReadOutputChannels'
   character(:), allocatable  :: TmpList(:)
   integer(IntKi)              :: i
   integer(IntKi)              :: TmpErrStat
   character(ErrMsgLen)        :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'output_channels:OutList', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpList) > MaxUserOutputs) then
      call SetErrStat(ErrID_Fatal, 'OutList may contain at most '//trim(Num2LStr(MaxUserOutputs))// &
         ' channels; found '//trim(Num2LStr(size(TmpList)))//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   call AllocAry(InputFileData%UserOutputs, MaxUserOutputs, "HydroDyn Input File's UserOutputs", TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%UserOutputs = ''
   InputFileData%NUserOutputs = size(TmpList)
   do i = 1, InputFileData%NUserOutputs
      if (len_trim(TmpList(i)) > ChanLen) then
         call SetErrStat(ErrID_Fatal, 'OutList entry "'//trim(TmpList(i))//'" is longer than the '// &
            trim(Num2LStr(ChanLen))//'-character channel-name limit.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      InputFileData%UserOutputs(i) = TmpList(i)
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadOutputChannels

end module HydroDyn_Yaml
