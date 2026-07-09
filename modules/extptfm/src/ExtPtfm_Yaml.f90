!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of ExtPtfm_MCKF.
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
!> Reader for the YAML form of the ExtPtfm_MCKF primary input file. Fills the same
!! ExtPtfm_InputFile (InputFileData) scalar/list fields that the text-format ReadPrimaryFile
!! reader (in ExtPtfm_MCKF_IO.f90) fills up through the OutList section -- everything the text
!! reader reads from the primary file itself, and nothing more.
!!
!! Funnel structure (mirrors SubDyn's SD_Input): the format-specific reading branch is what
!! this module replaces. The cross-cutting post-processing that ReadPrimaryFile performs after
!! the read (ReadReducedFile -> the Guyan/Craig-Bampton mass/damping/stiffness reduced file;
!! CheckReducedInputs; ReadConnFile/CheckConnInputs; ReadForceFile for the user/connection
!! forcing time series; ReduceNumberOfDOF) is identical for both formats, runs once, and stays
!! in ReadPrimaryFile -- so this module is USEd BY ExtPtfm_MCKF_IO at the funnel call site and
!! only fills InputFileData, never touching the reduced/connection/forcing files.
!!
!! Schema: top-level keys mirror the text file's section banners: simulation_control,
!! reduction_inputs, connections, user_forcing, output, outputs.
!!
!! Second-order / path-only files (NEVER inlined -- they stay path strings, resolved relative
!! to the primary file exactly like the text path, in the shared ReadPrimaryFile
!! post-processing):
!!  - reduction_inputs:RedFile   -- the large Guyan/Craig-Bampton reduced superelement data
!!                                  (mass/damping/stiffness matrices), read by ReadReducedFile.
!!  - connections:ConnFile       -- the connection-points file, read by ReadConnFile.
!!  - user_forcing:ForceFile     -- the user modal-forcing time series, read by ReadForceFile.
!!  - user_forcing:FConnFile     -- the user connection-forcing time series, read by ReadForceFile.
!!
!! Counts derive from list lengths per the project-wide rule: the text file's NActiveDOFList,
!! NInitPosList, NInitVelList, and NumOuts are never separate YAML keys.
!!
!! ActiveCBDOF / InitPosList / InitVelList allocation semantics (matched to the text path,
!! whose allocation *state* -- not just contents -- is read downstream via `allocated(...)`):
!!  - reduction_inputs:ActiveCBDOF absent  -> unallocated (text NActiveDOFList < 0: "all CB
!!    modes active", no DOF reduction).  Present (incl. an empty list []) -> allocated to its
!!    length (text NActiveDOFList >= 0: reduce to exactly the listed DOF; length 0 == Guyan
!!    modes only).
!!  - reduction_inputs:InitPosList / InitVelList absent or empty -> unallocated (text
!!    NInitPosList/NInitVelList <= 0: all DOF initialized to 0). Present with >=1 value ->
!!    allocated and read.
!!
!! Deliberate simplifications versus the text format (documented, not silent):
!!  - There is no "FileFormat"/legacy-format branch in ExtPtfm's primary-file reader (unlike
!!    SubDyn): the primary file is a single fixed-schema text file, so the YAML schema is a
!!    straight 1:1 of that single format with no legacy fallbacks to reproduce.
!!  - simulation_control:DT accepts the literal "default" (== the text path's "DEFAULT"
!!    sentinel, stored as DT = -1 so the glue code substitutes its coupling interval) or a
!!    number, exactly like the text path.
!!  - The two commented-out text-reader fields (RedFileCst, EquilStart) have no YAML key, since
!!    the text path does not read them either; they keep their type defaults on both paths.
module ExtPtfm_Yaml

   use NWTC_Library
   use ExtPtfm_MCKF_Types
   use YamlInput

   implicit none
   private

   public :: ExtPtfm_ParseYamlFile
   public :: ExtPtfm_ParseYamlFileInfo

contains

!> Load and parse a YAML-format ExtPtfm primary input file.
subroutine ExtPtfm_ParseYamlFile(InputFile, PriPath, InputFileData, ErrStat, ErrMsg)
   character(*),            intent(in   ) :: InputFile     !< the .yaml primary input file
   character(*),            intent(in   ) :: PriPath       !< path of the primary file (for relative sub-file resolution)
   type(ExtPtfm_InputFile), intent(inout) :: InputFileData
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ExtPtfm_ParseYamlFile'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call Yaml_LoadFile(InputFile, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call ParseYamlDoc(Doc, PriPath, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! typo guard: anything never looked up is reported (warning severity)
   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ExtPtfm_ParseYamlFile

!> Parse YAML-format ExtPtfm input arriving as a FileInfoType -- the passed-data channel used
!! for inline module input from a YAML primary file (the glue code's inline SubFile handover
!! when CompSub selects ExtPtfm_MCKF).
subroutine ExtPtfm_ParseYamlFileInfo(FileInfoIn, PriPath, InputFileData, ErrStat, ErrMsg)
   type(FileInfoType),      intent(in   ) :: FileInfoIn    !< YAML text lines with provenance
   character(*),            intent(in   ) :: PriPath       !< path of the primary file (for relative sub-file resolution)
   type(ExtPtfm_InputFile), intent(inout) :: InputFileData
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ExtPtfm_ParseYamlFileInfo'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call Yaml_LoadFileInfo(FileInfoIn, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call ParseYamlDoc(Doc, PriPath, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ExtPtfm_ParseYamlFileInfo

!> Fill InputFileData from a parsed document. Split from the two file-entry wrappers,
!! mirroring the project-wide ParseYamlDoc convention.
subroutine ParseYamlDoc(Doc, PriPath, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),           intent(inout) :: Doc
   character(*),            intent(in   ) :: PriPath
   type(ExtPtfm_InputFile), intent(inout) :: InputFileData
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ExtPtfm_ParseYamlDoc'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call ParseSimulationControl(Doc, InputFileData, TmpErrStat, TmpErrMsg);       if (Failed()) return
   call ParseReductionInputs(Doc, PriPath, InputFileData, TmpErrStat, TmpErrMsg); if (Failed()) return
   call ParseConnections(Doc, PriPath, InputFileData, TmpErrStat, TmpErrMsg);     if (Failed()) return
   call ParseUserForcing(Doc, PriPath, InputFileData, TmpErrStat, TmpErrMsg);     if (Failed()) return
   call ParseOutput(Doc, InputFileData, TmpErrStat, TmpErrMsg);                   if (Failed()) return
   call ParseOutList(Doc, InputFileData, TmpErrStat, TmpErrMsg);                  if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseYamlDoc

!----------------------------------------------------------------------------------------
! section parsers
!----------------------------------------------------------------------------------------

!> simulation_control: Echo (consumed by the caller before this is reached; re-read here so it
!! is marked "used" and not flagged by the typo guard), DT ("default" or a number), IntMethod.
subroutine ParseSimulationControl(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),           intent(inout) :: Doc
   type(ExtPtfm_InputFile), intent(inout) :: InputFileData
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ExtPtfm_ParseSimulationControl'
   character(64)           :: ChStr
   logical                 :: Echo
   integer(IntKi)          :: IOS
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'simulation_control:Echo', Echo, TmpErrStat, TmpErrMsg, Default=.false.); if (Failed()) return

   ! DT: -1 is the text path's "use the glue code's coupling interval" sentinel; the literal
   ! "default" keeps DT = -1 (Default='DEFAULT' lets YamlGet return the literal text back --
   ! same as BeamDyn_Yaml/SubDyn_Yaml's DT handling), otherwise read the number.
   InputFileData%DT = -1.0_DbKi
   call YamlGet(Doc, 'simulation_control:DT', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if (trim(ChStr) /= 'DEFAULT') then
      read(ChStr, *, iostat=IOS) InputFileData%DT
      call CheckIOS(IOS, '', 'DT', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if

   call YamlGet(Doc, 'simulation_control:IntMethod', InputFileData%IntMethod, TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseSimulationControl

!> reduction_inputs: RBMod, RedFile (path), ActiveCBDOF (optional list), InitPosList and
!! InitVelList (optional lists). See the module header for the allocation semantics of the
!! three optional lists.
subroutine ParseReductionInputs(Doc, PriPath, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),           intent(inout) :: Doc
   character(*),            intent(in   ) :: PriPath
   type(ExtPtfm_InputFile), intent(inout) :: InputFileData
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter     :: RoutineName = 'ExtPtfm_ParseReductionInputs'
   integer(IntKi), allocatable :: TmpIntAry(:)
   real(ReKi),     allocatable :: TmpReAry(:)
   logical                     :: WasFound
   integer(IntKi)              :: TmpErrStat
   character(ErrMsgLen)        :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'reduction_inputs:RBMod', InputFileData%RBMod, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%hasRBMode = InputFileData%RBMod > 0_IntKi

   call YamlGet(Doc, 'reduction_inputs:RedFile', InputFileData%RedFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (PathIsRelative(InputFileData%RedFile)) InputFileData%RedFile = trim(PriPath)//trim(InputFileData%RedFile)

   ! ActiveCBDOF: absent -> unallocated (all CB modes); present (incl. []) -> allocated to length
   call YamlGet(Doc, 'reduction_inputs:ActiveCBDOF', TmpIntAry, TmpErrStat, TmpErrMsg, Found=WasFound); if (Failed()) return
   if (WasFound) then
      call AllocAry(InputFileData%ActiveCBDOF, size(TmpIntAry), 'ActiveCBDOF', TmpErrStat, TmpErrMsg); if (Failed()) return
      if (size(TmpIntAry) > 0) InputFileData%ActiveCBDOF = TmpIntAry
   end if

   ! InitPosList / InitVelList: absent or empty -> unallocated; present with values -> allocated
   call YamlGet(Doc, 'reduction_inputs:InitPosList', TmpReAry, TmpErrStat, TmpErrMsg, Found=WasFound); if (Failed()) return
   if (WasFound) then
      if (size(TmpReAry) > 0) then
         call AllocAry(InputFileData%InitPosList, size(TmpReAry), 'InitPosList', TmpErrStat, TmpErrMsg); if (Failed()) return
         InputFileData%InitPosList = TmpReAry
      end if
   end if

   call YamlGet(Doc, 'reduction_inputs:InitVelList', TmpReAry, TmpErrStat, TmpErrMsg, Found=WasFound); if (Failed()) return
   if (WasFound) then
      if (size(TmpReAry) > 0) then
         call AllocAry(InputFileData%InitVelList, size(TmpReAry), 'InitVelList', TmpErrStat, TmpErrMsg); if (Failed()) return
         InputFileData%InitVelList = TmpReAry
      end if
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseReductionInputs

!> connections: HasConnections, ConnFile (path, resolved relative to the primary file).
subroutine ParseConnections(Doc, PriPath, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),           intent(inout) :: Doc
   character(*),            intent(in   ) :: PriPath
   type(ExtPtfm_InputFile), intent(inout) :: InputFileData
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ExtPtfm_ParseConnections'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'connections:HasConnections', InputFileData%HasConnections, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'connections:ConnFile', InputFileData%ConnFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (PathIsRelative(InputFileData%ConnFile)) InputFileData%ConnFile = trim(PriPath)//trim(InputFileData%ConnFile)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseConnections

!> user_forcing: HasUserForcing, ForceFile (path), HasConnForcing, FConnFile (path). Both file
!! paths are resolved relative to the primary file, exactly like the text path.
subroutine ParseUserForcing(Doc, PriPath, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),           intent(inout) :: Doc
   character(*),            intent(in   ) :: PriPath
   type(ExtPtfm_InputFile), intent(inout) :: InputFileData
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ExtPtfm_ParseUserForcing'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'user_forcing:HasUserForcing', InputFileData%HasUserForcing, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'user_forcing:ForceFile', InputFileData%ForceFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (PathIsRelative(InputFileData%ForceFile)) InputFileData%ForceFile = trim(PriPath)//trim(InputFileData%ForceFile)

   call YamlGet(Doc, 'user_forcing:HasConnForcing', InputFileData%HasConnForcing, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'user_forcing:FConnFile', InputFileData%FConnFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (PathIsRelative(InputFileData%FConnFile)) InputFileData%FConnFile = trim(PriPath)//trim(InputFileData%FConnFile)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseUserForcing

!> output: SumPrint, OutFile, TabDelim, OutFmt, Tstart.
subroutine ParseOutput(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),           intent(inout) :: Doc
   type(ExtPtfm_InputFile), intent(inout) :: InputFileData
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ExtPtfm_ParseOutput'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'output:SumPrint', InputFileData%SumPrint, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:OutFile',  InputFileData%OutFile,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:TabDelim', InputFileData%TabDelim, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:OutFmt',   InputFileData%OutFmt,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:Tstart',   InputFileData%Tstart,   TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseOutput

!> outputs:OutList -- a sequence of channel names (no terminating END, per project
!! convention). NumOuts derives from the list length.
subroutine ParseOutList(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),           intent(inout) :: Doc
   type(ExtPtfm_InputFile), intent(inout) :: InputFileData
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'ExtPtfm_ParseOutList'
   character(:), allocatable :: TmpList(:)
   integer(IntKi)            :: i
   integer(IntKi)            :: TmpErrStat
   character(ErrMsgLen)      :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'outputs:OutList', TmpList, TmpErrStat, TmpErrMsg); if (Failed()) return

   InputFileData%NumOuts = size(TmpList)
   call AllocAry(InputFileData%OutList, max(InputFileData%NumOuts, 1), 'ExtPtfm Input File OutList', TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%OutList = ''
   do i = 1, InputFileData%NumOuts
      if (len_trim(TmpList(i)) > ChanLen) then
         call SetErrStat(ErrID_Fatal, 'outputs:OutList entry "'//trim(TmpList(i))//'" is longer than the '// &
            trim(Num2LStr(ChanLen))//'-character channel-name limit.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      InputFileData%OutList(i) = TmpList(i)
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseOutList

end module ExtPtfm_Yaml
