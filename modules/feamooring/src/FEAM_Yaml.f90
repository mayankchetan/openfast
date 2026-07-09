!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of FEAMooring.
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
!> Reader for the YAML form of the FEAMooring primary input file. Fills the same
!! FEAM_InputFile (InputFileData) scalar/list fields that the text-format ReadPrimaryFile
!! reader (in FEAM.f90) fills -- everything the text reader reads from the primary file
!! itself, and nothing more.
!!
!! Funnel structure (mirrors SubDyn's SD_Input / ExtPtfm's ReadPrimaryFile): the
!! format-specific reading branch is what this module replaces. The one cross-cutting
!! post-processing step that ReadPrimaryFile performs after the read -- converting each
!! line's anchor/fairlead azimuth angle from degrees to radians (LAngAnch, LAngFair *= D2R)
!! -- is identical for both formats, runs once, and stays in ReadPrimaryFile. So this
!! module fills LAngAnch/LAngFair in raw degrees exactly like the text branch fills them
!! before the shared conversion, and never applies D2R itself.
!!
!! Schema: top-level keys mirror the text file's section banners:
!!   simulation_control : Echo, DT, NumElems, Gravity, WtrDens, MaxIter, Eps
!!   lines              : a sequence of per-line mappings (one entry per mooring line)
!!   output             : SumPrint, OutFile, TabDelim, OutFmt, Tstart
!!   outputs            : OutList
!!
!! No path-only / second-order external files: unlike ExtPtfm (RedFile/ConnFile/...) or
!! SubDyn (SSIfile), FEAMooring's primary input file references no further data files, so
!! every field is inlined in the YAML and nothing stays a path.
!!
!! Counts derive from list lengths per the project-wide rule: the text file's NumLines is
!! never a YAML key -- it is len(lines) -- and NumOuts is len(outputs:OutList). (NumElems
!! is kept as a scalar key: it is the number of finite elements per line, not a list count.)
!!
!! DT / Gravity / WtrDens "default" semantics (matched to the text path): FEAM_ReadInput
!! pre-seeds InputFileData%DT/Gravity/WtrDens with the glue code's defaults *before* the
!! reader runs, and the text reader overwrites them only when the field is not the literal
!! "default". This reader does the same: the key defaults to the literal 'DEFAULT' and the
!! pre-seeded value is kept unless a number is supplied.
module FEAM_Yaml

   use NWTC_Library
   use FEAMooring_Types
   use YamlInput

   implicit none
   private

   public :: FEAM_ParseYamlFile
   public :: FEAM_ParseYamlFileInfo

contains

!> Load and parse a YAML-format FEAMooring primary input file.
subroutine FEAM_ParseYamlFile(InputFile, PriPath, InputFileData, ErrStat, ErrMsg)
   character(*),         intent(in   ) :: InputFile     !< the .yaml primary input file
   character(*),         intent(in   ) :: PriPath       !< path of the primary file (for relative sub-file resolution)
   type(FEAM_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'FEAM_ParseYamlFile'
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

end subroutine FEAM_ParseYamlFile

!> Parse YAML-format FEAMooring input arriving as a FileInfoType -- the passed-data channel
!! used for inline module input from a YAML primary file (the glue code's inline MooringFile
!! handover when CompMooring selects FEAMooring).
subroutine FEAM_ParseYamlFileInfo(FileInfoIn, PriPath, InputFileData, ErrStat, ErrMsg)
   type(FileInfoType),   intent(in   ) :: FileInfoIn    !< YAML text lines with provenance
   character(*),         intent(in   ) :: PriPath       !< path of the primary file (for relative sub-file resolution)
   type(FEAM_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'FEAM_ParseYamlFileInfo'
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

end subroutine FEAM_ParseYamlFileInfo

!> Fill InputFileData from a parsed document. Split from the two file-entry wrappers,
!! mirroring the project-wide ParseYamlDoc convention. PriPath is currently unused (FEAMooring
!! references no further files from its primary input) but is threaded through for parity with
!! the sibling YAML readers and in case a path-only field is ever added.
subroutine ParseYamlDoc(Doc, PriPath, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   character(*),         intent(in   ) :: PriPath
   type(FEAM_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'FEAM_ParseYamlDoc'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call ParseSimulationControl(Doc, InputFileData, TmpErrStat, TmpErrMsg); if (Failed()) return
   call ParseLines(Doc, InputFileData, TmpErrStat, TmpErrMsg);             if (Failed()) return
   call ParseOutput(Doc, InputFileData, TmpErrStat, TmpErrMsg);            if (Failed()) return
   call ParseOutList(Doc, InputFileData, TmpErrStat, TmpErrMsg);           if (Failed()) return

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
!! is marked "used" and not flagged by the typo guard), DT / Gravity / WtrDens (each "default"
!! or a number -- "default" keeps the glue-code value already stored in InputFileData),
!! NumElems, MaxIter, Eps. NumLines is NOT read here: it derives from the lines list length.
subroutine ParseSimulationControl(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   type(FEAM_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'FEAM_ParseSimulationControl'
   character(64)           :: ChStr
   logical                 :: Echo
   integer(IntKi)          :: IOS
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'simulation_control:Echo', Echo, TmpErrStat, TmpErrMsg, Default=.false.); if (Failed()) return

   ! DT: "default" keeps the glue-code communication interval already stored in InputFileData%DT
   call YamlGet(Doc, 'simulation_control:DT', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if (trim(ChStr) /= 'DEFAULT') then
      read(ChStr, *, iostat=IOS) InputFileData%DT
      call CheckIOS(IOS, '', 'DT', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if

   call YamlGet(Doc, 'simulation_control:NumElems', InputFileData%NumElems, TmpErrStat, TmpErrMsg); if (Failed()) return

   ! Gravity: "default" keeps the glue-code gravitational acceleration already stored
   call YamlGet(Doc, 'simulation_control:Gravity', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if (trim(ChStr) /= 'DEFAULT') then
      read(ChStr, *, iostat=IOS) InputFileData%Gravity
      call CheckIOS(IOS, '', 'Gravity', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if

   ! WtrDens: "default" keeps the glue-code water density already stored
   call YamlGet(Doc, 'simulation_control:WtrDens', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if (trim(ChStr) /= 'DEFAULT') then
      read(ChStr, *, iostat=IOS) InputFileData%WtrDens
      call CheckIOS(IOS, '', 'WtrDens', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if

   call YamlGet(Doc, 'simulation_control:MaxIter', InputFileData%MaxIter, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'simulation_control:Eps',     InputFileData%Eps,     TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseSimulationControl

!> lines: a sequence of per-line mappings. NumLines derives from the sequence length; every
!! per-line array is allocated to NumLines and filled. Angles (LAngAnch, LAngFair) are stored
!! in raw degrees here -- the shared ReadPrimaryFile post-processing converts them to radians
!! once, for both formats. GSL is a 3-element list stored into GSL(J,2,:) exactly like the text
!! reader's ReadAryLines; GSL(J,1,:) is left allocated-but-unset (matched to the text path,
!! which never fills or uses it).
subroutine ParseLines(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   type(FEAM_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter     :: RoutineName = 'FEAM_ParseLines'
   integer(IntKi)              :: iSeq, iRow, J
   real(ReKi),     allocatable :: GSLRow(:)
   integer(IntKi)              :: TmpErrStat
   character(ErrMsgLen)        :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'lines', iSeq, TmpErrStat, TmpErrMsg); if (Failed()) return

   InputFileData%NumLines = int(Yaml_NumChildren(Doc, iSeq), IntKi)
   if (InputFileData%NumLines < 1) then
      call SetErrStat(ErrID_Fatal, 'The "lines" list must contain at least one mooring line.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   ! Allocate every per-line array to NumLines (mirrors the text reader's AllocAry block).
   call AllocAry(InputFileData%LEAStiff,   InputFileData%NumLines,       'Axial Stiffness array',        TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(InputFileData%LineCI,     InputFileData%NumLines,       'LineCI array',                 TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(InputFileData%LineCD,     InputFileData%NumLines,       'LineCD array',                 TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(InputFileData%LMassDen,   InputFileData%NumLines,       'Mass Density array',           TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(InputFileData%LDMassDen,  InputFileData%NumLines,       'Displaced Mass Density array', TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(InputFileData%BottmStiff, InputFileData%NumLines,       'Bottom Stiffness array',       TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(InputFileData%LUnstrLen,  InputFileData%NumLines,       'Unstretched Length',           TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(InputFileData%LRadAnch,   InputFileData%NumLines,       'Anchor Radius array',          TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(InputFileData%LAngAnch,   InputFileData%NumLines,       'Anchor Angle array',           TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(InputFileData%LDpthAnch,  InputFileData%NumLines,       'Anchor Depth array',           TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(InputFileData%LRadFair,   InputFileData%NumLines,       'Fairlead Radius array',        TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(InputFileData%LAngFair,   InputFileData%NumLines,       'Fairlead Angle array',         TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(InputFileData%LDrftFair,  InputFileData%NumLines,       'Fairlead Draft array',         TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(InputFileData%Tension,    InputFileData%NumLines,       'Line Top Tension array',       TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(InputFileData%GSL,        InputFileData%NumLines, 2, 3, 'Linear Stiffness array',       TmpErrStat, TmpErrMsg); if (Failed()) return

   do J = 1, InputFileData%NumLines
      iRow = Yaml_Child(Doc, iSeq, J)

      call YamlGet(Doc, 'LEAStiff',   InputFileData%LEAStiff(J),   TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'LMassDen',   InputFileData%LMassDen(J),   TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'LDMassDen',  InputFileData%LDMassDen(J),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'LineCI',     InputFileData%LineCI(J),     TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'LineCD',     InputFileData%LineCD(J),     TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'LUnstrLen',  InputFileData%LUnstrLen(J),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'BottmStiff', InputFileData%BottmStiff(J), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'LRadAnch',   InputFileData%LRadAnch(J),   TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'LAngAnch',   InputFileData%LAngAnch(J),   TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'LDpthAnch',  InputFileData%LDpthAnch(J),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'LRadFair',   InputFileData%LRadFair(J),   TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'LAngFair',   InputFileData%LAngFair(J),   TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'LDrftFair',  InputFileData%LDrftFair(J),  TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Tension',    InputFileData%Tension(J),    TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return

      ! GSL: the three linear spring stiffnesses (x,y,z), stored into GSL(J,2,:) as the text
      ! reader does via ReadAryLines; GSL(J,1,:) is intentionally left unset.
      call YamlGet(Doc, 'GSL', GSLRow, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      if (size(GSLRow) /= size(InputFileData%GSL, 3)) then
         call SetErrStat(ErrID_Fatal, 'lines['//trim(Num2LStr(J))//']:GSL must be a list of '// &
            trim(Num2LStr(size(InputFileData%GSL,3)))//' values (the linear spring stiffness in x, y, z).', &
            ErrStat, ErrMsg, RoutineName)
         return
      end if
      InputFileData%GSL(J,2,:) = GSLRow
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseLines

!> output: SumPrint, OutFile, TabDelim, OutFmt, Tstart.
subroutine ParseOutput(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   type(FEAM_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'FEAM_ParseOutput'
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
   type(YamlDoc),        intent(inout) :: Doc
   type(FEAM_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'FEAM_ParseOutList'
   character(:), allocatable :: TmpList(:)
   integer(IntKi)            :: i
   integer(IntKi)            :: TmpErrStat
   character(ErrMsgLen)      :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'outputs:OutList', TmpList, TmpErrStat, TmpErrMsg); if (Failed()) return

   InputFileData%NumOuts = size(TmpList)
   call AllocAry(InputFileData%OutList, max(InputFileData%NumOuts, 1), 'FEAMooring Input File OutList', TmpErrStat, TmpErrMsg); if (Failed()) return
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

end module FEAM_Yaml
