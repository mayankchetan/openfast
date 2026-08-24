!**********************************************************************************************************************************
! Copyright (C) 2026 National Renewable Energy Laboratory
!
! This file is part of the NWTC Subroutine Library.
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
!> YamlInput: reader for OpenFAST YAML-format input files.
!!
!! Parses a documented subset of standard YAML (block/flow mappings and sequences, quoted
!! scalars, comments, anchors/aliases, standard merge keys, and the OpenFAST-local
!! `!include` tag) into a flat node arena carrying per-node file:line provenance, and
!! provides the path-addressed YamlGet lookup family used by module input readers.
!!
!! Representation: a parsed document is a YamlDoc holding all nodes in one flat array
!! (Nodes(1) is the root) linked by integer Parent/FirstChild/NextSibling indices. A
!! recursive derived type is deliberately avoided — compiler support for self-referential
!! allocatable components is unreliable (gfortran's generated deallocator recurses
!! unboundedly), and the flat form keeps registry copy/pack routines trivial. Node
!! indices are stable once created; "a node" in this API is always (Doc, index).
!!
!! Interoperability invariant: every file accepted here is a valid standard YAML document.
!! OpenFAST-specific semantics appear only as visible local tags (`!include`) — standard
!! syntax is never given custom meaning.
!!
!! This module coexists with (and does not modify) the FileInfoType/ParseVar text-format
!! pipeline in NWTC_IO. The YAML *writer* for summary files lives separately in YAML.f90.
module YamlInput

   use NWTC_Base
   use NWTC_IO, only: Conv2UC, GetPath, PathIsRelative, Num2LStr, GetNewUnit, OpenFInpFile, FileInfoType

   implicit none
   private

   !> Node kinds
   integer(IntKi), parameter, public :: YAML_UNDEF  = 0  !< uninitialized node
   integer(IntKi), parameter, public :: YAML_MAP    = 1  !< block or flow mapping
   integer(IntKi), parameter, public :: YAML_SEQ    = 2  !< block or flow sequence
   integer(IntKi), parameter, public :: YAML_SCALAR = 3  !< scalar leaf (raw text preserved)

   !> One node of a parsed YAML document, stored flat in YamlDoc%Nodes and linked by
   !! index. Scalar holds the raw, unconverted text — numeric conversion happens at
   !! lookup so error messages can quote the source.
   type, public :: YamlNode
      integer(IntKi)              :: Kind = YAML_UNDEF
      character(:), allocatable   :: Key          !< key within parent mapping (unallocated for seq items / root)
      character(:), allocatable   :: Scalar       !< raw scalar text (Kind == YAML_SCALAR)
      integer(IntKi)              :: FileIndx = 0 !< index into YamlDoc%FileList (provenance)
      integer(IntKi)              :: FileLine = 0 !< 1-based line number within that file
      logical                     :: Used = .false. !< set by lookups; Yaml_WarnUnused reports stragglers
      integer(IntKi)              :: Parent      = 0 !< index of parent node (0 for root)
      integer(IntKi)              :: FirstChild  = 0 !< index of first child (0 = none)
      integer(IntKi)              :: LastChild   = 0 !< index of last child (0 = none)
      integer(IntKi)              :: NextSibling = 0 !< index of next sibling (0 = none)
      integer(IntKi)              :: NumChildren = 0
   end type YamlNode

   !> A parsed YAML document: the flat node arena (Nodes(1) is the root) plus the list of
   !! every physical file involved (top file first, then files pulled in via !include),
   !! mirroring FileInfoType%FileList.
   type, public :: YamlDoc
      type(YamlNode), allocatable :: Nodes(:)
      integer(IntKi)              :: NumNodes = 0
      character(1024), allocatable :: FileList(:)
   end type YamlDoc

   !> Opt-in error accumulator (initially unused by modules): with Accumulate=.true.,
   !! Collect absorbs fatal ErrStat/ErrMsg pairs instead of letting them abort, so a
   !! reader can attempt every key and report all problems at once via Finalize. With
   !! Accumulate=.false. (default) Collect is a no-op and fail-fast behavior is
   !! unchanged. Shaped to plug into the -CheckInput attempt-everything machinery.
   type, public :: YamlErrAcc
      logical                           :: Accumulate = .false.
      integer(IntKi)                    :: NumErrors  = 0
      character(ErrMsgLen), allocatable :: Messages(:)
   contains
      procedure :: Collect  => YamlErrAcc_Collect
      procedure :: Failed   => YamlErrAcc_Failed
      procedure :: Finalize => YamlErrAcc_Finalize
   end type YamlErrAcc

   public :: IsYamlExt
   public :: Yaml_LoadFile
   public :: Yaml_LoadString
   public :: Yaml_LoadFileInfo
   public :: Yaml_Serialize
   public :: Yaml_NumChildren
   public :: Yaml_Child
   public :: Yaml_ChildByKey
   public :: YamlGet
   public :: YamlGetNode
   public :: Yaml_MarkUsed
   public :: Yaml_WarnUnused

   !> Path-addressed, order-independent, case-insensitive typed lookups. Paths are
   !! ':'-separated (e.g. 'simulation_control:TMax'); Default= supplies a value for a
   !! missing key or an explicit 'default' scalar; Found= makes a key optional; From=
   !! roots the path at a subtree node instead of the document root.
   interface YamlGet
      module procedure YamlGetR8Var, YamlGetSiVar, YamlGetInVar, YamlGetLoVar, YamlGetChVar
      module procedure YamlGetInAry, YamlGetSiAry, YamlGetR8Ary, YamlGetChAry
      module procedure YamlGetSiMat, YamlGetR8Mat
   end interface YamlGet

   !> One significant source line after scanning: physical position plus decommented text
   !! with the indentation removed. Blank and comment-only lines never reach the parser.
   type :: ScanLineType
      integer                   :: Indent   = 0  !< number of leading spaces
      integer                   :: FileLine = 0  !< 1-based physical line number
      integer                   :: FileIndx = 1  !< index into YamlDoc%FileList
      character(:), allocatable :: Text          !< decommented content, indent stripped, right-trimmed
   end type ScanLineType

   character(*), parameter :: StringSourceName = '(string input)'

   !> Transient parser state: the anchor registry (&name -> node index; redefinition
   !! shadows, per YAML) plus the !include machinery (echo unit, depth-capped stack of
   !! in-progress files for cycle detection).
   type :: YamlParseState
      character(64),  allocatable :: AnchorNames(:)
      integer(IntKi), allocatable :: AnchorNodes(:)
      integer                     :: NumAnchors = 0
      logical                     :: AllowInclude = .false.  !< only file-based loads may !include
      integer(IntKi)              :: UnEc = 0                !< echo unit (0 = no echo)
      integer                     :: Depth = 0               !< current include nesting depth
      character(1024)             :: Stack(10) = ''          !< files currently being parsed
   end type YamlParseState

   integer, parameter :: MaxMergeSrcs    = 16  !< aliases allowed in one "<<:" merge list
   integer, parameter :: MaxIncludeDepth = 10  !< !include nesting cap (also sizes Stack)

contains

!> Returns true when FileName's extension (judged on the basename only) is .yaml or .yml,
!! case-insensitive. This is the single format-detection rule for all OpenFAST input files.
logical function IsYamlExt(FileName) result(IsYaml)
   character(*), intent(in) :: FileName
   character(8)             :: Ext        ! longest extension we compare is "YAML"
   integer                  :: ISep, IDot, ExtLen

   IsYaml = .false.

   ! extension is judged on the basename: find the last path separator (either flavor)
   ISep = max( index(FileName, '/', back=.true.), index(FileName, '\', back=.true.) )
   IDot = index(FileName(ISep+1:), '.', back=.true.)
   if (IDot == 0) return                          ! no extension
   ExtLen = len_trim(FileName) - (ISep + IDot)
   if (ExtLen < 3 .or. ExtLen > 4) return         ! only .yml / .yaml can match

   Ext = FileName(ISep+IDot+1:len_trim(FileName))
   call Conv2UC(Ext)
   IsYaml = ( trim(Ext) == 'YAML' .or. trim(Ext) == 'YML' )
end function IsYamlExt

!> Number of children of node iNode.
integer(IntKi) function Yaml_NumChildren(Doc, iNode) result(N)
   type(YamlDoc),  intent(in) :: Doc
   integer(IntKi), intent(in) :: iNode
   N = Doc%Nodes(iNode)%NumChildren
end function Yaml_NumChildren

!> Index of the k-th child (in file order) of node iNode; 0 when out of range.
integer(IntKi) function Yaml_Child(Doc, iNode, k) result(iChild)
   type(YamlDoc),  intent(in) :: Doc
   integer(IntKi), intent(in) :: iNode
   integer(IntKi), intent(in) :: k
   integer(IntKi) :: n
   iChild = 0
   if (k < 1 .or. k > Doc%Nodes(iNode)%NumChildren) return
   iChild = Doc%Nodes(iNode)%FirstChild
   do n = 2, k
      iChild = Doc%Nodes(iChild)%NextSibling
   end do
end function Yaml_Child

!> Index of the child of mapping iNode whose key matches (exact, case-sensitive here —
!! the case-insensitive path lookup layers on top); 0 when absent.
integer(IntKi) function Yaml_ChildByKey(Doc, iNode, Key) result(iChild)
   type(YamlDoc),  intent(in) :: Doc
   integer(IntKi), intent(in) :: iNode
   character(*),   intent(in) :: Key
   iChild = Doc%Nodes(iNode)%FirstChild
   do while (iChild > 0)
      if (allocated(Doc%Nodes(iChild)%Key)) then
         if (Doc%Nodes(iChild)%Key == Key) return
      end if
      iChild = Doc%Nodes(iChild)%NextSibling
   end do
end function Yaml_ChildByKey

!----------------------------------------------------------------------------------------------------------------------------------
! Error accumulation
!----------------------------------------------------------------------------------------------------------------------------------

!> In accumulate mode, absorb a fatal ErrStat/ErrMsg (record it, reset to None) so the
!! caller can keep attempting lookups. Otherwise leave the error untouched (fail-fast).
subroutine YamlErrAcc_Collect(this, ErrStat, ErrMsg)
   class(YamlErrAcc), intent(inout) :: this
   integer(IntKi),    intent(inout) :: ErrStat
   character(*),      intent(inout) :: ErrMsg

   character(ErrMsgLen), allocatable :: Tmp(:)

   if (.not. this%Accumulate) return
   if (ErrStat < AbortErrLev) return

   if (.not. allocated(this%Messages)) then
      allocate(this%Messages(8))
   else if (this%NumErrors == size(this%Messages)) then
      allocate(Tmp(2*size(this%Messages)))
      Tmp(1:this%NumErrors) = this%Messages
      call move_alloc(Tmp, this%Messages)
   end if

   this%NumErrors = this%NumErrors + 1
   this%Messages(this%NumErrors) = ErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
end subroutine YamlErrAcc_Collect

!> True when any error has been collected. Not meant as a per-lookup abort gate (the
!! point of accumulate mode is to keep going); consult it to skip dependent work or
!! after Finalize.
logical function YamlErrAcc_Failed(this) result(HasFailed)
   class(YamlErrAcc), intent(in) :: this
   HasFailed = (this%NumErrors > 0)
end function YamlErrAcc_Failed

!> Report everything collected as one fatal error (no-op when nothing was collected).
subroutine YamlErrAcc_Finalize(this, ErrStat, ErrMsg)
   class(YamlErrAcc), intent(in   ) :: this
   integer(IntKi),    intent(  out) :: ErrStat
   character(*),      intent(  out) :: ErrMsg

   integer :: i

   ErrStat = ErrID_None
   ErrMsg  = ""
   if (this%NumErrors == 0) return

   ErrStat = ErrID_Fatal
   ErrMsg  = trim(Num2LStr(this%NumErrors))//' input error(s) found:'
   do i = 1, this%NumErrors
      if (len_trim(ErrMsg) + len_trim(this%Messages(i)) + 2 > len(ErrMsg)) exit
      ErrMsg = trim(ErrMsg)//new_line('a')//trim(this%Messages(i))
   end do
end subroutine YamlErrAcc_Finalize

!----------------------------------------------------------------------------------------------------------------------------------
! Lookups (the YamlGet family)
!----------------------------------------------------------------------------------------------------------------------------------

!> Case-insensitive key comparison (trimmed).
logical function KeyMatch(A, B) result(Match)
   character(*), intent(in) :: A, B
   character(len(A)) :: UA
   character(len(B)) :: UB
   Match = .false.
   if (len_trim(A) /= len_trim(B)) return
   UA = A
   UB = B
   call Conv2UC(UA)
   call Conv2UC(UB)
   Match = (trim(UA) == trim(UB))
end function KeyMatch

!> " on line #N of "file"" fragment for a node (post-parse errors).
function NodeRef(Doc, iNode) result(Ref)
   type(YamlDoc),  intent(in) :: Doc
   integer(IntKi), intent(in) :: iNode
   character(:), allocatable  :: Ref
   Ref = 'on line #'//trim(Num2LStr(Doc%Nodes(iNode)%FileLine))//' of "'// &
         trim(Doc%FileList(Doc%Nodes(iNode)%FileIndx))//'"'
end function NodeRef

!> Resolve a ':'-separated path from iStart, marking traversed nodes Used. On a miss,
!! iNode is 0 and iContext is the deepest mapping that did match (for error location).
subroutine FindPath(Doc, Path, iStart, iNode, iContext)
   type(YamlDoc),  intent(inout) :: Doc
   character(*),   intent(in   ) :: Path
   integer(IntKi), intent(in   ) :: iStart
   integer(IntKi), intent(  out) :: iNode
   integer(IntKi), intent(  out) :: iContext

   integer        :: SegBeg, SegEnd
   integer(IntKi) :: iCur, iChild

   iCur = iStart
   Doc%Nodes(iCur)%Used = .true.
   iContext = iCur
   iNode    = 0

   SegBeg = 1
   do while (SegBeg <= len_trim(Path))
      SegEnd = index(Path(SegBeg:), ':')
      if (SegEnd == 0) then
         SegEnd = len_trim(Path)
      else
         SegEnd = SegBeg + SegEnd - 2
      end if

      if (Doc%Nodes(iCur)%Kind /= YAML_MAP) return    ! can't descend into a scalar/sequence

      iChild = Doc%Nodes(iCur)%FirstChild
      do while (iChild > 0)
         if (allocated(Doc%Nodes(iChild)%Key)) then
            if (KeyMatch(Doc%Nodes(iChild)%Key, Path(SegBeg:SegEnd))) exit
         end if
         iChild = Doc%Nodes(iChild)%NextSibling
      end do
      if (iChild == 0) return                         ! segment not found; iContext holds the mapping

      iCur = iChild
      Doc%Nodes(iCur)%Used = .true.
      iContext = iCur
      SegBeg = SegEnd + 2                             ! skip the ':'
   end do

   iNode = iCur
end subroutine FindPath

!> Shared front half of every typed getter: resolve the path and apply the
!! missing-key policy. On return: iNode > 0 means proceed with conversion;
!! iNode == 0 with ErrStat==None means the caller should apply Default/Found handling
!! (UseDefault tells it to assign Default); fatal otherwise.
subroutine LookupCommon(Doc, Path, From, HasDefault, HasFound, Found, iNode, UseDefault, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   character(*),   intent(in   ) :: Path
   integer(IntKi), intent(in   ), optional :: From
   logical,        intent(in   ) :: HasDefault
   logical,        intent(in   ) :: HasFound
   logical,        intent(  out) :: Found
   integer(IntKi), intent(  out) :: iNode
   logical,        intent(  out) :: UseDefault
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'YamlGet'
   integer(IntKi)          :: iStart, iContext

   ErrStat    = ErrID_None
   ErrMsg     = ""
   UseDefault = .false.

   iStart = 1
   if (present(From)) iStart = From

   call FindPath(Doc, Path, iStart, iNode, iContext)
   Found = (iNode > 0)
   if (Found) return

   if (HasDefault) then
      UseDefault = .true.
   else if (.not. HasFound) then
      call SetErrStat(ErrID_Fatal, '>> The required key "'//trim(Path)//'" was not found in "'// &
         trim(Doc%FileList(Doc%Nodes(iContext)%FileIndx))//'" (mapping starting on line #'// &
         trim(Num2LStr(Doc%Nodes(iContext)%FileLine))//').', ErrStat, ErrMsg, RoutineName)
   end if
end subroutine LookupCommon

!> Fetch the raw scalar text of a resolved node, handling the 'default' keyword and
!! kind errors. UseDefault comes back true when the scalar says 'default' and a
!! Default= was supplied; fatal when it says 'default' without one.
subroutine ScalarText(Doc, Path, iNode, HasDefault, Text, UseDefault, ErrStat, ErrMsg)
   type(YamlDoc),             intent(in   ) :: Doc
   character(*),              intent(in   ) :: Path
   integer(IntKi),            intent(in   ) :: iNode
   logical,                   intent(in   ) :: HasDefault
   character(:), allocatable, intent(  out) :: Text
   logical,                   intent(  out) :: UseDefault
   integer(IntKi),            intent(  out) :: ErrStat
   character(*),              intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'YamlGet'
   character(16)           :: UText

   ErrStat    = ErrID_None
   ErrMsg     = ""
   UseDefault = .false.
   Text       = ''

   if (Doc%Nodes(iNode)%Kind /= YAML_SCALAR) then
      call SetErrStat(ErrID_Fatal, '>> The key "'//trim(Path)//'" '//NodeRef(Doc, iNode)// &
         ' is not a scalar value.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   Text = Doc%Nodes(iNode)%Scalar
   if (len(Text) <= 16) then
      UText = Text
      call Conv2UC(UText)
      if (trim(UText) == 'DEFAULT') then
         if (HasDefault) then
            UseDefault = .true.
         else
            call SetErrStat(ErrID_Fatal, '>> The key "'//trim(Path)//'" '//NodeRef(Doc, iNode)// &
               ' is set to "default" but no default exists for it.', ErrStat, ErrMsg, RoutineName)
         end if
      end if
   end if
end subroutine ScalarText

!> Uniform conversion-failure message.
subroutine ConvFail(Doc, Path, iNode, TypeName, Text, ErrStat, ErrMsg)
   type(YamlDoc),  intent(in   ) :: Doc
   character(*),   intent(in   ) :: Path
   integer(IntKi), intent(in   ) :: iNode
   character(*),   intent(in   ) :: TypeName
   character(*),   intent(in   ) :: Text
   integer(IntKi), intent(inout) :: ErrStat
   character(*),   intent(inout) :: ErrMsg

   call SetErrStat(ErrID_Fatal, '>> The key "'//trim(Path)//'" '//NodeRef(Doc, iNode)// &
      ' was not assigned a valid '//TypeName//' value. Text: "'//trim(Text)//'"', &
      ErrStat, ErrMsg, 'YamlGet')
end subroutine ConvFail

subroutine YamlGetR8Var(Doc, Path, Var, ErrStat, ErrMsg, Default, Found, From)
   type(YamlDoc),  intent(inout) :: Doc
   character(*),   intent(in   ) :: Path
   real(R8Ki),     intent(inout) :: Var
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg
   real(R8Ki),     intent(in   ), optional :: Default
   logical,        intent(  out), optional :: Found
   integer(IntKi), intent(in   ), optional :: From

   character(:), allocatable :: Text
   integer(IntKi) :: iNode
   logical        :: WasFound, UseDefault
   integer        :: ios

   call LookupCommon(Doc, Path, From, present(Default), present(Found), WasFound, iNode, UseDefault, ErrStat, ErrMsg)
   if (present(Found)) Found = WasFound
   if (ErrStat >= AbortErrLev) return
   if (.not. UseDefault .and. iNode > 0) then
      call ScalarText(Doc, Path, iNode, present(Default), Text, UseDefault, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
   end if
   if (UseDefault) then
      Var = Default
   else if (iNode > 0) then
      read(Text, *, iostat=ios) Var
      if (ios /= 0) call ConvFail(Doc, Path, iNode, 'REAL', Text, ErrStat, ErrMsg)
   end if
end subroutine YamlGetR8Var

subroutine YamlGetSiVar(Doc, Path, Var, ErrStat, ErrMsg, Default, Found, From)
   type(YamlDoc),  intent(inout) :: Doc
   character(*),   intent(in   ) :: Path
   real(SiKi),     intent(inout) :: Var
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg
   real(SiKi),     intent(in   ), optional :: Default
   logical,        intent(  out), optional :: Found
   integer(IntKi), intent(in   ), optional :: From

   character(:), allocatable :: Text
   integer(IntKi) :: iNode
   logical        :: WasFound, UseDefault
   integer        :: ios

   call LookupCommon(Doc, Path, From, present(Default), present(Found), WasFound, iNode, UseDefault, ErrStat, ErrMsg)
   if (present(Found)) Found = WasFound
   if (ErrStat >= AbortErrLev) return
   if (.not. UseDefault .and. iNode > 0) then
      call ScalarText(Doc, Path, iNode, present(Default), Text, UseDefault, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
   end if
   if (UseDefault) then
      Var = Default
   else if (iNode > 0) then
      read(Text, *, iostat=ios) Var
      if (ios /= 0) call ConvFail(Doc, Path, iNode, 'REAL', Text, ErrStat, ErrMsg)
   end if
end subroutine YamlGetSiVar

subroutine YamlGetInVar(Doc, Path, Var, ErrStat, ErrMsg, Default, Found, From)
   type(YamlDoc),  intent(inout) :: Doc
   character(*),   intent(in   ) :: Path
   integer(IntKi), intent(inout) :: Var
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg
   integer(IntKi), intent(in   ), optional :: Default
   logical,        intent(  out), optional :: Found
   integer(IntKi), intent(in   ), optional :: From

   character(:), allocatable :: Text
   integer(IntKi) :: iNode
   logical        :: WasFound, UseDefault
   integer        :: ios

   call LookupCommon(Doc, Path, From, present(Default), present(Found), WasFound, iNode, UseDefault, ErrStat, ErrMsg)
   if (present(Found)) Found = WasFound
   if (ErrStat >= AbortErrLev) return
   if (.not. UseDefault .and. iNode > 0) then
      call ScalarText(Doc, Path, iNode, present(Default), Text, UseDefault, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
   end if
   if (UseDefault) then
      Var = Default
   else if (iNode > 0) then
      read(Text, *, iostat=ios) Var
      if (ios /= 0) call ConvFail(Doc, Path, iNode, 'INTEGER', Text, ErrStat, ErrMsg)
   end if
end subroutine YamlGetInVar

subroutine YamlGetLoVar(Doc, Path, Var, ErrStat, ErrMsg, Default, Found, From)
   type(YamlDoc),  intent(inout) :: Doc
   character(*),   intent(in   ) :: Path
   logical,        intent(inout) :: Var
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg
   logical,        intent(in   ), optional :: Default
   logical,        intent(  out), optional :: Found
   integer(IntKi), intent(in   ), optional :: From

   character(:), allocatable :: Text
   character(8)   :: UText
   integer(IntKi) :: iNode
   logical        :: WasFound, UseDefault

   call LookupCommon(Doc, Path, From, present(Default), present(Found), WasFound, iNode, UseDefault, ErrStat, ErrMsg)
   if (present(Found)) Found = WasFound
   if (ErrStat >= AbortErrLev) return
   if (.not. UseDefault .and. iNode > 0) then
      call ScalarText(Doc, Path, iNode, present(Default), Text, UseDefault, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
   end if
   if (UseDefault) then
      Var = Default
   else if (iNode > 0) then
      if (len_trim(Text) > 8) then
         call ConvFail(Doc, Path, iNode, 'LOGICAL', Text, ErrStat, ErrMsg)
         return
      end if
      UText = Text
      call Conv2UC(UText)
      select case (trim(UText))
      case ('TRUE', 'T', '.TRUE.')
         Var = .true.
      case ('FALSE', 'F', '.FALSE.')
         Var = .false.
      case default
         call ConvFail(Doc, Path, iNode, 'LOGICAL', Text, ErrStat, ErrMsg)
      end select
   end if
end subroutine YamlGetLoVar

subroutine YamlGetChVar(Doc, Path, Var, ErrStat, ErrMsg, Default, Found, From)
   type(YamlDoc),  intent(inout) :: Doc
   character(*),   intent(in   ) :: Path
   character(*),   intent(inout) :: Var
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg
   character(*),   intent(in   ), optional :: Default
   logical,        intent(  out), optional :: Found
   integer(IntKi), intent(in   ), optional :: From

   character(:), allocatable :: Text
   integer(IntKi) :: iNode
   logical        :: WasFound, UseDefault

   call LookupCommon(Doc, Path, From, present(Default), present(Found), WasFound, iNode, UseDefault, ErrStat, ErrMsg)
   if (present(Found)) Found = WasFound
   if (ErrStat >= AbortErrLev) return
   if (.not. UseDefault .and. iNode > 0) then
      call ScalarText(Doc, Path, iNode, present(Default), Text, UseDefault, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
   end if
   if (UseDefault) then
      Var = Default
   else if (iNode > 0) then
      if (len_trim(Text) > len(Var)) then
         call SetErrStat(ErrID_Fatal, '>> The value of key "'//trim(Path)//'" '//NodeRef(Doc, iNode)// &
            ' is longer than the '//trim(Num2LStr(len(Var)))//'-character limit: "'//trim(Text)//'"', &
            ErrStat, ErrMsg, 'YamlGet')
         return
      end if
      Var = Text
   end if
end subroutine YamlGetChVar

!> Shared front half of the array getters: resolve, apply missing policy, and verify the
!! node is a sequence. iSeq==0 with ErrStat==None means "missing but optional" (Found=).
subroutine LookupSeq(Doc, Path, From, HasFound, Found, iSeq, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   character(*),   intent(in   ) :: Path
   integer(IntKi), intent(in   ), optional :: From
   logical,        intent(in   ) :: HasFound
   logical,        intent(  out) :: Found
   integer(IntKi), intent(  out) :: iSeq
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   logical :: UseDefault

   call LookupCommon(Doc, Path, From, .false., HasFound, Found, iSeq, UseDefault, ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev .or. iSeq == 0) return

   if (Doc%Nodes(iSeq)%Kind /= YAML_SEQ) then
      call SetErrStat(ErrID_Fatal, '>> The key "'//trim(Path)//'" '//NodeRef(Doc, iSeq)// &
         ' must be a sequence ("[...]" or "- item" list).', ErrStat, ErrMsg, 'YamlGet')
      iSeq = 0
   end if
end subroutine LookupSeq

subroutine YamlGetInAry(Doc, Path, Ary, ErrStat, ErrMsg, Found, From)
   type(YamlDoc),               intent(inout) :: Doc
   character(*),                intent(in   ) :: Path
   integer(IntKi), allocatable, intent(  out) :: Ary(:)
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg
   logical,        intent(  out), optional :: Found
   integer(IntKi), intent(in   ), optional :: From

   integer(IntKi) :: iSeq, iItem
   logical        :: WasFound
   integer        :: k, ios

   call LookupSeq(Doc, Path, From, present(Found), WasFound, iSeq, ErrStat, ErrMsg)
   if (present(Found)) Found = WasFound
   if (ErrStat >= AbortErrLev .or. iSeq == 0) return

   allocate(Ary(Doc%Nodes(iSeq)%NumChildren))
   iItem = Doc%Nodes(iSeq)%FirstChild
   k = 0
   do while (iItem > 0)
      k = k + 1
      Doc%Nodes(iItem)%Used = .true.
      if (Doc%Nodes(iItem)%Kind /= YAML_SCALAR) then
         call SetErrStat(ErrID_Fatal, '>> Element '//trim(Num2LStr(k))//' of "'//trim(Path)//'" '// &
            NodeRef(Doc, iItem)//' is not a scalar.', ErrStat, ErrMsg, 'YamlGet')
         return
      end if
      read(Doc%Nodes(iItem)%Scalar, *, iostat=ios) Ary(k)
      if (ios /= 0) then
         call ConvFail(Doc, Path, iItem, 'INTEGER', Doc%Nodes(iItem)%Scalar, ErrStat, ErrMsg)
         return
      end if
      iItem = Doc%Nodes(iItem)%NextSibling
   end do
end subroutine YamlGetInAry

subroutine YamlGetSiAry(Doc, Path, Ary, ErrStat, ErrMsg, Found, From)
   type(YamlDoc),           intent(inout) :: Doc
   character(*),            intent(in   ) :: Path
   real(SiKi), allocatable, intent(  out) :: Ary(:)
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg
   logical,        intent(  out), optional :: Found
   integer(IntKi), intent(in   ), optional :: From

   integer(IntKi) :: iSeq, iItem
   logical        :: WasFound
   integer        :: k, ios

   call LookupSeq(Doc, Path, From, present(Found), WasFound, iSeq, ErrStat, ErrMsg)
   if (present(Found)) Found = WasFound
   if (ErrStat >= AbortErrLev .or. iSeq == 0) return

   allocate(Ary(Doc%Nodes(iSeq)%NumChildren))
   iItem = Doc%Nodes(iSeq)%FirstChild
   k = 0
   do while (iItem > 0)
      k = k + 1
      Doc%Nodes(iItem)%Used = .true.
      if (Doc%Nodes(iItem)%Kind /= YAML_SCALAR) then
         call SetErrStat(ErrID_Fatal, '>> Element '//trim(Num2LStr(k))//' of "'//trim(Path)//'" '// &
            NodeRef(Doc, iItem)//' is not a scalar.', ErrStat, ErrMsg, 'YamlGet')
         return
      end if
      read(Doc%Nodes(iItem)%Scalar, *, iostat=ios) Ary(k)
      if (ios /= 0) then
         call ConvFail(Doc, Path, iItem, 'REAL', Doc%Nodes(iItem)%Scalar, ErrStat, ErrMsg)
         return
      end if
      iItem = Doc%Nodes(iItem)%NextSibling
   end do
end subroutine YamlGetSiAry

subroutine YamlGetR8Ary(Doc, Path, Ary, ErrStat, ErrMsg, Found, From)
   type(YamlDoc),           intent(inout) :: Doc
   character(*),            intent(in   ) :: Path
   real(R8Ki), allocatable, intent(  out) :: Ary(:)
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg
   logical,        intent(  out), optional :: Found
   integer(IntKi), intent(in   ), optional :: From

   integer(IntKi) :: iSeq, iItem
   logical        :: WasFound
   integer        :: k, ios

   call LookupSeq(Doc, Path, From, present(Found), WasFound, iSeq, ErrStat, ErrMsg)
   if (present(Found)) Found = WasFound
   if (ErrStat >= AbortErrLev .or. iSeq == 0) return

   allocate(Ary(Doc%Nodes(iSeq)%NumChildren))
   iItem = Doc%Nodes(iSeq)%FirstChild
   k = 0
   do while (iItem > 0)
      k = k + 1
      Doc%Nodes(iItem)%Used = .true.
      if (Doc%Nodes(iItem)%Kind /= YAML_SCALAR) then
         call SetErrStat(ErrID_Fatal, '>> Element '//trim(Num2LStr(k))//' of "'//trim(Path)//'" '// &
            NodeRef(Doc, iItem)//' is not a scalar.', ErrStat, ErrMsg, 'YamlGet')
         return
      end if
      read(Doc%Nodes(iItem)%Scalar, *, iostat=ios) Ary(k)
      if (ios /= 0) then
         call ConvFail(Doc, Path, iItem, 'REAL', Doc%Nodes(iItem)%Scalar, ErrStat, ErrMsg)
         return
      end if
      iItem = Doc%Nodes(iItem)%NextSibling
   end do
end subroutine YamlGetR8Ary

subroutine YamlGetChAry(Doc, Path, Ary, ErrStat, ErrMsg, Found, From)
   type(YamlDoc),             intent(inout) :: Doc
   character(*),              intent(in   ) :: Path
   character(:), allocatable, intent(  out) :: Ary(:)
   integer(IntKi),            intent(  out) :: ErrStat
   character(*),              intent(  out) :: ErrMsg
   logical,        intent(  out), optional :: Found
   integer(IntKi), intent(in   ), optional :: From

   integer(IntKi) :: iSeq, iItem
   logical        :: WasFound
   integer        :: k, MaxLen

   call LookupSeq(Doc, Path, From, present(Found), WasFound, iSeq, ErrStat, ErrMsg)
   if (present(Found)) Found = WasFound
   if (ErrStat >= AbortErrLev .or. iSeq == 0) return

   MaxLen = 1
   iItem = Doc%Nodes(iSeq)%FirstChild
   do while (iItem > 0)
      if (Doc%Nodes(iItem)%Kind == YAML_SCALAR) MaxLen = max(MaxLen, len_trim(Doc%Nodes(iItem)%Scalar))
      iItem = Doc%Nodes(iItem)%NextSibling
   end do

   allocate(character(MaxLen) :: Ary(Doc%Nodes(iSeq)%NumChildren))
   iItem = Doc%Nodes(iSeq)%FirstChild
   k = 0
   do while (iItem > 0)
      k = k + 1
      Doc%Nodes(iItem)%Used = .true.
      if (Doc%Nodes(iItem)%Kind /= YAML_SCALAR) then
         call SetErrStat(ErrID_Fatal, '>> Element '//trim(Num2LStr(k))//' of "'//trim(Path)//'" '// &
            NodeRef(Doc, iItem)//' is not a scalar.', ErrStat, ErrMsg, 'YamlGet')
         return
      end if
      Ary(k) = Doc%Nodes(iItem)%Scalar
      iItem = Doc%Nodes(iItem)%NextSibling
   end do
end subroutine YamlGetChAry

subroutine YamlGetSiMat(Doc, Path, Mat, ErrStat, ErrMsg, Found, From)
   type(YamlDoc),           intent(inout) :: Doc
   character(*),            intent(in   ) :: Path
   real(SiKi), allocatable, intent(  out) :: Mat(:,:)
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg
   logical,        intent(  out), optional :: Found
   integer(IntKi), intent(in   ), optional :: From

   real(R8Ki), allocatable :: Mat8(:,:)

   call YamlGetR8Mat(Doc, Path, Mat8, ErrStat, ErrMsg, Found, From)
   if (ErrStat >= AbortErrLev .or. .not. allocated(Mat8)) return
   allocate(Mat(size(Mat8,1), size(Mat8,2)))
   Mat = real(Mat8, SiKi)
end subroutine YamlGetSiMat

subroutine YamlGetR8Mat(Doc, Path, Mat, ErrStat, ErrMsg, Found, From)
   type(YamlDoc),           intent(inout) :: Doc
   character(*),            intent(in   ) :: Path
   real(R8Ki), allocatable, intent(  out) :: Mat(:,:)
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg
   logical,        intent(  out), optional :: Found
   integer(IntKi), intent(in   ), optional :: From

   integer(IntKi) :: iSeq, iRow, iCell
   logical        :: WasFound
   integer        :: r, c, NRows, NCols, ios

   call LookupSeq(Doc, Path, From, present(Found), WasFound, iSeq, ErrStat, ErrMsg)
   if (present(Found)) Found = WasFound
   if (ErrStat >= AbortErrLev .or. iSeq == 0) return

   NRows = Doc%Nodes(iSeq)%NumChildren
   NCols = 0
   if (NRows > 0) NCols = Doc%Nodes(Doc%Nodes(iSeq)%FirstChild)%NumChildren
   allocate(Mat(NRows, NCols))

   iRow = Doc%Nodes(iSeq)%FirstChild
   r = 0
   do while (iRow > 0)
      r = r + 1
      Doc%Nodes(iRow)%Used = .true.
      if (Doc%Nodes(iRow)%Kind /= YAML_SEQ) then
         call SetErrStat(ErrID_Fatal, '>> Row '//trim(Num2LStr(r))//' of "'//trim(Path)//'" '// &
            NodeRef(Doc, iRow)//' must be a flow sequence ("[...]").', ErrStat, ErrMsg, 'YamlGet')
         return
      end if
      if (Doc%Nodes(iRow)%NumChildren /= NCols) then
         call SetErrStat(ErrID_Fatal, '>> Row '//trim(Num2LStr(r))//' of "'//trim(Path)//'" '// &
            NodeRef(Doc, iRow)//' has '//trim(Num2LStr(Doc%Nodes(iRow)%NumChildren))// &
            ' columns; expected '//trim(Num2LStr(NCols))//' (from the first row).', ErrStat, ErrMsg, 'YamlGet')
         return
      end if
      iCell = Doc%Nodes(iRow)%FirstChild
      c = 0
      do while (iCell > 0)
         c = c + 1
         Doc%Nodes(iCell)%Used = .true.
         read(Doc%Nodes(iCell)%Scalar, *, iostat=ios) Mat(r, c)
         if (ios /= 0) then
            call ConvFail(Doc, Path, iCell, 'REAL', Doc%Nodes(iCell)%Scalar, ErrStat, ErrMsg)
            return
         end if
         iCell = Doc%Nodes(iCell)%NextSibling
      end do
      iRow = Doc%Nodes(iRow)%NextSibling
   end do
end subroutine YamlGetR8Mat

!> Resolve a path to a node index (for custom shapes: tables, module sections). The node
!! is marked Used; walk it with Yaml_Child/Yaml_ChildByKey/Yaml_NumChildren and mark
!! consumed descendants via Yaml_MarkUsed.
subroutine YamlGetNode(Doc, Path, iNode, ErrStat, ErrMsg, Found, From)
   type(YamlDoc),  intent(inout) :: Doc
   character(*),   intent(in   ) :: Path
   integer(IntKi), intent(  out) :: iNode
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg
   logical,        intent(  out), optional :: Found
   integer(IntKi), intent(in   ), optional :: From

   logical :: WasFound, UseDefault

   call LookupCommon(Doc, Path, From, .false., present(Found), WasFound, iNode, UseDefault, ErrStat, ErrMsg)
   if (present(Found)) Found = WasFound
end subroutine YamlGetNode

!> Mark a node (and, when Recurse, its whole subtree) as consumed, so Yaml_WarnUnused
!! does not flag content read through the raw walking helpers.
recursive subroutine Yaml_MarkUsed(Doc, iNode, Recurse)
   type(YamlDoc),  intent(inout) :: Doc
   integer(IntKi), intent(in   ) :: iNode
   logical,        intent(in   ) :: Recurse

   integer(IntKi) :: iChild

   Doc%Nodes(iNode)%Used = .true.
   if (.not. Recurse) return
   iChild = Doc%Nodes(iNode)%FirstChild
   do while (iChild > 0)
      call Yaml_MarkUsed(Doc, iChild, .true.)
      iChild = Doc%Nodes(iChild)%NextSibling
   end do
end subroutine Yaml_MarkUsed

!> After a consumer finishes its lookups, warn (ErrID_Warn) about any key it never read —
!! the typo detector. Only the topmost unused node of a subtree is reported.
subroutine Yaml_WarnUnused(Doc, ErrStat, ErrMsg)
   type(YamlDoc),  intent(in   ) :: Doc
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'Yaml_WarnUnused'
   character(ErrMsgLen)    :: List
   integer                 :: Count

   ErrStat = ErrID_None
   ErrMsg  = ""
   List    = ""
   Count   = 0

   if (Doc%NumNodes < 1) return
   call Walk(1_IntKi, '')

   if (Count > 0) then
      call SetErrStat(ErrID_Warn, 'The following input keys were not recognized and were ignored:'// &
         trim(List), ErrStat, ErrMsg, RoutineName)
   end if

contains

   recursive subroutine Walk(iNode, Prefix)
      integer(IntKi), intent(in) :: iNode
      character(*),   intent(in) :: Prefix

      integer(IntKi)            :: iChild
      character(:), allocatable :: Name
      integer                   :: k

      iChild = Doc%Nodes(iNode)%FirstChild
      k = 0
      do while (iChild > 0)
         k = k + 1
         if (allocated(Doc%Nodes(iChild)%Key)) then
            Name = Doc%Nodes(iChild)%Key
         else
            Name = '['//trim(Num2LStr(k))//']'
         end if
         if (.not. Doc%Nodes(iChild)%Used) then
            Count = Count + 1
            if (len_trim(List) < len(List) - 200) then
               List = trim(List)//new_line('a')//'   '//Prefix//Name//' ('// &
                      trim(Doc%FileList(Doc%Nodes(iChild)%FileIndx))//':'// &
                      trim(Num2LStr(Doc%Nodes(iChild)%FileLine))//')'
            end if
         else
            call Walk(iChild, Prefix//Name//':')
         end if
         iChild = Doc%Nodes(iChild)%NextSibling
      end do
   end subroutine Walk

end subroutine Yaml_WarnUnused

!> Parse a YAML file (and any !include'd files) into Doc. When UnEc > 0, each physical
!! file is echoed verbatim (comments included) under a banner naming the file.
subroutine Yaml_LoadFile(FileName, Doc, ErrStat, ErrMsg, UnEc)
   character(*),   intent(in   ) :: FileName
   type(YamlDoc),  intent(  out) :: Doc
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg
   integer(IntKi), intent(in   ), optional :: UnEc

   character(*), parameter :: RoutineName = 'Yaml_LoadFile'
   type(YamlParseState)    :: PS
   integer(IntKi)          :: iRoot
   integer(IntKi)          :: ErrStat2
   character(ErrMsgLen)    :: ErrMsg2

   ErrStat = ErrID_None
   ErrMsg  = ""

   PS%AllowInclude = .true.
   if (present(UnEc)) PS%UnEc = UnEc

   allocate(Doc%FileList(0))
   iRoot = AddNode(Doc, 0_IntKi)

   call ParseFileInto(Doc, iRoot, trim(FileName), PS, ErrStat2, ErrMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
end subroutine Yaml_LoadFile

!> Read, scan, and parse one physical YAML file into an existing node — used for both the
!! top file and every !include splice. Handles cycle detection, the depth cap, verbatim
!! echo, and FileList/provenance bookkeeping. FileName must already be resolved.
recursive subroutine ParseFileInto(Doc, iNode, FileName, PS, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   integer(IntKi),       intent(in   ) :: iNode
   character(*),         intent(in   ) :: FileName
   type(YamlParseState), intent(inout) :: PS
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter                    :: RoutineName = 'ParseFileInto'
   character(MaxFileInfoLineLen), allocatable :: Lines(:)
   type(ScanLineType), allocatable            :: SL(:)
   integer(IntKi)                             :: FileIndx
   integer                                    :: i, Cur
   integer(IntKi)                             :: ErrStat2
   character(ErrMsgLen)                       :: ErrMsg2

   ErrStat = ErrID_None
   ErrMsg  = ""

   ! cycle detection over the in-progress stack
   do i = 1, PS%Depth
      if (trim(PS%Stack(i)) == FileName) then
         call SetErrStat(ErrID_Fatal, '>> Include cycle detected: "'//FileName// &
            '" is already being parsed (include chain: '//trim(IncludeChain(PS))//' -> '// &
            FileName//').', ErrStat, ErrMsg, RoutineName)
         return
      end if
   end do
   if (PS%Depth >= MaxIncludeDepth) then
      call SetErrStat(ErrID_Fatal, '>> Includes nested deeper than '// &
         trim(Num2LStr(MaxIncludeDepth))//' levels at "'//FileName//'" (include chain: '// &
         trim(IncludeChain(PS))//').', ErrStat, ErrMsg, RoutineName)
      return
   end if
   PS%Depth = PS%Depth + 1
   PS%Stack(PS%Depth) = FileName

   call ReadFileLines(FileName, Lines, ErrStat2, ErrMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   if (ErrStat >= AbortErrLev) then
      PS%Depth = PS%Depth - 1
      return
   end if

   ! verbatim echo — comments and all — under a banner naming the file
   if (PS%UnEc > 0) then
      write(PS%UnEc, '(A)') '!===== begin echo of "'//FileName//'" ====='
      do i = 1, size(Lines)
         write(PS%UnEc, '(A)') trim(Lines(i))
      end do
      write(PS%UnEc, '(A)') '!===== end echo of "'//FileName//'" ====='
   end if

   FileIndx = AddFile(Doc, FileName)

   call ScanSource(Lines, int(FileIndx), Doc%FileList, SL, ErrStat2, ErrMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   if (ErrStat < AbortErrLev) then
      if (size(SL) == 0) then
         call SetErrStat(ErrID_Fatal, 'No data found in "'//FileName//'".', ErrStat, ErrMsg, RoutineName)
      else
         Doc%Nodes(iNode)%FileIndx = FileIndx
         Doc%Nodes(iNode)%FileLine = SL(1)%FileLine
         Cur = 1
         call ParseBlockNode(SL, Cur, SL(1)%Indent, Doc, iNode, PS, ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
         if (ErrStat < AbortErrLev .and. Cur <= size(SL)) then
            call SetErrStat(ErrID_Fatal, LineRef(SL(Cur), Doc%FileList)// &
               ' Unexpected content after the end of the top-level block.', ErrStat, ErrMsg, RoutineName)
         end if
      end if
   end if

   PS%Depth = PS%Depth - 1
end subroutine ParseFileInto

!> " a -> b -> c" rendering of the current include stack, for cycle/depth errors.
function IncludeChain(PS) result(Chain)
   type(YamlParseState), intent(in) :: PS
   character(:), allocatable        :: Chain
   integer :: i
   Chain = ''
   do i = 1, PS%Depth
      if (i > 1) Chain = Chain//' -> '
      Chain = Chain//trim(PS%Stack(i))
   end do
end function IncludeChain

!> Append a file name to Doc%FileList, returning its (1-based) index.
integer(IntKi) function AddFile(Doc, FileName) result(Indx)
   type(YamlDoc), intent(inout) :: Doc
   character(*),  intent(in   ) :: FileName

   character(1024), allocatable :: Tmp(:)
   integer                      :: n

   n = size(Doc%FileList)
   allocate(Tmp(n+1))
   if (n > 0) Tmp(1:n) = Doc%FileList
   Tmp(n+1) = FileName
   call move_alloc(Tmp, Doc%FileList)
   Indx = n + 1
end function AddFile

!> Read every line of a text file into an array (no interpretation).
subroutine ReadFileLines(FileName, Lines, ErrStat, ErrMsg)
   character(*),                               intent(in   ) :: FileName
   character(MaxFileInfoLineLen), allocatable, intent(  out) :: Lines(:)
   integer(IntKi),                             intent(  out) :: ErrStat
   character(*),                               intent(  out) :: ErrMsg

   character(*), parameter                    :: RoutineName = 'ReadFileLines'
   character(MaxFileInfoLineLen), allocatable :: Tmp(:)
   character(MaxFileInfoLineLen)              :: Buf
   integer                                    :: Un, ios, n
   integer(IntKi)                             :: ErrStat2
   character(ErrMsgLen)                       :: ErrMsg2

   ErrStat = ErrID_None
   ErrMsg  = ""

   Un = -1
   call GetNewUnit(Un, ErrStat2, ErrMsg2)
   call OpenFInpFile(Un, FileName, ErrStat2, ErrMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   if (ErrStat >= AbortErrLev) return

   allocate(Lines(64))
   n = 0
   do
      read(Un, '(A)', iostat=ios) Buf
      if (ios /= 0) exit
      if (n == size(Lines)) then
         allocate(Tmp(2*n))
         Tmp(1:n) = Lines
         call move_alloc(Tmp, Lines)
      end if
      n = n + 1
      Lines(n) = Buf
   end do
   close(Un)
   Lines = Lines(1:n)
end subroutine ReadFileLines

!> Parse YAML source given as an array of lines (no disk access; !include is not available
!! through this entry). Used by unit tests and passed-data couplings.
subroutine Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   character(*),   intent(in   ) :: Lines(:)
   type(YamlDoc),  intent(  out) :: Doc
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter          :: RoutineName = 'Yaml_LoadString'
   type(ScanLineType), allocatable  :: SL(:)
   type(YamlParseState)             :: PS
   integer(IntKi)                   :: ErrStat2
   character(ErrMsgLen)             :: ErrMsg2
   integer                          :: Cur
   integer(IntKi)                   :: iRoot

   ErrStat = ErrID_None
   ErrMsg  = ""

   allocate(Doc%FileList(1))
   Doc%FileList(1) = StringSourceName

   call ScanSource(Lines, 1, Doc%FileList, SL, ErrStat2, ErrMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   if (ErrStat >= AbortErrLev) return

   if (size(SL) == 0) then
      call SetErrStat(ErrID_Fatal, 'No data found in '//StringSourceName//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   iRoot = AddNode(Doc, 0_IntKi)
   Doc%Nodes(iRoot)%FileIndx = SL(1)%FileIndx
   Doc%Nodes(iRoot)%FileLine = SL(1)%FileLine

   Cur = 1
   call ParseBlockNode(SL, Cur, SL(1)%Indent, Doc, iRoot, PS, ErrStat2, ErrMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   if (ErrStat >= AbortErrLev) return

   if (Cur <= size(SL)) then
      call SetErrStat(ErrID_Fatal, LineRef(SL(Cur), Doc%FileList)// &
         ' Unexpected content after the end of the top-level block.', ErrStat, ErrMsg, RoutineName)
   end if
end subroutine Yaml_LoadString

!> Parse YAML content carried in a FileInfoType — the passed-data channel used to hand
!! inline module input (or python-supplied YAML lines) to a module. The FileInfoType's
!! per-line FileLine/FileIndx/FileList provenance is honored, so error messages name the
!! original file and line even though the content arrived in memory. !include is not
!! available through this entry (includes are resolved before content is passed).
subroutine Yaml_LoadFileInfo(FileInfo, Doc, ErrStat, ErrMsg)
   type(FileInfoType), intent(in   ) :: FileInfo
   type(YamlDoc),      intent(  out) :: Doc
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter          :: RoutineName = 'Yaml_LoadFileInfo'
   type(ScanLineType), allocatable  :: SL(:)
   type(YamlParseState)             :: PS
   integer(IntKi)                   :: iRoot
   integer                          :: i, Cur
   logical                          :: HaveFileList
   integer(IntKi)                   :: ErrStat2
   character(ErrMsgLen)             :: ErrMsg2

   ErrStat = ErrID_None
   ErrMsg  = ""

   HaveFileList = .false.
   if (allocated(FileInfo%FileList)) HaveFileList = (FileInfo%NumFiles >= 1)
   if (HaveFileList) then
      allocate(Doc%FileList(FileInfo%NumFiles))
      do i = 1, FileInfo%NumFiles
         Doc%FileList(i) = FileInfo%FileList(i)
      end do
   else
      allocate(Doc%FileList(1))
      Doc%FileList(1) = '(passed file info)'
   end if

   if (FileInfo%NumLines < 1) then
      call SetErrStat(ErrID_Fatal, 'No data found in the passed file info.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   if (HaveFileList) then
      call ScanSource(FileInfo%Lines(1:FileInfo%NumLines), 1, Doc%FileList, SL, ErrStat2, ErrMsg2, &
                      LineNos=FileInfo%FileLine, FileIndxs=FileInfo%FileIndx)
   else
      call ScanSource(FileInfo%Lines(1:FileInfo%NumLines), 1, Doc%FileList, SL, ErrStat2, ErrMsg2, &
                      LineNos=FileInfo%FileLine)
   end if
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   if (ErrStat >= AbortErrLev) return

   if (size(SL) == 0) then
      call SetErrStat(ErrID_Fatal, 'No data found in the passed file info.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   iRoot = AddNode(Doc, 0_IntKi)
   Doc%Nodes(iRoot)%FileIndx = SL(1)%FileIndx
   Doc%Nodes(iRoot)%FileLine = SL(1)%FileLine

   Cur = 1
   call ParseBlockNode(SL, Cur, SL(1)%Indent, Doc, iRoot, PS, ErrStat2, ErrMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   if (ErrStat >= AbortErrLev) return

   if (Cur <= size(SL)) then
      call SetErrStat(ErrID_Fatal, LineRef(SL(Cur), Doc%FileList)// &
         ' Unexpected content after the end of the top-level block.', ErrStat, ErrMsg, RoutineName)
   end if
end subroutine Yaml_LoadFileInfo

!> Serialize the subtree rooted at iNode into a FileInfoType: YAML text lines whose
!! per-line FileLine/FileIndx point back at each node's true source (FileList is copied
!! from the document). This is how the glue code hands an inline module section to a
!! module through the existing passed-FileInfoType channel without losing error
!! provenance; Yaml_LoadFileInfo is the receiving end.
subroutine Yaml_Serialize(Doc, iNode, FileInfo, ErrStat, ErrMsg)
   type(YamlDoc),      intent(in   ) :: Doc
   integer(IntKi),     intent(in   ) :: iNode
   type(FileInfoType), intent(  out) :: FileInfo
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'Yaml_Serialize'
   integer                 :: i, NLines

   ErrStat = ErrID_None
   ErrMsg  = ""

   if (Doc%Nodes(iNode)%Kind /= YAML_MAP .and. Doc%Nodes(iNode)%Kind /= YAML_SEQ) then
      call SetErrStat(ErrID_Fatal, 'Only mappings and sequences can be serialized.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   FileInfo%NumFiles = size(Doc%FileList)
   allocate(FileInfo%FileList(FileInfo%NumFiles))
   do i = 1, FileInfo%NumFiles
      FileInfo%FileList(i) = Doc%FileList(i)
   end do

   ! two passes: count, then fill
   NLines = 0
   call EmitChildren(iNode, 0, .true., NLines)
   FileInfo%NumLines = NLines
   allocate(FileInfo%Lines(NLines), FileInfo%FileLine(NLines), FileInfo%FileIndx(NLines))
   NLines = 0
   call EmitChildren(iNode, 0, .false., NLines)

contains

   recursive subroutine EmitChildren(iParent, Indent, CountOnly, N)
      integer(IntKi), intent(in   ) :: iParent
      integer,        intent(in   ) :: Indent
      logical,        intent(in   ) :: CountOnly
      integer,        intent(inout) :: N

      integer(IntKi)            :: iChild
      character(:), allocatable :: Prefix, Line
      logical                   :: IsMapCtx

      IsMapCtx = (Doc%Nodes(iParent)%Kind == YAML_MAP)
      iChild = Doc%Nodes(iParent)%FirstChild
      do while (iChild > 0)
         if (IsMapCtx) then
            Prefix = QuoteIfNeeded(Doc%Nodes(iChild)%Key)//':'
         else
            Prefix = '-'
         end if

         select case (Doc%Nodes(iChild)%Kind)
         case (YAML_SCALAR)
            Line = Prefix//' '//QuoteIfNeeded(Doc%Nodes(iChild)%Scalar)
            call PutLine(iChild, Indent, Line, CountOnly, N)
         case (YAML_MAP, YAML_SEQ)
            if (Doc%Nodes(iChild)%NumChildren == 0) then
               if (Doc%Nodes(iChild)%Kind == YAML_MAP) then
                  Line = Prefix//' {}'
               else
                  Line = Prefix//' []'
               end if
               call PutLine(iChild, Indent, Line, CountOnly, N)
            else
               call PutLine(iChild, Indent, Prefix, CountOnly, N)
               call EmitChildren(iChild, Indent+2, CountOnly, N)
            end if
         end select
         iChild = Doc%Nodes(iChild)%NextSibling
      end do
   end subroutine EmitChildren

   subroutine PutLine(iSrc, Indent, Text, CountOnly, N)
      integer(IntKi), intent(in   ) :: iSrc
      integer,        intent(in   ) :: Indent
      character(*),   intent(in   ) :: Text
      logical,        intent(in   ) :: CountOnly
      integer,        intent(inout) :: N

      N = N + 1
      if (CountOnly) return
      FileInfo%Lines(N)    = repeat(' ', Indent)//Text
      FileInfo%FileLine(N) = Doc%Nodes(iSrc)%FileLine
      FileInfo%FileIndx(N) = Doc%Nodes(iSrc)%FileIndx
   end subroutine PutLine

   !> Double-quote (with \" and \\ escapes) any token that plain YAML could misread.
   function QuoteIfNeeded(TextIn) result(TextOut)
      character(:), allocatable, intent(in) :: TextIn
      character(:), allocatable             :: TextOut

      character(*), parameter :: SafeChars = &
         'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.+-/\()'
      integer :: i2
      logical :: Plain

      if (.not. allocated(TextIn)) then
         TextOut = '""'
         return
      end if
      if (len(TextIn) == 0) then
         TextOut = '""'
         return
      end if

      Plain = .true.
      do i2 = 1, len(TextIn)
         if (index(SafeChars, TextIn(i2:i2)) == 0) then
            Plain = .false.
            exit
         end if
      end do
      if (Plain) then
         TextOut = TextIn
      else
         TextOut = ''
         do i2 = 1, len(TextIn)
            if (TextIn(i2:i2) == '"' .or. TextIn(i2:i2) == '\') TextOut = TextOut//'\'
            TextOut = TextOut//TextIn(i2:i2)
         end do
         TextOut = '"'//TextOut//'"'
      end if
   end function QuoteIfNeeded

end subroutine Yaml_Serialize

!----------------------------------------------------------------------------------------------------------------------------------
! Internal: node arena
!----------------------------------------------------------------------------------------------------------------------------------

!> Append a fresh node to the arena, linked under iParent (0 for the root), and return
!! its index. Node indices are stable for the life of the document.
integer(IntKi) function AddNode(Doc, iParent) result(iNew)
   type(YamlDoc),  intent(inout) :: Doc
   integer(IntKi), intent(in   ) :: iParent

   type(YamlNode), allocatable :: Tmp(:)
   integer(IntKi)              :: Cap

   if (.not. allocated(Doc%Nodes)) then
      allocate(Doc%Nodes(16))
      Doc%NumNodes = 0
   else if (Doc%NumNodes == size(Doc%Nodes)) then
      Cap = 2*size(Doc%Nodes)
      allocate(Tmp(Cap))
      Tmp(1:Doc%NumNodes) = Doc%Nodes(1:Doc%NumNodes)
      call move_alloc(Tmp, Doc%Nodes)
   end if

   Doc%NumNodes = Doc%NumNodes + 1
   iNew = Doc%NumNodes
   Doc%Nodes(iNew) = YamlNode()          ! default-initialized record
   Doc%Nodes(iNew)%Parent = iParent

   if (iParent > 0) then
      if (Doc%Nodes(iParent)%FirstChild == 0) then
         Doc%Nodes(iParent)%FirstChild = iNew
      else
         Doc%Nodes(Doc%Nodes(iParent)%LastChild)%NextSibling = iNew
      end if
      Doc%Nodes(iParent)%LastChild   = iNew
      Doc%Nodes(iParent)%NumChildren = Doc%Nodes(iParent)%NumChildren + 1
   end if
end function AddNode

!> Deep-copy the subtree rooted at iSrc as a new child of iParent, returning the new
!! node's index. Provenance is retained from the source (alias copies report the
!! anchor's true location); Used flags reset. Indices are stable, so copying from the
!! same arena that is growing is safe — only indices are held across AddNode calls.
recursive function CopySubtree(Doc, iSrc, iParent) result(iNew)
   type(YamlDoc),  intent(inout) :: Doc
   integer(IntKi), intent(in   ) :: iSrc
   integer(IntKi), intent(in   ) :: iParent
   integer(IntKi)                :: iNew

   iNew = AddNode(Doc, iParent)
   if (allocated(Doc%Nodes(iSrc)%Key)) Doc%Nodes(iNew)%Key = Doc%Nodes(iSrc)%Key
   call CopyInto(Doc, iSrc, iNew)
end function CopySubtree

!> Copy kind, scalar text, provenance, and (deep) children of iSrc into the existing node
!! iDest — the node's own Key and parent linkage are left untouched (an alias takes the
!! host's key but the anchor's content and provenance).
recursive subroutine CopyInto(Doc, iSrc, iDest)
   type(YamlDoc),  intent(inout) :: Doc
   integer(IntKi), intent(in   ) :: iSrc
   integer(IntKi), intent(in   ) :: iDest

   integer(IntKi) :: iEntry, iNext, iNew

   Doc%Nodes(iDest)%Kind     = Doc%Nodes(iSrc)%Kind
   Doc%Nodes(iDest)%FileIndx = Doc%Nodes(iSrc)%FileIndx
   Doc%Nodes(iDest)%FileLine = Doc%Nodes(iSrc)%FileLine
   Doc%Nodes(iDest)%Used     = .false.
   if (allocated(Doc%Nodes(iSrc)%Scalar)) Doc%Nodes(iDest)%Scalar = Doc%Nodes(iSrc)%Scalar

   iEntry = Doc%Nodes(iSrc)%FirstChild
   do while (iEntry > 0)
      iNext = Doc%Nodes(iEntry)%NextSibling       ! read before any arena growth
      iNew  = CopySubtree(Doc, iEntry, iDest)
      iEntry = iNext
   end do
end subroutine CopyInto

!> Register (or redefine) an anchor name for a completed node.
subroutine RegisterAnchor(PS, Name, iNode)
   type(YamlParseState), intent(inout) :: PS
   character(*),         intent(in   ) :: Name
   integer(IntKi),       intent(in   ) :: iNode

   character(64),  allocatable :: TmpN(:)
   integer(IntKi), allocatable :: TmpI(:)
   integer                     :: i

   do i = 1, PS%NumAnchors
      if (trim(PS%AnchorNames(i)) == Name) then   ! redefinition shadows, per YAML
         PS%AnchorNodes(i) = iNode
         return
      end if
   end do

   if (.not. allocated(PS%AnchorNames)) then
      allocate(PS%AnchorNames(8), PS%AnchorNodes(8))
   else if (PS%NumAnchors == size(PS%AnchorNames)) then
      allocate(TmpN(2*size(PS%AnchorNames)), TmpI(2*size(PS%AnchorNodes)))
      TmpN(1:PS%NumAnchors) = PS%AnchorNames
      TmpI(1:PS%NumAnchors) = PS%AnchorNodes
      call move_alloc(TmpN, PS%AnchorNames)
      call move_alloc(TmpI, PS%AnchorNodes)
   end if

   PS%NumAnchors = PS%NumAnchors + 1
   PS%AnchorNames(PS%NumAnchors) = Name
   PS%AnchorNodes(PS%NumAnchors) = iNode
end subroutine RegisterAnchor

!> Node index for an anchor name; 0 when undefined.
integer(IntKi) function ResolveAlias(PS, Name) result(iNode)
   type(YamlParseState), intent(in) :: PS
   character(*),         intent(in) :: Name
   integer :: i
   iNode = 0
   do i = 1, PS%NumAnchors
      if (trim(PS%AnchorNames(i)) == Name) then
         iNode = PS%AnchorNodes(i)
         return
      end if
   end do
end function ResolveAlias

!----------------------------------------------------------------------------------------------------------------------------------
! Internal: scanning
!----------------------------------------------------------------------------------------------------------------------------------

!> " on line #N of "file"" fragment used by every parser error, built from a scan line.
function LineRef(SL, FileList) result(Ref)
   type(ScanLineType), intent(in) :: SL
   character(*),       intent(in) :: FileList(:)
   character(:), allocatable      :: Ref
   Ref = '>> Error on line #'//trim(Num2LStr(SL%FileLine))//' of "'//trim(FileList(SL%FileIndx))//'":'
end function LineRef

!> Scan raw source lines into significant ScanLines: measure indentation (spaces only —
!! a tab anywhere in the indent is fatal), strip comments quote-awareness included, drop
!! blank/comment-only lines, tolerate one leading '---' document marker.
subroutine ScanSource(Lines, FileIndx, FileList, SL, ErrStat, ErrMsg, LineNos, FileIndxs)
   character(*),       intent(in   )              :: Lines(:)
   integer,            intent(in   )              :: FileIndx
   character(*),       intent(in   )              :: FileList(:)
   type(ScanLineType), allocatable, intent(  out) :: SL(:)
   integer(IntKi),     intent(  out)              :: ErrStat
   character(*),       intent(  out)              :: ErrMsg
   integer(IntKi),     intent(in   ), optional    :: LineNos(:)   !< per-line source line numbers (passed FileInfo)
   integer(IntKi),     intent(in   ), optional    :: FileIndxs(:) !< per-line source file indices (passed FileInfo)

   character(*), parameter   :: RoutineName = 'ScanSource'
   integer                   :: i, j, NSig, Indent, TextLen, FL, FX
   character(:), allocatable :: Text
   logical                   :: DocMarkerSeen

   ErrStat = ErrID_None
   ErrMsg  = ""
   DocMarkerSeen = .false.

   allocate(SL(size(Lines)))
   NSig = 0

   do i = 1, size(Lines)
      FL = i
      FX = FileIndx
      if (present(LineNos))   FL = LineNos(i)
      if (present(FileIndxs)) FX = FileIndxs(i)
      TextLen = len_trim(Lines(i))

      ! measure indentation; reject tabs within it
      Indent = 0
      do j = 1, TextLen
         if (Lines(i)(j:j) == ' ') then
            Indent = Indent + 1
         else if (Lines(i)(j:j) == char(9)) then
            call SetErrStat(ErrID_Fatal, '>> Error on line #'//trim(Num2LStr(FL))//' of "'// &
               trim(FileList(FX))//'": Tab character in indentation; YAML requires spaces.', &
               ErrStat, ErrMsg, RoutineName)
            return
         else
            exit
         end if
      end do
      if (Indent >= TextLen) cycle   ! blank line

      call StripComment(Lines(i)(Indent+1:TextLen), Text)
      if (len_trim(Text) == 0) cycle ! comment-only line

      ! document markers: tolerate a single leading '---'; reject anything further
      if (trim(Text) == '---') then
         if (NSig == 0 .and. .not. DocMarkerSeen) then
            DocMarkerSeen = .true.
            cycle
         else
            call SetErrStat(ErrID_Fatal, '>> Error on line #'//trim(Num2LStr(FL))//' of "'// &
               trim(FileList(FX))//'": Multi-document YAML streams are not supported by OpenFAST '// &
               '(only one leading "---" is allowed).', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if

      NSig = NSig + 1
      SL(NSig)%Indent   = Indent
      SL(NSig)%FileLine = FL
      SL(NSig)%FileIndx = FX
      SL(NSig)%Text     = trim(Text)
   end do

   SL = SL(1:NSig)
end subroutine ScanSource

!> Remove a trailing comment from a content line, honoring quoted strings. A '#' starts a
!! comment when it is outside quotes and is either the first character or preceded by
!! whitespace (per YAML). The returned text is right-trimmed.
subroutine StripComment(TextIn, TextOut)
   character(*),              intent(in   ) :: TextIn
   character(:), allocatable, intent(  out) :: TextOut

   integer      :: i
   character(1) :: Quote  ! active quote character, or ' ' when outside quotes

   Quote = ' '
   i = 1
   do while (i <= len(TextIn))
      select case (TextIn(i:i))
      case ('"', "'")
         if (Quote == ' ') then
            Quote = TextIn(i:i)
         else if (Quote == TextIn(i:i)) then
            if (Quote == "'" .and. i < len(TextIn)) then
               if (TextIn(i+1:i+1) == "'") then  ! '' escape inside single quotes
                  i = i + 1
               else
                  Quote = ' '
               end if
            else
               Quote = ' '
            end if
         end if
      case ('\')
         if (Quote == '"') i = i + 1             ! skip escaped char inside double quotes
      case ('#')
         if (Quote == ' ') then
            if (i == 1) then
               TextOut = ''
               return
            else if (TextIn(i-1:i-1) == ' ' .or. TextIn(i-1:i-1) == char(9)) then
               TextOut = trim(TextIn(1:i-1))
               return
            end if
         end if
      end select
      i = i + 1
   end do
   TextOut = trim(TextIn)
end subroutine StripComment

!----------------------------------------------------------------------------------------------------------------------------------
! Internal: block parsing
!----------------------------------------------------------------------------------------------------------------------------------

!> Parse the block starting at SL(Cur) (a mapping or a sequence at indentation Indent)
!! into node iNode. On return Cur points at the first line no longer part of this block.
recursive subroutine ParseBlockNode(SL, Cur, Indent, Doc, iNode, PS, ErrStat, ErrMsg)
   type(ScanLineType),   intent(in   ) :: SL(:)
   integer,              intent(inout) :: Cur
   integer,              intent(in   ) :: Indent
   type(YamlDoc),        intent(inout) :: Doc
   integer(IntKi),       intent(in   ) :: iNode
   type(YamlParseState), intent(inout) :: PS
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   if (IsSeqItem(SL(Cur)%Text)) then
      call ParseBlockSeq(SL, Cur, Indent, Doc, iNode, PS, ErrStat, ErrMsg)
   else
      call ParseBlockMap(SL, Cur, Indent, Doc, iNode, PS, ErrStat, ErrMsg)
   end if
end subroutine ParseBlockNode

!> True when a content line is a block-sequence item ("- item" or a bare "-").
logical function IsSeqItem(Text) result(IsItem)
   character(*), intent(in) :: Text
   ! Fortran does not short-circuit .and., so guard the substring explicitly
   ! (a 1-character content line would otherwise index Text(1:2) out of bounds).
   if (len(Text) >= 2) then
      IsItem = (Text(1:2) == '- ')
   else
      IsItem = (Text == '-')
   end if
end function IsSeqItem

!> Parse a block mapping whose keys sit at indentation Indent, adding entries under iNode.
!! Handles "&anchor" capture on values, "<<:" merge keys (applied after the block so
!! explicit keys always win), and delegates alias expansion to SetScalar.
recursive subroutine ParseBlockMap(SL, Cur, Indent, Doc, iNode, PS, ErrStat, ErrMsg)
   type(ScanLineType),   intent(in   ) :: SL(:)
   integer,              intent(inout) :: Cur
   integer,              intent(in   ) :: Indent
   type(YamlDoc),        intent(inout) :: Doc
   integer(IntKi),       intent(in   ) :: iNode
   type(YamlParseState), intent(inout) :: PS
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'ParseBlockMap'
   character(:), allocatable :: Key, Value, AnchName
   integer(IntKi)            :: iChild, iDup
   integer(IntKi)            :: MergeSrcs(MaxMergeSrcs)
   integer                   :: nMergeSrc
   logical                   :: MergeSeen
   integer(IntKi)            :: ErrStat2
   character(ErrMsgLen)      :: ErrMsg2

   ErrStat = ErrID_None
   ErrMsg  = ""
   Doc%Nodes(iNode)%Kind = YAML_MAP
   MergeSeen = .false.
   nMergeSrc = 0

   do while (Cur <= size(SL))
      if (SL(Cur)%Indent < Indent) exit           ! end of this block
      if (SL(Cur)%Indent > Indent) then
         call SetErrStat(ErrID_Fatal, LineRef(SL(Cur), Doc%FileList)//' Unexpected indentation '// &
            '(expected a key at column '//trim(Num2LStr(Indent+1))//').', ErrStat, ErrMsg, RoutineName)
         return
      end if
      if (IsSeqItem(SL(Cur)%Text)) exit           ! sequence item belongs to the enclosing key

      call SplitKeyValue(SL(Cur), Doc%FileList, Key, Value, ErrStat2, ErrMsg2)
      call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if (ErrStat >= AbortErrLev) return

      ! "<<:" — standard merge key: collect sources now, apply after the block completes
      if (Key == '<<') then
         if (MergeSeen) then
            call SetErrStat(ErrID_Fatal, LineRef(SL(Cur), Doc%FileList)//' A mapping may contain '// &
               'only one "<<" merge key.', ErrStat, ErrMsg, RoutineName)
            return
         end if
         MergeSeen = .true.
         call ParseMergeValue(SL(Cur), Value, Doc, PS, MergeSrcs, nMergeSrc, ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
         if (ErrStat >= AbortErrLev) return
         Cur = Cur + 1
         cycle
      end if

      ! duplicate keys are ambiguous decks: hard error naming both definitions
      iDup = Yaml_ChildByKey(Doc, iNode, Key)
      if (iDup > 0) then
         call SetErrStat(ErrID_Fatal, '>> Duplicate key "'//Key//'" on line #'// &
            trim(Num2LStr(SL(Cur)%FileLine))//' of "'//trim(Doc%FileList(SL(Cur)%FileIndx))// &
            '"; first defined on line #'//trim(Num2LStr(Doc%Nodes(iDup)%FileLine))//'.', &
            ErrStat, ErrMsg, RoutineName)
         return
      end if

      ! "&anchor" prefix on the value: remember the name, keep parsing the remainder
      call TakeAnchor(Value, AnchName)

      iChild = AddNode(Doc, iNode)
      Doc%Nodes(iChild)%Key      = Key
      Doc%Nodes(iChild)%FileIndx = SL(Cur)%FileIndx
      Doc%Nodes(iChild)%FileLine = SL(Cur)%FileLine

      if (len(Value) > 0) then
         ! value on the same line: a scalar, flow collection, or alias
         call SetScalar(Doc, iChild, Value, SL(Cur), PS, ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
         if (ErrStat >= AbortErrLev) return
         Cur = Cur + 1
      else
         ! no value on this line: nested block (deeper, or a sequence at the same
         ! indentation), or an empty scalar
         Cur = Cur + 1
         if (NestedFollows(SL, Cur, Indent)) then
            call ParseBlockNode(SL, Cur, SL(Cur)%Indent, Doc, iChild, PS, ErrStat2, ErrMsg2)
            call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
            if (ErrStat >= AbortErrLev) return
         else
            Doc%Nodes(iChild)%Kind   = YAML_SCALAR
            Doc%Nodes(iChild)%Scalar = ''
         end if
      end if

      ! anchors bind once their node is complete (a self-referencing alias is undefined)
      if (len(AnchName) > 0) call RegisterAnchor(PS, AnchName, iChild)
   end do

   call ApplyMerges(Doc, iNode, MergeSrcs, nMergeSrc, ErrStat2, ErrMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
end subroutine ParseBlockMap

!> Strip a leading "&name" from a value text, returning the name ('' when none).
subroutine TakeAnchor(Value, AnchName)
   character(:), allocatable, intent(inout) :: Value
   character(:), allocatable, intent(  out) :: AnchName

   integer :: SpacePos

   AnchName = ''
   if (len(Value) == 0) return
   if (Value(1:1) /= '&') return

   SpacePos = index(Value, ' ')
   if (SpacePos == 0) then
      AnchName = Value(2:)
      Value    = ''
   else
      AnchName = Value(2:SpacePos-1)
      Value    = trim(adjustl(Value(SpacePos+1:)))
   end if
end subroutine TakeAnchor

!> Parse the value of a "<<:" merge key: one alias or a flow list of aliases, each of
!! which must resolve to a mapping. Appends the resolved node indices to MergeSrcs.
subroutine ParseMergeValue(SLine, Value, Doc, PS, MergeSrcs, nMergeSrc, ErrStat, ErrMsg)
   type(ScanLineType),   intent(in   ) :: SLine
   character(*),         intent(in   ) :: Value
   type(YamlDoc),        intent(in   ) :: Doc
   type(YamlParseState), intent(in   ) :: PS
   integer(IntKi),       intent(inout) :: MergeSrcs(:)
   integer,              intent(inout) :: nMergeSrc
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'ParseMergeValue'
   character(:), allocatable :: Token
   integer                   :: Pos

   ErrStat = ErrID_None
   ErrMsg  = ""

   if (len(Value) == 0) then
      call SetErrStat(ErrID_Fatal, LineRef(SLine, Doc%FileList)//' The "<<" merge key requires an '// &
         'inline value: an alias (*name) or a list of aliases ([*a, *b]).', ErrStat, ErrMsg, RoutineName)
      return
   end if

   if (Value(1:1) == '*') then
      call AddMergeSrc(trim(Value))
   else if (Value(1:1) == '[') then
      Pos = 2
      do
         call FlowToken(Value, Pos, .false., Token)
         if (len_trim(Token) > 0) call AddMergeSrc(trim(Token))
         if (ErrStat >= AbortErrLev) return
         if (Pos > len(Value)) then
            call SetErrStat(ErrID_Fatal, LineRef(SLine, Doc%FileList)//' Unterminated alias list '// &
               'on the "<<" merge key.', ErrStat, ErrMsg, RoutineName)
            return
         end if
         if (Value(Pos:Pos) == ']') exit
         Pos = Pos + 1   ! consume ','
      end do
   else
      call SetErrStat(ErrID_Fatal, LineRef(SLine, Doc%FileList)//' The "<<" merge key value must be '// &
         'an alias (*name) or a list of aliases ([*a, *b]); found "'//trim(Value)//'".', &
         ErrStat, ErrMsg, RoutineName)
   end if

contains

   subroutine AddMergeSrc(AliasText)
      character(*), intent(in) :: AliasText
      integer(IntKi) :: iSrc

      if (AliasText(1:1) /= '*') then
         call SetErrStat(ErrID_Fatal, LineRef(SLine, Doc%FileList)//' Items in a "<<" merge list must '// &
            'be aliases (*name); found "'//AliasText//'".', ErrStat, ErrMsg, RoutineName)
         return
      end if
      iSrc = ResolveAlias(PS, AliasText(2:))
      if (iSrc == 0) then
         call SetErrStat(ErrID_Fatal, LineRef(SLine, Doc%FileList)//' Undefined anchor "'// &
            AliasText(2:)//'" referenced by the "<<" merge key.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      if (Doc%Nodes(iSrc)%Kind /= YAML_MAP) then
         call SetErrStat(ErrID_Fatal, LineRef(SLine, Doc%FileList)//' The "<<" merge key can only '// &
            'merge mappings; anchor "'//AliasText(2:)//'" is not a mapping.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      if (nMergeSrc >= size(MergeSrcs)) then
         call SetErrStat(ErrID_Fatal, LineRef(SLine, Doc%FileList)//' Too many aliases in one "<<" '// &
            'merge list (limit '//trim(Num2LStr(size(MergeSrcs)))//').', ErrStat, ErrMsg, RoutineName)
         return
      end if
      nMergeSrc = nMergeSrc + 1
      MergeSrcs(nMergeSrc) = iSrc
   end subroutine AddMergeSrc

end subroutine ParseMergeValue

!> Apply collected merge sources to a completed mapping: entries whose keys the host does
!! not already have are deep-copied in, in source order (host keys and earlier sources win).
subroutine ApplyMerges(Doc, iHost, MergeSrcs, nMergeSrc, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   integer(IntKi), intent(in   ) :: iHost
   integer(IntKi), intent(in   ) :: MergeSrcs(:)
   integer,        intent(in   ) :: nMergeSrc
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   integer        :: i
   integer(IntKi) :: iEntry, iNext, iNew

   ErrStat = ErrID_None
   ErrMsg  = ""

   do i = 1, nMergeSrc
      iEntry = Doc%Nodes(MergeSrcs(i))%FirstChild
      do while (iEntry > 0)
         iNext = Doc%Nodes(iEntry)%NextSibling    ! read before any arena growth
         if (Yaml_ChildByKey(Doc, iHost, Doc%Nodes(iEntry)%Key) == 0) then
            iNew = CopySubtree(Doc, iEntry, iHost)
         end if
         iEntry = iNext
      end do
   end do
end subroutine ApplyMerges

!> After consuming "key:" at indentation Indent, decide whether the next line opens the
!! key's nested block: any deeper line does, and so does a sequence item at the same
!! indentation (YAML permits a block sequence at its key's own indent). False when the
!! source is exhausted (Fortran .and. does not short-circuit, so the bound check lives here).
logical function NestedFollows(SL, Cur, Indent) result(Follows)
   type(ScanLineType), intent(in) :: SL(:)
   integer,            intent(in) :: Cur
   integer,            intent(in) :: Indent
   Follows = .false.
   if (Cur > size(SL)) return
   Follows = SL(Cur)%Indent > Indent .or. &
             (SL(Cur)%Indent == Indent .and. IsSeqItem(SL(Cur)%Text))
end function NestedFollows

!> Parse a block sequence whose "-" markers sit at indentation Indent, adding items under
!! iNode. Items may carry "&anchor" prefixes and may be aliases ("- *T1").
recursive subroutine ParseBlockSeq(SL, Cur, Indent, Doc, iNode, PS, ErrStat, ErrMsg)
   type(ScanLineType),   intent(in   ) :: SL(:)
   integer,              intent(inout) :: Cur
   integer,              intent(in   ) :: Indent
   type(YamlDoc),        intent(inout) :: Doc
   integer(IntKi),       intent(in   ) :: iNode
   type(YamlParseState), intent(inout) :: PS
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter          :: RoutineName = 'ParseBlockSeq'
   type(ScanLineType), allocatable  :: Slice(:)
   character(:), allocatable        :: ItemText, AnchName
   integer(IntKi)                   :: iChild
   integer(IntKi)                   :: ErrStat2
   character(ErrMsgLen)             :: ErrMsg2
   integer                          :: NSlice, SubCur, ItemLine

   ErrStat = ErrID_None
   ErrMsg  = ""
   Doc%Nodes(iNode)%Kind = YAML_SEQ

   do while (Cur <= size(SL))
      if (SL(Cur)%Indent /= Indent) return
      if (.not. IsSeqItem(SL(Cur)%Text)) return

      iChild = AddNode(Doc, iNode)
      Doc%Nodes(iChild)%FileIndx = SL(Cur)%FileIndx
      Doc%Nodes(iChild)%FileLine = SL(Cur)%FileLine
      ItemLine = SL(Cur)%FileLine

      ! inline text after "- ", with any "&anchor" prefix taken off first
      if (len_trim(SL(Cur)%Text) > 1) then
         ItemText = trim(adjustl(SL(Cur)%Text(2:)))
      else
         ItemText = ''
      end if
      call TakeAnchor(ItemText, AnchName)

      ! Re-baseline the item: its inline text acts as a line at Indent+2, followed by
      ! every subsequent line deeper than Indent. Parsing the slice as its own block
      ! handles "- scalar", "- key: value" map items, and "-" + nested block.
      allocate(Slice(size(SL)))
      NSlice = 0
      if (len(ItemText) > 0) then
         NSlice = NSlice + 1
         Slice(NSlice)          = SL(Cur)
         Slice(NSlice)%Indent   = Indent + 2
         Slice(NSlice)%Text     = ItemText
      end if
      Cur = Cur + 1
      do while (Cur <= size(SL))
         if (SL(Cur)%Indent <= Indent) exit
         NSlice = NSlice + 1
         Slice(NSlice) = SL(Cur)
         Cur = Cur + 1
      end do

      if (NSlice == 0) then
         Doc%Nodes(iChild)%Kind   = YAML_SCALAR  ! bare "-" with nothing nested
         Doc%Nodes(iChild)%Scalar = ''
      else if (NSlice == 1 .and. ItemLine == Slice(1)%FileLine .and. FindColon(Slice(1)%Text) == 0 &
               .and. .not. IsSeqItem(Slice(1)%Text)) then
         ! One content line with no mapping separator: a plain scalar item -- unless that
         ! line is itself a sequence item ("- ..."), which is a nested sequence ("- - 0"
         ! in flowed-out form). The IsSeqItem exclusion matters for serialized inline
         ! module input (Yaml_Serialize emits nested sequences as a bare "-" line plus
         ! deeper "- <scalar>" lines, all carrying the SAME source-line provenance when
         ! the original was a one-line flow collection, so the ItemLine==FileLine check
         ! alone cannot tell "- 0" apart from an inline scalar; a single-column matrix
         ! row like AddF0's "[0]" would otherwise collapse to the scalar "- 0").
         call SetScalar(Doc, iChild, Slice(1)%Text, Slice(1), PS, ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
         if (ErrStat >= AbortErrLev) return
      else
         SubCur = 1
         call ParseBlockNode(Slice(1:NSlice), SubCur, Slice(1)%Indent, Doc, iChild, PS, ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
         if (ErrStat >= AbortErrLev) return
         if (SubCur <= NSlice) then
            call SetErrStat(ErrID_Fatal, LineRef(Slice(SubCur), Doc%FileList)// &
               ' Unexpected content within a sequence item.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if

      if (len(AnchName) > 0) call RegisterAnchor(PS, AnchName, iChild)
      deallocate(Slice)
   end do
end subroutine ParseBlockSeq

!----------------------------------------------------------------------------------------------------------------------------------
! Internal: scalars, keys, and small utilities
!----------------------------------------------------------------------------------------------------------------------------------

!> Locate the mapping separator in a content line: a ':' outside quotes that is followed
!! by a space or ends the line. Returns 0 when there is none.
integer function FindColon(Text) result(Pos)
   character(*), intent(in) :: Text
   integer      :: i
   character(1) :: Quote

   Pos = 0
   Quote = ' '
   i = 1
   do while (i <= len(Text))
      select case (Text(i:i))
      case ('"', "'")
         if (Quote == ' ') then
            Quote = Text(i:i)
         else if (Quote == Text(i:i)) then
            if (Quote == "'" .and. i < len(Text)) then
               if (Text(i+1:i+1) == "'") then
                  i = i + 1
               else
                  Quote = ' '
               end if
            else
               Quote = ' '
            end if
         end if
      case ('\')
         if (Quote == '"') i = i + 1
      case (':')
         if (Quote == ' ') then
            if (i == len(Text)) then
               Pos = i
               return
            else if (Text(i+1:i+1) == ' ') then
               Pos = i
               return
            end if
         end if
      end select
      i = i + 1
   end do
end function FindColon

!> Split a mapping line into key and value text. The key may be quoted; the value may be
!! empty ("key:"). Fatal when the line has no mapping separator.
subroutine SplitKeyValue(SL, FileList, Key, Value, ErrStat, ErrMsg)
   type(ScanLineType),        intent(in   ) :: SL
   character(*),              intent(in   ) :: FileList(:)
   character(:), allocatable, intent(  out) :: Key
   character(:), allocatable, intent(  out) :: Value
   integer(IntKi),            intent(  out) :: ErrStat
   character(*),              intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SplitKeyValue'
   integer                 :: ColonPos

   ErrStat = ErrID_None
   ErrMsg  = ""

   ColonPos = FindColon(SL%Text)
   if (ColonPos == 0) then
      call SetErrStat(ErrID_Fatal, LineRef(SL, FileList)//' Expected "key: value" but found "'// &
         trim(SL%Text)//'".', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call Unquote(trim(SL%Text(1:ColonPos-1)), SL, FileList, Key, ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return
   if (len(Key) == 0) then
      call SetErrStat(ErrID_Fatal, LineRef(SL, FileList)//' Empty mapping key.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   if (ColonPos == len(SL%Text)) then
      Value = ''
   else
      Value = trim(adjustl(SL%Text(ColonPos+1:)))
   end if
end subroutine SplitKeyValue

!> Fill node iNode with an inline value: a flow collection ("[...]"/"{...}"), or a scalar
!! (unquoted as needed; raw text stored — typed conversion happens at lookup). Block
!! scalars and tags are rejected here with named-line errors.
recursive subroutine SetScalar(Doc, iNode, Text, SL, PS, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   integer(IntKi),       intent(in   ) :: iNode
   character(*),         intent(in   ) :: Text
   type(ScanLineType),   intent(in   ) :: SL
   type(YamlParseState), intent(inout) :: PS
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'SetScalar'
   character(:), allocatable :: Unquoted
   integer                   :: Pos, TagEnd
   integer(IntKi)            :: iSrc

   ErrStat = ErrID_None
   ErrMsg  = ""

   select case (Text(1:1))
   case ('[', '{')
      Pos = 1
      call ParseFlow(Doc, iNode, Text, Pos, SL, PS, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
      if (len_trim(Text(Pos:)) > 0) then
         call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Unexpected content after '// &
            'the flow collection: "'//trim(Text(Pos:))//'".', ErrStat, ErrMsg, RoutineName)
      end if
   case ('*')
      if (index(Text, ' ') > 0) then
         call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Unexpected content after '// &
            'the alias "'//Text(1:index(Text,' ')-1)//'".', ErrStat, ErrMsg, RoutineName)
         return
      end if
      iSrc = ResolveAlias(PS, Text(2:))
      if (iSrc == 0) then
         call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Undefined anchor "'//Text(2:)// &
            '" (anchors must be defined before they are referenced).', ErrStat, ErrMsg, RoutineName)
         return
      end if
      call CopyInto(Doc, iSrc, iNode)
   case ('&')
      call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Anchors ("&") are supported on '// &
         'block mapping values and sequence items, not here.', ErrStat, ErrMsg, RoutineName)
   case ('|', '>')
      call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Block scalars ("|", ">") are '// &
         'not supported by OpenFAST; use a quoted string or a flow collection.', ErrStat, ErrMsg, RoutineName)
   case ('!')
      TagEnd = index(Text, ' ')
      if (TagEnd == 0) TagEnd = len(Text) + 1
      if (Text(1:TagEnd-1) == '!include' .and. PS%AllowInclude) then
         call ProcessInclude(Doc, iNode, trim(adjustl(Text(TagEnd:))), SL, PS, ErrStat, ErrMsg)
      else
         call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Unknown tag "'//Text(1:TagEnd-1)// &
            '". The only tag OpenFAST supports is !include (available in file-based input).', &
            ErrStat, ErrMsg, RoutineName)
      end if
   case default
      call Unquote(Text, SL, Doc%FileList, Unquoted, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
      Doc%Nodes(iNode)%Kind   = YAML_SCALAR
      Doc%Nodes(iNode)%Scalar = Unquoted
   end select
end subroutine SetScalar

!> Handle "!include <path>" at a value position: resolve the path relative to the
!! including file (the same rule as the text format's @file), then splice the included
!! file's root content into iNode. Errors gain the referencing file:line context.
recursive subroutine ProcessInclude(Doc, iNode, PathText, SL, PS, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   integer(IntKi),       intent(in   ) :: iNode
   character(*),         intent(in   ) :: PathText
   type(ScanLineType),   intent(in   ) :: SL
   type(YamlParseState), intent(inout) :: PS
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'ProcessInclude'
   character(:), allocatable :: Path
   character(1024)           :: PriPath
   integer(IntKi)            :: ErrStat2
   character(ErrMsgLen)      :: ErrMsg2

   ErrStat = ErrID_None
   ErrMsg  = ""

   call Unquote(PathText, SL, Doc%FileList, Path, ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return
   if (len_trim(Path) == 0) then
      call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' "!include" requires a file path.', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if

   ! resolve relative to the directory of the *including* file
   if (PathIsRelative(Path)) then
      call GetPath(Doc%FileList(SL%FileIndx), PriPath)
      Path = trim(PriPath)//Path
   end if

   call ParseFileInto(Doc, iNode, Path, PS, ErrStat2, ErrMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   if (ErrStat >= AbortErrLev) then
      ! prepend the referencing location so the user can find the offending !include
      call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' while processing !include "'// &
         Path//'".', ErrStat, ErrMsg, RoutineName)
   end if
end subroutine ProcessInclude

!> Parse a flow collection ("[a, b]" / "{k: v}") starting at Text(Pos:Pos) into iNode.
!! On return Pos points just past the closing bracket. Nested flow collections recurse;
!! commas and colons inside quoted strings are honored.
recursive subroutine ParseFlow(Doc, iNode, Text, Pos, SL, PS, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   integer(IntKi),       intent(in   ) :: iNode
   character(*),         intent(in   ) :: Text
   integer,              intent(inout) :: Pos
   type(ScanLineType),   intent(in   ) :: SL
   type(YamlParseState), intent(inout) :: PS
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'ParseFlow'
   character(1)              :: Open, Close, c
   logical                   :: IsMap, ExpectItem
   integer(IntKi)            :: iChild, iDup
   character(:), allocatable :: Token, Key
   integer(IntKi)            :: ErrStat2
   character(ErrMsgLen)      :: ErrMsg2

   ErrStat = ErrID_None
   ErrMsg  = ""

   Open  = Text(Pos:Pos)
   IsMap = (Open == '{')
   Close = ']'
   if (IsMap) Close = '}'
   if (IsMap) then
      Doc%Nodes(iNode)%Kind = YAML_MAP
   else
      Doc%Nodes(iNode)%Kind = YAML_SEQ
   end if
   Pos = Pos + 1
   ExpectItem = .false.        ! a leading close bracket (empty collection) is legal

   do
      call SkipSpaces(Text, Pos)
      if (Pos > len(Text)) then
         call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Unterminated flow collection '// &
            '(missing "'//Close//'").', ErrStat, ErrMsg, RoutineName)
         return
      end if

      c = Text(Pos:Pos)
      if (c == Close .and. .not. ExpectItem) then
         Pos = Pos + 1
         return
      end if

      ! --- one item ---
      iChild = AddNode(Doc, iNode)
      Doc%Nodes(iChild)%FileIndx = SL%FileIndx
      Doc%Nodes(iChild)%FileLine = SL%FileLine

      if (IsMap) then
         call FlowToken(Text, Pos, .true., Token)
         call Unquote(Token, SL, Doc%FileList, Key, ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
         if (ErrStat >= AbortErrLev) return
         call SkipSpaces(Text, Pos)
         if (Pos > len(Text) .or. Text(min(Pos,len(Text)):min(Pos,len(Text))) /= ':') then
            call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Expected "key: value" inside '// &
               'the flow mapping near "'//trim(Token)//'".', ErrStat, ErrMsg, RoutineName)
            return
         end if
         Pos = Pos + 1                              ! consume ':'
         if (Key == '<<') then
            call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' The "<<" merge key is not '// &
               'supported inside flow mappings; use a block mapping.', ErrStat, ErrMsg, RoutineName)
            return
         end if
         iDup = Yaml_ChildByKey(Doc, iNode, Key)
         if (iDup > 0 .and. iDup /= iChild) then
            call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Duplicate key "'//Key// &
               '" in flow mapping.', ErrStat, ErrMsg, RoutineName)
            return
         end if
         Doc%Nodes(iChild)%Key = Key
         call SkipSpaces(Text, Pos)
      end if

      if (Pos <= len(Text) .and. (Text(min(Pos,len(Text)):min(Pos,len(Text))) == '[' .or. &
                                  Text(min(Pos,len(Text)):min(Pos,len(Text))) == '{')) then
         call ParseFlow(Doc, iChild, Text, Pos, SL, PS, ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
         if (ErrStat >= AbortErrLev) return
      else
         call FlowToken(Text, Pos, .false., Token)
         if (len_trim(Token) == 0) then
            call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Empty item in flow collection.', &
               ErrStat, ErrMsg, RoutineName)
            return
         end if
         if (Token(1:1) == '*') then
            ! alias item: deep-copy the anchored subtree into this element
            iDup = ResolveAlias(PS, trim(Token(2:)))       ! iDup reused as source index
            if (iDup == 0) then
               call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Undefined anchor "'// &
                  trim(Token(2:))//'" in flow collection.', ErrStat, ErrMsg, RoutineName)
               return
            end if
            call CopyInto(Doc, iDup, iChild)
         else if (Token(1:1) == '&') then
            call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Anchors ("&") are not '// &
               'supported inside flow collections.', ErrStat, ErrMsg, RoutineName)
            return
         else
            call Unquote(trim(Token), SL, Doc%FileList, Key, ErrStat2, ErrMsg2)   ! Key reused as buffer
            call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
            if (ErrStat >= AbortErrLev) return
            Doc%Nodes(iChild)%Kind   = YAML_SCALAR
            Doc%Nodes(iChild)%Scalar = Key
         end if
      end if

      ! --- separator or close ---
      call SkipSpaces(Text, Pos)
      if (Pos > len(Text)) then
         call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Unterminated flow collection '// &
            '(missing "'//Close//'").', ErrStat, ErrMsg, RoutineName)
         return
      end if
      c = Text(Pos:Pos)
      if (c == ',') then
         Pos = Pos + 1
         ExpectItem = .true.
      else if (c == Close) then
         Pos = Pos + 1
         return
      else
         call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Expected "," or "'//Close// &
            '" in flow collection but found "'//c//'".', ErrStat, ErrMsg, RoutineName)
         return
      end if
   end do
end subroutine ParseFlow

!> Advance Pos past any spaces.
subroutine SkipSpaces(Text, Pos)
   character(*), intent(in   ) :: Text
   integer,      intent(inout) :: Pos
   do while (Pos <= len(Text))
      if (Text(Pos:Pos) /= ' ') return
      Pos = Pos + 1
   end do
end subroutine SkipSpaces

!> Extract one raw token from a flow context starting at Text(Pos:), stopping (outside
!! quotes) at ',', ']', '}' — and additionally at ':' when AtColon is true (flow-map
!! keys). The token keeps its quotes (Unquote resolves them); Pos lands on the stopper.
subroutine FlowToken(Text, Pos, AtColon, Token)
   character(*),              intent(in   ) :: Text
   integer,                   intent(inout) :: Pos
   logical,                   intent(in   ) :: AtColon
   character(:), allocatable, intent(  out) :: Token

   integer      :: Start
   character(1) :: c, Quote

   call SkipSpaces(Text, Pos)
   Start = Pos
   Quote = ' '
   do while (Pos <= len(Text))
      c = Text(Pos:Pos)
      if (Quote /= ' ') then
         if (c == Quote) then
            if (Quote == "'" .and. Pos < len(Text)) then
               if (Text(Pos+1:Pos+1) == "'") then
                  Pos = Pos + 1                     ! '' escape
               else
                  Quote = ' '
               end if
            else
               Quote = ' '
            end if
         else if (c == '\' .and. Quote == '"') then
            Pos = Pos + 1
         end if
      else
         select case (c)
         case ('"', "'")
            Quote = c
         case (',', ']', '}')
            exit
         case (':')
            if (AtColon) exit
         end select
      end if
      Pos = Pos + 1
   end do
   Token = trim(Text(Start:Pos-1))
end subroutine FlowToken

!> Resolve quoting on a scalar/key token: double quotes process \" and \\ escapes, single
!! quotes process the '' escape, anything else is taken verbatim (plain scalar).
subroutine Unquote(TextIn, SL, FileList, TextOut, ErrStat, ErrMsg)
   character(*),              intent(in   ) :: TextIn
   type(ScanLineType),        intent(in   ) :: SL
   character(*),              intent(in   ) :: FileList(:)
   character(:), allocatable, intent(  out) :: TextOut
   integer(IntKi),            intent(  out) :: ErrStat
   character(*),              intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'Unquote'
   character(len(TextIn))    :: Buf
   integer                   :: i, n
   character(1)              :: Quote

   ErrStat = ErrID_None
   ErrMsg  = ""

   if (len(TextIn) == 0) then
      TextOut = ''
      return
   end if

   Quote = TextIn(1:1)
   if (Quote /= '"' .and. Quote /= "'") then
      TextOut = trim(TextIn)                     ! plain scalar
      return
   end if

   if (len(TextIn) < 2 .or. TextIn(len(TextIn):len(TextIn)) /= Quote) then
      call SetErrStat(ErrID_Fatal, LineRef(SL, FileList)//' Unterminated '//Quote//'quoted'//Quote// &
         ' string: '//trim(TextIn), ErrStat, ErrMsg, RoutineName)
      return
   end if

   n = 0
   i = 2
   do while (i <= len(TextIn) - 1)
      if (Quote == '"' .and. TextIn(i:i) == '\' .and. i < len(TextIn) - 1) then
         if (TextIn(i+1:i+1) == '"' .or. TextIn(i+1:i+1) == '\') then
            i = i + 1   ! take the escaped character
         end if
      else if (Quote == "'" .and. TextIn(i:i) == "'" .and. i < len(TextIn) - 1) then
         if (TextIn(i+1:i+1) == "'") i = i + 1   ! '' collapses to '
      end if
      n = n + 1
      Buf(n:n) = TextIn(i:i)
      i = i + 1
   end do
   TextOut = Buf(1:n)
end subroutine Unquote

end module YamlInput