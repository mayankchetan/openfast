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
   use NWTC_IO, only: Conv2UC, GetPath, PathIsRelative, Num2LStr

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

   public :: IsYamlExt
   public :: Yaml_LoadFile
   public :: Yaml_LoadString
   public :: Yaml_NumChildren
   public :: Yaml_Child
   public :: Yaml_ChildByKey

   !> One significant source line after scanning: physical position plus decommented text
   !! with the indentation removed. Blank and comment-only lines never reach the parser.
   type :: ScanLineType
      integer                   :: Indent   = 0  !< number of leading spaces
      integer                   :: FileLine = 0  !< 1-based physical line number
      integer                   :: FileIndx = 1  !< index into YamlDoc%FileList
      character(:), allocatable :: Text          !< decommented content, indent stripped, right-trimmed
   end type ScanLineType

   character(*), parameter :: StringSourceName = '(string input)'

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

!> Parse a YAML file (and any !include'd files) into Doc. When UnEc > 0, each physical
!! file is echoed verbatim (comments included) under a banner naming the file.
subroutine Yaml_LoadFile(FileName, Doc, ErrStat, ErrMsg, UnEc)
   character(*),   intent(in   ) :: FileName
   type(YamlDoc),  intent(  out) :: Doc
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg
   integer(IntKi), intent(in   ), optional :: UnEc
   ErrStat = ErrID_Fatal
   ErrMsg  = 'Yaml_LoadFile: not implemented'
end subroutine Yaml_LoadFile

!> Parse YAML source given as an array of lines (no disk access; !include is not available
!! through this entry). Used by unit tests and passed-data couplings.
subroutine Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   character(*),   intent(in   ) :: Lines(:)
   type(YamlDoc),  intent(  out) :: Doc
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter          :: RoutineName = 'Yaml_LoadString'
   type(ScanLineType), allocatable  :: SL(:)
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
   call ParseBlockNode(SL, Cur, SL(1)%Indent, Doc, iRoot, ErrStat2, ErrMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   if (ErrStat >= AbortErrLev) return

   if (Cur <= size(SL)) then
      call SetErrStat(ErrID_Fatal, LineRef(SL(Cur), Doc%FileList)// &
         ' Unexpected content after the end of the top-level block.', ErrStat, ErrMsg, RoutineName)
   end if
end subroutine Yaml_LoadString

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
subroutine ScanSource(Lines, FileIndx, FileList, SL, ErrStat, ErrMsg)
   character(*),       intent(in   )              :: Lines(:)
   integer,            intent(in   )              :: FileIndx
   character(*),       intent(in   )              :: FileList(:)
   type(ScanLineType), allocatable, intent(  out) :: SL(:)
   integer(IntKi),     intent(  out)              :: ErrStat
   character(*),       intent(  out)              :: ErrMsg

   character(*), parameter   :: RoutineName = 'ScanSource'
   integer                   :: i, j, NSig, Indent, TextLen
   character(:), allocatable :: Text
   logical                   :: DocMarkerSeen

   ErrStat = ErrID_None
   ErrMsg  = ""
   DocMarkerSeen = .false.

   allocate(SL(size(Lines)))
   NSig = 0

   do i = 1, size(Lines)
      TextLen = len_trim(Lines(i))

      ! measure indentation; reject tabs within it
      Indent = 0
      do j = 1, TextLen
         if (Lines(i)(j:j) == ' ') then
            Indent = Indent + 1
         else if (Lines(i)(j:j) == char(9)) then
            call SetErrStat(ErrID_Fatal, '>> Error on line #'//trim(Num2LStr(i))//' of "'// &
               trim(FileList(FileIndx))//'": Tab character in indentation; YAML requires spaces.', &
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
            call SetErrStat(ErrID_Fatal, '>> Error on line #'//trim(Num2LStr(i))//' of "'// &
               trim(FileList(FileIndx))//'": Multi-document YAML streams are not supported by OpenFAST '// &
               '(only one leading "---" is allowed).', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if

      NSig = NSig + 1
      SL(NSig)%Indent   = Indent
      SL(NSig)%FileLine = i
      SL(NSig)%FileIndx = FileIndx
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
recursive subroutine ParseBlockNode(SL, Cur, Indent, Doc, iNode, ErrStat, ErrMsg)
   type(ScanLineType), intent(in   ) :: SL(:)
   integer,            intent(inout) :: Cur
   integer,            intent(in   ) :: Indent
   type(YamlDoc),      intent(inout) :: Doc
   integer(IntKi),     intent(in   ) :: iNode
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   if (IsSeqItem(SL(Cur)%Text)) then
      call ParseBlockSeq(SL, Cur, Indent, Doc, iNode, ErrStat, ErrMsg)
   else
      call ParseBlockMap(SL, Cur, Indent, Doc, iNode, ErrStat, ErrMsg)
   end if
end subroutine ParseBlockNode

!> True when a content line is a block-sequence item ("- item" or a bare "-").
logical function IsSeqItem(Text) result(IsItem)
   character(*), intent(in) :: Text
   IsItem = (Text == '-') .or. (len(Text) >= 2 .and. Text(1:2) == '- ')
end function IsSeqItem

!> Parse a block mapping whose keys sit at indentation Indent, adding entries under iNode.
recursive subroutine ParseBlockMap(SL, Cur, Indent, Doc, iNode, ErrStat, ErrMsg)
   type(ScanLineType), intent(in   ) :: SL(:)
   integer,            intent(inout) :: Cur
   integer,            intent(in   ) :: Indent
   type(YamlDoc),      intent(inout) :: Doc
   integer(IntKi),     intent(in   ) :: iNode
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'ParseBlockMap'
   character(:), allocatable :: Key, Value
   integer(IntKi)            :: iChild, iDup
   integer(IntKi)            :: ErrStat2
   character(ErrMsgLen)      :: ErrMsg2

   ErrStat = ErrID_None
   ErrMsg  = ""
   Doc%Nodes(iNode)%Kind = YAML_MAP

   do while (Cur <= size(SL))
      if (SL(Cur)%Indent < Indent) return         ! end of this block
      if (SL(Cur)%Indent > Indent) then
         call SetErrStat(ErrID_Fatal, LineRef(SL(Cur), Doc%FileList)//' Unexpected indentation '// &
            '(expected a key at column '//trim(Num2LStr(Indent+1))//').', ErrStat, ErrMsg, RoutineName)
         return
      end if
      if (IsSeqItem(SL(Cur)%Text)) return         ! sequence item belongs to the enclosing key

      call SplitKeyValue(SL(Cur), Doc%FileList, Key, Value, ErrStat2, ErrMsg2)
      call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      if (ErrStat >= AbortErrLev) return

      ! duplicate keys are ambiguous decks: hard error naming both definitions
      iDup = Yaml_ChildByKey(Doc, iNode, Key)
      if (iDup > 0) then
         call SetErrStat(ErrID_Fatal, '>> Duplicate key "'//Key//'" on line #'// &
            trim(Num2LStr(SL(Cur)%FileLine))//' of "'//trim(Doc%FileList(SL(Cur)%FileIndx))// &
            '"; first defined on line #'//trim(Num2LStr(Doc%Nodes(iDup)%FileLine))//'.', &
            ErrStat, ErrMsg, RoutineName)
         return
      end if

      iChild = AddNode(Doc, iNode)
      Doc%Nodes(iChild)%Key      = Key
      Doc%Nodes(iChild)%FileIndx = SL(Cur)%FileIndx
      Doc%Nodes(iChild)%FileLine = SL(Cur)%FileLine

      if (len(Value) > 0) then
         ! value on the same line: a scalar
         call SetScalar(Doc, iChild, Value, SL(Cur), ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
         if (ErrStat >= AbortErrLev) return
         Cur = Cur + 1
      else
         ! no value on this line: nested block (deeper, or a sequence at the same
         ! indentation), or an empty scalar
         Cur = Cur + 1
         if (NestedFollows(SL, Cur, Indent)) then
            call ParseBlockNode(SL, Cur, SL(Cur)%Indent, Doc, iChild, ErrStat2, ErrMsg2)
            call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
            if (ErrStat >= AbortErrLev) return
         else
            Doc%Nodes(iChild)%Kind   = YAML_SCALAR
            Doc%Nodes(iChild)%Scalar = ''
         end if
      end if
   end do
end subroutine ParseBlockMap

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

!> Parse a block sequence whose "-" markers sit at indentation Indent, adding items under iNode.
recursive subroutine ParseBlockSeq(SL, Cur, Indent, Doc, iNode, ErrStat, ErrMsg)
   type(ScanLineType), intent(in   ) :: SL(:)
   integer,            intent(inout) :: Cur
   integer,            intent(in   ) :: Indent
   type(YamlDoc),      intent(inout) :: Doc
   integer(IntKi),     intent(in   ) :: iNode
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter          :: RoutineName = 'ParseBlockSeq'
   type(ScanLineType), allocatable  :: Slice(:)
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

      ! Re-baseline the item: its inline text (after "- ") acts as a line at Indent+2,
      ! followed by every subsequent line deeper than Indent. Parsing the slice as its
      ! own block handles "- scalar", "- key: value" map items, and "-" + nested block.
      allocate(Slice(size(SL)))
      NSlice = 0
      if (len_trim(SL(Cur)%Text) > 1) then       ! "- something"
         NSlice = NSlice + 1
         Slice(NSlice)          = SL(Cur)
         Slice(NSlice)%Indent   = Indent + 2
         Slice(NSlice)%Text     = trim(adjustl(SL(Cur)%Text(2:)))
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
      else if (NSlice == 1 .and. ItemLine == Slice(1)%FileLine .and. FindColon(Slice(1)%Text) == 0) then
         call SetScalar(Doc, iChild, Slice(1)%Text, Slice(1), ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
         if (ErrStat >= AbortErrLev) return
      else
         SubCur = 1
         call ParseBlockNode(Slice(1:NSlice), SubCur, Slice(1)%Indent, Doc, iChild, ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
         if (ErrStat >= AbortErrLev) return
         if (SubCur <= NSlice) then
            call SetErrStat(ErrID_Fatal, LineRef(Slice(SubCur), Doc%FileList)// &
               ' Unexpected content within a sequence item.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if

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
recursive subroutine SetScalar(Doc, iNode, Text, SL, ErrStat, ErrMsg)
   type(YamlDoc),      intent(inout) :: Doc
   integer(IntKi),     intent(in   ) :: iNode
   character(*),       intent(in   ) :: Text
   type(ScanLineType), intent(in   ) :: SL
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'SetScalar'
   character(:), allocatable :: Unquoted
   integer                   :: Pos, TagEnd

   ErrStat = ErrID_None
   ErrMsg  = ""

   select case (Text(1:1))
   case ('[', '{')
      Pos = 1
      call ParseFlow(Doc, iNode, Text, Pos, SL, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
      if (len_trim(Text(Pos:)) > 0) then
         call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Unexpected content after '// &
            'the flow collection: "'//trim(Text(Pos:))//'".', ErrStat, ErrMsg, RoutineName)
      end if
   case ('|', '>')
      call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Block scalars ("|", ">") are '// &
         'not supported by OpenFAST; use a quoted string or a flow collection.', ErrStat, ErrMsg, RoutineName)
   case ('!')
      TagEnd = index(Text, ' ')
      if (TagEnd == 0) TagEnd = len(Text) + 1
      call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Unknown tag "'//Text(1:TagEnd-1)// &
         '". The only tag OpenFAST supports is !include (available in file-based input).', &
         ErrStat, ErrMsg, RoutineName)
   case default
      call Unquote(Text, SL, Doc%FileList, Unquoted, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
      Doc%Nodes(iNode)%Kind   = YAML_SCALAR
      Doc%Nodes(iNode)%Scalar = Unquoted
   end select
end subroutine SetScalar

!> Parse a flow collection ("[a, b]" / "{k: v}") starting at Text(Pos:Pos) into iNode.
!! On return Pos points just past the closing bracket. Nested flow collections recurse;
!! commas and colons inside quoted strings are honored.
recursive subroutine ParseFlow(Doc, iNode, Text, Pos, SL, ErrStat, ErrMsg)
   type(YamlDoc),      intent(inout) :: Doc
   integer(IntKi),     intent(in   ) :: iNode
   character(*),       intent(in   ) :: Text
   integer,            intent(inout) :: Pos
   type(ScanLineType), intent(in   ) :: SL
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

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
         call ParseFlow(Doc, iChild, Text, Pos, SL, ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
         if (ErrStat >= AbortErrLev) return
      else
         call FlowToken(Text, Pos, .false., Token)
         if (len_trim(Token) == 0) then
            call SetErrStat(ErrID_Fatal, LineRef(SL, Doc%FileList)//' Empty item in flow collection.', &
               ErrStat, ErrMsg, RoutineName)
            return
         end if
         call Unquote(trim(Token), SL, Doc%FileList, Key, ErrStat2, ErrMsg2)   ! Key reused as buffer
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
         if (ErrStat >= AbortErrLev) return
         Doc%Nodes(iChild)%Kind   = YAML_SCALAR
         Doc%Nodes(iChild)%Scalar = Key
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