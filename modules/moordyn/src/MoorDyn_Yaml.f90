!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of MoorDyn.
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
!> Reader for the YAML form of the MoorDyn primary input file.
!!
!! MoorDyn is unlike the other converted modules: its text parser (the NextLine walker
!! inside MD_Init, MoorDyn.f90) does not fill an intermediate input-file structure -- it
!! parses free-form section tables and builds the mooring-system objects (bodies, rods,
!! points, lines, state indexing) in the same loop. Duplicating that walker for YAML
!! would mean duplicating the model-building logic itself, so this reader instead parses
!! the YAML schema and MATERIALIZES the equivalent canonical text deck as a FileInfoType,
!! which MD_Init's unchanged walker then consumes. Every cell value is carried as the raw
!! scalar text from the YAML file and emitted verbatim into the materialized row, so the
!! walker's list-directed READs parse exactly the same strings the text path would --
!! semantic and bit-level parity by construction, with all cross-referencing, defaulting,
!! keyword handling (attachment types, "SEABED" depth, bar-separated multi-values,
!! SYROPE:/lookup-table filenames, option-name aliases, unknown-option warnings) done by
!! the one true parser.
!!
!! Schema: top-level keys mirror the text file's section headers -- line_types,
!! rod_types, bodies, rods, points, lines, syrope_ic, external_loads, control, failure
!! (each a list of row mappings keyed by the text columns), options (a mapping of
!! option keyword -> value, keyword spelling/aliases exactly as the text format,
!! emitted in document order), and outputs:OutList (a list of channel names). Every
!! section is optional at the YAML level; MD_Init applies its own requirements (e.g.
!! at least one line type and one line) identically for both formats.
!!
!! Table row counts (nLineTypes, nBodies, nRods, nPoints, nLines, nCtrlChans, nFails,
!! nSyropeLineICs, nExtLds) derive from list lengths; they are never YAML keys.
!!
!! line_types rows take the walker's three arities: the 10 base columns, optionally
!! +Cl (the 11-column VIV form, which leaves dF/cF at the walker's Thorsen defaults
!! 0.08/0.18), optionally +dF+cF (the 13-column form; dF and cF must be given together
!! and require Cl). Bar-separated multi-value cells (EA, BA, bodies' CG/I/CdA/Ca,
!! external_loads' Fext/Blin/Bquad) and keyword cells (attachments like "Body1Pinned",
!! points' Z "seabed", EA's "SYROPE:<file>" or lookup-table filenames, output flag
!! character sets) are carried as single strings, exactly the text tokens.
!!
!! Sub-files stay path strings resolved by MoorDyn itself relative to PriPath: the
!! WtrDpth option's bathymetry grid file, the WaterKin file, EA/BA/EI lookup tables,
!! and SYROPE working-curve files.
!!
!! Because a materialized cell becomes one whitespace-delimited token in a table row,
!! a value must not contain any of the walker's word separators (blanks, tabs, commas,
!! semicolons, quotes) or a "---" run (the walker's section-header sentinel); such
!! values are impossible in the text format too, and are rejected here with a clear
!! message instead of a confusing downstream column-count error.
module MoorDyn_Yaml

   use NWTC_Library
   use YamlInput

   implicit none
   private

   public :: MD_ParseYamlFile
   public :: MD_ParseYamlFileInfo

   !> maximum length of one materialized deck line (also FileInfoType's own limit)
   integer(IntKi), parameter :: DeckLineLen = MaxFileInfoLineLen
   !> maximum length of one table cell / option value carried verbatim
   integer(IntKi), parameter :: CellLen = 1024

   !> growable buffer of materialized text-deck lines
   type :: DeckBuf
      character(DeckLineLen), allocatable :: Lines(:)
      integer(IntKi)                      :: N = 0
   end type DeckBuf

contains

!> Load a YAML-format MoorDyn primary input file and materialize the equivalent
!! canonical text deck into FileInfo (the same structure ProcessComFile produces on the
!! text path); MD_Init's walker consumes it unchanged.
subroutine MD_ParseYamlFile(InputFileName, FileInfo, ErrStat, ErrMsg)
   character(*),       intent(in   ) :: InputFileName !< the .yaml primary input file
   type(FileInfoType), intent(  out) :: FileInfo      !< materialized text-format deck
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'MD_ParseYamlFile'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call BuildDeckFromDoc(Doc, FileInfo, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! typo guard: anything never looked up is reported (warning severity)
   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine MD_ParseYamlFile

!> Parse YAML-format MoorDyn input arriving as a FileInfoType -- the passed-data channel
!! used for inline module input from a YAML primary file (the glue code's inline
!! MooringFile handover when CompMooring selects MoorDyn) -- and materialize the
!! equivalent canonical text deck for MD_Init's walker.
subroutine MD_ParseYamlFileInfo(FileInfoIn, FileInfo, ErrStat, ErrMsg)
   type(FileInfoType), intent(in   ) :: FileInfoIn !< YAML text lines with provenance
   type(FileInfoType), intent(  out) :: FileInfo   !< materialized text-format deck
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'MD_ParseYamlFileInfo'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call Yaml_LoadFileInfo(FileInfoIn, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call BuildDeckFromDoc(Doc, FileInfo, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine MD_ParseYamlFileInfo

!> Materialize the canonical text deck from a parsed document. Sections are emitted in
!! a fixed order that satisfies the walker's cross-reference needs (types before the
!! objects naming them; lines before syrope_ic/control/failure; bodies/rods/points
!! before external_loads/failure). OPTIONS placement is immaterial: the walker parses
!! options in its first (counting) pass, before any object is built.
subroutine BuildDeckFromDoc(Doc, FileInfo, ErrStat, ErrMsg)
   type(YamlDoc),      intent(inout) :: Doc
   type(FileInfoType), intent(  out) :: FileInfo
   integer(IntKi),     intent(  out) :: ErrStat
   character(*),       intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'MD_BuildDeckFromDoc'
   type(DeckBuf)           :: Buf
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call EmitLineTypes(Doc, Buf, TmpErrStat, TmpErrMsg);  if (Failed()) return
   call EmitRodTypes(Doc, Buf, TmpErrStat, TmpErrMsg);   if (Failed()) return
   call EmitBodies(Doc, Buf, TmpErrStat, TmpErrMsg);     if (Failed()) return
   call EmitRods(Doc, Buf, TmpErrStat, TmpErrMsg);       if (Failed()) return
   call EmitPoints(Doc, Buf, TmpErrStat, TmpErrMsg);     if (Failed()) return
   call EmitLines(Doc, Buf, TmpErrStat, TmpErrMsg);      if (Failed()) return
   call EmitSyropeIC(Doc, Buf, TmpErrStat, TmpErrMsg);   if (Failed()) return
   call EmitExtLoads(Doc, Buf, TmpErrStat, TmpErrMsg);   if (Failed()) return
   call EmitControl(Doc, Buf, TmpErrStat, TmpErrMsg);    if (Failed()) return
   call EmitFailure(Doc, Buf, TmpErrStat, TmpErrMsg);    if (Failed()) return
   call EmitOptions(Doc, Buf, TmpErrStat, TmpErrMsg);    if (Failed()) return
   call EmitOutputs(Doc, Buf, TmpErrStat, TmpErrMsg);    if (Failed()) return

   if (Buf%N < 1) then
      call SetErrStat(ErrID_Fatal, 'The YAML MoorDyn input contains no recognized sections '// &
         '(expected keys like "line_types", "points", "lines", "options", "outputs").', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if

   call InitFileInfo(Buf%Lines(1:Buf%N), FileInfo, TmpErrStat, TmpErrMsg)
   if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine BuildDeckFromDoc

!----------------------------------------------------------------------------------------
! deck-buffer helpers
!----------------------------------------------------------------------------------------

!> Append one line to the materialized deck, growing the buffer as needed.
subroutine AddLine(Buf, Text, ErrStat, ErrMsg)
   type(DeckBuf),  intent(inout) :: Buf
   character(*),   intent(in   ) :: Text
   integer(IntKi), intent(inout) :: ErrStat
   character(*),   intent(inout) :: ErrMsg

   character(DeckLineLen), allocatable :: TmpLines(:)
   integer(IntKi)                      :: NewCap

   if (len_trim(Text) > DeckLineLen) then
      call SetErrStat(ErrID_Fatal, 'Materialized MoorDyn input line exceeds '// &
         trim(Num2LStr(DeckLineLen))//' characters.', ErrStat, ErrMsg, 'MD_Yaml_AddLine')
      return
   end if

   if (.not. allocated(Buf%Lines)) then
      allocate(Buf%Lines(64))
   else if (Buf%N >= size(Buf%Lines)) then
      NewCap = 2*size(Buf%Lines)
      allocate(TmpLines(NewCap))
      TmpLines(1:Buf%N) = Buf%Lines(1:Buf%N)
      call move_alloc(TmpLines, Buf%Lines)
   end if

   Buf%N = Buf%N + 1
   Buf%Lines(Buf%N) = Text
end subroutine AddLine

!> Validate that a cell value can survive the round trip through a whitespace-delimited
!! table row: non-empty, no word separators (blank, tab, comma, semicolon, quotes -- the
!! separator set of NWTC_IO's CountWords/GetWords and of list-directed READ), and no
!! "---" run (the walker's section-header sentinel).
logical function BadCell(Text, Path, RowNum, ErrStat, ErrMsg)
   character(*),   intent(in   ) :: Text
   character(*),   intent(in   ) :: Path   !< schema location for the message
   integer(IntKi), intent(in   ) :: RowNum !< 1-based row number (0 = not a table row)
   integer(IntKi), intent(inout) :: ErrStat
   character(*),   intent(inout) :: ErrMsg

   character(*), parameter :: Seps = ' ,;''"'//char(9)
   character(64)           :: Where

   if (RowNum > 0) then
      Where = '"'//trim(Path)//'" entry '//trim(Num2LStr(RowNum))
   else
      Where = '"'//trim(Path)//'"'
   end if

   BadCell = .true.
   if (len_trim(Text) == 0) then
      call SetErrStat(ErrID_Fatal, trim(Where)//' has an empty value.', ErrStat, ErrMsg, 'MD_Yaml_BadCell')
   else if (scan(trim(Text), Seps) > 0) then
      call SetErrStat(ErrID_Fatal, trim(Where)//' value "'//trim(Text)//'" contains a blank, tab, comma, '// &
         'semicolon, or quote; such values cannot be represented in MoorDyn''s table format.', &
         ErrStat, ErrMsg, 'MD_Yaml_BadCell')
   else if (index(Text, '---') > 0) then
      call SetErrStat(ErrID_Fatal, trim(Where)//' value "'//trim(Text)//'" contains "---", which MoorDyn '// &
         'reserves for section headers.', ErrStat, ErrMsg, 'MD_Yaml_BadCell')
   else
      BadCell = .false.
   end if
end function BadCell

!> Fetch one required cell of a table row as raw text (verbatim), trimmed of enclosing
!! whitespace, and validate it as a single table token.
subroutine GetCell(Doc, iRow, Key, Path, RowNum, Cell, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   integer(IntKi), intent(in   ) :: iRow
   character(*),   intent(in   ) :: Key
   character(*),   intent(in   ) :: Path
   integer(IntKi), intent(in   ) :: RowNum
   character(*),   intent(  out) :: Cell
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   Cell    = ""

   call YamlGet(Doc, Key, Cell, ErrStat, ErrMsg, From=iRow)
   if (ErrStat >= AbortErrLev) then
      ErrMsg = trim(ErrMsg)//' ["'//trim(Path)//'" entry '//trim(Num2LStr(RowNum))//']'
      return
   end if
   Cell = adjustl(Cell)
   if (BadCell(Cell, trim(Path)//':'//Key, RowNum, ErrStat, ErrMsg)) return
end subroutine GetCell

!> Join a list of integers as a comma-separated token run ("3,4,7") -- the exact shape
!! the walker's comma-counting variable-arity rows (syrope_ic / control / failure)
!! require: the walker derives the ID count from the number of commas on the row.
subroutine JoinIDs(IDs, Path, RowNum, Text, ErrStat, ErrMsg)
   integer(IntKi), intent(in   ) :: IDs(:)
   character(*),   intent(in   ) :: Path
   integer(IntKi), intent(in   ) :: RowNum
   character(*),   intent(  out) :: Text
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   integer(IntKi) :: k

   ErrStat = ErrID_None
   ErrMsg  = ""
   Text    = ""

   if (size(IDs) < 1) then
      call SetErrStat(ErrID_Fatal, '"'//trim(Path)//'" entry '//trim(Num2LStr(RowNum))// &
         ': "Lines" must list at least one line ID.', ErrStat, ErrMsg, 'MD_Yaml_JoinIDs')
      return
   end if

   Text = trim(Num2LStr(IDs(1)))
   do k = 2, size(IDs)
      Text = trim(Text)//','//trim(Num2LStr(IDs(k)))
   end do
end subroutine JoinIDs

!----------------------------------------------------------------------------------------
! section emitters
!----------------------------------------------------------------------------------------

!> line_types -> "--- LINE TYPES ---" table. 10 base columns (Name Diam MassDen EA BA EI
!! Cd Ca CdAx CaAx), 11 with Cl (VIV; walker then defaults dF=0.08/cF=0.18), 13 with
!! Cl+dF+cF. The walker accepts exactly 10, 11, or 13 words, so dF/cF must be given
!! together and only alongside Cl.
subroutine EmitLineTypes(Doc, Buf, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   type(DeckBuf),  intent(inout) :: Buf
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'MD_EmitLineTypes'
   character(*), parameter :: SecKey = 'line_types'
   character(CellLen)      :: Cells(10), ClText, DFText, CFText
   character(DeckLineLen)  :: Line
   logical                 :: SecFound, HasCl, HasDF, HasCF
   integer(IntKi)          :: iSeq, iRow, i, k, NRows
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg
   character(8), parameter :: ColKeys(10) = [character(8) :: &
      'Name', 'Diam', 'MassDen', 'EA', 'BA', 'EI', 'Cd', 'Ca', 'CdAx', 'CaAx']

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, SecKey, iSeq, TmpErrStat, TmpErrMsg, Found=SecFound)
   if (Failed()) return
   if (.not. SecFound) return

   call AddLine(Buf, '---------------------- LINE TYPES ----------------------', ErrStat, ErrMsg)
   call AddLine(Buf, 'TypeName   Diam    Mass/m     EA     BA/-zeta    EI     Cd    Ca   CdAx  CaAx', ErrStat, ErrMsg)
   call AddLine(Buf, '(name)     (m)     (kg/m)     (N)    (N-s/-)   (N-m^2)  (-)   (-)   (-)   (-)', ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return

   NRows = int(Yaml_NumChildren(Doc, iSeq))
   do i = 1, NRows
      iRow = Yaml_Child(Doc, iSeq, i)
      do k = 1, size(ColKeys)
         call GetCell(Doc, iRow, trim(ColKeys(k)), SecKey, i, Cells(k), TmpErrStat, TmpErrMsg)
         if (Failed()) return
      end do

      call YamlGet(Doc, 'Cl', ClText, TmpErrStat, TmpErrMsg, Found=HasCl, From=iRow)
      if (Failed()) return
      call YamlGet(Doc, 'dF', DFText, TmpErrStat, TmpErrMsg, Found=HasDF, From=iRow)
      if (Failed()) return
      call YamlGet(Doc, 'cF', CFText, TmpErrStat, TmpErrMsg, Found=HasCF, From=iRow)
      if (Failed()) return

      if (HasDF .neqv. HasCF) then
         call SetErrStat(ErrID_Fatal, '"'//SecKey//'" entry '//trim(Num2LStr(i))// &
            ': "dF" and "cF" must be given together (the 13-column line-type form).', &
            ErrStat, ErrMsg, RoutineName)
         return
      end if
      if (HasDF .and. .not. HasCl) then
         call SetErrStat(ErrID_Fatal, '"'//SecKey//'" entry '//trim(Num2LStr(i))// &
            ': "dF"/"cF" require "Cl" (MoorDyn has no 12-column line-type form).', &
            ErrStat, ErrMsg, RoutineName)
         return
      end if

      Line = trim(Cells(1))
      do k = 2, size(ColKeys)
         Line = trim(Line)//'  '//trim(Cells(k))
      end do
      if (HasCl) then
         ClText = adjustl(ClText)
         if (BadCell(ClText, SecKey//':Cl', i, ErrStat, ErrMsg)) return
         Line = trim(Line)//'  '//trim(ClText)
      end if
      if (HasDF) then
         DFText = adjustl(DFText)
         CFText = adjustl(CFText)
         if (BadCell(DFText, SecKey//':dF', i, ErrStat, ErrMsg)) return
         if (BadCell(CFText, SecKey//':cF', i, ErrStat, ErrMsg)) return
         Line = trim(Line)//'  '//trim(DFText)//'  '//trim(CFText)
      end if

      call AddLine(Buf, Line, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine EmitLineTypes

!> rod_types -> "--- ROD TYPES ---" table (Name Diam MassDen Cd Ca CdEnd CaEnd).
subroutine EmitRodTypes(Doc, Buf, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   type(DeckBuf),  intent(inout) :: Buf
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'MD_EmitRodTypes'
   character(*), parameter :: SecKey = 'rod_types'
   character(8), parameter :: ColKeys(7) = [character(8) :: &
      'Name', 'Diam', 'MassDen', 'Cd', 'Ca', 'CdEnd', 'CaEnd']

   call EmitSimpleTable(Doc, Buf, SecKey, ColKeys, &
      '---------------------- ROD TYPES ----------------------', &
      'TypeName   Diam    Mass/m    Cd     Ca     CdEnd    CaEnd', &
      '(name)     (m)     (kg/m)    (-)    (-)    (-)      (-)', &
      RoutineName, ErrStat, ErrMsg)
end subroutine EmitRodTypes

!> bodies -> "--- BODIES ---" table (ID Attachment X0 Y0 Z0 r0 p0 y0 M CG I V CdA Ca;
!! CG/I/CdA/Ca are carried whole, including any bar-separated multi-values).
subroutine EmitBodies(Doc, Buf, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   type(DeckBuf),  intent(inout) :: Buf
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter  :: RoutineName = 'MD_EmitBodies'
   character(*), parameter  :: SecKey = 'bodies'
   character(10), parameter :: ColKeys(14) = [character(10) :: &
      'ID', 'Attachment', 'X0', 'Y0', 'Z0', 'r0', 'p0', 'y0', 'M', 'CG', 'I', 'V', 'CdA', 'Ca']

   call EmitSimpleTable(Doc, Buf, SecKey, ColKeys, &
      '---------------------- BODIES ----------------------', &
      'ID   Attachment  X0   Y0   Z0   r0   p0   y0   Mass  CG*  I*  Volume  CdA*  Ca*', &
      '(#)  (word/ID)   (m)  (m)  (m)  (deg) (deg) (deg) (kg) (m) (kg-m^2) (m^3) (m^2) (-)', &
      RoutineName, ErrStat, ErrMsg)
end subroutine EmitBodies

!> rods -> "--- RODS ---" table (ID RodType Attachment Xa Ya Za Xb Yb Zb NumSegs
!! Outputs; Outputs is the per-rod output flag character set, e.g. "p" or "-").
subroutine EmitRods(Doc, Buf, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   type(DeckBuf),  intent(inout) :: Buf
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter  :: RoutineName = 'MD_EmitRods'
   character(*), parameter  :: SecKey = 'rods'
   character(10), parameter :: ColKeys(11) = [character(10) :: &
      'ID', 'RodType', 'Attachment', 'Xa', 'Ya', 'Za', 'Xb', 'Yb', 'Zb', 'NumSegs', 'Outputs']

   call EmitSimpleTable(Doc, Buf, SecKey, ColKeys, &
      '---------------------- RODS ----------------------', &
      'ID   RodType  Attachment  Xa    Ya    Za    Xb    Yb    Zb   NumSegs  RodOutputs', &
      '(#)  (name)   (word/ID)   (m)   (m)   (m)   (m)   (m)   (m)  (-)      (-)', &
      RoutineName, ErrStat, ErrMsg)
end subroutine EmitRods

!> points -> "--- POINTS ---" table (ID Attachment X Y Z M V CdA Ca; Z may be a scalar
!! depth or the "seabed"/"ground"/"floor" keyword resolved against the bathymetry).
subroutine EmitPoints(Doc, Buf, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   type(DeckBuf),  intent(inout) :: Buf
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter  :: RoutineName = 'MD_EmitPoints'
   character(*), parameter  :: SecKey = 'points'
   character(10), parameter :: ColKeys(9) = [character(10) :: &
      'ID', 'Attachment', 'X', 'Y', 'Z', 'M', 'V', 'CdA', 'Ca']

   call EmitSimpleTable(Doc, Buf, SecKey, ColKeys, &
      '---------------------- POINTS ----------------------', &
      'ID   Attachment  X     Y     Z     M     V     CdA   Ca', &
      '(#)  (word/ID)   (m)   (m)   (m)   (kg)  (m^3) (m^2) (-)', &
      RoutineName, ErrStat, ErrMsg)
end subroutine EmitPoints

!> lines -> "--- LINES ---" table (ID LineType AttachA AttachB UnstrLen NumSegs Outputs).
subroutine EmitLines(Doc, Buf, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   type(DeckBuf),  intent(inout) :: Buf
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter  :: RoutineName = 'MD_EmitLines'
   character(*), parameter  :: SecKey = 'lines'
   character(10), parameter :: ColKeys(7) = [character(10) :: &
      'ID', 'LineType', 'AttachA', 'AttachB', 'UnstrLen', 'NumSegs', 'Outputs']

   call EmitSimpleTable(Doc, Buf, SecKey, ColKeys, &
      '---------------------- LINES ----------------------', &
      'ID   LineType  AttachA  AttachB  UnstrLen  NumSegs  LineOutputs', &
      '(#)  (name)    (ID)     (ID)     (m)       (-)      (-)', &
      RoutineName, ErrStat, ErrMsg)
end subroutine EmitLines

!> Shared fixed-column table emitter: header + two label lines (the walker skips both
!! blindly), then one whitespace-joined row per list entry, every cell verbatim.
subroutine EmitSimpleTable(Doc, Buf, SecKey, ColKeys, Header, LabelLine, UnitsLine, RoutineName, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   type(DeckBuf),  intent(inout) :: Buf
   character(*),   intent(in   ) :: SecKey
   character(*),   intent(in   ) :: ColKeys(:)
   character(*),   intent(in   ) :: Header
   character(*),   intent(in   ) :: LabelLine
   character(*),   intent(in   ) :: UnitsLine
   character(*),   intent(in   ) :: RoutineName
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(CellLen)     :: Cell
   character(DeckLineLen) :: Line
   logical                :: SecFound
   integer(IntKi)         :: iSeq, iRow, i, k, NRows
   integer(IntKi)         :: TmpErrStat
   character(ErrMsgLen)   :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, SecKey, iSeq, TmpErrStat, TmpErrMsg, Found=SecFound)
   if (Failed()) return
   if (.not. SecFound) return

   call AddLine(Buf, Header, ErrStat, ErrMsg)
   call AddLine(Buf, LabelLine, ErrStat, ErrMsg)
   call AddLine(Buf, UnitsLine, ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return

   NRows = int(Yaml_NumChildren(Doc, iSeq))
   do i = 1, NRows
      iRow = Yaml_Child(Doc, iSeq, i)
      Line = ""
      do k = 1, size(ColKeys)
         call GetCell(Doc, iRow, trim(ColKeys(k)), SecKey, i, Cell, TmpErrStat, TmpErrMsg)
         if (Failed()) return
         if (k == 1) then
            Line = trim(Cell)
         else
            Line = trim(Line)//'  '//trim(Cell)
         end if
      end do
      call AddLine(Buf, Line, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine EmitSimpleTable

!> syrope_ic -> "--- SYROPE IC ---" table. Each row is {Lines: [line IDs], Tmax, Tmean};
!! the materialized row is "id1,...,idN Tmax Tmean" -- the walker counts the commas to
!! learn N, then READs N IDs plus the two trailing scalars.
subroutine EmitSyropeIC(Doc, Buf, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   type(DeckBuf),  intent(inout) :: Buf
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'MD_EmitSyropeIC'
   character(*), parameter :: SecKey = 'syrope_ic'
   integer(IntKi), allocatable :: IDs(:)
   character(CellLen)      :: TmaxText, TmeanText
   character(DeckLineLen)  :: IDText, Line
   logical                 :: SecFound
   integer(IntKi)          :: iSeq, iRow, i, NRows
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, SecKey, iSeq, TmpErrStat, TmpErrMsg, Found=SecFound)
   if (Failed()) return
   if (.not. SecFound) return

   call AddLine(Buf, '---------------------- SYROPE IC ----------------------', ErrStat, ErrMsg)
   call AddLine(Buf, 'Line(s)   Tmax    Tmean', ErrStat, ErrMsg)
   call AddLine(Buf, '(,)       (N)     (N)', ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return

   NRows = int(Yaml_NumChildren(Doc, iSeq))
   do i = 1, NRows
      iRow = Yaml_Child(Doc, iSeq, i)
      call YamlGet(Doc, 'Lines', IDs, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      call JoinIDs(IDs, SecKey, i, IDText, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
      call GetCell(Doc, iRow, 'Tmax', SecKey, i, TmaxText, TmpErrStat, TmpErrMsg)
      if (Failed()) return
      call GetCell(Doc, iRow, 'Tmean', SecKey, i, TmeanText, TmpErrStat, TmpErrMsg)
      if (Failed()) return

      Line = trim(IDText)//'  '//trim(TmaxText)//'  '//trim(TmeanText)
      call AddLine(Buf, Line, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine EmitSyropeIC

!> external_loads -> "--- EXTERNAL LOADS ---" table (ID Object Fext Blin Bquad CSys;
!! Fext/Blin/Bquad are carried whole, including bar-separated multi-values; CSys is the
!! walker's G/L/- coordinate-system token).
subroutine EmitExtLoads(Doc, Buf, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   type(DeckBuf),  intent(inout) :: Buf
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'MD_EmitExtLoads'
   character(*), parameter :: SecKey = 'external_loads'
   character(8), parameter :: ColKeys(6) = [character(8) :: &
      'ID', 'Object', 'Fext', 'Blin', 'Bquad', 'CSys']

   call EmitSimpleTable(Doc, Buf, SecKey, ColKeys, &
      '---------------------- EXTERNAL LOADS ----------------------', &
      'ID   Object   Fext    Blin     Bquad     CSys', &
      '(#)  (word)   (N)     (Ns/m)   (Ns^2/m^2) (-)', &
      RoutineName, ErrStat, ErrMsg)
end subroutine EmitExtLoads

!> control -> "--- CONTROL ---" table. Each row is {ChannelID, Lines: [line IDs]}; the
!! materialized row is "ChannelID id1,...,idN" (channel first, then the comma-joined ID
!! run the walker counts).
subroutine EmitControl(Doc, Buf, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   type(DeckBuf),  intent(inout) :: Buf
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'MD_EmitControl'
   character(*), parameter :: SecKey = 'control'
   integer(IntKi), allocatable :: IDs(:)
   character(CellLen)      :: ChanText
   character(DeckLineLen)  :: IDText, Line
   logical                 :: SecFound
   integer(IntKi)          :: iSeq, iRow, i, NRows
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, SecKey, iSeq, TmpErrStat, TmpErrMsg, Found=SecFound)
   if (Failed()) return
   if (.not. SecFound) return

   call AddLine(Buf, '---------------------- CONTROL ----------------------', ErrStat, ErrMsg)
   call AddLine(Buf, 'ChannelID   Line(s)', ErrStat, ErrMsg)
   call AddLine(Buf, '()          (,)', ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return

   NRows = int(Yaml_NumChildren(Doc, iSeq))
   do i = 1, NRows
      iRow = Yaml_Child(Doc, iSeq, i)
      call GetCell(Doc, iRow, 'ChannelID', SecKey, i, ChanText, TmpErrStat, TmpErrMsg)
      if (Failed()) return
      call YamlGet(Doc, 'Lines', IDs, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      call JoinIDs(IDs, SecKey, i, IDText, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return

      Line = trim(ChanText)//'  '//trim(IDText)
      call AddLine(Buf, Line, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine EmitControl

!> failure -> "--- FAILURE ---" table. Each row is {ID, Attachment, Lines: [line IDs],
!! FailTime, FailTen}; the materialized row is "ID Attachment id1,...,idN FailTime
!! FailTen" (the walker counts the commas in the ID run).
subroutine EmitFailure(Doc, Buf, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   type(DeckBuf),  intent(inout) :: Buf
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'MD_EmitFailure'
   character(*), parameter :: SecKey = 'failure'
   integer(IntKi), allocatable :: IDs(:)
   character(CellLen)      :: IDCell, AttachText, TimeText, TenText
   character(DeckLineLen)  :: IDText, Line
   logical                 :: SecFound
   integer(IntKi)          :: iSeq, iRow, i, NRows
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, SecKey, iSeq, TmpErrStat, TmpErrMsg, Found=SecFound)
   if (Failed()) return
   if (.not. SecFound) return

   call AddLine(Buf, '---------------------- FAILURE ----------------------', ErrStat, ErrMsg)
   call AddLine(Buf, 'FailureID   Point     Line(s)   FailTime   FailTen', ErrStat, ErrMsg)
   call AddLine(Buf, '()          (word/ID) (,)       (s or 0)   (N or 0)', ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return

   NRows = int(Yaml_NumChildren(Doc, iSeq))
   do i = 1, NRows
      iRow = Yaml_Child(Doc, iSeq, i)
      call GetCell(Doc, iRow, 'ID', SecKey, i, IDCell, TmpErrStat, TmpErrMsg)
      if (Failed()) return
      call GetCell(Doc, iRow, 'Attachment', SecKey, i, AttachText, TmpErrStat, TmpErrMsg)
      if (Failed()) return
      call YamlGet(Doc, 'Lines', IDs, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      call JoinIDs(IDs, SecKey, i, IDText, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
      call GetCell(Doc, iRow, 'FailTime', SecKey, i, TimeText, TmpErrStat, TmpErrMsg)
      if (Failed()) return
      call GetCell(Doc, iRow, 'FailTen', SecKey, i, TenText, TmpErrStat, TmpErrMsg)
      if (Failed()) return

      Line = trim(IDCell)//'  '//trim(AttachText)//'  '//trim(IDText)//'  '// &
             trim(TimeText)//'  '//trim(TenText)
      call AddLine(Buf, Line, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine EmitFailure

!> options -> "--- OPTIONS ---" keyword-value lines ("<value> <keyword>"), one per
!! mapping entry, in document order (the text format is order-sensitive only for which
!! options land in the log file after writeLog opens it). Keyword spellings -- including
!! the walker's aliases (rhoW/rho, WtrDpth/depth/WaterDepth, kBot/kb, cBot/cb,
!! cv/fricDamp) -- pass through verbatim; unknown keywords get the walker's own
!! non-fatal warning, exactly as in the text format.
subroutine EmitOptions(Doc, Buf, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   type(DeckBuf),  intent(inout) :: Buf
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'MD_EmitOptions'
   character(*), parameter :: SecKey = 'options'
   character(CellLen)      :: KeyText, ValText
   character(DeckLineLen)  :: Line
   logical                 :: SecFound
   integer(IntKi)          :: iMap, iOpt, i, NOpts
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, SecKey, iMap, TmpErrStat, TmpErrMsg, Found=SecFound)
   if (Failed()) return
   if (.not. SecFound) return

   if (Doc%Nodes(iMap)%Kind /= YAML_MAP) then
      call SetErrStat(ErrID_Fatal, '"'//SecKey//'" must be a mapping of option keyword to value.', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if

   call AddLine(Buf, '---------------------- OPTIONS ----------------------', ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return

   NOpts = int(Yaml_NumChildren(Doc, iMap))
   do i = 1, NOpts
      iOpt = Yaml_Child(Doc, iMap, i)
      if (.not. allocated(Doc%Nodes(iOpt)%Key)) then
         call SetErrStat(ErrID_Fatal, '"'//SecKey//'" entry '//trim(Num2LStr(i))// &
            ' has no keyword.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      KeyText = Doc%Nodes(iOpt)%Key
      if (BadCell(KeyText, SecKey, i, ErrStat, ErrMsg)) return

      ! fetch by key (verbatim raw text) so the lookup also marks the node used
      call YamlGet(Doc, trim(KeyText), ValText, TmpErrStat, TmpErrMsg, From=iMap)
      if (Failed()) return
      ValText = adjustl(ValText)
      if (BadCell(ValText, SecKey//':'//trim(KeyText), 0, ErrStat, ErrMsg)) return

      Line = trim(ValText)//'  '//trim(KeyText)
      call AddLine(Buf, Line, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine EmitOptions

!> outputs:OutList -> "--- OUTPUTS ---" channel-name lines, one channel per line,
!! terminated by "END" (the walker gathers whitespace-separated names line by line
!! until END or the next section header).
subroutine EmitOutputs(Doc, Buf, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   type(DeckBuf),  intent(inout) :: Buf
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'MD_EmitOutputs'
   character(:), allocatable :: TmpList(:)
   logical                   :: SecFound
   integer(IntKi)            :: i
   integer(IntKi)            :: TmpErrStat
   character(ErrMsgLen)      :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'outputs:OutList', TmpList, TmpErrStat, TmpErrMsg, Found=SecFound)
   if (Failed()) return
   if (.not. SecFound) return

   call AddLine(Buf, '---------------------- OUTPUTS ----------------------', ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return

   do i = 1, size(TmpList)
      if (BadCell(TmpList(i), 'outputs:OutList', i, ErrStat, ErrMsg)) return
      call AddLine(Buf, trim(adjustl(TmpList(i))), ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
   end do

   call AddLine(Buf, 'END', ErrStat, ErrMsg)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine EmitOutputs

end module MoorDyn_Yaml
