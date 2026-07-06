module test_YamlInput

use testdrive, only: new_unittest, unittest_type, error_type, check
use NWTC_IO
use YamlInput

implicit none
private
public :: test_YamlInput_suite

contains

!> Collect all exported unit tests
subroutine test_YamlInput_suite(testsuite)
   type(unittest_type), allocatable, intent(out) :: testsuite(:)
   testsuite = [ &
               new_unittest("test_IsYamlExt_positive", test_IsYamlExt_positive), &
               new_unittest("test_IsYamlExt_negative", test_IsYamlExt_negative), &
               new_unittest("test_parse_nested_map", test_parse_nested_map), &
               new_unittest("test_parse_sequences", test_parse_sequences), &
               new_unittest("test_parse_quoting", test_parse_quoting), &
               new_unittest("test_parse_provenance", test_parse_provenance), &
               new_unittest("test_parse_tab_indent_fatal", test_parse_tab_indent_fatal), &
               new_unittest("test_parse_duplicate_key_fatal", test_parse_duplicate_key_fatal), &
               new_unittest("test_flow_sequences", test_flow_sequences), &
               new_unittest("test_flow_map", test_flow_map), &
               new_unittest("test_flow_unterminated_fatal", test_flow_unterminated_fatal), &
               new_unittest("test_block_scalar_fatal", test_block_scalar_fatal), &
               new_unittest("test_multidoc_fatal", test_multidoc_fatal), &
               new_unittest("test_unknown_tag_fatal", test_unknown_tag_fatal), &
               new_unittest("test_alias_scalar", test_alias_scalar), &
               new_unittest("test_alias_subtree", test_alias_subtree), &
               new_unittest("test_merge_key", test_merge_key), &
               new_unittest("test_merge_list_precedence", test_merge_list_precedence), &
               new_unittest("test_merge_in_sequence", test_merge_in_sequence), &
               new_unittest("test_undefined_alias_fatal", test_undefined_alias_fatal), &
               new_unittest("test_merge_bad_value_fatal", test_merge_bad_value_fatal) &
               ]
end subroutine

subroutine test_alias_scalar(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg
   integer(IntKi) :: iV

   character(64) :: Lines(2)
   Lines(1) = "a: &A 5.0"
   Lines(2) = "b: *A"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_None, trim(ErrMsg))
   if (allocated(error)) return

   iV = Yaml_ChildByKey(Doc, 1_IntKi, "a")
   call check(error, Doc%Nodes(iV)%Scalar, "5.0")
   if (allocated(error)) return
   iV = Yaml_ChildByKey(Doc, 1_IntKi, "b")
   call check(error, Doc%Nodes(iV)%Scalar, "5.0")
   if (allocated(error)) return
   ! copied nodes keep the anchor's provenance
   call check(error, int(Doc%Nodes(iV)%FileLine), 1)
end subroutine

subroutine test_alias_subtree(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg
   integer(IntKi) :: iT2, iV, iX

   character(64) :: Lines(5)
   Lines(1) = "t1: &T1"
   Lines(2) = "  ed: ED.yaml"
   Lines(3) = "  x: [1, 2]"
   Lines(4) = "t2: *T1"
   Lines(5) = "z: 0"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_None, trim(ErrMsg))
   if (allocated(error)) return

   iT2 = Yaml_ChildByKey(Doc, 1_IntKi, "t2")
   call check(error, Doc%Nodes(iT2)%Kind, YAML_MAP)
   if (allocated(error)) return
   call check(error, int(Yaml_NumChildren(Doc, iT2)), 2)
   if (allocated(error)) return
   iV = Yaml_ChildByKey(Doc, iT2, "ed")
   call check(error, Doc%Nodes(iV)%Scalar, "ED.yaml")
   if (allocated(error)) return
   iX = Yaml_ChildByKey(Doc, iT2, "x")
   call check(error, Doc%Nodes(iX)%Kind, YAML_SEQ)
   if (allocated(error)) return
   call check(error, int(Yaml_NumChildren(Doc, iX)), 2)
   if (allocated(error)) return
   iV = Yaml_Child(Doc, iX, 2_IntKi)
   call check(error, Doc%Nodes(iV)%Scalar, "2")
end subroutine

subroutine test_merge_key(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg
   integer(IntKi) :: iH, iV

   character(64) :: Lines(7)
   Lines(1) = "base: &B"
   Lines(2) = "  a: 1"
   Lines(3) = "  b: 2"
   Lines(4) = "host:"
   Lines(5) = "  <<: *B"
   Lines(6) = "  b: 9"
   Lines(7) = "  c: 3"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_None, trim(ErrMsg))
   if (allocated(error)) return

   iH = Yaml_ChildByKey(Doc, 1_IntKi, "host")
   call check(error, int(Yaml_NumChildren(Doc, iH)), 3)
   if (allocated(error)) return
   iV = Yaml_ChildByKey(Doc, iH, "a")
   call check(error, iV > 0, .true., "merged key a missing")
   if (allocated(error)) return
   call check(error, Doc%Nodes(iV)%Scalar, "1")
   if (allocated(error)) return
   ! explicit host keys always win over merged ones
   iV = Yaml_ChildByKey(Doc, iH, "b")
   call check(error, Doc%Nodes(iV)%Scalar, "9")
   if (allocated(error)) return
   iV = Yaml_ChildByKey(Doc, iH, "c")
   call check(error, Doc%Nodes(iV)%Scalar, "3")
end subroutine

subroutine test_merge_list_precedence(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg
   integer(IntKi) :: iH, iV

   character(64) :: Lines(8)
   Lines(1) = "x: &X"
   Lines(2) = "  k: 1"
   Lines(3) = "y: &Y"
   Lines(4) = "  k: 2"
   Lines(5) = "  m: 5"
   Lines(6) = "host:"
   Lines(7) = "  <<: [*X, *Y]"
   Lines(8) = "  own: 7"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_None, trim(ErrMsg))
   if (allocated(error)) return

   iH = Yaml_ChildByKey(Doc, 1_IntKi, "host")
   ! earlier alias in the merge list takes precedence (YAML merge-key spec)
   iV = Yaml_ChildByKey(Doc, iH, "k")
   call check(error, Doc%Nodes(iV)%Scalar, "1")
   if (allocated(error)) return
   iV = Yaml_ChildByKey(Doc, iH, "m")
   call check(error, Doc%Nodes(iV)%Scalar, "5")
   if (allocated(error)) return
   iV = Yaml_ChildByKey(Doc, iH, "own")
   call check(error, Doc%Nodes(iV)%Scalar, "7")
end subroutine

subroutine test_merge_in_sequence(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg
   integer(IntKi) :: iT, iItem, iV

   ! the FAST.Farm turbine-copy pattern
   character(64) :: Lines(6)
   Lines(1) = "turbines:"
   Lines(2) = "  - &T1"
   Lines(3) = "    ed: ED1.yaml"
   Lines(4) = "    sd: SD1.yaml"
   Lines(5) = "  - <<: *T1"
   Lines(6) = "    sd: SD2.yaml"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_None, trim(ErrMsg))
   if (allocated(error)) return

   iT = Yaml_ChildByKey(Doc, 1_IntKi, "turbines")
   call check(error, int(Yaml_NumChildren(Doc, iT)), 2)
   if (allocated(error)) return
   iItem = Yaml_Child(Doc, iT, 2_IntKi)
   call check(error, Doc%Nodes(iItem)%Kind, YAML_MAP)
   if (allocated(error)) return
   iV = Yaml_ChildByKey(Doc, iItem, "ed")
   call check(error, iV > 0, .true., "merged ed missing in turbine 2")
   if (allocated(error)) return
   call check(error, Doc%Nodes(iV)%Scalar, "ED1.yaml")
   if (allocated(error)) return
   iV = Yaml_ChildByKey(Doc, iItem, "sd")
   call check(error, Doc%Nodes(iV)%Scalar, "SD2.yaml")
end subroutine

subroutine test_undefined_alias_fatal(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg

   character(64) :: Lines(1)
   Lines(1) = "b: *NOPE"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_Fatal)
   if (allocated(error)) return
   call check(error, index(ErrMsg, "NOPE") > 0, .true., "error must name the anchor: "//trim(ErrMsg))
   if (allocated(error)) return
   call check(error, index(ErrMsg, "line #1") > 0, .true., "error must name line #1: "//trim(ErrMsg))
end subroutine

subroutine test_merge_bad_value_fatal(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg

   character(64) :: Lines(2)
   Lines(1) = "host:"
   Lines(2) = "  <<: 42"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_Fatal)
   if (allocated(error)) return
   call check(error, index(ErrMsg, "line #2") > 0, .true., "error must name line #2: "//trim(ErrMsg))
   if (allocated(error)) return
   call check(error, index(ErrMsg, "<<") > 0, .true., "error must mention the merge key: "//trim(ErrMsg))
end subroutine

subroutine test_flow_sequences(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg
   integer(IntKi) :: iA, iM, iRow, iV

   character(64) :: Lines(4)
   Lines(1) = "amps: [1, 2.5, -3e2]"
   Lines(2) = "matrix:"
   Lines(3) = "  - [1.0, 2.0]"
   Lines(4) = "  - [3.0, 4.0]"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_None, trim(ErrMsg))
   if (allocated(error)) return

   iA = Yaml_ChildByKey(Doc, 1_IntKi, "amps")
   call check(error, Doc%Nodes(iA)%Kind, YAML_SEQ)
   if (allocated(error)) return
   call check(error, int(Yaml_NumChildren(Doc, iA)), 3)
   if (allocated(error)) return
   iV = Yaml_Child(Doc, iA, 2_IntKi)
   call check(error, Doc%Nodes(iV)%Scalar, "2.5")
   if (allocated(error)) return
   iV = Yaml_Child(Doc, iA, 3_IntKi)
   call check(error, Doc%Nodes(iV)%Scalar, "-3e2")
   if (allocated(error)) return
   ! flow items carry the line they appear on
   call check(error, int(Doc%Nodes(iV)%FileLine), 1)
   if (allocated(error)) return

   ! sequence of flow sequences = matrix rows
   iM = Yaml_ChildByKey(Doc, 1_IntKi, "matrix")
   call check(error, Doc%Nodes(iM)%Kind, YAML_SEQ)
   if (allocated(error)) return
   call check(error, int(Yaml_NumChildren(Doc, iM)), 2)
   if (allocated(error)) return
   iRow = Yaml_Child(Doc, iM, 2_IntKi)
   call check(error, Doc%Nodes(iRow)%Kind, YAML_SEQ)
   if (allocated(error)) return
   iV = Yaml_Child(Doc, iRow, 1_IntKi)
   call check(error, Doc%Nodes(iV)%Scalar, "3.0")
end subroutine

subroutine test_flow_map(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg
   integer(IntKi) :: iP, iV

   character(80) :: Lines(2)
   Lines(1) = 'point: {x: 1.0, y: -2.0, label: "a, b"}'
   Lines(2) = "empty: []"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_None, trim(ErrMsg))
   if (allocated(error)) return

   iP = Yaml_ChildByKey(Doc, 1_IntKi, "point")
   call check(error, Doc%Nodes(iP)%Kind, YAML_MAP)
   if (allocated(error)) return
   call check(error, int(Yaml_NumChildren(Doc, iP)), 3)
   if (allocated(error)) return
   iV = Yaml_ChildByKey(Doc, iP, "y")
   call check(error, Doc%Nodes(iV)%Scalar, "-2.0")
   if (allocated(error)) return
   ! commas inside quotes do not split flow items
   iV = Yaml_ChildByKey(Doc, iP, "label")
   call check(error, Doc%Nodes(iV)%Scalar, "a, b")
   if (allocated(error)) return

   iV = Yaml_ChildByKey(Doc, 1_IntKi, "empty")
   call check(error, Doc%Nodes(iV)%Kind, YAML_SEQ)
   if (allocated(error)) return
   call check(error, int(Yaml_NumChildren(Doc, iV)), 0)
end subroutine

subroutine test_flow_unterminated_fatal(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg

   character(64) :: Lines(2)
   Lines(1) = "ok: 1"
   Lines(2) = "bad: [1, 2"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_Fatal)
   if (allocated(error)) return
   call check(error, index(ErrMsg, "line #2") > 0, .true., "error must name line #2: "//trim(ErrMsg))
end subroutine

subroutine test_block_scalar_fatal(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg

   character(64) :: Lines(2)
   Lines(1) = "description: |"
   Lines(2) = "  a folded block"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_Fatal)
   if (allocated(error)) return
   call check(error, index(ErrMsg, "line #1") > 0, .true., "error must name line #1: "//trim(ErrMsg))
   if (allocated(error)) return
   call check(error, index(ErrMsg, "lock scalar") > 0, .true., "error must say block scalars unsupported: "//trim(ErrMsg))
end subroutine

subroutine test_multidoc_fatal(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg

   character(64) :: Lines(3)
   Lines(1) = "---"
   Lines(2) = "a: 1"
   Lines(3) = "---"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_Fatal)
   if (allocated(error)) return
   call check(error, index(ErrMsg, "line #3") > 0, .true., "error must name line #3: "//trim(ErrMsg))
   if (allocated(error)) return
   call check(error, index(ErrMsg, "ulti-document") > 0, .true., "error must mention multi-document: "//trim(ErrMsg))
end subroutine

subroutine test_unknown_tag_fatal(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg

   character(64) :: Lines(1)
   Lines(1) = "blade: !mystery blade1.yaml"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_Fatal)
   if (allocated(error)) return
   call check(error, index(ErrMsg, "!mystery") > 0, .true., "error must name the tag: "//trim(ErrMsg))
   if (allocated(error)) return
   call check(error, index(ErrMsg, "line #1") > 0, .true., "error must name line #1: "//trim(ErrMsg))
end subroutine

subroutine test_IsYamlExt_positive(error)
   type(error_type), allocatable, intent(out) :: error

   call check(error, IsYamlExt("model.yaml"), .true.)
   if (allocated(error)) return
   call check(error, IsYamlExt("model.yml"), .true.)
   if (allocated(error)) return
   call check(error, IsYamlExt("MODEL.YAML"), .true.)
   if (allocated(error)) return
   call check(error, IsYamlExt("Model.Yml"), .true.)
   if (allocated(error)) return
   call check(error, IsYamlExt("path/to/deck.5MW.yaml"), .true.)
   if (allocated(error)) return
   ! extension is judged on the basename, not on directory names
   call check(error, IsYamlExt("dir.dat/file.yaml"), .true.)
end subroutine

subroutine test_IsYamlExt_negative(error)
   type(error_type), allocatable, intent(out) :: error

   call check(error, IsYamlExt("model.dat"), .false.)
   if (allocated(error)) return
   call check(error, IsYamlExt("model.fst"), .false.)
   if (allocated(error)) return
   call check(error, IsYamlExt("model.yamlx"), .false.)
   if (allocated(error)) return
   call check(error, IsYamlExt("model"), .false.)
   if (allocated(error)) return
   call check(error, IsYamlExt("yaml"), .false.)
   if (allocated(error)) return
   ! directory named *.yaml must not make a text file YAML
   call check(error, IsYamlExt("dir.yaml/file.dat"), .false.)
   if (allocated(error)) return
   call check(error, IsYamlExt(""), .false.)
end subroutine

subroutine test_parse_nested_map(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg
   integer(IntKi) :: iSC, iN, iV

   character(64) :: Lines(6)
   Lines(1) = "---"
   Lines(2) = "simulation_control:"
   Lines(3) = "  TMax: 60.0"
   Lines(4) = "  nested:"
   Lines(5) = "    DT: 0.0125"
   Lines(6) = "environment: sea"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_None, trim(ErrMsg))
   if (allocated(error)) return

   call check(error, Doc%Nodes(1)%Kind, YAML_MAP)
   if (allocated(error)) return
   call check(error, int(Yaml_NumChildren(Doc, 1_IntKi)), 2)
   if (allocated(error)) return

   iSC = Yaml_ChildByKey(Doc, 1_IntKi, "simulation_control")
   call check(error, iSC > 0, .true., "simulation_control key not found")
   if (allocated(error)) return
   call check(error, Doc%Nodes(iSC)%Kind, YAML_MAP)
   if (allocated(error)) return

   iV = Yaml_ChildByKey(Doc, iSC, "TMax")
   call check(error, iV > 0, .true., "TMax key not found")
   if (allocated(error)) return
   call check(error, Doc%Nodes(iV)%Kind, YAML_SCALAR)
   if (allocated(error)) return
   call check(error, Doc%Nodes(iV)%Scalar, "60.0")
   if (allocated(error)) return

   iN = Yaml_ChildByKey(Doc, iSC, "nested")
   call check(error, iN > 0, .true., "nested key not found")
   if (allocated(error)) return
   iV = Yaml_ChildByKey(Doc, iN, "DT")
   call check(error, iV > 0, .true., "DT key not found")
   if (allocated(error)) return
   call check(error, Doc%Nodes(iV)%Scalar, "0.0125")
   if (allocated(error)) return

   iV = Yaml_ChildByKey(Doc, 1_IntKi, "environment")
   call check(error, Doc%Nodes(iV)%Scalar, "sea")
end subroutine

subroutine test_parse_sequences(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg
   integer(IntKi) :: iCh, iT, iItem, iV

   character(64) :: Lines(9)
   Lines(1) = "channels:"
   Lines(2) = "  - RotSpeed"
   Lines(3) = "  - GenPwr"
   Lines(4) = "turbines:"
   Lines(5) = "  - name: T1"
   Lines(6) = "    x: 0.0"
   Lines(7) = "  - name: T2"
   Lines(8) = "    x: 100.0"
   Lines(9) = "flag: true"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_None, trim(ErrMsg))
   if (allocated(error)) return

   iCh = Yaml_ChildByKey(Doc, 1_IntKi, "channels")
   call check(error, Doc%Nodes(iCh)%Kind, YAML_SEQ)
   if (allocated(error)) return
   call check(error, int(Yaml_NumChildren(Doc, iCh)), 2)
   if (allocated(error)) return
   iItem = Yaml_Child(Doc, iCh, 1_IntKi)
   call check(error, Doc%Nodes(iItem)%Scalar, "RotSpeed")
   if (allocated(error)) return
   ! sequence items carry no key
   call check(error, allocated(Doc%Nodes(iItem)%Key), .false.)
   if (allocated(error)) return

   iT = Yaml_ChildByKey(Doc, 1_IntKi, "turbines")
   call check(error, Doc%Nodes(iT)%Kind, YAML_SEQ)
   if (allocated(error)) return
   call check(error, int(Yaml_NumChildren(Doc, iT)), 2)
   if (allocated(error)) return
   iItem = Yaml_Child(Doc, iT, 2_IntKi)
   call check(error, Doc%Nodes(iItem)%Kind, YAML_MAP)
   if (allocated(error)) return
   iV = Yaml_ChildByKey(Doc, iItem, "x")
   call check(error, Doc%Nodes(iV)%Scalar, "100.0")
end subroutine

subroutine test_parse_quoting(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg
   integer(IntKi) :: iV

   character(64) :: Lines(4)
   Lines(1) = 'title: "a # not comment"'
   Lines(2) = "windfile: 'C:\wind\file.bts'"
   Lines(3) = "time: 12:30:45"
   Lines(4) = 'quote: ''it''''s fine'''

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_None, trim(ErrMsg))
   if (allocated(error)) return

   iV = Yaml_ChildByKey(Doc, 1_IntKi, "title")
   call check(error, Doc%Nodes(iV)%Scalar, "a # not comment")
   if (allocated(error)) return
   iV = Yaml_ChildByKey(Doc, 1_IntKi, "windfile")
   call check(error, Doc%Nodes(iV)%Scalar, "C:\wind\file.bts")
   if (allocated(error)) return
   ! plain scalar may contain ':' when not followed by a space
   iV = Yaml_ChildByKey(Doc, 1_IntKi, "time")
   call check(error, Doc%Nodes(iV)%Scalar, "12:30:45")
   if (allocated(error)) return
   ! single-quote escaping: '' -> '
   iV = Yaml_ChildByKey(Doc, 1_IntKi, "quote")
   call check(error, Doc%Nodes(iV)%Scalar, "it's fine")
end subroutine

subroutine test_parse_provenance(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg
   integer(IntKi) :: iSC, iV

   character(64) :: Lines(7)
   Lines(1) = "# leading comment"
   Lines(2) = ""
   Lines(3) = "simulation_control:   # trailing comment"
   Lines(4) = ""
   Lines(5) = "  # indented comment"
   Lines(6) = "  TMax: 60.0"
   Lines(7) = "  DT: 0.0125"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_None, trim(ErrMsg))
   if (allocated(error)) return

   ! comments and blank lines shift nothing: FileLine is the physical line number
   iSC = Yaml_ChildByKey(Doc, 1_IntKi, "simulation_control")
   call check(error, int(Doc%Nodes(iSC)%FileLine), 3)
   if (allocated(error)) return
   iV = Yaml_ChildByKey(Doc, iSC, "TMax")
   call check(error, int(Doc%Nodes(iV)%FileLine), 6)
   if (allocated(error)) return
   iV = Yaml_ChildByKey(Doc, iSC, "DT")
   call check(error, int(Doc%Nodes(iV)%FileLine), 7)
   if (allocated(error)) return
   call check(error, int(Doc%Nodes(iSC)%FileIndx), 1)
end subroutine

subroutine test_parse_tab_indent_fatal(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg

   character(64) :: Lines(2)
   Lines(1) = "a:"
   Lines(2) = char(9)//"b: 1"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_Fatal)
   if (allocated(error)) return
   call check(error, index(ErrMsg, "line #2") > 0, .true., "error must name line #2: "//trim(ErrMsg))
   if (allocated(error)) return
   call check(error, index(ErrMsg, "Tab") > 0 .or. index(ErrMsg, "tab") > 0, &
              .true., "error must mention tabs: "//trim(ErrMsg))
end subroutine

subroutine test_parse_duplicate_key_fatal(error)
   type(error_type), allocatable, intent(out) :: error
   type(YamlDoc) :: Doc
   integer(IntKi) :: ErrStat
   character(ErrMsgLen) :: ErrMsg

   character(64) :: Lines(3)
   Lines(1) = "a: 1"
   Lines(2) = "b: 2"
   Lines(3) = "a: 3"

   call Yaml_LoadString(Lines, Doc, ErrStat, ErrMsg)
   call check(error, ErrStat, ErrID_Fatal)
   if (allocated(error)) return
   call check(error, index(ErrMsg, "line #3") > 0, .true., "error must name duplicate line #3: "//trim(ErrMsg))
   if (allocated(error)) return
   call check(error, index(ErrMsg, "line #1") > 0, .true., "error must name original line #1: "//trim(ErrMsg))
   if (allocated(error)) return
   call check(error, index(ErrMsg, '"a"') > 0, .true., "error must name the key: "//trim(ErrMsg))
end subroutine

end module
