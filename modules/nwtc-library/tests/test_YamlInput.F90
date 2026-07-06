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
               new_unittest("test_parse_duplicate_key_fatal", test_parse_duplicate_key_fatal) &
               ]
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
