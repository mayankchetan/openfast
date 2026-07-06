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
               new_unittest("test_IsYamlExt_negative", test_IsYamlExt_negative) &
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

end module
