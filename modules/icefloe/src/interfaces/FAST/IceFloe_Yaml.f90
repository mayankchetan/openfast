!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of the IceFloe suite of subroutines
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
!> Reader for the YAML form of the IceFloe primary input file. Fills the same iceInputType
!! (unregistered, hand-maintained -- iceInputParams%iceInputType) that the text-format
!! countIceInputs/readIceInputs pair (in modules/icefloe/src/icefloe/iceInput.f90) fills.
!!
!! IceFloe has no field-by-field registry like the other modules' primary-file readers: the
!! text format is a flat list of "NAME value" lines read into iceInput%params(:), and every
!! %getIceInput call downstream extracts a named value by (uppercased, substring) match. So,
!! unlike the schema-shaped <Mod>_Yaml.f90 readers elsewhere in the tree, this reader does not
!! know IceFloe's parameter names -- it treats the YAML document generically: every top-level
!! key of the root mapping becomes one iceInput%params(n) entry, in document order, with the
!! key stored uppercased (Conv2UC) exactly as the text reader stores its NAME, and the value
!! read as real(ReKi) exactly as the text reader's paramType%value is real(ReKi) (getIntInput/
!! getLogicalInput on the extraction side convert on demand, unchanged by this reader).
!!
!! Location: this module lives under interfaces/FAST/ (not modules/icefloe/src/icefloe/)
!! because it needs YamlInput, which links against the full NWTC_Library. The icefloe/ core
!! subdirectory intentionally builds against a reduced NWTC set (see iceInput.f90's "use
!! precision" / "use NWTC_IO, only: ...") for use by non-FAST interfaces, so it cannot use
!! YamlInput.
!!
!! Duplicate-name warning: mirrors readIceInputs' substring-match duplicate check so behavior
!! parity holds for YAML input that (accidentally or intentionally) repeats/overlaps a key.
module IceFloe_Yaml

   use NWTC_Library
   use YamlInput
   use iceInputParams, only : iceInputType

   implicit none
   private

   public :: IceFloe_ParseYamlInputs

contains

!> Load a YAML-format IceFloe primary input file and populate input%count / input%params(:)
!! from the top-level keys of the root mapping. No schema: any top-level key becomes a param.
subroutine IceFloe_ParseYamlInputs(InputFile, input, ErrStat, ErrMsg)
   character(*),        intent(in   ) :: InputFile   !< the .yaml/.yml primary input file
   type(iceInputType),  intent(  out) :: input
   integer(IntKi),      intent(  out) :: ErrStat
   character(*),        intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'IceFloe_ParseYamlInputs'
   type(YamlDoc)            :: Doc
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg
   integer(IntKi)           :: nChild, iChild, n, k
   character(132)           :: KeyUC

   ErrStat = ErrID_None
   ErrMsg  = ""

   call Yaml_LoadFile(InputFile, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   nChild = Yaml_NumChildren(Doc, 1_IntKi)
   input%count = nChild

   allocate(input%params(input%count), stat=TmpErrStat)
   if (TmpErrStat /= 0) then
      call SetErrStat(ErrID_Fatal, 'Error in input parameter array allocation in '//RoutineName, &
                       ErrStat, ErrMsg, RoutineName)
      return
   end if

   do n = 1, nChild
      iChild = Yaml_Child(Doc, 1_IntKi, n)

      KeyUC = Doc%Nodes(iChild)%Key
      call Conv2UC(KeyUC)
      input%params(n)%name = KeyUC

      call YamlGet(Doc, trim(Doc%Nodes(iChild)%Key), input%params(n)%value, TmpErrStat, TmpErrMsg)
      if (Failed()) return

      ! duplicate-name check, mirroring readIceInputs' substring-match warning
      do k = 1, n-1
         if (index(KeyUC, trim(input%params(k)%name)) > 0) then
            call SetErrStat(ErrID_Warn, 'Input parameter '//trim(KeyUC)//' has been specified twice.', &
                             ErrStat, ErrMsg, RoutineName)
            exit
         end if
      end do
   end do

   ! typo guard: anything never looked up is reported (warning severity) -- should be empty
   ! here since every top-level key is looked up above, but kept for parity with the other
   ! YAML readers and to catch any future divergence.
   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine IceFloe_ParseYamlInputs

end module IceFloe_Yaml
