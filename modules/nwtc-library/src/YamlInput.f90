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
!! `!include` tag) into a YamlNode tree carrying per-node file:line provenance, and
!! provides the path-addressed YamlGet lookup family used by module input readers.
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

   !> One node of a parsed YAML document. Children are map entries (each carrying Key)
   !! or sequence items (Key unallocated). Scalar holds the raw, unconverted text —
   !! numeric conversion happens at lookup so error messages can quote the source.
   type, public :: YamlNode
      integer(IntKi)              :: Kind = YAML_UNDEF
      character(:), allocatable   :: Key         !< key within parent mapping (unallocated for seq items / root)
      character(:), allocatable   :: Scalar      !< raw scalar text (Kind == YAML_SCALAR)
      type(YamlNode), allocatable :: Children(:) !< map entries / sequence items, in file order
      integer(IntKi)              :: FileIndx = 0 !< index into YamlDoc%FileList (provenance)
      integer(IntKi)              :: FileLine = 0 !< 1-based line number within that file
      logical                     :: Used = .false. !< set by lookups; Yaml_WarnUnused reports stragglers
   end type YamlNode

   !> A parsed YAML document: the root node plus the list of every physical file involved
   !! (top file first, then files pulled in via !include), mirroring FileInfoType%FileList.
   type, public :: YamlDoc
      type(YamlNode)               :: Root
      character(1024), allocatable :: FileList(:)
   end type YamlDoc

   public :: IsYamlExt
   public :: Yaml_LoadFile
   public :: Yaml_LoadString

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
   ErrStat = ErrID_Fatal
   ErrMsg  = 'Yaml_LoadString: not implemented'
end subroutine Yaml_LoadString

end module YamlInput
