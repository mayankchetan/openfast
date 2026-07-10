!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of OrcaFlexInterface.
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
!> Reader for the YAML form of the OrcaFlex Interface primary input file. Fills the same
!! Orca_InputFile (InputFileData) scalar fields that the text-format ReadPrimaryFile reader
!! (in OrcaFlexInterface.f90) fills -- everything the text reader reads from the primary file
!! itself, and nothing more.
!!
!! Funnel structure (mirrors FEAMooring's FEAM_ReadInput / SubDyn's SD_Input / IceDyn's
!! IceD_ReadInput): the format-specific reading branch inside ReadPrimaryFile is what this
!! module replaces. OrcaFlex's primary-file reader DOES have a cross-cutting post-processing
!! step after the read -- resolving DirRoot and DLL_FileName against PriPath when they are
!! relative paths -- so that step is hoisted out of the format IF/ELSE in ReadPrimaryFile and
!! runs once after this parser returns (for both the text and YAML paths alike). This reader
!! therefore stores the two path strings RAW/unresolved, exactly as the text reader does before
!! its own PathIsRelative check.
!!
!! Schema: top-level keys mirror the text file's section banner. There is only one section:
!!   simulation_control : Echo (optional; accepted and ignored -- see below), DirRoot,
!!                         DLL_FileName
!!
!! Echo handling: the text reader's Echo switch only controls whether an echo file of the text
!! input is written while reading; it does not drive any downstream state (InputFileData has no
!! Echo field). This reader accepts an optional `Echo` key under `simulation_control` for
!! parity with the text file's banner, but -- exactly like the text path's value not surviving
!! past the read -- the value is not stored or otherwise used (there is nothing to echo when
!! parsing an already-in-memory YAML document). Matches how ExtPtfm_Yaml/IceDyn_Yaml treat
!! read-and-discard switches: accepted if present, never required, never propagated.
!!
!! No OutList: OrcaFlexInterface's text reader's DT and OutList reads are commented out (it
!! always emits the full fixed 18-channel OutList assembled in Orca_Init), so this reader
!! defines neither.
!!
!! DirRoot and DLL_FileName are path STRING values naming external files (the OrcaFlex
!! simulation input and the OrcaFlex DLL); they are read here as plain strings and are never
!! inlined or converted -- consistent with the project-wide rule that referenced external files
!! stay path strings.
module Orca_Yaml

   use NWTC_Library
   use OrcaFlexInterface_Types
   use YamlInput

   implicit none
   private

   public :: Orca_ParseYamlFile
   public :: Orca_ParseYamlFileInfo

contains

!> Load and parse a YAML-format OrcaFlex Interface primary input file.
subroutine Orca_ParseYamlFile(InputFile, PriPath, InputFileData, ErrStat, ErrMsg)
   character(*),          intent(in   ) :: InputFile     !< the .yaml primary input file
   character(*),          intent(in   ) :: PriPath       !< path of the primary file (unused here; the funnel resolves DirRoot/DLL_FileName after this returns)
   type(Orca_InputFile),  intent(inout) :: InputFileData
   integer(IntKi),        intent(  out) :: ErrStat
   character(*),          intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'Orca_ParseYamlFile'
   type(YamlDoc)            :: Doc
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call Yaml_LoadFile(InputFile, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call ParseYamlDoc(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! typo guard: anything never looked up is reported (warning severity)
   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine Orca_ParseYamlFile

!> Parse YAML-format OrcaFlex Interface input arriving as a FileInfoType -- the passed-data
!! channel used for inline module input from a YAML primary file (the glue code's inline
!! MooringFile handover when CompMooring selects OrcaFlex).
subroutine Orca_ParseYamlFileInfo(FileInfoIn, PriPath, InputFileData, ErrStat, ErrMsg)
   type(FileInfoType),   intent(in   ) :: FileInfoIn    !< YAML text lines with provenance
   character(*),         intent(in   ) :: PriPath       !< path of the primary file (unused here; the funnel resolves DirRoot/DLL_FileName after this returns)
   type(Orca_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'Orca_ParseYamlFileInfo'
   type(YamlDoc)            :: Doc
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call Yaml_LoadFileInfo(FileInfoIn, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call ParseYamlDoc(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine Orca_ParseYamlFileInfo

!> Fill InputFileData from a parsed document. Split from the two file-entry wrappers, mirroring
!! the project-wide ParseYamlDoc convention.
subroutine ParseYamlDoc(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),         intent(inout) :: Doc
   type(Orca_InputFile),  intent(inout) :: InputFileData
   integer(IntKi),        intent(  out) :: ErrStat
   character(*),          intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'Orca_ParseYamlDoc'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   ! Echo (optional, accepted and discarded -- see module header)
   call YamlGetOptionalEcho(Doc, TmpErrStat, TmpErrMsg); if (Failed()) return

   ! DirRoot / DLL_FileName -- read RAW; the ReadPrimaryFile funnel resolves relative
   ! paths against PriPath once, after this returns (shared by both text and YAML paths).
   call YamlGet(Doc, 'simulation_control:DirRoot',     InputFileData%DirRoot,     TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'simulation_control:DLL_FileName', InputFileData%DLL_FileName, TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

   !> Echo is a read-and-discard switch in the text reader (it only controls echo-file
   !! writing while reading; InputFileData has no Echo field to fill). Accept it if present
   !! so the section banner round-trips, but never require it and never store its value.
   subroutine YamlGetOptionalEcho(Doc, ErrStat, ErrMsg)
      type(YamlDoc),   intent(inout) :: Doc
      integer(IntKi),  intent(  out) :: ErrStat
      character(*),    intent(  out) :: ErrMsg

      integer(IntKi) :: iVal
      logical        :: EchoDummy
      logical        :: Found

      ErrStat = ErrID_None
      ErrMsg  = ""

      call YamlGetNode(Doc, 'simulation_control:Echo', iVal, ErrStat, ErrMsg, Found=Found)
      if (ErrStat >= AbortErrLev) return
      if (Found) then
         call YamlGet(Doc, 'simulation_control:Echo', EchoDummy, ErrStat, ErrMsg)
      end if
   end subroutine YamlGetOptionalEcho

end subroutine ParseYamlDoc

end module Orca_Yaml
