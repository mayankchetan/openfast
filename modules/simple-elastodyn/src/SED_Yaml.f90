!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of Simplified-ElastoDyn (SED)
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
!> Reader for the YAML form of the Simplified ElastoDyn (SED) primary input file. Fills the
!! same SED_InputFile structure as SED_ParsePrimaryFileData (the text path), so validation
!! and everything downstream is shared between the two formats.
!!
!! Schema: sections mirror the text file's banners (general, degrees_of_freedom,
!! initial_conditions, turbine_configuration, mass_and_inertia, drivetrain, output).
!! Keys keep their documented names. Only DT accepts the literal scalar "default"
!! (mirroring the text format's ParseVarWDefault, falling back to the interval supplied
!! by the caller); every other key is required. SumPrint is not read (the text-format
!! parser never reads it either -- SED_InputFile%SumPrint stays at its type-default
!! .false. on both paths). Unit conversions (deg->rad, rpm->rad/s) are applied in the
!! same order and on the same raw values as the text path (SED_IO.f90's
!! SED_ParsePrimaryFileData), so verbatim numeric literals produce bit-identical results.
module SED_Yaml

   use NWTC_Library
   use YamlInput
   use SED_Types
   use SED_Output_Params, only: MaxOutPts

   implicit none
   private

   public :: SED_ParseYamlFile
   public :: SED_ParseYamlFileInfo

contains

!> Load and parse a YAML-format SED primary input file. Mirrors the contract of
!! ProcessComFile + SED_ParsePrimaryFileData (the text path), including echo handling.
subroutine SED_ParseYamlFile(InputFileName, InitInp, RootName, interval, InputFileData, ErrStat, ErrMsg)
   character(*),               intent(in   ) :: InputFileName !< the .yaml primary input file
   type(SED_InitInputType),    intent(in   ) :: InitInp       !< Init input (unused directly, kept for signature parity)
   character(*),               intent(in   ) :: RootName      !< module root name, for the echo file
   real(DbKi),                 intent(in   ) :: interval      !< default DT supplied by the caller
   type(SED_InputFile),        intent(inout) :: InputFileData !< the shared input-file structure
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SED_ParseYamlFile'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: UnEc
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1

   call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   InputFileData%Echo = .false.
   call YamlGet(Doc, 'general:Echo', InputFileData%Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (InputFileData%Echo) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included), which supersedes the text path's line-by-line echo
      call OpenEcho(UnEc, trim(RootName)//'.ech', TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for Simplified ElastoDyn primary input file: '//trim(InputFileName)
      call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   call ParseYamlDoc(Doc, interval, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! typo guard: anything never looked up is reported (warning severity)
   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

   call Cleanup()

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
      if (Failed) call Cleanup()
   end function Failed

   subroutine Cleanup()
      if (UnEc > 0_IntKi) close(UnEc)
   end subroutine Cleanup

end subroutine SED_ParseYamlFile

!> Parse YAML-format SED input arriving as a FileInfoType -- the passed-data channel used
!! for inline module input from a YAML primary file (the glue code's inline EDFile
!! handover when CompElast selects SED). Per-line provenance in the FileInfoType keeps
!! error messages pointing at the original file and line.
subroutine SED_ParseYamlFileInfo(FileInfo, InitInp, RootName, interval, InputFileData, ErrStat, ErrMsg)
   type(FileInfoType),         intent(in   ) :: FileInfo
   type(SED_InitInputType),    intent(in   ) :: InitInp
   character(*),               intent(in   ) :: RootName
   real(DbKi),                 intent(in   ) :: interval
   type(SED_InputFile),        intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SED_ParseYamlFileInfo'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: UnEc
   integer(IntKi)          :: i
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1

   call Yaml_LoadFileInfo(FileInfo, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   InputFileData%Echo = .false.
   call YamlGet(Doc, 'general:Echo', InputFileData%Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (InputFileData%Echo) then
      call OpenEcho(UnEc, trim(RootName)//'.ech', TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for Simplified ElastoDyn primary input (passed YAML data)'
      do i = 1, FileInfo%NumLines
         write(UnEc, '(A)') trim(FileInfo%Lines(i))
      end do
   end if

   call ParseYamlDoc(Doc, interval, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

   call Cleanup()

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
      if (Failed) call Cleanup()
   end function Failed

   subroutine Cleanup()
      if (UnEc > 0_IntKi) close(UnEc)
   end subroutine Cleanup

end subroutine SED_ParseYamlFileInfo

!> Fill InputFileData from a parsed document. Split from the file/FileInfo wrappers so
!! both entry points share it (mirrors AeroDisk_Yaml's ParseYamlDoc split).
subroutine ParseYamlDoc(Doc, interval, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),         intent(inout) :: Doc
   real(DbKi),            intent(in   ) :: interval
   type(SED_InputFile),   intent(inout) :: InputFileData
   integer(IntKi),        intent(  out) :: ErrStat
   character(*),          intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'SED_ParseYamlDoc'
   character(:), allocatable  :: TmpList(:)
   integer(IntKi)              :: i
   integer(IntKi)              :: TmpErrStat
   character(ErrMsgLen)        :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   !----------------------------------------------------------------------------------
   ! general (required; DT accepts the scalar "default")
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'general:Echo', InputFileData%Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return
   call YamlGet(Doc, 'general:IntMethod', InputFileData%IntMethod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'general:DT', InputFileData%DT, TmpErrStat, TmpErrMsg, Default=interval)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! degrees_of_freedom (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'degrees_of_freedom:GenDOF', InputFileData%GenDOF, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:YawDOF', InputFileData%YawDOF, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! initial_conditions (required) -- unit conversions mirror the text path exactly
   ! (SED_IO.f90's SED_ParsePrimaryFileData): Azimuth, BlPitch, NacYaw, PtfmPitch
   ! deg->rad; RotSpeed rpm->rad/s.
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'initial_conditions:Azimuth', InputFileData%Azimuth, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%Azimuth = InputFileData%Azimuth * D2R

   call YamlGet(Doc, 'initial_conditions:BlPitch', InputFileData%BlPitch, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%BlPitch = InputFileData%BlPitch * D2R

   call YamlGet(Doc, 'initial_conditions:RotSpeed', InputFileData%RotSpeed, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%RotSpeed = InputFileData%RotSpeed * RPM2RPS

   call YamlGet(Doc, 'initial_conditions:NacYaw', InputFileData%NacYaw, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%NacYaw = InputFileData%NacYaw * D2R

   call YamlGet(Doc, 'initial_conditions:PtfmPitch', InputFileData%PtfmPitch, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%PtfmPitch = InputFileData%PtfmPitch * D2R

   !----------------------------------------------------------------------------------
   ! turbine_configuration (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'turbine_configuration:NumBl', InputFileData%NumBl, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:TipRad', InputFileData%TipRad, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:HubRad', InputFileData%HubRad, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'turbine_configuration:PreCone', InputFileData%PreCone, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%PreCone = InputFileData%PreCone * D2R

   call YamlGet(Doc, 'turbine_configuration:OverHang', InputFileData%OverHang, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'turbine_configuration:ShftTilt', InputFileData%ShftTilt, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%ShftTilt = InputFileData%ShftTilt * D2R

   call YamlGet(Doc, 'turbine_configuration:Twr2Shft', InputFileData%Twr2Shft, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:TowerHt', InputFileData%TowerHt, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! mass_and_inertia (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'mass_and_inertia:RotIner', InputFileData%RotIner, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_and_inertia:GenIner', InputFileData%GenIner, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! drivetrain (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'drivetrain:GBoxRatio', InputFileData%GBoxRatio, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! output (required) -- SumPrint is not read (the text-format parser never reads it
   ! either -- InputFileData%SumPrint stays at its type-default .false. on both paths)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'output:OutList', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpList) > MaxOutPts) then
      call SetErrStat(ErrID_Fatal, 'OutList may contain at most '//trim(Num2LStr(MaxOutPts))// &
         ' channels; found '//trim(Num2LStr(size(TmpList)))//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   call AllocAry(InputFileData%OutList, MaxOutPts, "SED Input File's OutList", TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%OutList = ''
   InputFileData%NumOuts = size(TmpList)
   do i = 1, InputFileData%NumOuts
      if (len_trim(TmpList(i)) > ChanLen) then
         call SetErrStat(ErrID_Fatal, 'OutList entry "'//trim(TmpList(i))//'" is longer than the '// &
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

end subroutine ParseYamlDoc

end module SED_Yaml
