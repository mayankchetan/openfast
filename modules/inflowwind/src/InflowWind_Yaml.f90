!**********************************************************************************************************************************
! Copyright (C) 2026 National Renewable Energy Laboratory
!
! This file is part of InflowWind.
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
!> Reader for the YAML form of the InflowWind primary input file. Fills the same
!! InflowWind_InputFile structure as InflowWind_ParseInputFileInfo (the text path), so
!! validation and everything downstream is shared between the two formats.
!!
!! Schema: sections mirror the text file's banners; leaf keys are the documented
!! parameter names (matched case-insensitively). Differences from the text format:
!!  - counts are derived from list lengths (NWindVel from WindVxiList, NumBeam from
!!    FocalDistanceX) instead of being separate keys;
!!  - the wind-type sections (steady_wind, uniform_wind, turbsim_wind, bladed_wind,
!!    hawc_wind) are optional except the one the selected WindType requires; the
!!    general, lidar, and output sections are always required.
module InflowWind_Yaml

   use NWTC_Library
   use YamlInput
   use InflowWind_Types

   implicit none
   private

   public :: InflowWind_ParseYamlFile
   public :: InflowWind_ParseYamlFileInfo

   integer(IntKi), parameter :: MaxOutPts = 98   ! same limit as InflowWind_Subs

contains

!> Load and parse a YAML-format InflowWind primary input file. Mirrors the contract of
!! ProcessComFile + InflowWind_ParseInputFileInfo, including the echo file and the
!! FAST.Farm fixed-wind-file-name handling.
subroutine InflowWind_ParseYamlFile(InputFileName, EchoFileName, FixedWindFileRootName, TurbineID, &
                                    InputFileData, ErrStat, ErrMsg)
   character(*),               intent(in   ) :: InputFileName         !< the .yaml primary input file
   character(*),               intent(in   ) :: EchoFileName          !< echo file to create when Echo is true
   logical,                    intent(in   ) :: FixedWindFileRootName !< FAST.Farm fixed (DEFAULT) wind file names
   integer(IntKi),             intent(in   ) :: TurbineID             !< FAST.Farm turbine ID for fixed file names
   type(InflowWind_InputFile), intent(inout) :: InputFileData         !< the shared input-file structure
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'InflowWind_ParseYamlFile'
   type(YamlDoc)           :: Doc
   character(1024)         :: PriPath
   integer(IntKi)          :: UnEc
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1
   call GetPath(InputFileName, PriPath)

   call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   InputFileData%EchoFlag = .false.
   call YamlGet(Doc, 'general:Echo', InputFileData%EchoFlag, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (InputFileData%EchoFlag) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included), which supersedes the text path's line-by-line echo
      call OpenEcho(UnEc, trim(EchoFileName), TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for InflowWind input file: '//trim(InputFileName)
      call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   call ParseYamlDoc(Doc, PriPath, FixedWindFileRootName, TurbineID, InputFileData, TmpErrStat, TmpErrMsg)
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

end subroutine InflowWind_ParseYamlFile

!> Parse YAML-format InflowWind input arriving as a FileInfoType — the passed-data
!! channel used for inline module input from a YAML primary file (or python-supplied
!! YAML lines). Per-line provenance in the FileInfoType keeps error messages pointing
!! at the original file and line. When Echo is true the passed lines are echoed
!! verbatim (they carry the original comments only if the caller preserved them;
!! serialized inline input echoes in its serialized form).
subroutine InflowWind_ParseYamlFileInfo(FileInfo, PriPath, InputFileName, EchoFileName, &
                                        FixedWindFileRootName, TurbineID, InputFileData, ErrStat, ErrMsg)
   type(FileInfoType),         intent(in   ) :: FileInfo
   character(*),               intent(in   ) :: PriPath               !< path for resolving relative file names
   character(*),               intent(in   ) :: InputFileName         !< name used in messages/echo header
   character(*),               intent(in   ) :: EchoFileName
   logical,                    intent(in   ) :: FixedWindFileRootName
   integer(IntKi),             intent(in   ) :: TurbineID
   type(InflowWind_InputFile), intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'InflowWind_ParseYamlFileInfo'
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

   InputFileData%EchoFlag = .false.
   call YamlGet(Doc, 'general:Echo', InputFileData%EchoFlag, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (InputFileData%EchoFlag) then
      call OpenEcho(UnEc, trim(EchoFileName), TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for InflowWind input (passed YAML data): '//trim(InputFileName)
      do i = 1, FileInfo%NumLines
         write(UnEc, '(A)') trim(FileInfo%Lines(i))
      end do
   end if

   call ParseYamlDoc(Doc, PriPath, FixedWindFileRootName, TurbineID, InputFileData, TmpErrStat, TmpErrMsg)
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

end subroutine InflowWind_ParseYamlFileInfo

!> Fill InputFileData from a parsed document. Split from the file wrapper so a passed
!! subtree (inline module input) can reuse it later.
subroutine ParseYamlDoc(Doc, PriPath, FixedWindFileRootName, TurbineID, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   character(*),               intent(in   ) :: PriPath
   logical,                    intent(in   ) :: FixedWindFileRootName
   integer(IntKi),             intent(in   ) :: TurbineID
   type(InflowWind_InputFile), intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'InflowWind_ParseYamlDoc'
   real(ReKi), allocatable    :: TmpAry(:)
   character(:), allocatable  :: TmpList(:)
   integer(IntKi)             :: iSec
   logical                    :: SecFound
   integer(IntKi)             :: i
   integer(IntKi)             :: TmpErrStat
   character(ErrMsgLen)       :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   !----------------------------------------------------------------------------------
   ! general (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'general:Echo', InputFileData%EchoFlag, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return
   call YamlGet(Doc, 'general:WindType', InputFileData%WindType, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'general:PropagationDir', InputFileData%PropagationDir, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'general:VFlowAng', InputFileData%VFlowAngle, TmpErrStat, TmpErrMsg, Default=0.0_ReKi)
   if (Failed()) return
   call YamlGet(Doc, 'general:VelInterpCubic', InputFileData%VelInterpCubic, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! NWindVel is derived from the list lengths (no separate count key in YAML)
   call YamlGet(Doc, 'general:WindVxiList', InputFileData%WindVxiList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'general:WindVyiList', InputFileData%WindVyiList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'general:WindVziList', InputFileData%WindVziList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%NWindVel = size(InputFileData%WindVxiList)
   if (size(InputFileData%WindVyiList) /= InputFileData%NWindVel .or. &
       size(InputFileData%WindVziList) /= InputFileData%NWindVel) then
      call SetErrStat(ErrID_Fatal, 'WindVxiList, WindVyiList, and WindVziList must all have the '// &
         'same number of entries.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   if (InputFileData%NWindVel > 9) then
      call SetErrStat(ErrID_Fatal, 'NWindVel (the length of WindVxiList) must be less than 10.', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if

   !----------------------------------------------------------------------------------
   ! wind-type sections (each required only when the selected WindType uses it)
   !----------------------------------------------------------------------------------

   ! steady_wind [WindType = 1]
   InputFileData%Steady_HWindSpeed = 0.0_ReKi
   InputFileData%Steady_RefHt      = 0.0_ReKi
   InputFileData%Steady_PLexp      = 0.0_ReKi
   call YamlGetNode(Doc, 'steady_wind', iSec, TmpErrStat, TmpErrMsg, Found=SecFound)
   if (Failed()) return
   call RequireSection(1_IntKi, SecFound, 'steady_wind')
   if (ErrStat >= AbortErrLev) return
   if (SecFound) then
      call YamlGet(Doc, 'HWindSpeed', InputFileData%Steady_HWindSpeed, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'RefHt', InputFileData%Steady_RefHt, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'PLexp', InputFileData%Steady_PLexp, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
   end if

   ! uniform_wind [WindType = 2]
   InputFileData%Uniform_FileName  = ''
   InputFileData%Uniform_RefHt     = 0.0_ReKi
   InputFileData%Uniform_RefLength = 0.0_ReKi
   call YamlGetNode(Doc, 'uniform_wind', iSec, TmpErrStat, TmpErrMsg, Found=SecFound)
   if (Failed()) return
   call RequireSection(2_IntKi, SecFound, 'uniform_wind')
   if (ErrStat >= AbortErrLev) return
   if (SecFound) then
      call YamlGet(Doc, 'FileName_Uni', InputFileData%Uniform_FileName, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      if (PathIsRelative(InputFileData%Uniform_FileName)) &
         InputFileData%Uniform_FileName = trim(PriPath)//trim(InputFileData%Uniform_FileName)
      if (FixedWindFileRootName) then
         if (TurbineID == 0) then
            InputFileData%Uniform_FileName = trim(InputFileData%Uniform_FileName)//trim(PathSep)//'Low.dat'
         else
            InputFileData%Uniform_FileName = trim(InputFileData%Uniform_FileName)//trim(PathSep)// &
                                             'HighT'//trim(Num2LStr(TurbineID))//'.dat'
         end if
      end if
      call YamlGet(Doc, 'RefHt_Uni', InputFileData%Uniform_RefHt, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'RefLength', InputFileData%Uniform_RefLength, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
   end if

   ! turbsim_wind [WindType = 3]
   InputFileData%TSFF_FileName = ''
   call YamlGetNode(Doc, 'turbsim_wind', iSec, TmpErrStat, TmpErrMsg, Found=SecFound)
   if (Failed()) return
   call RequireSection(3_IntKi, SecFound, 'turbsim_wind')
   if (ErrStat >= AbortErrLev) return
   if (SecFound) then
      call YamlGet(Doc, 'FileName_BTS', InputFileData%TSFF_FileName, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      if (PathIsRelative(InputFileData%TSFF_FileName)) &
         InputFileData%TSFF_FileName = trim(PriPath)//trim(InputFileData%TSFF_FileName)
      if (FixedWindFileRootName) then
         if (TurbineID == 0) then
            InputFileData%TSFF_FileName = trim(InputFileData%TSFF_FileName)//trim(PathSep)//'Low.bts'
         else
            InputFileData%TSFF_FileName = trim(InputFileData%TSFF_FileName)//trim(PathSep)// &
                                          'HighT'//trim(Num2LStr(TurbineID))//'.bts'
         end if
      end if
   end if

   ! bladed_wind [WindType = 4 or 7]
   InputFileData%BladedFF_FileName  = ''
   InputFileData%BladedFF_TowerFile = .false.
   call YamlGetNode(Doc, 'bladed_wind', iSec, TmpErrStat, TmpErrMsg, Found=SecFound)
   if (Failed()) return
   call RequireSection(4_IntKi, SecFound, 'bladed_wind')
   call RequireSection(7_IntKi, SecFound, 'bladed_wind')
   if (ErrStat >= AbortErrLev) return
   if (SecFound) then
      call YamlGet(Doc, 'FilenameRoot', InputFileData%BladedFF_FileName, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      if (PathIsRelative(InputFileData%BladedFF_FileName)) &
         InputFileData%BladedFF_FileName = trim(PriPath)//trim(InputFileData%BladedFF_FileName)
      call YamlGet(Doc, 'TowerFile', InputFileData%BladedFF_TowerFile, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
   end if

   ! hawc_wind [WindType = 5], with scaling and mean_profile sub-maps
   InputFileData%HAWC_FileName_u = ''
   InputFileData%HAWC_FileName_v = ''
   InputFileData%HAWC_FileName_w = ''
   InputFileData%HAWC_nx = 0;  InputFileData%HAWC_ny = 0;  InputFileData%HAWC_nz = 0
   InputFileData%HAWC_dx = 0.0_ReKi;  InputFileData%HAWC_dy = 0.0_ReKi;  InputFileData%HAWC_dz = 0.0_ReKi
   InputFileData%FF%RefHt = 0.0_ReKi
   InputFileData%FF%ScaleMethod = 0_IntKi
   InputFileData%FF%SF = 1.0_ReKi
   InputFileData%FF%SigmaF = 0.0_ReKi
   InputFileData%FF%URef = 0.0_ReKi
   InputFileData%FF%WindProfileType = 0_IntKi
   InputFileData%FF%PLExp = 0.0_ReKi
   InputFileData%FF%Z0 = 0.0_ReKi
   InputFileData%FF%XOffset = 0.0_ReKi
   call YamlGetNode(Doc, 'hawc_wind', iSec, TmpErrStat, TmpErrMsg, Found=SecFound)
   if (Failed()) return
   call RequireSection(5_IntKi, SecFound, 'hawc_wind')
   if (ErrStat >= AbortErrLev) return
   if (SecFound) then
      call YamlGet(Doc, 'FileName_u', InputFileData%HAWC_FileName_u, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'FileName_v', InputFileData%HAWC_FileName_v, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'FileName_w', InputFileData%HAWC_FileName_w, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      if (PathIsRelative(InputFileData%HAWC_FileName_u)) &
         InputFileData%HAWC_FileName_u = trim(PriPath)//trim(InputFileData%HAWC_FileName_u)
      if (PathIsRelative(InputFileData%HAWC_FileName_v)) &
         InputFileData%HAWC_FileName_v = trim(PriPath)//trim(InputFileData%HAWC_FileName_v)
      if (PathIsRelative(InputFileData%HAWC_FileName_w)) &
         InputFileData%HAWC_FileName_w = trim(PriPath)//trim(InputFileData%HAWC_FileName_w)
      if (FixedWindFileRootName) then
         if (TurbineID == 0) then
            InputFileData%HAWC_FileName_u = trim(InputFileData%HAWC_FileName_u)//trim(PathSep)//'Low_u.bin'
            InputFileData%HAWC_FileName_v = trim(InputFileData%HAWC_FileName_v)//trim(PathSep)//'Low_v.bin'
            InputFileData%HAWC_FileName_w = trim(InputFileData%HAWC_FileName_w)//trim(PathSep)//'Low_w.bin'
         else
            InputFileData%HAWC_FileName_u = trim(InputFileData%HAWC_FileName_u)//trim(PathSep)// &
                                            'HighT'//trim(Num2LStr(TurbineID))//'_u.bin'
            InputFileData%HAWC_FileName_v = trim(InputFileData%HAWC_FileName_v)//trim(PathSep)// &
                                            'HighT'//trim(Num2LStr(TurbineID))//'_v.bin'
            InputFileData%HAWC_FileName_w = trim(InputFileData%HAWC_FileName_w)//trim(PathSep)// &
                                            'HighT'//trim(Num2LStr(TurbineID))//'_w.bin'
         end if
      end if
      call YamlGet(Doc, 'nx', InputFileData%HAWC_nx, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'ny', InputFileData%HAWC_ny, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'nz', InputFileData%HAWC_nz, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'dx', InputFileData%HAWC_dx, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'dy', InputFileData%HAWC_dy, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'dz', InputFileData%HAWC_dz, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'RefHt_HAWC', InputFileData%FF%RefHt, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return

      call YamlGet(Doc, 'scaling:ScaleMethod', InputFileData%FF%ScaleMethod, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'scaling:SFx', InputFileData%FF%SF(1), TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'scaling:SFy', InputFileData%FF%SF(2), TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'scaling:SFz', InputFileData%FF%SF(3), TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'scaling:SigmaFx', InputFileData%FF%SigmaF(1), TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'scaling:SigmaFy', InputFileData%FF%SigmaF(2), TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'scaling:SigmaFz', InputFileData%FF%SigmaF(3), TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return

      call YamlGet(Doc, 'mean_profile:URef', InputFileData%FF%URef, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'mean_profile:WindProfile', InputFileData%FF%WindProfileType, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'mean_profile:PLExp_HAWC', InputFileData%FF%PLExp, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'mean_profile:Z0', InputFileData%FF%Z0, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      call YamlGet(Doc, 'mean_profile:XOffset', InputFileData%FF%XOffset, TmpErrStat, TmpErrMsg, Default=0.0_ReKi, From=iSec)
      if (Failed()) return
   end if

   !----------------------------------------------------------------------------------
   ! lidar (required — its parameters feed Lidar_Init unconditionally)
   !----------------------------------------------------------------------------------
   call YamlGetNode(Doc, 'lidar', iSec, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'SensorType', InputFileData%SensorType, TmpErrStat, TmpErrMsg, From=iSec)
   if (Failed()) return
   call YamlGet(Doc, 'NumPulseGate', InputFileData%NumPulseGate, TmpErrStat, TmpErrMsg, From=iSec)
   if (Failed()) return
   call YamlGet(Doc, 'PulseSpacing', InputFileData%PulseSpacing, TmpErrStat, TmpErrMsg, From=iSec)
   if (Failed()) return

   ! NumBeam is derived from the focal-distance list lengths (min 1, as in the text path)
   call YamlGet(Doc, 'FocalDistanceX', InputFileData%FocalDistanceX, TmpErrStat, TmpErrMsg, From=iSec)
   if (Failed()) return
   call YamlGet(Doc, 'FocalDistanceY', InputFileData%FocalDistanceY, TmpErrStat, TmpErrMsg, From=iSec)
   if (Failed()) return
   call YamlGet(Doc, 'FocalDistanceZ', InputFileData%FocalDistanceZ, TmpErrStat, TmpErrMsg, From=iSec)
   if (Failed()) return
   InputFileData%NumBeam = size(InputFileData%FocalDistanceX)
   if (size(InputFileData%FocalDistanceY) /= InputFileData%NumBeam .or. &
       size(InputFileData%FocalDistanceZ) /= InputFileData%NumBeam) then
      call SetErrStat(ErrID_Fatal, 'FocalDistanceX, FocalDistanceY, and FocalDistanceZ must all '// &
         'have the same number of entries.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   if (InputFileData%NumBeam < 1) then
      call SetErrStat(ErrID_Fatal, 'FocalDistanceX must have at least one entry.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call YamlGet(Doc, 'RotorApexOffsetPos', TmpAry, TmpErrStat, TmpErrMsg, From=iSec)
   if (Failed()) return
   if (size(TmpAry) /= 3) then
      call SetErrStat(ErrID_Fatal, 'RotorApexOffsetPos must have exactly 3 entries.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   InputFileData%RotorApexOffsetPos = TmpAry

   call YamlGet(Doc, 'URefLid', InputFileData%URefLid, TmpErrStat, TmpErrMsg, From=iSec)
   if (Failed()) return
   call YamlGet(Doc, 'MeasurementInterval', InputFileData%MeasurementInterval, TmpErrStat, TmpErrMsg, From=iSec)
   if (Failed()) return
   call YamlGet(Doc, 'LidRadialVel', InputFileData%LidRadialVel, TmpErrStat, TmpErrMsg, From=iSec)
   if (Failed()) return
   call YamlGet(Doc, 'ConsiderHubMotion', InputFileData%ConsiderHubMotion, TmpErrStat, TmpErrMsg, From=iSec)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! output (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'output:SumPrint', InputFileData%SumPrint, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'output:OutList', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpList) > MaxOutPts) then
      call SetErrStat(ErrID_Fatal, 'OutList may contain at most '//trim(Num2LStr(MaxOutPts))// &
         ' channels; found '//trim(Num2LStr(size(TmpList)))//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   call AllocAry(InputFileData%OutList, MaxOutPts, "InflowWind Input File's OutList", TmpErrStat, TmpErrMsg)
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

   !> Fatal when the selected WindType requires a section that is absent.
   subroutine RequireSection(ForWindType, SecPresent, SecName)
      integer(IntKi), intent(in) :: ForWindType
      logical,        intent(in) :: SecPresent
      character(*),   intent(in) :: SecName
      if (InputFileData%WindType == ForWindType .and. .not. SecPresent) then
         call SetErrStat(ErrID_Fatal, 'WindType = '//trim(Num2LStr(ForWindType))//' requires the "'// &
            SecName//'" section, which was not found.', ErrStat, ErrMsg, RoutineName)
      end if
   end subroutine RequireSection

end subroutine ParseYamlDoc

end module InflowWind_Yaml
