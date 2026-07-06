!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of AeroDisk
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
!> Reader for the YAML form of the AeroDisk primary input file. Fills the same
!! ADsk_InputFile structure as ADsk_ParsePrimaryFileData (the text path), so validation
!! and everything downstream is shared between the two formats.
!!
!! Schema: sections mirror the text file's banners (general, environmental_conditions,
!! actuator_disk, output). The Actuator Disk Properties table (up to five index
!! variables -- TSR, RtSpd, VRel, Pitch, Skew -- selecting six coefficients C_Fx, C_Fy,
!! C_Fz, C_Mx, C_My, C_Mz) is represented as a documented columns:/rows: table under
!! actuator_disk:table:
!!    columns - the names of the index columns actually present (any subset/order of
!!              "TSR", "RtSpd", "VRel", "Pitch", "Skew"), mirroring the text format's
!!              InColNames.
!!    dims    - the number of unique values expected for each column in `columns`
!!              (same length/order), mirroring InColDims; N_TSR/N_RtSpd/N_VRel/N_Pitch/
!!              N_Skew are derived from this list rather than being separate inputs.
!!    rows    - the full data grid: one row per index combination, each row holding the
!!              `columns` values (in that order) followed by the six fixed coefficient
!!              columns C_Fx, C_Fy, C_Fz, C_Mx, C_My, C_Mz -- exactly the column layout
!!              of the text-format table body.
!! DT, AirDens, and RotorRad accept the literal scalar "default" exactly like the text
!! format (YamlGet's Default= handling recognizes the keyword), falling back to the
!! interval/defAirDens/RotorRad values supplied by the caller. SumPrint is not read (the
!! text-format parser never reads it either -- ADsk_InputFile%SumPrint stays at its
!! type-default .false. on both paths).
module AeroDisk_Yaml

   use NWTC_Library
   use YamlInput
   use AeroDisk_Types
   use AeroDisk_IO, only: TableIndexType, UniqueRealValues

   implicit none
   private

   public :: ADsk_ParseYamlFile
   public :: ADsk_ParseYamlFileInfo

   integer(IntKi), parameter :: MaxOutPts = 34   ! same limit as AeroDisk_Output_Params

contains

!> Load and parse a YAML-format AeroDisk primary input file. Mirrors the contract of
!! ProcessComFile + ADsk_ParsePrimaryFileData (the text path), including echo handling.
subroutine ADsk_ParseYamlFile(InputFileName, InitInp, RootName, interval, InputFileData, ErrStat, ErrMsg)
   character(*),               intent(in   ) :: InputFileName !< the .yaml primary input file
   type(ADsk_InitInputType),   intent(in   ) :: InitInp       !< Init input (RotorRad/defAirDens defaults)
   character(*),               intent(in   ) :: RootName      !< module root name (already includes ".ADsk"), for the echo file
   real(DbKi),                 intent(in   ) :: interval      !< default DT supplied by the caller
   type(ADsk_InputFile),       intent(inout) :: InputFileData !< the shared input-file structure
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ADsk_ParseYamlFile'
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
      write(UnEc, '(A)') 'Echo file for AeroDisk primary input file: '//trim(InputFileName)
      call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   call ParseYamlDoc(Doc, InitInp, interval, InputFileData, TmpErrStat, TmpErrMsg)
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

end subroutine ADsk_ParseYamlFile

!> Parse YAML-format AeroDisk input arriving as a FileInfoType -- the passed-data
!! channel used for inline module input from a YAML primary file (the glue code's
!! inline AeroFile handover). Per-line provenance in the FileInfoType keeps error
!! messages pointing at the original file and line.
subroutine ADsk_ParseYamlFileInfo(FileInfo, InitInp, RootName, interval, InputFileData, ErrStat, ErrMsg)
   type(FileInfoType),         intent(in   ) :: FileInfo
   type(ADsk_InitInputType),   intent(in   ) :: InitInp
   character(*),               intent(in   ) :: RootName
   real(DbKi),                 intent(in   ) :: interval
   type(ADsk_InputFile),       intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ADsk_ParseYamlFileInfo'
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
      write(UnEc, '(A)') 'Echo file for AeroDisk primary input (passed YAML data)'
      do i = 1, FileInfo%NumLines
         write(UnEc, '(A)') trim(FileInfo%Lines(i))
      end do
   end if

   call ParseYamlDoc(Doc, InitInp, interval, InputFileData, TmpErrStat, TmpErrMsg)
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

end subroutine ADsk_ParseYamlFileInfo

!> Fill InputFileData from a parsed document. Split from the file/FileInfo wrappers so
!! both entry points share it (mirrors InflowWind_Yaml's ParseYamlDoc split).
subroutine ParseYamlDoc(Doc, InitInp, interval, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(ADsk_InitInputType),   intent(in   ) :: InitInp
   real(DbKi),                 intent(in   ) :: interval
   type(ADsk_InputFile),       intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'ADsk_ParseYamlDoc'
   character(:), allocatable  :: TmpList(:)
   character(:), allocatable  :: NameList(:)
   integer(IntKi), allocatable :: DimList(:)
   real(SiKi), allocatable    :: Mat(:,:)
   type(TableIndexType)       :: TabIdx
   integer(IntKi)             :: iSec
   integer(IntKi)             :: NumCols
   integer(IntKi)             :: i
   integer(IntKi)             :: TmpErrStat
   character(ErrMsgLen)       :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   !----------------------------------------------------------------------------------
   ! general (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'general:Echo', InputFileData%Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return
   call YamlGet(Doc, 'general:DT', InputFileData%DT, TmpErrStat, TmpErrMsg, Default=interval)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! environmental_conditions (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'environmental_conditions:AirDens', InputFileData%AirDens, TmpErrStat, TmpErrMsg, &
                Default=InitInp%defAirDens)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! actuator_disk (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'actuator_disk:RotorRad', InputFileData%RotorRad, TmpErrStat, TmpErrMsg, Default=InitInp%RotorRad)
   if (Failed()) return

   call YamlGetNode(Doc, 'actuator_disk:table', iSec, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'columns', NameList, TmpErrStat, TmpErrMsg, From=iSec)
   if (Failed()) return
   call YamlGet(Doc, 'dims', DimList, TmpErrStat, TmpErrMsg, From=iSec)
   if (Failed()) return
   if (size(DimList) /= size(NameList)) then
      call SetErrStat(ErrID_Fatal, 'actuator_disk:table:columns and actuator_disk:table:dims must have '// &
         'the same number of entries.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call ResolveColumns(NameList, DimList, TabIdx, InputFileData%AeroTable, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'rows', Mat, TmpErrStat, TmpErrMsg, From=iSec)
   if (Failed()) return

   NumCols = size(NameList) + 6
   if (size(Mat,2) /= NumCols) then
      call SetErrStat(ErrID_Fatal, 'actuator_disk:table:rows must have '//trim(Num2LStr(NumCols))// &
         ' columns (the '//trim(Num2LStr(size(NameList)))//' named index column(s) in "columns" plus the '// &
         '6 fixed coefficient columns C_Fx, C_Fy, C_Fz, C_Mx, C_My, C_Mz); found '// &
         trim(Num2LStr(size(Mat,2)))//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call PopulateTable(TabIdx, Mat, InputFileData%AeroTable, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! unit conversions, matching the text path exactly (AeroDisk_IO.f90's Get_RtAeroTableData):
   ! RtSpd rpm->rad/s, Pitch and Skew deg->rad. TSR and VRel are unconverted.
   if (InputFileData%AeroTable%N_RtSpd > 0_IntKi) &
      InputFileData%AeroTable%RtSpd = (InputFileData%AeroTable%RtSpd * Pi_S)/30.0_SiKi
   if (InputFileData%AeroTable%N_Pitch > 0_IntKi) &
      InputFileData%AeroTable%Pitch = (InputFileData%AeroTable%Pitch * Pi_S)/180.0_SiKi
   if (InputFileData%AeroTable%N_Skew  > 0_IntKi) &
      InputFileData%AeroTable%Skew  = (InputFileData%AeroTable%Skew  * Pi_S)/180.0_SiKi

   !----------------------------------------------------------------------------------
   ! output (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'output:OutList', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpList) > MaxOutPts) then
      call SetErrStat(ErrID_Fatal, 'OutList may contain at most '//trim(Num2LStr(MaxOutPts))// &
         ' channels; found '//trim(Num2LStr(size(TmpList)))//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   call AllocAry(InputFileData%OutList, MaxOutPts, "AeroDisk Input File's OutList", TmpErrStat, TmpErrMsg)
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

!> Upper-case helper (Conv2UC is a subroutine; CASE needs a function form).
function UC(Str) result(Out)
   character(*), intent(in) :: Str
   character(len(Str))      :: Out
   Out = Str
   call Conv2UC(Out)
end function UC

!> Resolve actuator_disk:table:columns/dims into the same TableIndexType + per-DOF
!! counts that the text-format parser computes in Get_InColNames/Get_InColDims
!! (AeroDisk_IO.f90), so PopulateTable can reuse the identical placement logic.
subroutine ResolveColumns(NameList, DimList, TabIdx, AeroTable, ErrStat, ErrMsg)
   character(*),            intent(in   ) :: NameList(:)
   integer(IntKi),          intent(in   ) :: DimList(:)
   type(TableIndexType),    intent(  out) :: TabIdx
   type(ADsk_AeroTable),    intent(inout) :: AeroTable
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ResolveColumns'
   integer(IntKi)          :: i

   ErrStat = ErrID_None
   ErrMsg  = ''
   TabIdx%ColTSR   = 0_IntKi
   TabIdx%ColRtSpd = 0_IntKi
   TabIdx%ColVRel  = 0_IntKi
   TabIdx%ColSkew  = 0_IntKi
   TabIdx%ColPitch = 0_IntKi
   AeroTable%N_TSR   = 0_IntKi
   AeroTable%N_RtSpd = 0_IntKi
   AeroTable%N_VRel  = 0_IntKi
   AeroTable%N_Pitch = 0_IntKi
   AeroTable%N_Skew  = 0_IntKi

   do i = 1, size(NameList)
      select case (trim(UC(trim(NameList(i)))))
      case ('TSR')
         call SetCol(TabIdx%ColTSR,   AeroTable%N_TSR,   'TSR')
      case ('RTSPD')
         call SetCol(TabIdx%ColRtSpd, AeroTable%N_RtSpd, 'RtSpd')
      case ('VREL')
         call SetCol(TabIdx%ColVRel,  AeroTable%N_VRel,  'VRel')
      case ('SKEW')
         call SetCol(TabIdx%ColSkew,  AeroTable%N_Skew,  'Skew')
      case ('PITCH')
         call SetCol(TabIdx%ColPitch, AeroTable%N_Pitch, 'Pitch')
      case default
         call SetErrStat(ErrID_Fatal, 'actuator_disk:table:columns entry "'//trim(NameList(i))// &
            '" is not a recognized column name (expected some combination of "TSR", "RtSpd", '// &
            '"VRel", "Pitch", "Skew").', ErrStat, ErrMsg, RoutineName)
      end select
      if (ErrStat >= AbortErrLev) return
   end do

   TabIdx%NumColNamesGiven = size(NameList)

   if ((TabIdx%ColTSR + TabIdx%ColRtSpd + TabIdx%ColVRel + TabIdx%ColSkew + TabIdx%ColPitch) <= 0_IntKi) then
      call SetErrStat(ErrID_Fatal, 'actuator_disk:table:columns must include at least one of '// &
         '"TSR", "RtSpd", "VRel", "Pitch", or "Skew".', ErrStat, ErrMsg, RoutineName)
      return
   end if

   if (AeroTable%N_TSR   < 0_IntKi) call SetErrStat(ErrID_Fatal, &
      'actuator_disk:table:dims entry for "TSR" must not be negative.',   ErrStat, ErrMsg, RoutineName)
   if (AeroTable%N_RtSpd < 0_IntKi) call SetErrStat(ErrID_Fatal, &
      'actuator_disk:table:dims entry for "RtSpd" must not be negative.', ErrStat, ErrMsg, RoutineName)
   if (AeroTable%N_VRel  < 0_IntKi) call SetErrStat(ErrID_Fatal, &
      'actuator_disk:table:dims entry for "VRel" must not be negative.',  ErrStat, ErrMsg, RoutineName)
   if (AeroTable%N_Skew  < 0_IntKi) call SetErrStat(ErrID_Fatal, &
      'actuator_disk:table:dims entry for "Skew" must not be negative.',  ErrStat, ErrMsg, RoutineName)
   if (AeroTable%N_Pitch < 0_IntKi) call SetErrStat(ErrID_Fatal, &
      'actuator_disk:table:dims entry for "Pitch" must not be negative.', ErrStat, ErrMsg, RoutineName)

contains

   subroutine SetCol(ColOut, NOut, Nm)
      integer(IntKi), intent(inout) :: ColOut
      integer(IntKi), intent(  out) :: NOut
      character(*),   intent(in   ) :: Nm
      if (ColOut /= 0_IntKi) then
         call SetErrStat(ErrID_Fatal, 'actuator_disk:table:columns lists "'//trim(Nm)//'" more than once.', &
            ErrStat, ErrMsg, RoutineName)
         return
      end if
      ColOut = i
      NOut   = DimList(i)
   end subroutine SetCol

end subroutine ResolveColumns

!> Scatter the raw (NumRows x NumCols) table matrix into the dense 5-D AeroTable arrays,
!! mirroring PopulateAeroTabs/CheckAeroTabs in AeroDisk_IO.f90's Get_RtAeroTableData
!! (same UniqueRealValues/LocateStp placement algorithm and completeness check).
subroutine PopulateTable(TabIdx, Mat, AeroTable, ErrStat, ErrMsg)
   type(TableIndexType), intent(in   ) :: TabIdx
   real(SiKi),           intent(in   ) :: Mat(:,:)
   type(ADsk_AeroTable), intent(inout) :: AeroTable
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'PopulateTable'
   integer(IntKi)          :: NumRows, NumCols
   integer(IntKi)          :: ErrStat2
   character(ErrMsgLen)    :: ErrMsg2
   logical, allocatable    :: Mask(:,:,:,:,:)
   integer(IntKi)          :: Sz(5)
   integer(IntKi)          :: row
   integer(IntKi)          :: iTSR, iRtSpd, iVRel, iPitch, iSkew
   real(SiKi)              :: TmpR6(6)

   ErrStat = ErrID_None
   ErrMsg  = ''

   NumRows = size(Mat,1)
   NumCols = size(Mat,2)

   ! unique sorted index values per active column
   if (AeroTable%N_TSR   > 0_IntKi) then
      call GetTabIndexVals( Mat(:,TabIdx%ColTSR  ),'TSR'  ,AeroTable%N_TSR  ,AeroTable%TSR  , ErrStat2, ErrMsg2)
      if (Failed()) return
   end if
   if (AeroTable%N_RtSpd > 0_IntKi) then
      call GetTabIndexVals( Mat(:,TabIdx%ColRtSpd),'RtSpd',AeroTable%N_RtSpd,AeroTable%RtSpd, ErrStat2, ErrMsg2)
      if (Failed()) return
   end if
   if (AeroTable%N_VRel  > 0_IntKi) then
      call GetTabIndexVals( Mat(:,TabIdx%ColVRel ),'VRel' ,AeroTable%N_VRel ,AeroTable%VRel , ErrStat2, ErrMsg2)
      if (Failed()) return
   end if
   if (AeroTable%N_Pitch > 0_IntKi) then
      call GetTabIndexVals( Mat(:,TabIdx%ColPitch),'Pitch',AeroTable%N_Pitch,AeroTable%Pitch, ErrStat2, ErrMsg2)
      if (Failed()) return
   end if
   if (AeroTable%N_Skew  > 0_IntKi) then
      call GetTabIndexVals( Mat(:,TabIdx%ColSkew ),'Skew' ,AeroTable%N_Skew ,AeroTable%Skew , ErrStat2, ErrMsg2)
      if (Failed()) return
   end if

   Sz(1) = max(AeroTable%N_TSR,  1_IntKi)
   Sz(2) = max(AeroTable%N_RtSpd,1_IntKi)
   Sz(3) = max(AeroTable%N_VRel, 1_IntKi)
   Sz(4) = max(AeroTable%N_Pitch,1_IntKi)
   Sz(5) = max(AeroTable%N_Skew, 1_IntKi)

   if (NumRows /= Sz(1)*Sz(2)*Sz(3)*Sz(4)*Sz(5)) then
      call SetErrStat(ErrID_Fatal, 'actuator_disk:table:rows has '//trim(Num2LStr(NumRows))//' row(s); expected '// &
         trim(Num2LStr(Sz(1)*Sz(2)*Sz(3)*Sz(4)*Sz(5)))//' (the product of the unique-value counts in each '// &
         'active index column).', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call AllocAry(AeroTable%C_Fx, Sz(1),Sz(2),Sz(3),Sz(4),Sz(5), 'AeroTable%C_Fx', ErrStat2, ErrMsg2)
   if (Failed()) return; AeroTable%C_Fx = 0.0_SiKi
   call AllocAry(AeroTable%C_Fy, Sz(1),Sz(2),Sz(3),Sz(4),Sz(5), 'AeroTable%C_Fy', ErrStat2, ErrMsg2)
   if (Failed()) return; AeroTable%C_Fy = 0.0_SiKi
   call AllocAry(AeroTable%C_Fz, Sz(1),Sz(2),Sz(3),Sz(4),Sz(5), 'AeroTable%C_Fz', ErrStat2, ErrMsg2)
   if (Failed()) return; AeroTable%C_Fz = 0.0_SiKi
   call AllocAry(AeroTable%C_Mx, Sz(1),Sz(2),Sz(3),Sz(4),Sz(5), 'AeroTable%C_Mx', ErrStat2, ErrMsg2)
   if (Failed()) return; AeroTable%C_Mx = 0.0_SiKi
   call AllocAry(AeroTable%C_My, Sz(1),Sz(2),Sz(3),Sz(4),Sz(5), 'AeroTable%C_My', ErrStat2, ErrMsg2)
   if (Failed()) return; AeroTable%C_My = 0.0_SiKi
   call AllocAry(AeroTable%C_Mz, Sz(1),Sz(2),Sz(3),Sz(4),Sz(5), 'AeroTable%C_Mz', ErrStat2, ErrMsg2)
   if (Failed()) return; AeroTable%C_Mz = 0.0_SiKi

   allocate(Mask(Sz(1),Sz(2),Sz(3),Sz(4),Sz(5)), STAT=ErrStat2)
   if (ErrStat2 /= 0) then
      call SetErrStat(ErrID_Fatal, 'Could not allocate array for data mask', ErrStat, ErrMsg, RoutineName)
      return
   end if
   Mask = .false.

   do row = 1, NumRows
      TmpR6(1:6) = Mat(row, NumCols-5:NumCols)
      iTSR   = 1_IntKi
      iRtSpd = 1_IntKi
      iVRel  = 1_IntKi
      iPitch = 1_IntKi
      iSkew  = 1_IntKi
      if (AeroTable%N_TSR   > 0_IntKi) call LocateStp( Mat(row,TabIdx%ColTSR  ),AeroTable%TSR  ,iTSR  ,AeroTable%N_TSR  )
      if (AeroTable%N_RtSpd > 0_IntKi) call LocateStp( Mat(row,TabIdx%ColRtSpd),AeroTable%RtSpd,iRtSpd,AeroTable%N_RtSpd)
      if (AeroTable%N_VRel  > 0_IntKi) call LocateStp( Mat(row,TabIdx%ColVRel ),AeroTable%VRel ,iVRel ,AeroTable%N_VRel )
      if (AeroTable%N_Pitch > 0_IntKi) call LocateStp( Mat(row,TabIdx%ColPitch),AeroTable%Pitch,iPitch,AeroTable%N_Pitch)
      if (AeroTable%N_Skew  > 0_IntKi) call LocateStp( Mat(row,TabIdx%ColSkew ),AeroTable%Skew ,iSkew ,AeroTable%N_Skew )
      AeroTable%C_Fx(iTSR,iRtSpd,iVRel,iPitch,iSkew) = TmpR6(1)
      AeroTable%C_Fy(iTSR,iRtSpd,iVRel,iPitch,iSkew) = TmpR6(2)
      AeroTable%C_Fz(iTSR,iRtSpd,iVRel,iPitch,iSkew) = TmpR6(3)
      AeroTable%C_Mx(iTSR,iRtSpd,iVRel,iPitch,iSkew) = TmpR6(4)
      AeroTable%C_My(iTSR,iRtSpd,iVRel,iPitch,iSkew) = TmpR6(5)
      AeroTable%C_Mz(iTSR,iRtSpd,iVRel,iPitch,iSkew) = TmpR6(6)
      if (Mask(iTSR,iRtSpd,iVRel,iPitch,iSkew)) then
         call SetErrStat(ErrID_Fatal, 'Duplicate data entry in "actuator_disk:table:rows" row '// &
            trim(Num2LStr(row))//'.', ErrStat, ErrMsg, RoutineName)
         return
      else
         Mask(iTSR,iRtSpd,iVRel,iPitch,iSkew) = .true.
      end if
   end do

   if (.not. all(Mask)) then
      call SetErrStat(ErrID_Fatal, 'Data missing from "actuator_disk:table:rows" (not every combination of '// &
         'the active index columns is present).', ErrStat, ErrMsg, RoutineName)
      return
   end if

contains

   logical function Failed()
      call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

   subroutine GetTabIndexVals(TabCol, ColName, NumExpect, UniqueArray, ErrStat3, ErrMsg3)
      real(SiKi),              intent(in   ) :: TabCol(:)
      character(*),            intent(in   ) :: ColName
      integer(IntKi),          intent(in   ) :: NumExpect
      real(SiKi), allocatable, intent(  out) :: UniqueArray(:)
      integer(IntKi),          intent(  out) :: ErrStat3
      character(ErrMsgLen),    intent(  out) :: ErrMsg3
      integer(IntKi) :: NumFound
      call UniqueRealValues( TabCol, UniqueArray, NumFound, ErrStat3, ErrMsg3 )
      if (NumExpect /= NumFound) then
         call SetErrStat(ErrID_Fatal, 'Expecting '//trim(Num2LStr(NumExpect))//' unique '//ColName// &
            ' entries in "actuator_disk:table:rows", but found '//trim(Num2LStr(NumFound))//' instead.', &
            ErrStat3, ErrMsg3, '')
      end if
   end subroutine GetTabIndexVals

end subroutine PopulateTable

end module AeroDisk_Yaml
