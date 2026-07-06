!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of ServoDyn.
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
!> Reader for the YAML form of the ServoDyn primary input file. Fills the same
!! SrvD_InputFile structure as ParseInputFileInfo (the text path in ServoDyn_IO.f90), so
!! validation (ValidatePrimaryData) and everything downstream is shared between the two
!! formats.
!!
!! Schema: sections mirror the text file's banners (general, pitch_control,
!! generator_torque_control, simple_variable_speed, simple_induction_generator,
!! thevenin_generator, high_speed_shaft_brake, yaw_control, aero_flow_control,
!! structural_control, cable_control, bladed_interface, torque_speed_lookup, output).
!! Keys keep their documented names.
!!
!! DT and DLL_DT accept the literal scalar "default" exactly like the text format
!! (ParseVarWDefault): DT falls back to the glue/driver-supplied interval, DLL_DT to the
!! just-read DT. The per-blade parameters (PitNeut, PitSpr, PitDamp, TPitManS,
!! PitManRat, BlPitchF) -- three separate keyword lines in the text format -- are
!! 3-entry lists. NumBStC/NumNStC/NumTStC/NumSStC are not YAML keys: they are derived
!! from the lengths of the BStCfiles/NStCfiles/TStCfiles/SStCfiles path lists (StC
!! sub-files are their own file type, always referenced by path -- never inlined).
!! DLL_NumTrq is likewise derived from the (equal) lengths of the GenSpd_TLU/GenTrq_TLU
!! lists, and NumOuts from output:OutList.
!!
!! Unit conversions (deg->rad, rpm->rad/s, %->fraction, N-m/rpm^2->N-m/(rad/s)^2) are
!! applied in the same order and on the same raw values as the text path, so verbatim
!! numeric literals produce bit-identical results. EXavrSWAP is not read (the text
!! parser hard-codes it .TRUE. -- so does this reader).
module ServoDyn_Yaml

   use NWTC_Library
   use YamlInput
   use ServoDyn_Types
   use ServoDyn_IO, only: MaxOutPts

   implicit none
   private

   public :: SrvD_ParseYamlFile
   public :: SrvD_ParseYamlFileInfo

contains

!> Load and parse a YAML-format ServoDyn primary input file. Mirrors the contract of
!! ProcessComFile + ParseInputFileInfo (the text path), including echo handling.
subroutine SrvD_ParseYamlFile(InputFileName, PriPath, RootName, Default_DT, InputFileData, ErrStat, ErrMsg)
   character(*),               intent(in   ) :: InputFileName !< the .yaml primary input file
   character(*),               intent(in   ) :: PriPath       !< path of the primary input file (for relative sub-file paths)
   character(*),               intent(in   ) :: RootName      !< module root name, for the echo file
   real(DbKi),                 intent(in   ) :: Default_DT    !< default DT supplied by the caller (glue code)
   type(SrvD_InputFile),       intent(inout) :: InputFileData !< the shared input-file structure
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SrvD_ParseYamlFile'
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
      write(UnEc, '(A)') 'Echo file for ServoDyn primary input file: '//trim(InputFileName)
      call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   call ParseYamlDoc(Doc, PriPath, Default_DT, InputFileData, TmpErrStat, TmpErrMsg)
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

end subroutine SrvD_ParseYamlFile

!> Parse YAML-format ServoDyn input arriving as a FileInfoType -- the passed-data
!! channel used for inline module input from a YAML primary file (the glue code's
!! inline ServoFile handover when CompServo selects ServoDyn). Per-line provenance in
!! the FileInfoType keeps error messages pointing at the original file and line.
subroutine SrvD_ParseYamlFileInfo(FileInfo, PriPath, RootName, Default_DT, InputFileData, ErrStat, ErrMsg)
   type(FileInfoType),         intent(in   ) :: FileInfo
   character(*),               intent(in   ) :: PriPath
   character(*),               intent(in   ) :: RootName
   real(DbKi),                 intent(in   ) :: Default_DT
   type(SrvD_InputFile),       intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SrvD_ParseYamlFileInfo'
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
      write(UnEc, '(A)') 'Echo file for ServoDyn primary input (passed YAML data)'
      do i = 1, FileInfo%NumLines
         write(UnEc, '(A)') trim(FileInfo%Lines(i))
      end do
   end if

   call ParseYamlDoc(Doc, PriPath, Default_DT, InputFileData, TmpErrStat, TmpErrMsg)
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

end subroutine SrvD_ParseYamlFileInfo

!> Fill InputFileData from a parsed document. Split from the file/FileInfo wrappers so
!! both entry points share it (mirrors the other modules' ParseYamlDoc split).
subroutine ParseYamlDoc(Doc, PriPath, Default_DT, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   character(*),               intent(in   ) :: PriPath
   real(DbKi),                 intent(in   ) :: Default_DT
   type(SrvD_InputFile),       intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'SrvD_ParseYamlDoc'
   character(:), allocatable  :: TmpList(:)
   real(ReKi),   allocatable  :: TmpReAry(:)
   real(DbKi),   allocatable  :: TmpDbAry(:)
   real(ReKi),   allocatable  :: GenSpdTmp(:)
   real(ReKi),   allocatable  :: GenTrqTmp(:)
   integer(IntKi)             :: i
   integer(IntKi)             :: TmpErrStat
   character(ErrMsgLen)       :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   !----------------------------------------------------------------------------------
   ! general (required) -- DT accepts "default" (= the glue/driver-supplied interval)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'general:Echo', InputFileData%Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return
   call YamlGet(Doc, 'general:DT', InputFileData%DT, TmpErrStat, TmpErrMsg, Default=Default_DT)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! pitch_control (required) -- per-blade values are 3-entry lists (the text format's
   ! PitNeut(1..3) etc.); conversions deg->rad match the text path
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'pitch_control:PCMode', InputFileData%PCMode, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'pitch_control:TPCOn', InputFileData%TPCOn, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'pitch_control:PitNeut', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), size(InputFileData%PitNeut), 'pitch_control:PitNeut')) return
   InputFileData%PitNeut = TmpReAry*D2R

   call YamlGet(Doc, 'pitch_control:PitSpr', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), size(InputFileData%PitSpr), 'pitch_control:PitSpr')) return
   InputFileData%PitSpr = TmpReAry

   call YamlGet(Doc, 'pitch_control:PitDamp', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), size(InputFileData%PitDamp), 'pitch_control:PitDamp')) return
   InputFileData%PitDamp = TmpReAry

   call YamlGet(Doc, 'pitch_control:TPitManS', TmpDbAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpDbAry), size(InputFileData%TPitManS), 'pitch_control:TPitManS')) return
   InputFileData%TPitManS = TmpDbAry

   call YamlGet(Doc, 'pitch_control:PitManRat', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), size(InputFileData%PitManRat), 'pitch_control:PitManRat')) return
   InputFileData%PitManRat = TmpReAry*D2R

   call YamlGet(Doc, 'pitch_control:BlPitchF', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), size(InputFileData%BlPitchF), 'pitch_control:BlPitchF')) return
   InputFileData%BlPitchF = TmpReAry*D2R

   !----------------------------------------------------------------------------------
   ! generator_torque_control (required) -- GenEff %->fraction, SpdGenOn rpm->rad/s
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'generator_torque_control:VSContrl', InputFileData%VSContrl, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'generator_torque_control:GenModel', InputFileData%GenModel, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'generator_torque_control:GenEff', InputFileData%GenEff, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%GenEff = InputFileData%GenEff*0.01
   call YamlGet(Doc, 'generator_torque_control:GenTiStr', InputFileData%GenTiStr, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'generator_torque_control:GenTiStp', InputFileData%GenTiStp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'generator_torque_control:SpdGenOn', InputFileData%SpdGenOn, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%SpdGenOn = InputFileData%SpdGenOn*RPM2RPS
   call YamlGet(Doc, 'generator_torque_control:TimGenOn', InputFileData%TimGenOn, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'generator_torque_control:TimGenOf', InputFileData%TimGenOf, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! simple_variable_speed (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'simple_variable_speed:VS_RtGnSp', InputFileData%VS_RtGnSp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%VS_RtGnSp = InputFileData%VS_RtGnSp*RPM2RPS
   call YamlGet(Doc, 'simple_variable_speed:VS_RtTq', InputFileData%VS_RtTq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'simple_variable_speed:VS_Rgn2K', InputFileData%VS_Rgn2K, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%VS_Rgn2K = InputFileData%VS_Rgn2K/( RPM2RPS**2 )
   call YamlGet(Doc, 'simple_variable_speed:VS_SlPc', InputFileData%VS_SlPc, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%VS_SlPc = InputFileData%VS_SlPc*.01

   !----------------------------------------------------------------------------------
   ! simple_induction_generator (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'simple_induction_generator:SIG_SlPc', InputFileData%SIG_SlPc, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%SIG_SlPc = InputFileData%SIG_SlPc*.01
   call YamlGet(Doc, 'simple_induction_generator:SIG_SySp', InputFileData%SIG_SySp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%SIG_SySp = InputFileData%SIG_SySp*RPM2RPS
   call YamlGet(Doc, 'simple_induction_generator:SIG_RtTq', InputFileData%SIG_RtTq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'simple_induction_generator:SIG_PORt', InputFileData%SIG_PORt, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! thevenin_generator (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'thevenin_generator:TEC_Freq', InputFileData%TEC_Freq, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'thevenin_generator:TEC_NPol', InputFileData%TEC_NPol, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'thevenin_generator:TEC_SRes', InputFileData%TEC_SRes, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'thevenin_generator:TEC_RRes', InputFileData%TEC_RRes, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'thevenin_generator:TEC_VLL', InputFileData%TEC_VLL, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'thevenin_generator:TEC_SLR', InputFileData%TEC_SLR, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'thevenin_generator:TEC_RLR', InputFileData%TEC_RLR, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'thevenin_generator:TEC_MR', InputFileData%TEC_MR, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! high_speed_shaft_brake (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'high_speed_shaft_brake:HSSBrMode', InputFileData%HSSBrMode, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'high_speed_shaft_brake:THSSBrDp', InputFileData%THSSBrDp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'high_speed_shaft_brake:HSSBrDT', InputFileData%HSSBrDT, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'high_speed_shaft_brake:HSSBrTqF', InputFileData%HSSBrTqF, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! yaw_control (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'yaw_control:YCMode', InputFileData%YCMode, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'yaw_control:TYCOn', InputFileData%TYCOn, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'yaw_control:YawNeut', InputFileData%YawNeut, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%YawNeut = InputFileData%YawNeut*D2R
   call YamlGet(Doc, 'yaw_control:YawSpr', InputFileData%YawSpr, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'yaw_control:YawDamp', InputFileData%YawDamp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'yaw_control:TYawManS', InputFileData%TYawManS, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'yaw_control:YawManRat', InputFileData%YawManRat, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%YawManRat = InputFileData%YawManRat*D2R
   call YamlGet(Doc, 'yaw_control:NacYawF', InputFileData%NacYawF, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%NacYawF = InputFileData%NacYawF*D2R

   !----------------------------------------------------------------------------------
   ! aero_flow_control (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'aero_flow_control:AfCmode', InputFileData%AfCmode, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'aero_flow_control:AfC_Mean', InputFileData%AfC_mean, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'aero_flow_control:AfC_Amp', InputFileData%AfC_Amp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'aero_flow_control:AfC_Phase', InputFileData%AfC_Phase, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%AfC_phase = InputFileData%AfC_phase*D2R

   !----------------------------------------------------------------------------------
   ! structural_control (required) -- NumBStC/NumNStC/NumTStC/NumSStC derive from the
   ! path-list lengths; relative paths resolve against the primary input file, exactly
   ! as the text path. StC files are their own file type -- never inlined here.
   !----------------------------------------------------------------------------------
   call GetStCFileList('structural_control:BStCfiles', InputFileData%BStCfiles, InputFileData%NumBStC)
   if (ErrStat >= AbortErrLev) return
   call GetStCFileList('structural_control:NStCfiles', InputFileData%NStCfiles, InputFileData%NumNStC)
   if (ErrStat >= AbortErrLev) return
   call GetStCFileList('structural_control:TStCfiles', InputFileData%TStCfiles, InputFileData%NumTStC)
   if (ErrStat >= AbortErrLev) return
   call GetStCFileList('structural_control:SStCfiles', InputFileData%SStCfiles, InputFileData%NumSStC)
   if (ErrStat >= AbortErrLev) return

   !----------------------------------------------------------------------------------
   ! cable_control (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'cable_control:CCmode', InputFileData%CCmode, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! bladed_interface (required) -- DLL_DT accepts "default" (= DT, just read);
   ! EXavrSWAP is hard-coded .TRUE. exactly like the text path (never read)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'bladed_interface:DLL_FileName', InputFileData%DLL_FileName, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if ( PathIsRelative( InputFileData%DLL_FileName ) ) InputFileData%DLL_FileName = trim(PriPath)//trim(InputFileData%DLL_FileName)
   call YamlGet(Doc, 'bladed_interface:DLL_InFile', InputFileData%DLL_InFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if ( PathIsRelative( InputFileData%DLL_InFile ) ) InputFileData%DLL_InFile = trim(PriPath)//trim(InputFileData%DLL_InFile)
   call YamlGet(Doc, 'bladed_interface:DLL_ProcName', InputFileData%DLL_ProcName, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'bladed_interface:DLL_DT', InputFileData%DLL_DT, TmpErrStat, TmpErrMsg, Default=InputFileData%DT)
   if (Failed()) return
   call YamlGet(Doc, 'bladed_interface:DLL_Ramp', InputFileData%DLL_Ramp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'bladed_interface:BPCutoff', InputFileData%BPCutoff, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%EXavrSWAP = .TRUE.    ! Hard coded, mirroring the text path (the read is commented out there)
   call YamlGet(Doc, 'bladed_interface:NacYaw_North', InputFileData%NacYaw_North, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%NacYaw_North = InputFileData%NacYaw_North*D2R
   call YamlGet(Doc, 'bladed_interface:Ptch_Cntrl', InputFileData%Ptch_Cntrl, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'bladed_interface:Ptch_SetPnt', InputFileData%Ptch_SetPnt, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%Ptch_SetPnt = InputFileData%Ptch_SetPnt*D2R
   call YamlGet(Doc, 'bladed_interface:Ptch_Min', InputFileData%Ptch_Min, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%Ptch_Min = InputFileData%Ptch_Min*D2R
   call YamlGet(Doc, 'bladed_interface:Ptch_Max', InputFileData%Ptch_Max, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%Ptch_Max = InputFileData%Ptch_Max*D2R
   call YamlGet(Doc, 'bladed_interface:PtchRate_Min', InputFileData%PtchRate_Min, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%PtchRate_Min = InputFileData%PtchRate_Min*D2R
   call YamlGet(Doc, 'bladed_interface:PtchRate_Max', InputFileData%PtchRate_Max, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%PtchRate_Max = InputFileData%PtchRate_Max*D2R
   call YamlGet(Doc, 'bladed_interface:Gain_OM', InputFileData%Gain_OM, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'bladed_interface:GenSpd_MinOM', InputFileData%GenSpd_MinOM, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%GenSpd_MinOM = InputFileData%GenSpd_MinOM*RPM2RPS
   call YamlGet(Doc, 'bladed_interface:GenSpd_MaxOM', InputFileData%GenSpd_MaxOM, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%GenSpd_MaxOM = InputFileData%GenSpd_MaxOM*RPM2RPS
   call YamlGet(Doc, 'bladed_interface:GenSpd_Dem', InputFileData%GenSpd_Dem, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%GenSpd_Dem = InputFileData%GenSpd_Dem*RPM2RPS
   call YamlGet(Doc, 'bladed_interface:GenTrq_Dem', InputFileData%GenTrq_Dem, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'bladed_interface:GenPwr_Dem', InputFileData%GenPwr_Dem, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! torque_speed_lookup (required) -- DLL_NumTrq derives from the (equal) lengths of
   ! GenSpd_TLU/GenTrq_TLU; rpm->rad/s conversion of GenSpd_TLU matches the text path
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'torque_speed_lookup:GenSpd_TLU', GenSpdTmp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'torque_speed_lookup:GenTrq_TLU', GenTrqTmp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(GenSpdTmp) /= size(GenTrqTmp)) then
      call SetErrStat(ErrID_Fatal, 'torque_speed_lookup:GenSpd_TLU and torque_speed_lookup:GenTrq_TLU must have '// &
         'the same number of entries.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   InputFileData%DLL_NumTrq = size(GenSpdTmp)
   if (InputFileData%DLL_NumTrq > 0) then
      call AllocAry( InputFileData%GenSpd_TLU, InputFileData%DLL_NumTrq, 'GenSpd_TLU', TmpErrStat, TmpErrMsg )
      if (Failed()) return
      call AllocAry( InputFileData%GenTrq_TLU, InputFileData%DLL_NumTrq, 'GenTrq_TLU', TmpErrStat, TmpErrMsg )
      if (Failed()) return
      InputFileData%GenSpd_TLU = GenSpdTmp*RPM2RPS
      InputFileData%GenTrq_TLU = GenTrqTmp
   end if

   !----------------------------------------------------------------------------------
   ! output (required) -- NumOuts derives from the OutList length
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'output:SumPrint', InputFileData%SumPrint, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'output:OutFile', InputFileData%OutFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'output:TabDelim', InputFileData%TabDelim, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'output:OutFmt', InputFileData%OutFmt, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'output:TStart', InputFileData%TStart, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'output:OutList', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpList) > MaxOutPts) then
      call SetErrStat(ErrID_Fatal, 'OutList may contain at most '//trim(Num2LStr(MaxOutPts))// &
         ' channels; found '//trim(Num2LStr(size(TmpList)))//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   call AllocAry(InputFileData%OutList, MaxOutPts, "ServoDyn Input File's Outlist", TmpErrStat, TmpErrMsg)
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

   !> Fatal when a fixed-size per-blade list has the wrong number of entries.
   logical function BadSize(NGiven, NExpect, Path)
      integer(IntKi), intent(in) :: NGiven
      integer(IntKi), intent(in) :: NExpect
      character(*),   intent(in) :: Path
      BadSize = (NGiven /= NExpect)
      if (BadSize) call SetErrStat(ErrID_Fatal, '"'//Path//'" must have exactly '//trim(Num2LStr(NExpect))// &
         ' entries (one per blade slot, matching the text format); found '//trim(Num2LStr(NGiven))//'.', &
         ErrStat, ErrMsg, RoutineName)
   end function BadSize

   !> One StC path list: the count derives from the list length; relative paths are
   !! resolved against the primary input file, exactly like the text path.
   subroutine GetStCFileList(Path, Files, Num)
      character(*),                   intent(in   ) :: Path
      character(1024), allocatable,   intent(inout) :: Files(:)
      integer(IntKi),                 intent(  out) :: Num

      integer(IntKi) :: k

      call YamlGet(Doc, Path, TmpList, TmpErrStat, TmpErrMsg)
      if (Failed()) return
      Num = size(TmpList)
      call AllocAry( Files, Num, Path, TmpErrStat, TmpErrMsg )
      if (Failed()) return
      do k = 1, Num
         Files(k) = TmpList(k)
         if ( PathIsRelative( Files(k) ) ) Files(k) = trim(PriPath)//trim(Files(k))
      end do
   end subroutine GetStCFileList

end subroutine ParseYamlDoc

end module ServoDyn_Yaml
