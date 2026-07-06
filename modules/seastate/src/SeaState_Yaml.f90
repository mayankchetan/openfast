!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of SeaState.
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
!> Reader for the YAML form of the SeaState primary input file. Fills the same
!! SeaSt_InputFile structure as SeaSt_ParseInput (the text path), so validation
!! (SeaStateInput_ProcessInitData) and everything downstream is shared between the two
!! formats.
!!
!! Schema: sections mirror the text file's banners (general, environmental_conditions,
!! spatial_discretization, waves, second_order_waves, constrained_waves, current,
!! maccamy_fuchs, output, output_channels).
!!
!! WtrDens, WtrDpth, and MSL2SWL accept the literal scalar "default" exactly like the
!! text format (falling back to the driver-supplied defWtrDens/defWtrDpth/defMSL2SWL).
!! Z_Depth also accepts "default", falling back to the computed value WtrDpth+MSL2SWL
!! (using the just-read WtrDpth/MSL2SWL), exactly as the text path's
!! ParseVarWDefault(..., InputFileData%WtrDpth+InputFileData%MSL2SWL, ...) does.
!! WavePkShp accepts "default", falling back to WavePkShpDefault(WaveMod, WaveHs, WaveTp)
!! -- also computed from just-read values, exactly as the text path.
!!
!! WaveMod is a variant field: either a plain integer (0-7) or the string "1P#" (a
!! regular wave with a user-specified phase, in degrees, converted to radians here just
!! as the text path does). WaveSeed2 is likewise variant: either an integer seed or the
!! string "RANLUX" naming an alternative generator -- both mirror the text-format
!! dual-parse exactly (read as text first, then try an integer read).
!!
!! CurrSSDir is a string field whose legal values are either a signed number (as text)
!! or the literal sentinel "DEFAULT" (resolved later, once WaveDir is known, in
!! SeaStateInput_ProcessInitData) -- it is NOT read through the "default" scalar
!! mechanism in the usual sense (there is no single fallback value to substitute at
!! parse time). To let the literal text "DEFAULT"/"default" pass through unmolested
!! instead of tripping YamlGet's built-in "is set to default but no default exists"
!! fatal (YamlInput.f90's ScalarText fires that check on ANY scalar equal to "default"
!! once len<=16, regardless of whether the field conceptually supports a fallback), this
!! reader passes Default='DEFAULT' to YamlGet: when the user's YAML text is (case-
!! insensitively) "default", YamlGet substitutes exactly that same literal string, so
!! CurrSSDirChr ends up holding "DEFAULT" either way -- bit-identical to the text path.
!!
!! WvKinFile stays a path string (matching Waves_InitInputType%WvKinFile); it is
!! resolved relative to the primary input file later, in SeaStateInput_ProcessInitData,
!! exactly as for the text path (that resolution keys off InitInp%InputFile, which is
!! set regardless of format).
!!
!! NWaveElev/NWaveKin are not user-facing keys: they are derived counts, computed from
!! the lengths of the WaveElevxi/WaveElevyi and WaveKinxi/WaveKinyi/WaveKinzi lists
!! (which must agree in length within each pair/triple), then range-checked 0-9 exactly
!! as the text path range-checks the read integers. NumOuts is likewise derived from the
!! length of output_channels:OutList.
module SeaState_Yaml

   use NWTC_Library
   use YamlInput
   use SeaState_Types
   use SeaState_Output, only: MaxOutPts
   use Waves, only: WavePkShpDefault
   use NWTC_RandomNumber ! for parameters pRNG_INTRINSIC and pRNG_RANLUX

   implicit none
   private

   public :: SeaSt_ParseYamlFile
   public :: SeaSt_ParseYamlFileInfo

contains

!> Load and parse a YAML-format SeaState primary input file. Mirrors the contract of
!! ProcessComFile + SeaSt_ParseInput (the text path), including echo handling.
subroutine SeaSt_ParseYamlFile(InputFileName, InitInp, RootName, InputFileData, ErrStat, ErrMsg)
   character(*),               intent(in   ) :: InputFileName !< the .yaml primary input file
   type(SeaSt_InitInputType),  intent(in   ) :: InitInp       !< Init input (defWtrDens/defWtrDpth/defMSL2SWL defaults)
   character(*),               intent(in   ) :: RootName      !< module root name, for the echo file
   type(SeaSt_InputFile),      intent(inout) :: InputFileData !< the shared input-file structure
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SeaSt_ParseYamlFile'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: UnEc
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1

   InputFileData%Echo = .false.  ! initialize for error handling (cleanup path)

   call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'general:Echo', InputFileData%Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (InputFileData%Echo) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included), which supersedes the text path's line-by-line echo
      call OpenEcho(UnEc, trim(RootName)//'.ech', TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for SeaState primary input file: '//trim(InputFileName)
      call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   call ParseYamlDoc(Doc, InitInp, InputFileData, TmpErrStat, TmpErrMsg)
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

end subroutine SeaSt_ParseYamlFile

!> Parse YAML-format SeaState input arriving as a FileInfoType -- the passed-data
!! channel used for inline module input from a YAML primary file (the glue code's
!! inline SeaStFile handover when CompSeaSt selects SeaState). Per-line provenance in
!! the FileInfoType keeps error messages pointing at the original file and line.
subroutine SeaSt_ParseYamlFileInfo(FileInfo, InitInp, RootName, InputFileData, ErrStat, ErrMsg)
   type(FileInfoType),         intent(in   ) :: FileInfo
   type(SeaSt_InitInputType),  intent(in   ) :: InitInp
   character(*),               intent(in   ) :: RootName
   type(SeaSt_InputFile),      intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SeaSt_ParseYamlFileInfo'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: UnEc
   integer(IntKi)          :: i
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1

   InputFileData%Echo = .false.

   call Yaml_LoadFileInfo(FileInfo, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'general:Echo', InputFileData%Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (InputFileData%Echo) then
      call OpenEcho(UnEc, trim(RootName)//'.ech', TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for SeaState primary input (passed YAML data)'
      do i = 1, FileInfo%NumLines
         write(UnEc, '(A)') trim(FileInfo%Lines(i))
      end do
   end if

   call ParseYamlDoc(Doc, InitInp, InputFileData, TmpErrStat, TmpErrMsg)
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

end subroutine SeaSt_ParseYamlFileInfo

!> Fill InputFileData from a parsed document. Split from the file/FileInfo wrappers so
!! both entry points share it (mirrors AeroDisk_Yaml/SED_Yaml's ParseYamlDoc split).
subroutine ParseYamlDoc(Doc, InitInp, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(SeaSt_InitInputType),  intent(in   ) :: InitInp
   type(SeaSt_InputFile),      intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'SeaSt_ParseYamlDoc'
   character(:), allocatable  :: TmpList(:)
   character(200)             :: WaveModText
   character(80)              :: WaveSeed2Text
   character(1)               :: Line1
   integer(IntKi)             :: IOS
   integer(IntKi)             :: i
   integer(IntKi)              :: TmpErrStat
   character(ErrMsgLen)        :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   !----------------------------------------------------------------------------------
   ! general (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'general:Echo', InputFileData%Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! environmental_conditions -- WtrDens/WtrDpth/MSL2SWL accept "default"
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'environmental_conditions:WtrDens', InputFileData%WtrDens, TmpErrStat, TmpErrMsg, &
                Default=InitInp%defWtrDens)
   if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:WtrDpth', InputFileData%WtrDpth, TmpErrStat, TmpErrMsg, &
                Default=InitInp%defWtrDpth)
   if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:MSL2SWL', InputFileData%MSL2SWL, TmpErrStat, TmpErrMsg, &
                Default=InitInp%defMSL2SWL)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! spatial_discretization -- Z_Depth accepts "default" (= WtrDpth+MSL2SWL, computed
   ! from the values just read, exactly like the text path)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'spatial_discretization:X_HalfWidth', InputFileData%X_HalfWidth, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'spatial_discretization:Y_HalfWidth', InputFileData%Y_HalfWidth, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'spatial_discretization:Z_Depth', InputFileData%Z_Depth, TmpErrStat, TmpErrMsg, &
                Default=(InputFileData%WtrDpth + InputFileData%MSL2SWL))
   if (Failed()) return
   call YamlGet(Doc, 'spatial_discretization:NX', InputFileData%NX, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'spatial_discretization:NY', InputFileData%NY, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'spatial_discretization:NZ', InputFileData%NZ, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! waves (required) -- WaveMod is variant (integer, or "1P#" string); WavePkShp
   ! accepts "default"; WaveSeed2 is variant (integer seed, or an RNG name string)
   !----------------------------------------------------------------------------------
   InputFileData%Waves%WavePhase = 0.0_SiKi

   call YamlGet(Doc, 'waves:WaveMod', WaveModText, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   read(WaveModText, *, IOSTAT=IOS) InputFileData%WaveMod
   if (IOS /= 0) then
      call Conv2UC(WaveModText)
      if (WaveModText(1:2) == '1P') then
         InputFileData%WaveMod = WaveMod_RegularUsrPh   ! Internally define WaveMod = 10: regular waves w/ specified phase

         read(WaveModText(3:), *, IOSTAT=IOS) InputFileData%Waves%WavePhase
         call CheckIOS(IOS, "", 'WavePhase', NumType, TmpErrStat, TmpErrMsg)
         if (Failed()) return

         InputFileData%Waves%WavePhase = InputFileData%Waves%WavePhase * D2R   ! deg -> rad
      else
         call SetErrStat(ErrID_Fatal, 'WaveMod incorrectly specified in SeaState YAML input file.', &
            ErrStat, ErrMsg, RoutineName)
         return
      end if
   end if

   call YamlGet(Doc, 'waves:WaveStMod', InputFileData%WaveStMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'waves:WvCrntMod', InputFileData%WvCrntMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'waves:WaveTMax', InputFileData%Waves%WaveTMax, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'waves:WaveDT', InputFileData%Waves%WaveDT, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'waves:WaveHs', InputFileData%Waves%WaveHs, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'waves:WaveTp', InputFileData%Waves%WaveTp, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'waves:WavePkShp', InputFileData%Waves%WavePkShp, TmpErrStat, TmpErrMsg, &
                Default=WavePkShpDefault(InputFileData%WaveMod, InputFileData%Waves%WaveHs, InputFileData%Waves%WaveTp))
   if (Failed()) return

   call YamlGet(Doc, 'waves:WvLowCOff', InputFileData%WvLowCOff, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'waves:WvHiCOff', InputFileData%WvHiCOff, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'waves:WaveDir', InputFileData%WaveDir, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'waves:WaveDirMod', InputFileData%WaveDirMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'waves:WaveDirSpread', InputFileData%Waves%WaveDirSpread, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'waves:WaveNDir', InputFileData%Waves%WaveNDir, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'waves:WaveDirRange', InputFileData%Waves%WaveDirRange, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%Waves%WaveDirRange = abs(InputFileData%Waves%WaveDirRange)   ! negative values treated as positive

   call YamlGet(Doc, 'waves:WaveSeed1', InputFileData%Waves%RNG%RandSeed(1), TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'waves:WaveSeed2', WaveSeed2Text, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   Line1 = adjustl(WaveSeed2Text)
   call Conv2UC(Line1)
   if ((Line1 == 'T') .or. (Line1 == 'F')) then
      call SetErrStat(ErrID_Fatal, ' WaveSeed2: Invalid RNG type.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   read(WaveSeed2Text, *, IOSTAT=IOS) InputFileData%Waves%RNG%RandSeed(2)
   if (IOS == 0) then   ! the user entered a number
      InputFileData%Waves%RNG%RNG_type = "NORMAL"
      InputFileData%Waves%RNG%pRNG = pRNG_INTRINSIC
   else
      InputFileData%Waves%RNG%RandSeed(2) = 0

      InputFileData%Waves%RNG%RNG_type = adjustl(WaveSeed2Text)
      call Conv2UC(InputFileData%Waves%RNG%RNG_type)

      if (InputFileData%Waves%RNG%RNG_type == "RANLUX") then
         InputFileData%Waves%RNG%pRNG = pRNG_RANLUX
      else
         call SetErrStat(ErrID_Fatal, ' WaveSeed2: Invalid alternative random number generator.', &
            ErrStat, ErrMsg, RoutineName)
         return
      end if
   end if

   call YamlGet(Doc, 'waves:WaveNDAmp', InputFileData%Waves%WaveNDAmp, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'waves:WvKinFile', InputFileData%Waves%WvKinFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! second_order_waves (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'second_order_waves:WvDiffQTF', InputFileData%Waves2%WvDiffQTFF, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'second_order_waves:WvSumQTF', InputFileData%Waves2%WvSumQTFF, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'second_order_waves:WvLowCOffD', InputFileData%WvLowCOffD, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'second_order_waves:WvHiCOffD', InputFileData%WvHiCOffD, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'second_order_waves:WvLowCOffS', InputFileData%WvLowCOffS, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'second_order_waves:WvHiCOffS', InputFileData%WvHiCOffS, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! constrained_waves (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'constrained_waves:ConstWaveMod', InputFileData%Waves%ConstWaveMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'constrained_waves:CrestHmax', InputFileData%Waves%CrestHmax, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'constrained_waves:CrestTime', InputFileData%Waves%CrestTime, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'constrained_waves:CrestXi', InputFileData%Waves%CrestXi, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'constrained_waves:CrestYi', InputFileData%Waves%CrestYi, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! current (required) -- CurrSSDir is a string; see the module-header note on the
   ! Default='DEFAULT' workaround used to let the literal sentinel text pass through
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'current:CurrMod', InputFileData%Current%CurrMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'current:CurrSSV0', InputFileData%Current%CurrSSV0, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'current:CurrSSDir', InputFileData%Current%CurrSSDirChr, TmpErrStat, TmpErrMsg, &
                Default='DEFAULT')
   if (Failed()) return
   call Conv2UC(InputFileData%Current%CurrSSDirChr)

   call YamlGet(Doc, 'current:CurrNSRef', InputFileData%Current%CurrNSRef, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'current:CurrNSV0', InputFileData%Current%CurrNSV0, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'current:CurrNSDir', InputFileData%Current%CurrNSDir, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'current:CurrDIV', InputFileData%Current%CurrDIV, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'current:CurrDIDir', InputFileData%Current%CurrDIDir, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! maccamy_fuchs (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'maccamy_fuchs:MCFD', InputFileData%MCFD, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! output (required) -- NWaveElev/NWaveKin are derived from list lengths, not
   ! separate keys; range-checked 0-9 exactly as the text path range-checks the
   ! explicitly-read integer
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'output:SeaStSum', InputFileData%SeaStSum, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'output:OutSwtch', InputFileData%OutSwtch, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'output:OutFmt', InputFileData%OutFmt, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'output:OutSFmt', InputFileData%OutSFmt, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'output:WaveElevxi', InputFileData%WaveElevxi, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'output:WaveElevyi', InputFileData%WaveElevyi, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(InputFileData%WaveElevxi) /= size(InputFileData%WaveElevyi)) then
      call SetErrStat(ErrID_Fatal, 'output:WaveElevxi and output:WaveElevyi must have the same number of entries.', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if
   InputFileData%NWaveElev = size(InputFileData%WaveElevxi)
   if (InputFileData%NWaveElev < 0 .OR. InputFileData%NWaveElev > 9) then
      call SetErrStat(ErrID_Fatal, 'NWaveElev (the number of entries in output:WaveElevxi/WaveElevyi) must be '// &
         'greater than or equal to zero and less than 10.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call YamlGet(Doc, 'output:WaveKinxi', InputFileData%WaveKinxi, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'output:WaveKinyi', InputFileData%WaveKinyi, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'output:WaveKinzi', InputFileData%WaveKinzi, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(InputFileData%WaveKinxi) /= size(InputFileData%WaveKinyi) .OR. &
       size(InputFileData%WaveKinxi) /= size(InputFileData%WaveKinzi)) then
      call SetErrStat(ErrID_Fatal, 'output:WaveKinxi, output:WaveKinyi, and output:WaveKinzi must all have '// &
         'the same number of entries.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   InputFileData%NWaveKin = size(InputFileData%WaveKinxi)
   if (InputFileData%NWaveKin < 0 .OR. InputFileData%NWaveKin > 9) then
      call SetErrStat(ErrID_Fatal, 'NWaveKin (the number of entries in output:WaveKinxi/WaveKinyi/WaveKinzi) must '// &
         'be greater than or equal to zero and less than 10.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   !----------------------------------------------------------------------------------
   ! output_channels (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'output_channels:OutList', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpList) > MaxOutPts) then
      call SetErrStat(ErrID_Fatal, 'OutList may contain at most '//trim(Num2LStr(MaxOutPts))// &
         ' channels; found '//trim(Num2LStr(size(TmpList)))//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   call AllocAry(InputFileData%OutList, MaxOutPts, "SeaState Input File's OutList", TmpErrStat, TmpErrMsg)
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

end module SeaState_Yaml
