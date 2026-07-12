!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of TurbSim.
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
!> Reader for the YAML form of the TurbSim primary input file. This is a SUBMODULE of
!! TS_FileIO (not a plain module) so that it can call TS_FileIO's own derivation
!! subroutines (ProcessLine_IECstandard, DefaultMetBndryCndtns, GetDefaultRS,
!! CalcIECScalingParams, DefaultUstar, getJetCoeffs, etc.) directly by host
!! association, with zero code duplication -- a plain `use TS_FileIO` is impossible
!! here because TS_FileIO's own ReadInputFile calls back into this file's parser at
!! its YAML funnel (a genuine mutual dependency that only a submodule resolves
!! cleanly in Fortran).
!!
!! TurbSim's sequential text reader (TS_FileIO.f90:36-1129) is unlike the other
!! class-B primaries handled so far: its "read a value" and "derive a value from it"
!! steps are tightly interleaved in one long routine (RNG initialization, IEC default
!! calculations, and -- critically -- RNG *draws* for the coherent-turbulence-scaling
!! section all happen inline between reads). So instead of "parse to raw fields, then
!! call one shared downstream validator" (the FAST.Farm shape), this parser mirrors
!! ReadInputFile's control flow statement-for-statement: every CALL ReadVar / CALL
!! ReadRVarDefault / CALL ReadCVarDefault / CALL ReadRAryDefault becomes a YAML fetch
!! of the identically-named key, and every intervening derivation call
!! (RandNum_Init, DefaultMetBndryCndtns, Calc_MO_zL, DefaultUstar, getJetCoeffs,
!! GetDefaultSCMod, GetDefaultRS, CalcIECScalingParams, GetDefaultCoh,
!! DefaultMixingLayerDepth, RndUnif, ...) is called in EXACTLY the same order with the
!! same arguments as the text path. This is what guarantees bit-identical RNG draws
!! (and therefore a bit-identical .bts) between the text and YAML decks.
!!
!! Default-token semantics (the other TurbSim-specific trap): TS_FileIO's
!! ReadCVarDefault/ReadRVarDefault/ReadRAryDefault (TS_FileIO.f90:1820-2032) accept the
!! literal token "default" (case-insensitive, exact match after TRIM+Conv2UC) in place
!! of a value, leaving the target variable UNTOUCHED and returning Def=.TRUE. -- the
!! actual default is filled in either by a value DefaultMetBndryCndtns(p) already
!! pre-populated before the read, or by an explicit DefaultXxx()/getXxx() call further
!! down once its dependencies are known. This parser's YS_Get*Default helpers below
!! reproduce that exact contract (including the "leave untouched" behavior) sourced
!! from a YAML scalar's raw text instead of a text-file line -- deliberately NOT using
!! YamlGet's own Default= (that overload requires the default to be a fixed value known
!! at the call site, which most of these fields are not: e.g. UStar's default depends on
!! URef/RefHt/Z0/ZL that may themselves still be pending). The "default" token is never
!! pre-resolved by the converter or by this parser -- it is preserved verbatim into the
!! same UseDefault-gated code paths the text reader already has.
!!
!! Schema: sections mirror the text file's banners -- runtime_options, turbine_model,
!! meteorological_boundary_conditions, non_iec_meteorological_boundary_conditions,
!! spatial_coherence_parameters, coherent_turbulence_scaling_parameters (the last is
!! present in the YAML file but -- exactly like the text path -- only actually read
!! when the spectral model is non-IEC; for IEC models the text reader never consumes
!! those lines at all, so no RNG draws happen there either).
!!
!! Path-valued fields (second-order rule, never inlined): UserFile, ProfileFile,
!! CTEventPath.
!!
!! The parser lives in a SUBMODULE of TS_FileIO (there is no separate plain module
!! here): this grants it direct access to every one of TS_FileIO's own subroutines
!! (host association), so the derivation/validation/RNG logic used by the text path
!! is reused verbatim, never re-implemented.
submodule (TS_FileIO) TS_Yaml_SubMod

   use YamlInput

   implicit none

contains

!> Entry point called from TS_FileIO's IsYamlExt funnel at the top of ReadInputFile.
!! Mirrors ReadInputFile's contract (same p/OtherSt_RandNum in/out roles, same echo
!! handling).
module subroutine TS_ParseYamlFile(InFile, p, OtherSt_RandNum, ErrStat, ErrMsg)
   character(*),                 intent(in   ) :: InFile             !< name of the primary TurbSim YAML input file
   type(TurbSim_ParameterType),  intent(inout) :: p                  !< TurbSim's parameters
   type(RandNum_OtherStateType), intent(inout) :: OtherSt_RandNum    !< other states for random numbers (next seed, etc)
   integer(IntKi),                intent(  out) :: ErrStat
   character(*),                   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'TS_ParseYamlFile'
   type(YamlDoc)            :: Doc
   character(1024)          :: PriPath
   logical                  :: Echo
   integer(IntKi)           :: UnEc
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1
   p%met%NumUSRz = 0   ! initialize the number of points in a user-defined wind profile (mirrors ReadInputFile:93)

   call GetPath(InFile, PriPath)   ! input files will be relative to the path where the primary input file is located

   call Yaml_LoadFile(InFile, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call WrScr1(' Reading the input file "'//trim(InFile)//'".')

   Echo = .false.
   call YamlGet(Doc, 'runtime_options:Echo', Echo, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   if (Echo) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included), which supersedes the text path's line-by-line echo
      call OpenEcho(UnEc, trim(p%RootName)//'.ech', TmpErrStat, TmpErrMsg, TurbSim_Ver)
      if (Failed()) return
      write (UnEc, '(/,A,/)') 'Data from '//trim(TurbSim_Ver%Name)//' primary input file "'//trim(InFile)//'":'
      call Yaml_LoadFile(InFile, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
      ! the reload reset the Used flags; re-read Echo so Yaml_WarnUnused doesn't flag it
      call YamlGet(Doc, 'runtime_options:Echo', Echo, TmpErrStat, TmpErrMsg)
      if (Failed()) return
   end if

   call ParseYamlBody(Doc, PriPath, p, OtherSt_RandNum, TmpErrStat, TmpErrMsg)
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
      if (UnEc > 0) close (UnEc)
   end subroutine Cleanup

end subroutine TS_ParseYamlFile

!> Fills p/OtherSt_RandNum from a parsed document, statement-for-statement mirroring
!! ReadInputFile (TS_FileIO.f90:172-1115). Split out of TS_ParseYamlFile purely so the
!! file-level header/echo handling above stays out of the section-by-section body.
subroutine ParseYamlBody(Doc, PriPath, p, OtherSt_RandNum, ErrStat, ErrMsg)
   type(YamlDoc),                 intent(inout) :: Doc
   character(*),                  intent(in   ) :: PriPath
   type(TurbSim_ParameterType),   intent(inout) :: p
   type(RandNum_OtherStateType),  intent(inout) :: OtherSt_RandNum
   integer(IntKi),                 intent(  out) :: ErrStat
   character(*),                    intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ParseYamlBody'

   real(ReKi)      :: InCVar(2)
   real(ReKi)      :: tmp
   real(ReKi)      :: TmpUary(3)
   real(ReKi)      :: TmpUstar(3)
   real(ReKi)      :: TmpUstarD
   real(ReKi)      :: RotorDiskHeights(3)
   real(ReKi)      :: TmpZLary(3)
   integer(IntKi)  :: TmpIndex
   integer(IntKi)  :: I

   logical :: getDefaultPLExp, getDefaultURef, getDefaultZJetMax
   logical :: Randomize, UseDefault, IsUnusedParameter

   character(1024) :: Line
   character(1)    :: Line1
   character(1024) :: UserFile, ProfileFile

   integer(IntKi)         :: TmpErrStat
   character(ErrMsgLen)   :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   !===============================================================================================================================
   ! runtime_options (mirrors ReadInputFile:173-267, minus Echo which is already consumed by the caller)
   !===============================================================================================================================

   call YamlGet(Doc, 'runtime_options:RandSeed1', p%RNG%RandSeed(1), TmpErrStat, TmpErrMsg); if (Failed()) return

   call YS_GetRawScalar(Doc, 'runtime_options:RandSeed2', Line, TmpErrStat, TmpErrMsg); if (Failed()) return
   read (Line, *, iostat=TmpErrStat) Line1   ! check the first character (T/F would be misread as 1/-1/0)
   call Conv2UC(Line1)
   if ((Line1 == 'T') .or. (Line1 == 'F')) then
      call SetErrStat(ErrID_Fatal, ' RandSeed(2): Invalid RNG type.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   read (Line, *, iostat=TmpErrStat) p%RNG%RandSeed(2)
   if (TmpErrStat == 0) then   ! the user entered a number
      p%RNG%RNG_type = "NORMAL"
      p%RNG%pRNG = pRNG_INTRINSIC
   else
      p%RNG%RNG_type = adjustl(Line)
      call Conv2UC(p%RNG%RNG_type)
      if (p%RNG%RNG_type == "RANLUX") then
         p%RNG%pRNG = pRNG_RANLUX
      else if (p%RNG%RNG_type == "RNSNLW") then
         p%RNG%pRNG = pRNG_SNLW3
      else
         call SetErrStat(ErrID_Fatal, ' RandSeed(2): Invalid alternative random number generator.', ErrStat, ErrMsg, RoutineName)
         return
      end if
   end if

   call YamlGet(Doc, 'runtime_options:WrBHHTP',  p%WrFile(FileExt_BIN),  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'runtime_options:WrFHHTP',  p%WrFile(FileExt_DAT),  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'runtime_options:WrADHH',   p%WrFile(FileExt_HH),   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'runtime_options:WrADFF',   p%WrFile(FileExt_BTS),  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'runtime_options:WrBLFF',   p%WrFile(FileExt_WND),  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'runtime_options:WrADTWR',  p%WrFile(FileExt_TWR),  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'runtime_options:WrHAWCFF', p%WrFile(FileExt_HAWC), TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'runtime_options:WrFMTFF',  p%WrFile(FileExt_UVW),  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'runtime_options:WrACT',    p%WrFile(FileExt_CTS),  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'runtime_options:ScaleIEC', p%IEC%ScaleIEC,         TmpErrStat, TmpErrMsg); if (Failed()) return

   !===============================================================================================================================
   ! turbine_model (mirrors ReadInputFile:279-344)
   !===============================================================================================================================

   call YamlGet(Doc, 'turbine_model:NumGrid_Z', p%grid%NumGrid_Z, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_model:NumGrid_Y', p%grid%NumGrid_Y, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_model:TimeStep',  p%grid%TimeStep,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_model:AnalysisTime', p%grid%AnalysisTime, TmpErrStat, TmpErrMsg); if (Failed()) return

   call YS_GetRawScalar(Doc, 'turbine_model:UsableTime', Line, TmpErrStat, TmpErrMsg); if (Failed()) return
   read (Line, *, iostat=TmpErrStat) p%grid%UsableTime
   if (TmpErrStat /= 0) then   ! Line didn't contain a number
      call Conv2UC(Line)
      if (trim(Line) == 'ALL') then
         p%grid%Periodic   = .true.
         p%grid%UsableTime = p%grid%AnalysisTime
      else
         call SetErrStat(ErrID_Fatal, 'The usable output time must be a number greater than zero (or the string "ALL").', &
                          ErrStat, ErrMsg, RoutineName)
         return
      end if
   else
      p%grid%Periodic = .false.
      call CheckRealVar(p%grid%UsableTime, 'UsableTime', TmpErrStat, TmpErrMsg)
      if (Failed()) return
   end if

   call YamlGet(Doc, 'turbine_model:HubHt',      p%grid%HubHt,      TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_model:GridHeight', p%grid%GridHeight, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_model:GridWidth',  p%grid%GridWidth,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_model:VFlowAng',   p%met%VFlowAng,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_model:HFlowAng',   p%met%HFlowAng,    TmpErrStat, TmpErrMsg); if (Failed()) return

   !..................................................................................................................................
   !  Do some error checking on the runtime options and turbine/model specifications before we read the meteorology data
   !  (mirrors ReadInputFile:350-376)
   !..................................................................................................................................

   if (p%IEC%ScaleIEC > 2 .or. p%IEC%ScaleIEC < 0) call SetErrStat(ErrID_Fatal, 'The value for parameter ScaleIEC must be 0, 1, or 2.', ErrStat, ErrMsg, RoutineName)
   if (p%grid%NumGrid_Z < 2) call SetErrStat(ErrID_Fatal, 'The matrix must be >= 2x2.', ErrStat, ErrMsg, RoutineName)
   if (p%grid%NumGrid_Y < 2) call SetErrStat(ErrID_Fatal, 'The matrix must be >= 2x2.', ErrStat, ErrMsg, RoutineName)
   if (0.5*p%grid%GridHeight > p%grid%HubHt) call SetErrStat(ErrID_Fatal, 'The hub must be higher than half of the grid height.', ErrStat, ErrMsg, RoutineName)
   if (p%grid%GridWidth <= 0.0_ReKi) call SetErrStat(ErrID_Fatal, 'The grid width must be greater than zero.', ErrStat, ErrMsg, RoutineName)
   if (p%grid%HubHt <= 0.0) call SetErrStat(ErrID_Fatal, 'The hub height must be greater than zero.', ErrStat, ErrMsg, RoutineName)
   if (p%grid%AnalysisTime <= 0.0) call SetErrStat(ErrID_Fatal, 'The analysis time must be greater than zero.', ErrStat, ErrMsg, RoutineName)
   if (p%grid%TimeStep <= 0.0) call SetErrStat(ErrID_Fatal, 'The time step must be greater than zero.', ErrStat, ErrMsg, RoutineName)
   if (abs(p%met%VFlowAng) > 45.0) call SetErrStat(ErrID_Fatal, 'The vertical flow angle must not exceed +/- 45 degrees.', ErrStat, ErrMsg, RoutineName)
   if (p%grid%UsableTime <= 0.0) call SetErrStat(ErrID_Fatal, 'The usable output time must be a number greater than zero or the string "ALL".', ErrStat, ErrMsg, RoutineName)
   if (ErrStat >= AbortErrLev) return

   !..................................................................................................................................
   !  initialize secondary parameters (mirrors ReadInputFile:365-376)
   !..................................................................................................................................
   call RandNum_Init(p%RNG, OtherSt_RandNum, TmpErrStat, TmpErrMsg); if (Failed()) return
   p%grid%RotorDiameter = min(p%grid%GridWidth, p%grid%GridHeight)
   if (ErrStat >= AbortErrLev) return

   !===============================================================================================================================
   ! meteorological_boundary_conditions (mirrors ReadInputFile:388-701)
   !===============================================================================================================================

   call YS_GetRawScalar(Doc, 'meteorological_boundary_conditions:TurbModel', p%met%TurbModel, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'meteorological_boundary_conditions:UserFile', UserFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (PathIsRelative(UserFile)) UserFile = trim(PriPath)//trim(UserFile)

   p%met%TurbModel = adjustl(p%met%TurbModel)
   call Conv2UC(p%met%TurbModel)

   p%met%IsIECModel = .false.
   p%usr%nPoints = 0
   select case (trim(p%met%TurbModel))
      case ('IECKAI')
         p%met%TMName = 'IEC Kaimal'; p%met%TurbModel_ID = SpecModel_IECKAI; p%met%IsIECModel = .true.
      case ('IECVKM')
         p%met%TMName = 'IEC von Karman'; p%met%TurbModel_ID = SpecModel_IECVKM; p%met%IsIECModel = .true.
      case ('TIDAL')
         p%met%TMName = 'Tidal Channel Turbulence'; p%met%TurbModel_ID = SpecModel_TIDAL
      case ('RIVER')
         p%met%TMName = 'River Turbulence'; p%met%TurbModel_ID = SpecModel_RIVER
      case ('SMOOTH')
         p%met%TMName = 'RISO Smooth Terrain'; p%met%TurbModel_ID = SpecModel_SMOOTH
      case ('WF_UPW')
         p%met%TMName = 'NREL Wind Farm Upwind'; p%met%TurbModel_ID = SpecModel_WF_UPW
      case ('WF_07D')
         p%met%TMName = 'NREL 7D Spacing Wind Farm'; p%met%TurbModel_ID = SpecModel_WF_07D
      case ('WF_14D')
         p%met%TMName = 'NREL 14D Spacing Wind Farm'; p%met%TurbModel_ID = SpecModel_WF_14D
      case ('NONE')
         p%met%TMName = 'Steady wind components'; p%met%TurbModel_ID = SpecModel_NONE
      case ('MODVKM')
         p%met%TMName = 'Modified von Karman'; p%met%TurbModel_ID = SpecModel_MODVKM; p%met%IsIECModel = .true.
      case ('API')
         p%met%TMName = 'API'; p%met%TurbModel_ID = SpecModel_API; p%met%IsIECModel = .true.
      case ('NWTCUP')
         p%met%TMName = 'NREL National Wind Technology Center'; p%met%TurbModel_ID = SpecModel_NWTCUP
      case ('GP_LLJ')
         p%met%TMName = 'Great Plains Low-Level Jet'; p%met%TurbModel_ID = SpecModel_GP_LLJ
      case ('USRVKM')
         p%met%TMName = 'von Karman model with user-defined specifications'; p%met%TurbModel_ID = SpecModel_USRVKM
      case ('USRINP')
         p%met%TMName = 'Uniform user-input'; p%met%TurbModel_ID = SpecModel_USER
         call GetUSRspec(UserFile, p, -1_IntKi, TmpErrStat, TmpErrMsg); if (Failed()) return
      case ('TIMESR')
         p%met%TMName = 'User-input time series'; p%met%TurbModel_ID = SpecModel_TimeSer
         call GetUSRTimeSeries(UserFile, p, -1_IntKi, TmpErrStat, TmpErrMsg); if (Failed()) return
         call TimeSeriesToSpectra(p, TmpErrStat, TmpErrMsg); if (Failed()) return
      case default
         call SetErrStat(ErrID_Fatal, 'The turbulence model must be one of the following: "IECKAI", "IECVKM", "SMOOTH",' &
                    //' "WF_UPW", "WF_07D", "WF_14D", "NWTCUP", "GP_LLJ", "TIDAL", "RIVER", "API", "USRINP", "TIMESR" "NONE".', ErrStat, ErrMsg, RoutineName)
         return
   end select

   call YS_GetRawScalar(Doc, 'meteorological_boundary_conditions:IECstandard', Line, TmpErrStat, TmpErrMsg); if (Failed()) return
   call ProcessLine_IECstandard(Line, p%met%IsIECModel, p%met%TurbModel_ID, p%IEC%IECstandard, p%IEC%IECedition, p%IEC%IECeditionStr, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YS_GetRawScalar(Doc, 'meteorological_boundary_conditions:IECturbc', Line, TmpErrStat, TmpErrMsg); if (Failed()) return
   call ProcessLine_IECturbc(Line, p%met%IsIECModel, p%IEC%IECstandard, p%IEC%IECedition, p%IEC%IECeditionStr, &
                              p%IEC%NumTurbInp, p%IEC%IECTurbC, p%IEC%PerTurbInt, p%met%KHtest, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YS_GetRawScalar(Doc, 'meteorological_boundary_conditions:IEC_WindType', Line, TmpErrStat, TmpErrMsg); if (Failed()) return
   call ProcessLine_IEC_WindType(Line, p, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! set default ETMc, WindProfileType, Z0, CohExp, and Latitude for use in the YS_Get*Default routines below
   call DefaultMetBndryCndtns(p)

   call YS_GetRVarDefault(Doc, 'meteorological_boundary_conditions:ETMc', p%IEC%ETMc, UseDefault, TmpErrStat, TmpErrMsg, &
                           IGNORE=(p%IEC%IEC_WindType /= IEC_ETM))
   if (Failed()) return

   call YS_GetCVarDefault(Doc, 'meteorological_boundary_conditions:WindProfileType', p%met%WindProfileType, UseDefault, TmpErrStat, TmpErrMsg)
   if (Failed()) return   ! converts WindProfileType to upper case when explicitly given

   call YamlGet(Doc, 'meteorological_boundary_conditions:ProfileFile', ProfileFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (PathIsRelative(ProfileFile)) ProfileFile = trim(PriPath)//trim(ProfileFile)

   call YamlGet(Doc, 'meteorological_boundary_conditions:RefHt', p%met%RefHt, TmpErrStat, TmpErrMsg); if (Failed()) return

   IsUnusedParameter = p%IEC%IEC_WindType > IEC_ETM .or. index('TU', p%met%WindProfileType(1:1)) > 0
   call YS_GetRVarDefault(Doc, 'meteorological_boundary_conditions:URef', p%met%URef, getDefaultURef, TmpErrStat, TmpErrMsg, &
                           IGNORE=IsUnusedParameter)
   if (Failed()) return
   getDefaultURef = getDefaultURef .and. .not. IsUnusedParameter

   IsUnusedParameter = trim(p%met%WindProfileType) /= 'JET'
   call YS_GetRVarDefault(Doc, 'meteorological_boundary_conditions:ZJetMax', p%met%ZJetMax, getDefaultZJetMax, TmpErrStat, TmpErrMsg, &
                           IGNORE=IsUnusedParameter)
   if (Failed()) return
   getDefaultZJetMax = getDefaultZJetMax .and. .not. IsUnusedParameter

   IsUnusedParameter = (trim(p%met%WindProfileType) /= "PL" .and. trim(p%met%WindProfileType) /= "IEC")
   call YS_GetRVarDefault(Doc, 'meteorological_boundary_conditions:PLExp', p%met%PLExp, getDefaultPLExp, TmpErrStat, TmpErrMsg, &
                           IGNORE=IsUnusedParameter)
   if (Failed()) return
   getDefaultPLExp = getDefaultPLExp .and. .not. IsUnusedParameter

   IsUnusedParameter = p%met%TurbModel_ID == SpecModel_TIDAL
   call YS_GetRVarDefault(Doc, 'meteorological_boundary_conditions:Z0', p%met%Z0, UseDefault, TmpErrStat, TmpErrMsg, &
                           IGNORE=IsUnusedParameter)
   if (Failed()) return

   !..................................................................................................................................
   !  error checking (mirrors ReadInputFile:590-701)
   !..................................................................................................................................
   if (p%IEC%IEC_WindType == IEC_ETM .and. p%IEC%ETMc <= 0.) call SetErrStat(ErrID_Fatal, 'The ETM "c" parameter must be a positive number', ErrStat, ErrMsg, RoutineName)

   select case (trim(p%met%WindProfileType))
      case ('JET')
         if (p%met%TurbModel_ID /= SpecModel_GP_LLJ) call SetErrStat(ErrID_Fatal, 'The jet wind profile is available with the GP_LLJ spectral model only.', ErrStat, ErrMsg, RoutineName)
      case ('LOG')
         if (p%IEC%IEC_WindType /= IEC_NTM) call SetErrStat(ErrID_Fatal, 'The IEC turbulence type must be NTM for the logarithmic wind profile.', ErrStat, ErrMsg, RoutineName)
      case ('PL')
      case ('H2L')
         if (p%met%TurbModel_ID /= SpecModel_TIDAL) call SetErrStat(ErrID_Fatal, 'The "H2L" mean profile type can be used with only the "TIDAL" spectral model.', ErrStat, ErrMsg, RoutineName)
      case ('IEC')
      case ('USR')
         call GetUSRProfiles(ProfileFile, p%met, -1_IntKi, TmpErrStat, TmpErrMsg); if (Failed()) return
      case ('TS')
         if (p%met%TurbModel_ID /= SpecModel_TimeSer) call SetErrStat(ErrID_Fatal, 'The "TS" mean profile type is valid only with the "TIMESR" spectral model.', ErrStat, ErrMsg, RoutineName)
      case ('API')
      case default
         call SetErrStat(ErrID_Fatal, 'The wind profile type must be "JET", "LOG", "PL", "IEC", "USR", "H2L", "TS", or default.', ErrStat, ErrMsg, RoutineName)
   end select

   if (p%met%TurbModel_ID == SpecModel_TIDAL .and. trim(p%met%WindProfileType) /= "H2L") then
      p%met%WindProfileType = 'H2L'
      call SetErrStat(ErrID_Warn, 'Overwriting wind profile type to "H2L" for the "TIDAL" spectral model.', ErrStat, ErrMsg, RoutineName)
   end if

   if (p%met%KHtest) then
      if (p%met%TurbModel_ID /= SpecModel_NWTCUP) call SetErrStat(ErrID_Fatal, 'The KH test can be used with the "NWTCUP" spectral model only.', ErrStat, ErrMsg, RoutineName)
      if (trim(p%met%WindProfileType) /= 'IEC' .and. trim(p%met%WindProfileType) /= 'PL') then
         call SetErrStat(ErrID_Warn, 'Overwriting wind profile type for the KH test.', ErrStat, ErrMsg, RoutineName)
         p%met%WindProfileType = 'IEC'
      end if
      if (.not. p%WrFile(FileExt_CTS)) then
         call SetErrStat(ErrID_Warn, 'Coherent turbulence time step files must be generated when using the "KHTEST" option.', ErrStat, ErrMsg, RoutineName)
         p%WrFile(FileExt_CTS) = .true.
      end if
      if (.not. EqualRealNos(p%met%PLExp, 0.3_ReKi)) then
         call SetErrStat(ErrID_Warn, 'Overwriting the power law exponent for KH test.', ErrStat, ErrMsg, RoutineName)
         p%met%PLExp = 0.3
      end if
   end if

   if (getDefaultURef) then
      if (p%usr%NPoints > 0) then
         p%met%RefHt = p%usr%pointzi(p%usr%RefPtID)
         p%met%URef  = p%usr%meanU(p%usr%RefPtID, 1)
         getDefaultURef = .false.
      else if (trim(p%met%WindProfileType) /= 'JET') then
         call SetErrStat(ErrID_Fatal, 'URef can be "default" for only the "JET" WindProfileType.', ErrStat, ErrMsg, RoutineName)
      end if
   end if

   if (p%met%Z0 <= 0.0_ReKi) call SetErrStat(ErrID_Fatal, 'The surface roughness length must be a positive number or "default".', ErrStat, ErrMsg, RoutineName)

   if (trim(p%met%WindProfileType) == 'JET' .and. .not. getDefaultZJetMax) then
      if (p%met%ZJetMax < ZJetMax_LB .or. p%met%ZJetMax > ZJetMax_UB) then
         call SetErrStat(ErrID_Fatal, 'The height of the maximum jet wind speed must be between '//trim(num2lstr(ZJetMax_LB))// &
                                       ' and '//trim(num2lstr(ZJetMax_UB))//' m.', ErrStat, ErrMsg, RoutineName)
      end if
   end if

   if (ErrStat >= AbortErrLev) return

   !.................................................
   ! overwrite RefHt and URef for cases where they are unused [USR wind profiles (or TS)] (mirrors ReadInputFile:672-701)
   !.................................................
   if (trim(p%met%WindProfileType) == 'USR' .or. trim(p%met%WindProfileType) == 'TS') then
      call getVelocity(p, p%met%URef, p%met%RefHt, p%met%RefHt, tmp, TmpErrStat, TmpErrMsg); if (Failed()) return
      p%met%URef = tmp
   else if (p%IEC%IEC_WindType > IEC_ETM) then
      p%met%RefHt = p%grid%HubHt
      p%met%URef  = p%IEC%VRef
   end if

   if (p%met%RefHt <= 0.0_ReKi) call SetErrStat(ErrID_Fatal, 'The reference height must be greater than zero.', ErrStat, ErrMsg, RoutineName)
   if (.not. getDefaultURef) then
      if (p%met%URef <= 0.0_ReKi) call SetErrStat(ErrID_Fatal, 'The reference wind speed must be greater than zero.', ErrStat, ErrMsg, RoutineName)
   end if
   if (ErrStat >= AbortErrLev) return

   !===============================================================================================================================
   ! non_iec_meteorological_boundary_conditions (mirrors ReadInputFile:713-870)
   !===============================================================================================================================

   IsUnusedParameter = p%met%IsIECModel .and. p%met%TurbModel_ID /= SpecModel_MODVKM
   call YS_GetRVarDefault(Doc, 'non_iec_meteorological_boundary_conditions:Latitude', p%met%Latitude, UseDefault, TmpErrStat, TmpErrMsg, &
                           IGNORE=IsUnusedParameter)
   if (Failed()) return

   call YamlGet(Doc, 'non_iec_meteorological_boundary_conditions:RICH_NO', p%met%Rich_No, TmpErrStat, TmpErrMsg); if (Failed()) return

   if (p%met%KHtest) then
      if (.not. EqualRealNos(p%met%Rich_No, 0.02_ReKi)) then
         p%met%Rich_No = 0.02
         call SetErrStat(ErrID_Warn, 'Overwriting the Richardson Number for KH test.', ErrStat, ErrMsg, RoutineName)
      end if
   else if (p%met%TurbModel_ID == SpecModel_USRVKM) then
      if (.not. EqualRealNos(p%met%Rich_No, 0.0_ReKi)) then
         call SetErrStat(ErrID_Warn, 'Overwriting the Richardson Number for the '//trim(p%met%TurbModel)//' model.', ErrStat, ErrMsg, RoutineName)
         p%met%Rich_No = 0.0
      end if
   else if (p%met%TurbModel_ID == SpecModel_NWTCUP .or. p%met%TurbModel_ID == SpecModel_GP_LLJ) then
      p%met%Rich_No = min(max(p%met%Rich_No, -1.0_ReKi), 1.0_ReKi)
   else if (p%met%IsIECModel) then
      p%met%Rich_No = 0.0
   end if

   call Calc_MO_zL(p%met%TurbModel_ID, p%met%Rich_No, p%grid%HubHt, p%met%ZL, p%met%L)

   call YS_GetRVarDefault(Doc, 'non_iec_meteorological_boundary_conditions:UStar', p%met%Ustar, UseDefault, TmpErrStat, TmpErrMsg, &
                           IGNORE=p%met%IsIECModel)
   if (Failed()) return
   if (UseDefault) then
      if (getDefaultURef) then
         call SetErrStat(ErrID_Fatal, 'The reference wind speed and friction velocity cannot both be "default."', ErrStat, ErrMsg, RoutineName)
      else
         call DefaultUstar(p)
      end if
   end if

   p%met%Fc = 2.0 * Omega * sin(abs(p%met%Latitude*D2R))

   if (getDefaultPLExp) p%met%PLExp = DefaultPowerLawExp(p)

   if (trim(p%met%WindProfileType) == 'JET') then
      if (getDefaultZJetMax) call DefaultZJetMax(p, OtherSt_RandNum)
      call getJetCoeffs(p, getDefaultURef, OtherSt_RandNum, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if

   p%met%UstarDiab = getUstarDiab(p%met%URef, p%met%RefHt, p%met%z0, p%met%ZL)

   if (ErrStat >= AbortErrLev) return

   call getVelocity(p, p%met%URef, p%met%RefHt, p%grid%HubHt, tmp, TmpErrStat, TmpErrMsg)
   p%UHub = tmp
   if (Failed()) return

   RotorDiskHeights = (/ p%grid%HubHt - 0.5*p%grid%RotorDiameter, p%grid%HubHt, p%grid%HubHt + 0.5*p%grid%RotorDiameter /)
   do TmpIndex = 1, size(RotorDiskHeights)
      RotorDiskHeights(TmpIndex) = max(min(RotorDiskHeights(TmpIndex), profileZmax), profileZmin)
   end do

   if (p%met%TurbModel_ID == SpecModel_GP_LLJ) then
      p%met%UstarSlope = 1.0_ReKi
      call getVelocityProfile(p, p%met%URef, p%met%RefHt, RotorDiskHeights, TmpUary, TmpErrStat, TmpErrMsg); if (Failed()) return
      TmpUstar = getUStarProfile(p, TmpUary, RotorDiskHeights, 0.0_ReKi, p%met%UstarSlope)
      p%met%UstarOffset = p%met%Ustar - sum(TmpUstar) / size(TmpUstar)
      TmpUstar(:) = TmpUstar(:) + p%met%UstarOffset
   else
      p%met%UstarSlope = 1.0_ReKi
      TmpUary  = (/ 0.0_ReKi, 0.0_ReKi, 0.0_ReKi /)
      TmpUstar = (/ 0.0_ReKi, 0.0_ReKi, 0.0_ReKi /)
      p%met%UstarOffset = 0.0_ReKi
   end if

   call GetDefaultSCMod(p%met%TurbModel_ID, p%met%SCMod)
   call GetDefaultRS(p, OtherSt_RandNum, TmpUStar(2), TmpErrStat, TmpErrMsg); if (Failed()) return
   call CalcIECScalingParams(p%IEC, p%grid%HubHt, p%UHub, p%met%InCDec, p%met%InCohB, p%met%TurbModel_ID, p%met%IsIECModel, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (.not. p%met%IsIECModel) then
      call GetDefaultCoh(p%met%TurbModel_ID, p%met%RICH_NO, p%UHub, p%grid%HubHt, p%met%IncDec, p%met%InCohB)
   end if

   IsUnusedParameter = p%met%ZL >= 0.0_ReKi .and. p%met%TurbModel_ID /= SpecModel_GP_LLJ
   call YS_GetRVarDefault(Doc, 'non_iec_meteorological_boundary_conditions:ZI', p%met%ZI, UseDefault, TmpErrStat, TmpErrMsg, &
                           IGNORE=IsUnusedParameter)
   if (Failed()) return
   if (IsUnusedParameter) then
      p%met%ZI = 999.9_ReKi
   else
      if (UseDefault) call DefaultMixingLayerDepth(p)
   end if

   ! NOTE: UWskip/UVskip/VWskip are deliberately left untouched here (no pre-set), exactly
   ! mirroring the text path (ReadInputFile:856-868): when IGNORE (IsIECModel) is true,
   ! ReadRVarDefault/YS_GetRVarDefault return immediately without assigning IGNORESTR at
   ! all, so its value is whatever p%met%xWskip already held on entry -- identical for both
   ! the text and YAML paths since both start from the same freshly-constructed p.
   call YS_GetRVarDefault(Doc, 'non_iec_meteorological_boundary_conditions:PC_UW', p%met%PC_UW, UseDefault, TmpErrStat, TmpErrMsg, &
                           IGNORE=p%met%IsIECModel, IGNORESTR=p%met%UWskip)
   if (Failed()) return

   call YS_GetRVarDefault(Doc, 'non_iec_meteorological_boundary_conditions:PC_UV', p%met%PC_UV, UseDefault, TmpErrStat, TmpErrMsg, &
                           IGNORE=p%met%IsIECModel, IGNORESTR=p%met%UVskip)
   if (Failed()) return

   call YS_GetRVarDefault(Doc, 'non_iec_meteorological_boundary_conditions:PC_VW', p%met%PC_VW, UseDefault, TmpErrStat, TmpErrMsg, &
                           IGNORE=p%met%IsIECModel, IGNORESTR=p%met%VWskip)
   if (Failed()) return

   !===============================================================================================================================
   ! spatial_coherence_parameters (mirrors ReadInputFile:881-935)
   !===============================================================================================================================

   do I = 1, 3
      call YS_GetCVarDefault(Doc, 'spatial_coherence_parameters:SCMod'//trim(num2lstr(I)), Line, UseDefault, TmpErrStat, TmpErrMsg)
      if (Failed()) return
      if (.not. UseDefault) then
         select case (trim(Line))
            case ("GENERAL")
               p%met%SCMod(I) = CohMod_GENERAL
            case ("IEC")
               p%met%SCMod(I) = CohMod_IEC
            case ("NONE")
               p%met%SCMod(I) = CohMod_NONE
            case ("API")
               p%met%SCMOD(I) = CohMod_API
               if (I /= 1) call SetErrStat(ErrID_Fatal, "API coherence model is valid only for the u-component", ErrStat, ErrMsg, RoutineName)
            case default
               p%met%SCMod(I) = CohMod_NONE
               if (I == 1) then
                  call SetErrStat(ErrID_Fatal, 'Unknown value for SCMod'//trim(num2lstr(I))//'. Valid entries are "GENERAL","IEC","API", or "NONE".', ErrStat, ErrMsg, RoutineName)
               else
                  call SetErrStat(ErrID_Fatal, 'Unknown value for SCMod'//trim(num2lstr(I))//'. Valid entries are "GENERAL","IEC", or "NONE".', ErrStat, ErrMsg, RoutineName)
               end if
         end select
      end if
   end do

   call YS_GetRAryDefault(Doc, 'spatial_coherence_parameters:InCDec1', InCVar, UseDefault, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (.not. UseDefault) then
      p%met%InCDec(1) = InCVar(1); p%met%InCohB(1) = InCVar(2)
   end if

   call YS_GetRAryDefault(Doc, 'spatial_coherence_parameters:InCDec2', InCVar, UseDefault, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (.not. UseDefault) then
      p%met%InCDec(2) = InCVar(1); p%met%InCohB(2) = InCVar(2)
   end if

   call YS_GetRAryDefault(Doc, 'spatial_coherence_parameters:InCDec3', InCVar, UseDefault, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (.not. UseDefault) then
      p%met%InCDec(3) = InCVar(1); p%met%InCohB(3) = InCVar(2)
   end if

   call YS_GetRVarDefault(Doc, 'spatial_coherence_parameters:CohExp', p%met%CohExp, UseDefault, TmpErrStat, TmpErrMsg); if (Failed()) return

   !..................................................................................................................................
   !  error checking + zlOffset/UstarSlope/UstarOffset adjustment (mirrors ReadInputFile:937-975)
   !..................................................................................................................................

   if (.not. p%met%IsIECModel) then
      if (abs(p%met%Latitude) < 5.0 .or. abs(p%met%Latitude) > 90.0) then
         call SetErrStat(ErrID_Fatal, 'The latitude must be between -90 and 90 degrees but not between -5 and 5 degrees.', ErrStat, ErrMsg, RoutineName)
      end if
      if (p%met%Ustar <= 0.0_ReKi) call SetErrStat(ErrID_Fatal, 'The friction velocity must be a positive number.', ErrStat, ErrMsg, RoutineName)
      if (p%met%ZI <= 0.0_ReKi) call SetErrStat(ErrID_Fatal, 'The mixing layer depth must be a positive number for unstable flows.', ErrStat, ErrMsg, RoutineName)
   end if

   if (p%met%COHEXP < 0.0_ReKi) call SetErrStat(ErrID_Fatal, 'The coherence exponent must be non-negative.', ErrStat, ErrMsg, RoutineName)

   do I = 1, 3
      if (p%met%InCDec(I) <= 0.0_ReKi) call SetErrStat(ErrID_Fatal, 'The '//Comp(I)//'-component coherence decrement must be a positive number.', ErrStat, ErrMsg, RoutineName)
   end do

   TmpZLary = getZLProfile(TmpUary, RotorDiskHeights, p%met%Rich_No, p%met%ZL, p%met%L, 0.0_ReKi, p%met%WindProfileType)
   p%met%zlOffset = p%met%ZL - sum(TmpZLary) / size(TmpZLary)

   if (.not. p%met%UWskip) then
      TmpUstarD = (TmpUstar(1) - 2.0*TmpUstar(2) + TmpUstar(3))
      if (.not. EqualRealNos(TmpUstarD, 0.0_ReKi)) then
         p%met%UstarSlope  = 3.0*(p%met%Ustar - sqrt(abs(p%met%PC_UW))) / TmpUstarD
         p%met%UstarOffset = sqrt(abs(p%met%PC_UW)) - p%met%UstarSlope*(TmpUstar(2) - p%met%UstarOffset)
      else
         p%met%UstarSlope  = 0.0
         p%met%UstarOffset = sqrt(abs(p%met%PC_UW))
      end if
   end if

   !===============================================================================================================================
   ! coherent_turbulence_scaling_parameters (mirrors ReadInputFile:978-1110): only actually
   ! read for non-IEC spectral models -- exactly like the text path, which never consumes
   ! these lines (and therefore never draws from the RNG for them) when IsIECModel is true.
   !===============================================================================================================================
   if (.not. p%met%IsIECModel) then

      if (p%WrFile(FileExt_CTS)) then

         call YamlGet(Doc, 'coherent_turbulence_scaling_parameters:CTEventPath', p%CohStr%CTEventPath, TmpErrStat, TmpErrMsg); if (Failed()) return

         call YS_GetRawScalar(Doc, 'coherent_turbulence_scaling_parameters:CTEventFile', Line, TmpErrStat, TmpErrMsg); if (Failed()) return

         if (p%met%KHtest) then
            p%CohStr%CText = 'les'
            p%CohStr%CTEventFile = trim(p%CohStr%CTEventPath)//PathSep//'Events.xtm'
            call WrScr(' LES events will be used for the KH test.')
         else
            p%CohStr%CText = Line   ! preserves case formatting, in case it matters
            call Conv2UC(Line)
            if (Line(1:6) == "RANDOM") then
               call RndUnif(p%RNG, OtherSt_RandNum, tmp)
               if (tmp <= 0.5) then
                  p%CohStr%CText = 'les'
               else
                  p%CohStr%CText = 'dns'
               end if
            end if
            p%CohStr%CTEventFile = trim(p%CohStr%CTEventPath)//PathSep//'Events.'//trim(p%CohStr%CText)
         end if

         call YamlGet(Doc, 'coherent_turbulence_scaling_parameters:Randomize', Randomize, TmpErrStat, TmpErrMsg); if (Failed()) return
         call YamlGet(Doc, 'coherent_turbulence_scaling_parameters:DistScl', p%CohStr%DistScl, TmpErrStat, TmpErrMsg); if (Failed()) return
         call YamlGet(Doc, 'coherent_turbulence_scaling_parameters:CTLy', p%CohStr%CTLy, TmpErrStat, TmpErrMsg); if (Failed()) return
         call YamlGet(Doc, 'coherent_turbulence_scaling_parameters:CTLz', p%CohStr%CTLz, TmpErrStat, TmpErrMsg); if (Failed()) return

         if (p%met%KHtest) then
            p%CohStr%DistScl = 1.0
            p%CohStr%CTLy    = 0.5
            p%CohStr%CTLz    = 0.5
            Randomize = .false.
            call SetErrStat(ErrID_Info, 'Billow will cover rotor disk for KH test.', ErrStat, ErrMsg, RoutineName)
         else if (Randomize) then
            call RndUnif(p%RNG, OtherSt_RandNum, tmp)
            if (tmp > 0.25 .or. p%grid%RotorDiameter <= 30.0) then
               p%CohStr%DistScl = 1.0
               p%CohStr%CTLy    = 0.5
               p%CohStr%CTLz    = 0.5
            else
               p%CohStr%DistScl = 0.5
               p%CohStr%CTLy    = 0.5
               if (tmp < 0.125) then
                  p%CohStr%CTLz = 0.0
               else
                  p%CohStr%CTLz = 1.0
               end if
            end if
         else
            if (p%CohStr%DistScl < 0.0) then
               call SetErrStat(ErrID_Fatal, 'The disturbance scale must be a positive.', ErrStat, ErrMsg, RoutineName)
            else if (p%grid%RotorDiameter <= 30.0 .and. p%CohStr%DistScl < 1.0) then
               call SetErrStat(ErrID_Fatal, 'The disturbance scale must be at least 1.0 for rotor diameters less than 30.', ErrStat, ErrMsg, RoutineName)
            else if (p%grid%RotorDiameter*p%CohStr%DistScl <= 15.0) then
               call SetErrStat(ErrID_Fatal, 'The coherent turbulence must be greater than 15 meters in height.  '// &
                           'Increase the rotor diameter or the disturbance scale. ', ErrStat, ErrMsg, RoutineName)
            end if
         end if

         call YamlGet(Doc, 'coherent_turbulence_scaling_parameters:CTStartTime', p%CohStr%CTStartTime, TmpErrStat, TmpErrMsg); if (Failed()) return
         p%CohStr%CTStartTime = max(p%CohStr%CTStartTime, 0.0_ReKi)

      end if   ! WrFile(FileExt_CTS)

   else   ! IECVKM, IECKAI, MODVKM, OR API models

      if (p%IEC%NumTurbInp .and. EqualRealNos(p%IEC%PerTurbInt, 0.0_ReKi)) then
         p%met%TurbModel = 'NONE'
         p%met%TurbModel_ID = SpecModel_NONE
      end if

   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseYamlBody

!> Required scalar raw text, case-preserved, no "default"-token special-casing --
!! the YAML counterpart of a plain `CALL ReadVar(..., CharVar, ...)` into a string
!! buffer. Used for fields the text reader post-processes itself (RandSeed2,
!! UsableTime, TurbModel, IECstandard, IECturbc, IEC_WindType, CTEventFile) rather
!! than routing through ReadCVarDefault/ReadRVarDefault.
subroutine YS_GetRawScalar(Doc, Path, Text, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   character(*),   intent(in   ) :: Path
   character(*),   intent(  out) :: Text
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'YS_GetRawScalar'
   integer(IntKi) :: iNode
   logical        :: WasFound

   ErrStat = ErrID_None
   ErrMsg  = ""
   Text    = ""

   call YamlGetNode(Doc, Path, iNode, ErrStat, ErrMsg, Found=WasFound)
   if (ErrStat >= AbortErrLev) return
   if (.not. WasFound) then
      call SetErrStat(ErrID_Fatal, '>> The required key "'//trim(Path)//'" was not found.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   if (Doc%Nodes(iNode)%Kind /= YAML_SCALAR) then
      call SetErrStat(ErrID_Fatal, '>> The key "'//trim(Path)//'" must be a scalar value.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   if (len_trim(Doc%Nodes(iNode)%Scalar) > len(Text)) then
      call SetErrStat(ErrID_Fatal, '>> The value of key "'//trim(Path)//'" is too long.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   call Yaml_MarkUsed(Doc, iNode, .false.)
   Text = Doc%Nodes(iNode)%Scalar
end subroutine YS_GetRawScalar

!> YAML counterpart of ReadCVarDefault (TS_FileIO.f90:1820): fetches the raw scalar
!! text; the literal "default" token (any case, exact match after TRIM+Conv2UC)
!! leaves CharVar untouched and sets Def=.TRUE., exactly like the text reader --
!! including its apparent quirk that a non-default value is stored upper-cased
!! (Conv2UC is applied to the whole line before the DEFAULT test, and CharVar is
!! assigned from that same upper-cased copy). When IGNORE is present and true, the
!! key must still exist (mirrors the text reader's unconditional line consumption)
!! but its content is discarded and Def=.TRUE. is forced without inspecting it.
subroutine YS_GetCVarDefault(Doc, Path, CharVar, Def, ErrStat, ErrMsg, IGNORE)
   type(YamlDoc),  intent(inout) :: Doc
   character(*),   intent(in   ) :: Path
   character(*),   intent(inout) :: CharVar
   logical,        intent(  out) :: Def
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg
   logical, intent(in), optional :: IGNORE

   character(1024) :: CharLine

   call YS_GetRawScalar(Doc, Path, CharLine, ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return

   if (present(IGNORE)) then
      if (IGNORE) then
         Def = .true.
         return
      end if
   end if

   call Conv2UC(CharLine)
   if (trim(CharLine) == 'DEFAULT') then
      Def = .true.
   else
      CharVar = CharLine
      Def = .false.
   end if
end subroutine YS_GetCVarDefault

!> YAML counterpart of ReadRAryDefault (TS_FileIO.f90:1873): a fixed 2-element real
!! array (InCDec1/2/3's "decrement, coherence-B" pair) that may instead be the
!! literal scalar "default".
subroutine YS_GetRAryDefault(Doc, Path, RealAry, Def, ErrStat, ErrMsg, IGNORE)
   type(YamlDoc),  intent(inout) :: Doc
   character(*),   intent(in   ) :: Path
   real(ReKi),     intent(inout) :: RealAry(:)
   logical,        intent(  out) :: Def
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg
   logical, intent(in), optional :: IGNORE

   character(*), parameter :: RoutineName = 'YS_GetRAryDefault'
   character(1024) :: CharLine
   integer(IntKi)  :: IOS, i
   integer(IntKi)  :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   call YS_GetRawScalar(Doc, Path, CharLine, ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return

   if (present(IGNORE)) then
      if (IGNORE) then
         Def = .true.
         return
      end if
   end if

   call Conv2UC(CharLine)
   if (trim(CharLine) == 'DEFAULT') then
      Def = .true.
   else
      read (CharLine, *, iostat=IOS) RealAry
      if (IOS /= 0) then
         RealAry = 0.0_ReKi
         read (CharLine, *, iostat=IOS) RealAry(1)
      end if
      call CheckIOS(IOS, '(YAML input)', Path, NumType, TmpErrStat, TmpErrMsg)
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      do i = 1, size(RealAry)
         call CheckRealVar(RealAry(i), Path, TmpErrStat, TmpErrMsg)
         call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      end do
      Def = .false.
   end if
end subroutine YS_GetRAryDefault

!> YAML counterpart of ReadRVarDefault (TS_FileIO.f90:1951): a real scalar that may
!! instead be the literal "default" (leaving RealVar untouched) or, when IGNORESTR
!! is present, the literal "none" (also leaving RealVar untouched, but additionally
!! setting IGNORESTR so the caller can skip using this parameter at all -- used by
!! PC_UW/PC_UV/PC_VW, which accept "none" as a distinct third option from a value or
!! "default").
subroutine YS_GetRVarDefault(Doc, Path, RealVar, Def, ErrStat, ErrMsg, IGNORE, IGNORESTR)
   type(YamlDoc),  intent(inout)   :: Doc
   character(*),   intent(in   )   :: Path
   real(ReKi),     intent(inout)   :: RealVar
   logical,        intent(  out)   :: Def
   integer(IntKi), intent(  out)   :: ErrStat
   character(*),   intent(  out)   :: ErrMsg
   logical, intent(in   ), optional :: IGNORE
   logical, intent(inout), optional :: IGNORESTR

   character(*), parameter :: RoutineName = 'YS_GetRVarDefault'
   character(1024) :: CharLine
   integer(IntKi)  :: IOS
   integer(IntKi)  :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   call YS_GetRawScalar(Doc, Path, CharLine, ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return

   if (present(IGNORE)) then
      if (IGNORE) then
         Def = .true.
         return
      end if
   end if

   call Conv2UC(CharLine)

   if (present(IGNORESTR)) then
      if (trim(CharLine) == 'NONE') then
         IGNORESTR = .true.
         Def       = .true.
         return
      end if
   end if

   if (trim(CharLine) == 'DEFAULT') then
      Def = .true.
      return
   else
      read (CharLine, *, iostat=IOS) RealVar
      call CheckIOS(IOS, '(YAML input)', Path, NumType, TmpErrStat, TmpErrMsg)
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      call CheckRealVar(RealVar, Path, TmpErrStat, TmpErrMsg)
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Def = .false.
      if (present(IGNORESTR)) IGNORESTR = .false.
   end if
end subroutine YS_GetRVarDefault

end submodule TS_Yaml_SubMod
