!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of AeroDyn.
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
!> Reader for the YAML form of the AeroDyn primary input file. Fills the same AD_InputFile
!! structure as ParsePrimaryFileInfo (the text path, AeroDyn_IO.f90), so validation
!! (ValidateInputData) and everything downstream is shared between the two formats.
!!
!! Schema: sections mirror the text file's banners (general, environmental_conditions,
!! bemt_options, dbemt_options, olaf_options, unsteady_aero_options, airfoil_info,
!! rotor_blade_properties, hub_properties, nacelle_properties, tail_fin_aerodynamics,
!! tower_influence, outputs, output_channels, nodal_outputs).
!!
!! Legacy-only keys (WakeMod, AFAeroMod, SkewMod, FrozenWake, UAMod, and the
!! "SkewModFactor" alias for SkewRedistrFactor) are text-format compatibility cruft with
!! no YAML equivalent; the YAML schema exposes only the current key names.
!!
!! Second-order files stay path strings, resolved relative to the primary input file, and
!! are never converted or inlined: AA_InputFile (AeroAcoustics), OLAFInputFileName (OLAF/
!! FVW -- OLAF's own parameters live entirely in that separate file), AFNames (airfoil
!! polars, one per entry), ADBlFile (per-blade AeroDyn blade files), and TFinFile
!! (tail-fin aerodynamics, one per rotor).
!!
!! Counted lists/tables (AFNames, ADBlFile, tower_influence:tower_nodes,
!! outputs:BlOutNd/TwOutNd, output_channels:OutList, nodal_outputs:BldNd_OutList) derive
!! their counts (NumAFfiles, NumTwrNds, NBlOuts, NTwOuts, NumOuts, BldNd_NumOuts) from
!! list length; they are never themselves YAML keys. BlOutNd/TwOutNd are clamped (with a
!! warning, not fatal, matching the text path) to at most 9 entries.
!!
!! hub_properties, nacelle_properties, tail_fin_aerodynamics, and tower_influence are
!! themselves YAML lists with exactly one entry per rotor -- the rotor count comes from
!! NumBlades(:), an argument supplied by the driver/glue code (as in the text path), never
!! read from the file itself.
!!
!! DEFAULT-sentinel fields accept the literal scalar "default" exactly like "DEFAULT" in
!! the text format (DTAero falls back to the glue-code/driver timestep; AirDens/KinVisc/
!! SpdSound/Patm/Pvap fall back to the driver's default fluid properties; several
!! BEMT-options fields fall back to literal defaults) via YamlGet's Default= argument,
!! which resolves the "default" keyword identically whether the key is present-but-
!! "default" or omitted entirely.
!!
!! nodal_outputs:BldNd_BlOutNd is carried as a raw string (BldNd_BlOutNd_Str), exactly as
!! the text path does, since its legal values ("ALL", "Tip", "Root", or a comma-separated
!! list of node numbers) are resolved later downstream (AeroDyn_AllBldNdOuts_IO.f90), not
!! by this parser.
module AeroDyn_Yaml

   use NWTC_Library
   use YamlInput
   use AeroDyn_Types
   use AeroDyn_IO_Params
   use AeroDyn_AllBldNdOuts_IO, only: BldNd_MaxOutPts

   implicit none
   private

   public :: AD_ParseYamlFile
   public :: AD_ParseYamlFileInfo

contains

!> Load and parse a YAML-format AeroDyn primary input file. Mirrors the contract of
!! ProcessComFile + ParsePrimaryFileInfo (the text path), including echo handling.
!! UnEc is returned open (matching ParsePrimaryFileInfo's contract) because
!! ReadInputFiles/Init_AFIparams append blade- and airfoil-file echo content onto the
!! same unit later in AD_Init; the caller (AD_Init's own Cleanup) closes it.
subroutine AD_ParseYamlFile(InputFileName, PriPath, InitInp, RootName, NumBlades, interval, InputFileData, UnEc, ErrStat, ErrMsg)
   character(*),              intent(in   ) :: InputFileName !< the .yaml primary input file
   character(*),              intent(in   ) :: PriPath       !< primary path (second-order file resolution)
   type(AD_InitInputType),    intent(in   ) :: InitInp       !< Input data for initialization routine
   character(*),              intent(in   ) :: RootName      !< root name for the echo file
   integer(IntKi),            intent(in   ) :: NumBlades(:)  !< Number of blades per rotor -- from InitInp
   real(DbKi),                intent(in   ) :: interval      !< timestep (DTAero default)
   type(AD_InputFile),        intent(inout) :: InputFileData !< the shared input-file structure
   integer(IntKi),            intent(  out) :: UnEc          !< echo file unit, left open on success
   integer(IntKi),            intent(  out) :: ErrStat
   character(*),              intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'AD_ParseYamlFile'
   type(YamlDoc)           :: Doc
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
      write(UnEc, '(A)') 'Echo file for AeroDyn primary input file: '//trim(InputFileName)
      call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   call ParseYamlDoc(Doc, PriPath, InitInp, NumBlades, interval, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! typo guard: anything never looked up is reported (warning severity)
   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
      if (Failed .and. UnEc > 0_IntKi) then
         close(UnEc)
         UnEc = -1
      end if
   end function Failed

end subroutine AD_ParseYamlFile

!> Parse YAML-format AeroDyn input arriving as a FileInfoType -- the passed-data channel
!! used for inline module input from a YAML primary file (the glue code's inline AeroFile
!! handover when CompAero selects AeroDyn). Per-line provenance in the FileInfoType keeps
!! error messages pointing at the original file and line.
!! UnEc is returned open (see AD_ParseYamlFile) for the same reason: ReadInputFiles/
!! Init_AFIparams append to the same unit later in AD_Init.
subroutine AD_ParseYamlFileInfo(FileInfo, PriPath, InitInp, RootName, NumBlades, interval, InputFileData, UnEc, ErrStat, ErrMsg)
   type(FileInfoType),        intent(in   ) :: FileInfo
   character(*),              intent(in   ) :: PriPath
   type(AD_InitInputType),    intent(in   ) :: InitInp
   character(*),              intent(in   ) :: RootName
   integer(IntKi),            intent(in   ) :: NumBlades(:)
   real(DbKi),                intent(in   ) :: interval
   type(AD_InputFile),        intent(inout) :: InputFileData
   integer(IntKi),            intent(  out) :: UnEc
   integer(IntKi),            intent(  out) :: ErrStat
   character(*),              intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'AD_ParseYamlFileInfo'
   type(YamlDoc)           :: Doc
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
      write(UnEc, '(A)') 'Echo file for AeroDyn primary input (passed YAML data)'
      do i = 1, FileInfo%NumLines
         write(UnEc, '(A)') trim(FileInfo%Lines(i))
      end do
   end if

   call ParseYamlDoc(Doc, PriPath, InitInp, NumBlades, interval, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
      if (Failed .and. UnEc > 0_IntKi) then
         close(UnEc)
         UnEc = -1
      end if
   end function Failed

end subroutine AD_ParseYamlFileInfo

!> Fill InputFileData from a parsed document. Split from the file/FileInfo wrappers so
!! both entry points share it (mirrors the other modules' ParseYamlDoc split).
subroutine ParseYamlDoc(Doc, PriPath, InitInp, NumBlades, interval, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   character(*),               intent(in   ) :: PriPath
   type(AD_InitInputType),     intent(in   ) :: InitInp
   integer(IntKi),             intent(in   ) :: NumBlades(:)
   real(DbKi),                 intent(in   ) :: interval
   type(AD_InputFile),         intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'AD_ParseYamlDoc'
   logical                     :: TwrAeroFlag
   real(ReKi)                  :: IndTolerDefault
   integer(IntKi)              :: TmpErrStat
   character(ErrMsgLen)        :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   ! programmatic fields, not read from any input file (mirrors ParsePrimaryFileInfo)
   InputFileData%UA_Init%UA_OUTS    = 0
   InputFileData%UA_Init%d_34_to_ac = 0.5_ReKi

   ! pre-allocate the output-channel lists to their fixed capacities, exactly as the text
   ! path does, so ReadOutputChannels/ReadNodalOutputChannels can fill the first N entries
   call AllocAry(InputFileData%OutList, MaxOutPts, "AeroDyn Input File's OutList", TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%OutList = ''
   call AllocAry(InputFileData%BldNd_OutList, 2*BldNd_MaxOutPts, "AeroDyn Input File's BldNd_OutList", TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%BldNd_OutList = ''

   !----------------------------------------------------------------------------------
   ! general (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'general:Echo', InputFileData%Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return
   call YamlGet(Doc, 'general:DTAero', InputFileData%DTAero, TmpErrStat, TmpErrMsg, Default=interval)
   if (Failed()) return
   call YamlGet(Doc, 'general:Wake_Mod', InputFileData%Wake_Mod, TmpErrStat, TmpErrMsg, Default=WakeMod_BEMT)
   if (Failed()) return
   call YamlGet(Doc, 'general:TwrPotent', InputFileData%TwrPotent, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'general:TwrShadow', InputFileData%TwrShadow, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'general:TwrAero', TwrAeroFlag, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (TwrAeroFlag) then
      InputFileData%TwrAero = TwrAero_NoVIV
   else
      InputFileData%TwrAero = TwrAero_None
   end if
   call YamlGet(Doc, 'general:CavitCheck', InputFileData%CavitCheck, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'general:NacelleDrag', InputFileData%NacelleDrag, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'general:CompAA', InputFileData%CompAA, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'general:AA_InputFile', InputFileData%AA_InputFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (PathIsRelative(InputFileData%AA_InputFile)) InputFileData%AA_InputFile = trim(PriPath)//trim(InputFileData%AA_InputFile)

   !----------------------------------------------------------------------------------
   ! environmental_conditions (required) -- defaults come from the driver/glue code
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'environmental_conditions:AirDens', InputFileData%AirDens, TmpErrStat, TmpErrMsg, Default=InitInp%defFldDens)
   if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:KinVisc', InputFileData%KinVisc, TmpErrStat, TmpErrMsg, Default=InitInp%defKinVisc)
   if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:SpdSound', InputFileData%SpdSound, TmpErrStat, TmpErrMsg, Default=InitInp%defSpdSound)
   if (Failed()) return
   InputFileData%UA_Init%a_s = InputFileData%SpdSound
   call YamlGet(Doc, 'environmental_conditions:Patm', InputFileData%Patm, TmpErrStat, TmpErrMsg, Default=InitInp%defPatm)
   if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:Pvap', InputFileData%Pvap, TmpErrStat, TmpErrMsg, Default=InitInp%defPvap)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! bemt_options (required) -- read unconditionally regardless of Wake_Mod, exactly as
   ! the text path does (the "[unused when Wake_Mod=0 or 3]" banner note is a usage
   ! caveat, not a conditional read)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'bemt_options:BEM_Mod', InputFileData%BEM_Mod, TmpErrStat, TmpErrMsg, Default=BEMMod_2D)
   if (Failed()) return
   call YamlGet(Doc, 'bemt_options:Skew_Mod', InputFileData%Skew_Mod, TmpErrStat, TmpErrMsg, Default=Skew_Mod_Active)
   if (Failed()) return
   call YamlGet(Doc, 'bemt_options:SkewMomCorr', InputFileData%SkewMomCorr, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return
   call YamlGet(Doc, 'bemt_options:SkewRedistr_Mod', InputFileData%SkewRedistr_Mod, TmpErrStat, TmpErrMsg, Default=1_IntKi)
   if (Failed()) return
   ! documented key is "SkewRedistrFactor"; the registry field keeps the legacy name
   ! SkewModFactor (same field the text path's "SkewModFactor"/"SkewRedistrFactor" rename
   ! branch both ultimately fill)
   call YamlGet(Doc, 'bemt_options:SkewRedistrFactor', InputFileData%SkewModFactor, TmpErrStat, TmpErrMsg, &
                Default=(15.0_ReKi * pi / 32.0_ReKi))
   if (Failed()) return
   call YamlGet(Doc, 'bemt_options:TipLoss', InputFileData%TipLoss, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'bemt_options:HubLoss', InputFileData%HubLoss, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'bemt_options:TanInd', InputFileData%TanInd, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'bemt_options:AIDrag', InputFileData%AIDrag, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'bemt_options:TIDrag', InputFileData%TIDrag, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   ! text path's default is precision-dependent (5e-5 single / 5e-10 double); mirror that logic here
   if (ReKi==SiKi) then
      IndTolerDefault = real(5E-5, ReKi)
   else
      IndTolerDefault = real(5D-10, ReKi)
   end if
   call YamlGet(Doc, 'bemt_options:IndToler', InputFileData%IndToler, TmpErrStat, TmpErrMsg, Default=IndTolerDefault)
   if (Failed()) return
   call YamlGet(Doc, 'bemt_options:MaxIter', InputFileData%MaxIter, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'bemt_options:SectAvg', InputFileData%SectAvg, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return
   call YamlGet(Doc, 'bemt_options:SectAvgWeighting', InputFileData%SA_Weighting, TmpErrStat, TmpErrMsg, Default=1_IntKi)
   if (Failed()) return
   call YamlGet(Doc, 'bemt_options:SectAvgNPoints', InputFileData%SA_nPerSec, TmpErrStat, TmpErrMsg, Default=5_IntKi)
   if (Failed()) return
   call YamlGet(Doc, 'bemt_options:SectAvgPsiBwd', InputFileData%SA_PsiBwd, TmpErrStat, TmpErrMsg, Default=-60.0_ReKi)
   if (Failed()) return
   call YamlGet(Doc, 'bemt_options:SectAvgPsiFwd', InputFileData%SA_PsiFwd, TmpErrStat, TmpErrMsg, Default=60.0_ReKi)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! dbemt_options (required) -- read unconditionally regardless of Wake_Mod
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'dbemt_options:DBEMT_Mod', InputFileData%DBEMT_Mod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'dbemt_options:tau1_const', InputFileData%tau1_const, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! olaf_options (required) -- OLAFInputFileName stays a path; OLAF's own parameters
   ! (WakeLength, DTfvw, CircSolvMethod, ...) live entirely in that separate file and are
   ! parsed by FVW_IO, never inlined into the AeroDyn primary schema
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'olaf_options:OLAFInputFileName', InputFileData%FVWFileName, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (PathIsRelative(InputFileData%FVWFileName)) InputFileData%FVWFileName = trim(PriPath)//trim(InputFileData%FVWFileName)

   !----------------------------------------------------------------------------------
   ! unsteady_aero_options (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'unsteady_aero_options:AoA34', InputFileData%AoA34, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'unsteady_aero_options:UA_Mod', InputFileData%UA_Init%UAMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'unsteady_aero_options:FLookup', InputFileData%UA_Init%FLookup, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'unsteady_aero_options:IntegrationMethod', InputFileData%UA_Init%IntegrationMethod, TmpErrStat, TmpErrMsg, &
                Default=UA_Method_ABM4)
   if (Failed()) return
   call YamlGet(Doc, 'unsteady_aero_options:UAStartRad', InputFileData%UAStartRad, TmpErrStat, TmpErrMsg, Default=0.0_ReKi)
   if (Failed()) return
   call YamlGet(Doc, 'unsteady_aero_options:UAEndRad', InputFileData%UAEndRad, TmpErrStat, TmpErrMsg, Default=1.0_ReKi)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! airfoil_info (required) -- AFNames is a list of path strings; NumAFfiles derives
   ! from its length
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'airfoil_info:AFTabMod', InputFileData%AFTabMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'airfoil_info:InCol_Alfa', InputFileData%InCol_Alfa, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'airfoil_info:InCol_Cl', InputFileData%InCol_Cl, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'airfoil_info:InCol_Cd', InputFileData%InCol_Cd, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'airfoil_info:InCol_Cm', InputFileData%InCol_Cm, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'airfoil_info:InCol_Cpmin', InputFileData%InCol_Cpmin, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call ReadAirfoilNames(Doc, PriPath, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! rotor_blade_properties (required) -- ADBlFile is a list of path strings sized to the
   ! total blade count (sum(NumBlades)), not the text format's legacy max(MaxBl,...)
   ! padding (unused padding slots are never referenced downstream)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'rotor_blade_properties:UseBlCm', InputFileData%UseBlCm, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call ReadBladeFiles(Doc, PriPath, NumBlades, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! hub_properties / nacelle_properties / tail_fin_aerodynamics / tower_influence
   ! (required) -- one list entry per rotor; the rotor count is size(NumBlades), supplied
   ! by the driver/glue code, never read from the file
   !----------------------------------------------------------------------------------
   call ReadHubProperties(Doc, NumBlades, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call ReadNacelleProperties(Doc, NumBlades, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call ReadTailFinAero(Doc, PriPath, NumBlades, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call ReadTowerInfluence(Doc, NumBlades, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! outputs (required) -- NBlOuts/NTwOuts derive from BlOutNd/TwOutNd length (clamped to
   ! 9, with a warning, exactly as the text path clamps)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'outputs:SumPrint', InputFileData%SumPrint, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%UA_Init%WrSum = InputFileData%SumPrint
   call ReadBlTwOutNd(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! output_channels (required) -- NumOuts derives from the OutList length
   !----------------------------------------------------------------------------------
   call ReadOutputChannels(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! nodal_outputs (required) -- BldNd_NumOuts derives from the BldNd_OutList length,
   ! then forced to 0 when BldNd_BladesOut<=0, exactly as the text path does
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'nodal_outputs:BldNd_BladesOut', InputFileData%BldNd_BladesOut, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   ! BldNd_BlOutNd is carried as raw text ("ALL", "Tip", "Root", or a number list),
   ! resolved downstream in AeroDyn_AllBldNdOuts_IO.f90 -- not parsed here, exactly as the
   ! text path's BldNd_BlOutNd_Str fallback does
   call YamlGet(Doc, 'nodal_outputs:BldNd_BlOutNd', InputFileData%BldNd_BlOutNd_Str, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call ReadNodalOutputChannels(Doc, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (InputFileData%BldNd_BladesOut <= 0) InputFileData%BldNd_NumOuts = 0

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseYamlDoc

!> airfoil_info:AFNames -- a list of airfoil-polar file paths; NumAFfiles derives from its
!! length. Contents are read by the AirfoilInfo module, never inlined here.
subroutine ReadAirfoilNames(Doc, PriPath, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   character(*),               intent(in   ) :: PriPath
   type(AD_InputFile),         intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'ReadAirfoilNames'
   character(:), allocatable  :: TmpList(:)
   integer(IntKi)             :: i
   integer(IntKi)             :: TmpErrStat
   character(ErrMsgLen)       :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'airfoil_info:AFNames', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   InputFileData%NumAFfiles = size(TmpList)
   call AllocAry(InputFileData%AFNames, InputFileData%NumAFfiles, 'AFNames', TmpErrStat, TmpErrMsg)
   if (Failed()) return
   do i = 1, InputFileData%NumAFfiles
      InputFileData%AFNames(i) = TmpList(i)
      if (PathIsRelative(InputFileData%AFNames(i))) InputFileData%AFNames(i) = trim(PriPath)//trim(InputFileData%AFNames(i))
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadAirfoilNames

!> rotor_blade_properties:ADBlFile -- a list of per-blade AeroDyn blade-file paths, one
!! entry per blade (sum(NumBlades) entries total, in rotor order). Contents are read by
!! ReadBladeInputs, never inlined here.
subroutine ReadBladeFiles(Doc, PriPath, NumBlades, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   character(*),               intent(in   ) :: PriPath
   integer(IntKi),             intent(in   ) :: NumBlades(:)
   type(AD_InputFile),         intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'ReadBladeFiles'
   character(:), allocatable  :: TmpList(:)
   integer(IntKi)             :: numBladesTot
   integer(IntKi)             :: i
   integer(IntKi)             :: TmpErrStat
   character(ErrMsgLen)       :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   numBladesTot = sum(NumBlades)

   call YamlGet(Doc, 'rotor_blade_properties:ADBlFile', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpList) /= numBladesTot) then
      call SetErrStat(ErrID_Fatal, '"rotor_blade_properties:ADBlFile" must have exactly '// &
         trim(Num2LStr(numBladesTot))//' entries (the total blade count across all rotors); found '// &
         trim(Num2LStr(size(TmpList)))//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call AllocAry(InputFileData%ADBlFile, numBladesTot, 'ADBlFile', TmpErrStat, TmpErrMsg)
   if (Failed()) return
   do i = 1, numBladesTot
      InputFileData%ADBlFile(i) = TmpList(i)
      if (PathIsRelative(InputFileData%ADBlFile(i))) InputFileData%ADBlFile(i) = trim(PriPath)//trim(InputFileData%ADBlFile(i))
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadBladeFiles

!> hub_properties -- one list entry per rotor: VolHub, HubCenBx.
subroutine ReadHubProperties(Doc, NumBlades, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   integer(IntKi),             intent(in   ) :: NumBlades(:)
   type(AD_InputFile),         intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadHubProperties'
   integer(IntKi) :: iOuter, iRow, iR
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'hub_properties', iOuter, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadCount(int(Yaml_NumChildren(Doc, iOuter)), size(NumBlades), 'hub_properties')) return

   do iR = 1, size(NumBlades)
      iRow = Yaml_Child(Doc, iOuter, iR)
      call YamlGet(Doc, 'VolHub', InputFileData%rotors(iR)%VolHub, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      call YamlGet(Doc, 'HubCenBx', InputFileData%rotors(iR)%HubCenBx, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

   logical function BadCount(NGiven, NExpect, Path)
      integer(IntKi), intent(in) :: NGiven
      integer(IntKi), intent(in) :: NExpect
      character(*),   intent(in) :: Path
      BadCount = (NGiven /= NExpect)
      if (BadCount) call SetErrStat(ErrID_Fatal, '"'//Path//'" must have exactly '//trim(Num2LStr(NExpect))// &
         ' entries (one per rotor); found '//trim(Num2LStr(NGiven))//'.', ErrStat, ErrMsg, RoutineName)
   end function BadCount

end subroutine ReadHubProperties

!> nacelle_properties -- one list entry per rotor: VolNac, NacCenB(3), NacArea(3),
!! NacCd(3), NacDragAC(3).
subroutine ReadNacelleProperties(Doc, NumBlades, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   integer(IntKi),             intent(in   ) :: NumBlades(:)
   type(AD_InputFile),         intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'ReadNacelleProperties'
   real(ReKi), allocatable    :: TmpVec(:)
   integer(IntKi)             :: iOuter, iRow, iR
   integer(IntKi)             :: TmpErrStat
   character(ErrMsgLen)       :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'nacelle_properties', iOuter, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadCount(int(Yaml_NumChildren(Doc, iOuter)), size(NumBlades), 'nacelle_properties')) return

   do iR = 1, size(NumBlades)
      iRow = Yaml_Child(Doc, iOuter, iR)
      call YamlGet(Doc, 'VolNac', InputFileData%rotors(iR)%VolNac, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return

      call YamlGet(Doc, 'NacCenB', TmpVec, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      if (BadVec3(TmpVec, 'NacCenB')) return
      InputFileData%rotors(iR)%NacCenB = TmpVec

      call YamlGet(Doc, 'NacArea', TmpVec, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      if (BadVec3(TmpVec, 'NacArea')) return
      InputFileData%rotors(iR)%NacArea = TmpVec

      call YamlGet(Doc, 'NacCd', TmpVec, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      if (BadVec3(TmpVec, 'NacCd')) return
      InputFileData%rotors(iR)%NacCd = TmpVec

      call YamlGet(Doc, 'NacDragAC', TmpVec, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      if (BadVec3(TmpVec, 'NacDragAC')) return
      InputFileData%rotors(iR)%NacDragAC = TmpVec
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

   logical function BadCount(NGiven, NExpect, Path)
      integer(IntKi), intent(in) :: NGiven
      integer(IntKi), intent(in) :: NExpect
      character(*),   intent(in) :: Path
      BadCount = (NGiven /= NExpect)
      if (BadCount) call SetErrStat(ErrID_Fatal, '"'//Path//'" must have exactly '//trim(Num2LStr(NExpect))// &
         ' entries (one per rotor); found '//trim(Num2LStr(NGiven))//'.', ErrStat, ErrMsg, RoutineName)
   end function BadCount

   logical function BadVec3(Vec, Nm)
      real(ReKi),   intent(in) :: Vec(:)
      character(*), intent(in) :: Nm
      BadVec3 = (size(Vec) /= 3)
      if (BadVec3) call SetErrStat(ErrID_Fatal, '"'//Nm//'" must have exactly 3 entries; found '// &
         trim(Num2LStr(size(Vec)))//'.', ErrStat, ErrMsg, RoutineName)
   end function BadVec3

end subroutine ReadNacelleProperties

!> tail_fin_aerodynamics -- one list entry per rotor: TFinAero, TFinFile. TFinFile stays a
!! path string; its contents are read by ReadTailFinInputs, never inlined here.
subroutine ReadTailFinAero(Doc, PriPath, NumBlades, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   character(*),               intent(in   ) :: PriPath
   integer(IntKi),             intent(in   ) :: NumBlades(:)
   type(AD_InputFile),         intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadTailFinAero'
   integer(IntKi) :: iOuter, iRow, iR
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'tail_fin_aerodynamics', iOuter, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadCount(int(Yaml_NumChildren(Doc, iOuter)), size(NumBlades), 'tail_fin_aerodynamics')) return

   do iR = 1, size(NumBlades)
      iRow = Yaml_Child(Doc, iOuter, iR)
      call YamlGet(Doc, 'TFinAero', InputFileData%rotors(iR)%TFinAero, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      call YamlGet(Doc, 'TFinFile', InputFileData%rotors(iR)%TFinFile, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      if (PathIsRelative(InputFileData%rotors(iR)%TFinFile)) &
         InputFileData%rotors(iR)%TFinFile = trim(PriPath)//trim(InputFileData%rotors(iR)%TFinFile)
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

   logical function BadCount(NGiven, NExpect, Path)
      integer(IntKi), intent(in) :: NGiven
      integer(IntKi), intent(in) :: NExpect
      character(*),   intent(in) :: Path
      BadCount = (NGiven /= NExpect)
      if (BadCount) call SetErrStat(ErrID_Fatal, '"'//Path//'" must have exactly '//trim(Num2LStr(NExpect))// &
         ' entries (one per rotor); found '//trim(Num2LStr(NGiven))//'.', ErrStat, ErrMsg, RoutineName)
   end function BadCount

end subroutine ReadTailFinAero

!> tower_influence -- one list entry per rotor, each holding a tower_nodes table (a list
!! of row mappings keyed TwrElev/TwrDiam/TwrCd/TwrTI/TwrCb/TwrCp/TwrCa); NumTwrNds derives
!! from that inner list's length.
subroutine ReadTowerInfluence(Doc, NumBlades, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   integer(IntKi),             intent(in   ) :: NumBlades(:)
   type(AD_InputFile),         intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadTowerInfluence'
   integer(IntKi) :: iOuter, iRotNode, iSeq, iRow, iR, I, N
   integer(IntKi) :: TmpErrStat
   character(ErrMsgLen) :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'tower_influence', iOuter, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadCount(int(Yaml_NumChildren(Doc, iOuter)), size(NumBlades), 'tower_influence')) return

   do iR = 1, size(NumBlades)
      iRotNode = Yaml_Child(Doc, iOuter, iR)
      call YamlGetNode(Doc, 'tower_nodes', iSeq, TmpErrStat, TmpErrMsg, From=iRotNode)
      if (Failed()) return

      N = int(Yaml_NumChildren(Doc, iSeq))
      InputFileData%rotors(iR)%NumTwrNds = N

      call AllocAry(InputFileData%rotors(iR)%TwrElev, N, 'TwrElev', TmpErrStat, TmpErrMsg); if (Failed()) return
      call AllocAry(InputFileData%rotors(iR)%TwrDiam, N, 'TwrDiam', TmpErrStat, TmpErrMsg); if (Failed()) return
      call AllocAry(InputFileData%rotors(iR)%TwrCd,   N, 'TwrCd',   TmpErrStat, TmpErrMsg); if (Failed()) return
      call AllocAry(InputFileData%rotors(iR)%TwrTI,   N, 'TwrTI',   TmpErrStat, TmpErrMsg); if (Failed()) return
      call AllocAry(InputFileData%rotors(iR)%TwrCb,   N, 'TwrCb',   TmpErrStat, TmpErrMsg); if (Failed()) return
      call AllocAry(InputFileData%rotors(iR)%TwrCp,   N, 'TwrCp',   TmpErrStat, TmpErrMsg); if (Failed()) return
      call AllocAry(InputFileData%rotors(iR)%TwrCa,   N, 'TwrCa',   TmpErrStat, TmpErrMsg); if (Failed()) return

      do I = 1, N
         iRow = Yaml_Child(Doc, iSeq, I)
         call YamlGet(Doc, 'TwrElev', InputFileData%rotors(iR)%TwrElev(I), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'TwrDiam', InputFileData%rotors(iR)%TwrDiam(I), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'TwrCd',   InputFileData%rotors(iR)%TwrCd(I),   TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'TwrTI',   InputFileData%rotors(iR)%TwrTI(I),   TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'TwrCb',   InputFileData%rotors(iR)%TwrCb(I),   TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'TwrCp',   InputFileData%rotors(iR)%TwrCp(I),   TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'TwrCa',   InputFileData%rotors(iR)%TwrCa(I),   TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      end do
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

   logical function BadCount(NGiven, NExpect, Path)
      integer(IntKi), intent(in) :: NGiven
      integer(IntKi), intent(in) :: NExpect
      character(*),   intent(in) :: Path
      BadCount = (NGiven /= NExpect)
      if (BadCount) call SetErrStat(ErrID_Fatal, '"'//Path//'" must have exactly '//trim(Num2LStr(NExpect))// &
         ' entries (one per rotor); found '//trim(Num2LStr(NGiven))//'.', ErrStat, ErrMsg, RoutineName)
   end function BadCount

end subroutine ReadTowerInfluence

!> outputs:BlOutNd/TwOutNd -- NBlOuts/NTwOuts derive from list length, clamped (with a
!! warning, not fatal, exactly as the text path clamps) to at most 9 entries.
subroutine ReadBlTwOutNd(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(AD_InputFile),         intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'ReadBlTwOutNd'
   integer(IntKi), allocatable :: TmpList(:)
   integer(IntKi)              :: TmpErrStat
   character(ErrMsgLen)        :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'outputs:BlOutNd', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpList) > size(InputFileData%BlOutNd)) then
      call SetErrStat(ErrID_Warn, ' Warning: number of blade output nodes exceeds '// &
         trim(Num2LStr(size(InputFileData%BlOutNd)))//'.', ErrStat, ErrMsg, RoutineName)
      InputFileData%NBlOuts = size(InputFileData%BlOutNd)
   else
      InputFileData%NBlOuts = size(TmpList)
   end if
   InputFileData%BlOutNd = 0_IntKi
   InputFileData%BlOutNd(1:InputFileData%NBlOuts) = TmpList(1:InputFileData%NBlOuts)

   call YamlGet(Doc, 'outputs:TwOutNd', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpList) > size(InputFileData%TwOutNd)) then
      call SetErrStat(ErrID_Warn, ' Warning: number of blade output nodes exceeds '// &
         trim(Num2LStr(size(InputFileData%TwOutNd)))//'.', ErrStat, ErrMsg, RoutineName)
      InputFileData%NTwOuts = size(InputFileData%TwOutNd)
   else
      InputFileData%NTwOuts = size(TmpList)
   end if
   InputFileData%TwOutNd = 0_IntKi
   InputFileData%TwOutNd(1:InputFileData%NTwOuts) = TmpList(1:InputFileData%NTwOuts)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadBlTwOutNd

!> output_channels:OutList -- NumOuts derives from the OutList length.
subroutine ReadOutputChannels(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(AD_InputFile),         intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'ReadOutputChannels'
   character(:), allocatable  :: TmpList(:)
   integer(IntKi)              :: i
   integer(IntKi)              :: TmpErrStat
   character(ErrMsgLen)        :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'output_channels:OutList', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpList) > size(InputFileData%OutList)) then
      call SetErrStat(ErrID_Fatal, 'output_channels:OutList may contain at most '// &
         trim(Num2LStr(size(InputFileData%OutList)))//' channels; found '//trim(Num2LStr(size(TmpList)))//'.', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if
   InputFileData%NumOuts = size(TmpList)
   do i = 1, InputFileData%NumOuts
      if (len_trim(TmpList(i)) > ChanLen) then
         call SetErrStat(ErrID_Fatal, 'output_channels:OutList entry "'//trim(TmpList(i))//'" is longer than the '// &
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

end subroutine ReadOutputChannels

!> nodal_outputs:BldNd_OutList -- BldNd_NumOuts derives from the list length.
subroutine ReadNodalOutputChannels(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   type(AD_InputFile),         intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'ReadNodalOutputChannels'
   character(:), allocatable  :: TmpList(:)
   integer(IntKi)              :: i
   integer(IntKi)              :: TmpErrStat
   character(ErrMsgLen)        :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'nodal_outputs:BldNd_OutList', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpList) > size(InputFileData%BldNd_OutList)) then
      call SetErrStat(ErrID_Fatal, 'nodal_outputs:BldNd_OutList may contain at most '// &
         trim(Num2LStr(size(InputFileData%BldNd_OutList)))//' channels; found '//trim(Num2LStr(size(TmpList)))//'.', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if
   InputFileData%BldNd_NumOuts = size(TmpList)
   do i = 1, InputFileData%BldNd_NumOuts
      if (len_trim(TmpList(i)) > ChanLen) then
         call SetErrStat(ErrID_Fatal, 'nodal_outputs:BldNd_OutList entry "'//trim(TmpList(i))//'" is longer than the '// &
            trim(Num2LStr(ChanLen))//'-character channel-name limit.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      InputFileData%BldNd_OutList(i) = TmpList(i)
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadNodalOutputChannels

end module AeroDyn_Yaml
