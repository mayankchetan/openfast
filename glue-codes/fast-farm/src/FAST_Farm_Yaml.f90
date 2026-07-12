!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of FAST.Farm.
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
!> Reader for the YAML form of the FAST.Farm primary input file. Fills the same
!! Farm_ParameterType/WD_InputFileType/AWAE_InputFileType fields as Farm_ReadPrimaryFile
!! (the text path, FAST_Farm_IO.f90:555-1033), so Farm_ValidateInput and everything
!! downstream is shared between the two formats.
!!
!! Schema: sections mirror the text file's banners -- simulation_control (Echo,
!! AbortLevel, TMax, Mod_AmbWind, Mod_WaveField, Mod_SharedMooring),
!! shared_mooring_system (MD_FileName, DT_Mooring, MooringVis),
!! ambient_wind_vtk, ambient_wind_inflowwind, ambient_wind_amrex (all three always
!! required, exactly as the text path unconditionally reads all three blocks
!! regardless of which Mod_AmbWind is active -- only the post-hoc select-case picks
!! which one actually populates DT_low/DT_high/WindFilePath), turbines,
!! wake_dynamics, curled_wake_parameters, wake_added_turbulence, visualization,
!! output, output_channels. A leading top-level `description` scalar supplies FTitle
!! (optional, defaults to '', same convention as FAST_Yaml.f90's own `description`).
!!
!! turbines is a sequence of block mappings (one per turbine; NumTurbines derives
!! from the list length) -- the headline anchors/merge showcase for this deck, since
!! wind-farm layouts are exactly the "define one turbine, copy it with a change"
!! use case documented in yaml_input.rst. Columns WT_X/WT_Y/WT_Z/WT_FASTInFile are
!! always present; the six high-resolution-grid columns (X0_High, Y0_High, Z0_High,
!! dX_High, dY_High, dZ_High) are read only when Mod_AmbWind is 2 or 3, exactly
!! mirroring the text path's per-row column-count switch (:749-754). WT_FASTInFile
!! resolves relative to the farm deck (text or YAML .fst both work downstream) --
!! second-order rule, stays a verbatim path, never inlined (paths-only turbines is
!! this task's fixed scope; fully-inline turbines are deferred to a follow-up).
!!
!! k_vAmb, k_vShr, WAT_k_Def, and WAT_k_Grad are each a 5-element list (matching the
!! text path's ReadAryWDefault "set of 5 parameters" reads) that may instead be the
!! literal scalar "default" -- YamlGet's own Default= only special-cases scalar
!! reads, so GetR8AryWDefault (below) reimplements that same "default" check for
!! these four fixed-length array keys.
!!
!! These stay path-valued (second-order rule, never inlined): MD_FileName (a .yaml
!! MoorDyn file already works via the Wave-2 funnel), WindFilePath, InflowFile,
!! WindDirPrefix, WAT_BoxFile, WT_FASTInFile. There is no SC_FileName/super-controller
!! field in this tree.
module FAST_Farm_Yaml

   use NWTC_Library
   use YamlInput
   use FAST_Farm_Types

   implicit none
   private

   public :: Farm_ParseYamlFile

contains

!> Load and parse a YAML-format FAST.Farm primary input file. Mirrors the contract of
!! Farm_ReadPrimaryFile (the text path), including echo handling.
subroutine Farm_ParseYamlFile(InputFile, p, WD_InitInp, AWAE_InitInp, OutList, ErrStat, ErrMsg)
   character(*),              intent(in   ) :: InputFile     !< the .yaml primary input file
   type(Farm_ParameterType),  intent(inout) :: p              !< FAST.Farm parameter data (p%OutFileRoot already set)
   type(WD_InputFileType),    intent(  out) :: WD_InitInp     !< input-file data for WakeDynamics module
   type(AWAE_InputFileType),  intent(  out) :: AWAE_InitInp   !< input-file data for AWAE module
   character(ChanLen),        intent(  out) :: OutList(:)     !< list of user-requested output channels
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'Farm_ParseYamlFile'
   type(YamlDoc)               :: Doc
   character(1024)             :: PriPath
   logical                     :: Echo, WasFound
   integer(IntKi)               :: UnEc
   integer(IntKi)               :: TmpErrStat
   character(ErrMsgLen)         :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1

   call GetPath(InputFile, PriPath)   ! input files (except WT_FASTInFile handled per-row) resolve relative to the farm deck

   call Yaml_LoadFile(InputFile, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   Echo = .false.
   call YamlGet(Doc, 'simulation_control:Echo', Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (Echo) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included), which supersedes the text path's line-by-line echo
      call OpenEcho(UnEc, trim(p%OutFileRoot)//'.ech', TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for FAST.Farm primary input file: '//trim(InputFile)
      call Yaml_LoadFile(InputFile, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
      ! the reload reset the Used flags; re-read Echo so Yaml_WarnUnused doesn't flag it
      call YamlGet(Doc, 'simulation_control:Echo', Echo, TmpErrStat, TmpErrMsg, Default=.false.)
      if (Failed()) return
   end if

   !----------------------------------------------------------------------------------
   ! header / description (optional; FTitle defaults to '')
   !----------------------------------------------------------------------------------
   p%FTitle = ''
   call YamlGet(Doc, 'description', p%FTitle, TmpErrStat, TmpErrMsg, Found=WasFound)
   if (Failed()) return
   call WrScr(' Heading of the FAST.Farm input file: ')
   call WrScr('   '//trim(p%FTitle))

   call ParseYamlDoc(Doc, PriPath, p, WD_InitInp, AWAE_InitInp, OutList, TmpErrStat, TmpErrMsg)
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

end subroutine Farm_ParseYamlFile

!> Fill p/WD_InitInp/AWAE_InitInp/OutList from a parsed document. Split from the file
!! wrapper so the header/echo handling above stays out of the section-by-section body.
subroutine ParseYamlDoc(Doc, PriPath, p, WD_InitInp, AWAE_InitInp, OutList, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   character(*),               intent(in   ) :: PriPath
   type(Farm_ParameterType),  intent(inout) :: p
   type(WD_InputFileType),    intent(  out) :: WD_InitInp
   type(AWAE_InputFileType),  intent(  out) :: AWAE_InitInp
   character(ChanLen),        intent(  out) :: OutList(:)
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'Farm_ParseYamlDoc'
   character(10)                :: AbortLevel
   real(DbKi)                   :: TmpTime
   real(DbKi)                   :: DT_High_IfW, DT_Low_IfW
   real(DbKi)                   :: DT_High_VTK, DT_Low_VTK
   real(DbKi)                   :: DT_High_AMReX, DT_Low_AMReX
   character(1024)               :: InflowPathIfW
   character(1024)               :: InflowPathVTK
   character(1024)               :: InflowPathAMReX
   real(ReKi)                    :: DefaultReVal
   real(ReKi)                    :: TmpRAry5(5)
   integer(IntKi), allocatable    :: TmpIntAry(:)
   real(ReKi), allocatable        :: TmpReAry(:)
   character(:), allocatable      :: TmpList(:)
   logical                        :: TabDelim
   integer(IntKi)                 :: OutFileFmt
   integer(IntKi)                 :: i
   integer(IntKi)                 :: TmpErrStat
   character(ErrMsgLen)           :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   !----------------------------------------------------------------------------------
   ! simulation_control (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'simulation_control:AbortLevel', AbortLevel, TmpErrStat, TmpErrMsg, Default='FATAL')
   if (Failed()) return
   call Conv2UC(AbortLevel)
   select case (trim(AbortLevel))
      case ('WARNING')
         AbortErrLev = ErrID_Warn
      case ('SEVERE')
         AbortErrLev = ErrID_Severe
      case ('FATAL')
         AbortErrLev = ErrID_Fatal
      case default
         call SetErrStat(ErrID_Fatal, 'Invalid AbortLevel specified in FAST.Farm input file. '// &
                          'Valid entries are "WARNING", "SEVERE", or "FATAL".', ErrStat, ErrMsg, RoutineName)
         return
   end select

   call YamlGet(Doc, 'simulation_control:TMax', p%TMax, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'simulation_control:Mod_AmbWind', AWAE_InitInp%Mod_AmbWind, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'simulation_control:Mod_WaveField', p%WaveFieldMod, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'simulation_control:Mod_SharedMooring', p%MooringMod, TmpErrStat, TmpErrMsg); if (Failed()) return

   !----------------------------------------------------------------------------------
   ! shared_mooring_system (required) -- MD_FileName is a farm-level MoorDyn input
   ! file; a .yaml MoorDyn file already works via the Wave-2 funnel (second-order rule:
   ! stays path-valued, never inlined)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'shared_mooring_system:MD_FileName', p%MD_FileName, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (PathIsRelative(p%MD_FileName)) p%MD_FileName = trim(PriPath)//trim(p%MD_FileName)
   call YamlGet(Doc, 'shared_mooring_system:DT_Mooring', p%DT_mooring, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'shared_mooring_system:MooringVis', p%WrMooringVis, TmpErrStat, TmpErrMsg); if (Failed()) return

   !----------------------------------------------------------------------------------
   ! ambient_wind_vtk (required -- always read, exactly as the text path unconditionally
   ! reads this block regardless of Mod_AmbWind)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'ambient_wind_vtk:DT_Low-VTK', DT_Low_VTK, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_vtk:DT_High-VTK', DT_High_VTK, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_vtk:WindFilePath', InflowPathVTK, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (PathIsRelative(InflowPathVTK)) InflowPathVTK = trim(PriPath)//trim(InflowPathVTK)
   call YamlGet(Doc, 'ambient_wind_vtk:ChkWndFiles', AWAE_InitInp%ChkWndFiles, TmpErrStat, TmpErrMsg); if (Failed()) return

   !----------------------------------------------------------------------------------
   ! ambient_wind_inflowwind (required -- always read)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'ambient_wind_inflowwind:DT_Low', DT_Low_IfW, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_inflowwind:DT_High', DT_High_IfW, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_inflowwind:nX_Low', AWAE_InitInp%nX_Low, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_inflowwind:nY_Low', AWAE_InitInp%nY_Low, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_inflowwind:nZ_Low', AWAE_InitInp%nZ_Low, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_inflowwind:X0_Low', AWAE_InitInp%X0_Low, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_inflowwind:Y0_Low', AWAE_InitInp%Y0_Low, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_inflowwind:Z0_Low', AWAE_InitInp%Z0_Low, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_inflowwind:dX_Low', AWAE_InitInp%dX_Low, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_inflowwind:dY_Low', AWAE_InitInp%dY_Low, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_inflowwind:dZ_Low', AWAE_InitInp%dZ_Low, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_inflowwind:nX_High', AWAE_InitInp%nX_High, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_inflowwind:nY_High', AWAE_InitInp%nY_High, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_inflowwind:nZ_High', AWAE_InitInp%nZ_High, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_inflowwind:InflowFile', InflowPathIfW, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (PathIsRelative(InflowPathIfW)) InflowPathIfW = trim(PriPath)//trim(InflowPathIfW)

   !----------------------------------------------------------------------------------
   ! ambient_wind_amrex (required -- always read)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'ambient_wind_amrex:WindDirPrefix', InflowPathAMReX, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (PathIsRelative(InflowPathAMReX)) InflowPathAMReX = trim(PriPath)//trim(InflowPathAMReX)
   call YamlGet(Doc, 'ambient_wind_amrex:DirStartIndex', AWAE_InitInp%DirStartIndex, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_amrex:DT_Low-AMReX', DT_Low_AMReX, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ambient_wind_amrex:DT_High-AMReX', DT_High_AMReX, TmpErrStat, TmpErrMsg); if (Failed()) return

   ! Ensure consistency between AWAE_Inputs and FAST.Farm time steps
   select case (AWAE_InitInp%Mod_AmbWind)
   case (1)
      p%DT_low = DT_Low_VTK
      p%DT_high = DT_High_VTK
      p%WindFilePath = InflowPathVTK
   case (2,3)
      p%DT_low = DT_Low_IfW
      p%DT_high = DT_High_IfW
      p%WindFilePath = InflowPathIfW
   case (4)
      p%DT_low = DT_Low_AMReX
      p%DT_high = DT_High_AMReX
      p%WindFilePath = InflowPathAMReX
   end select
   AWAE_InitInp%dt_low = p%DT_low
   AWAE_InitInp%dt_high = p%DT_high
   AWAE_InitInp%InflowFile = p%WindFilePath

   !----------------------------------------------------------------------------------
   ! turbines (required) -- a list of block mappings; NumTurbines derives from its
   ! length; high-res-grid columns present only when Mod_AmbWind is 2 or 3
   !----------------------------------------------------------------------------------
   call ReadTurbines(Doc, PriPath, p, AWAE_InitInp, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! wake_dynamics (required, with ReadVarWDefault-equivalent defaults on several keys)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'wake_dynamics:Mod_Wake', WD_InitInp%Mod_Wake, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'wake_dynamics:RotorDiamRef', p%RotorDiamRef, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'wake_dynamics:dr', WD_InitInp%dr, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'wake_dynamics:NumRadii', WD_InitInp%NumRadii, TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGet(Doc, 'wake_dynamics:NumDFull', WD_InitInp%NumDFull, TmpErrStat, TmpErrMsg, Default=15.0_ReKi)
   if (Failed()) return
   call YamlGet(Doc, 'wake_dynamics:NumDBuff', WD_InitInp%NumDBuff, TmpErrStat, TmpErrMsg, Default=5.0_ReKi)
   if (Failed()) return

   WD_InitInp%RotorDiamRef = p%RotorDiamRef

   ! f_c default: Eq. (32) of https://doi.org/10.1002/we.2785, with U=10, a=1/3
   DefaultReVal = 12.5_ReKi/(p%RotorDiamRef/2._ReKi)
   call YamlGet(Doc, 'wake_dynamics:f_c', WD_InitInp%f_c, TmpErrStat, TmpErrMsg, Default=DefaultReVal)
   if (Failed()) return

   call YamlGet(Doc, 'wake_dynamics:C_HWkDfl_O', WD_InitInp%C_HWkDfl_O, TmpErrStat, TmpErrMsg, Default=0.0_ReKi)
   if (Failed()) return

   if (WD_InitInp%Mod_Wake == Mod_Wake_Curl) then
      DefaultReVal = 0.0_ReKi
   else
      DefaultReVal = 0.3_ReKi
   end if
   call YamlGet(Doc, 'wake_dynamics:C_HWkDfl_OY', WD_InitInp%C_HWkDfl_OY, TmpErrStat, TmpErrMsg, Default=DefaultReVal)
   if (Failed()) return
   WD_InitInp%C_HWkDfl_OY = WD_InitInp%C_HWkDfl_OY/D2R   ! m/deg -> m/radians

   call YamlGet(Doc, 'wake_dynamics:C_HWkDfl_x', WD_InitInp%C_HWkDfl_x, TmpErrStat, TmpErrMsg, Default=0.0_ReKi)
   if (Failed()) return

   if (WD_InitInp%Mod_Wake == Mod_Wake_Curl) then
      DefaultReVal = 0.0_ReKi
   else
      DefaultReVal = -0.004_ReKi
   end if
   call YamlGet(Doc, 'wake_dynamics:C_HWkDfl_xY', WD_InitInp%C_HWkDfl_xY, TmpErrStat, TmpErrMsg, Default=DefaultReVal)
   if (Failed()) return
   WD_InitInp%C_HWkDfl_xY = WD_InitInp%C_HWkDfl_xY/D2R   ! 1/deg -> 1/radians

   call YamlGet(Doc, 'wake_dynamics:C_NearWake', WD_InitInp%C_NearWake, TmpErrStat, TmpErrMsg, Default=1.8_ReKi)
   if (Failed()) return

   call GetR8AryWDefault(Doc, 'wake_dynamics:k_vAmb', 5, TmpRAry5, &
      (/ 0.05_ReKi, 1.0_ReKi, 0.0_ReKi, 1.0_ReKi, 0.01_ReKi /), TmpErrStat, TmpErrMsg)
   if (Failed()) return
   WD_InitInp%k_vAmb      = TmpRAry5(1)
   WD_InitInp%C_vAmb_FMin = TmpRAry5(2)
   WD_InitInp%C_vAmb_DMin = TmpRAry5(3)
   WD_InitInp%C_vAmb_DMax = TmpRAry5(4)
   WD_InitInp%C_vAmb_Exp  = TmpRAry5(5)

   call GetR8AryWDefault(Doc, 'wake_dynamics:k_vShr', 5, TmpRAry5, &
      (/ 0.016_ReKi, 0.2_ReKi, 3.0_ReKi, 25.0_ReKi, 0.1_ReKi /), TmpErrStat, TmpErrMsg)
   if (Failed()) return
   WD_InitInp%k_vShr      = TmpRAry5(1)
   WD_InitInp%C_vShr_FMin = TmpRAry5(2)
   WD_InitInp%C_vShr_DMin = TmpRAry5(3)
   WD_InitInp%C_vShr_DMax = TmpRAry5(4)
   WD_InitInp%C_vShr_Exp  = TmpRAry5(5)

   call YamlGet(Doc, 'wake_dynamics:Mod_WakeDiam', WD_InitInp%Mod_WakeDiam, TmpErrStat, TmpErrMsg, &
      Default=WakeDiamMod_RotDiam)
   if (Failed()) return
   call YamlGet(Doc, 'wake_dynamics:C_WakeDiam', WD_InitInp%C_WakeDiam, TmpErrStat, TmpErrMsg, Default=0.95_ReKi)
   if (Failed()) return
   call YamlGet(Doc, 'wake_dynamics:Mod_Meander', AWAE_InitInp%Mod_Meander, TmpErrStat, TmpErrMsg, &
      Default=MeanderMod_WndwdJinc)
   if (Failed()) return
   call YamlGet(Doc, 'wake_dynamics:C_Meander', AWAE_InitInp%C_Meander, TmpErrStat, TmpErrMsg, Default=1.9_ReKi)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! curled_wake_parameters (required, with ReadVarWDefault-equivalent defaults)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'curled_wake_parameters:Swirl', WD_InitInp%Swirl, TmpErrStat, TmpErrMsg, Default=.true.)
   if (Failed()) return
   call YamlGet(Doc, 'curled_wake_parameters:k_VortexDecay', WD_InitInp%k_VortexDecay, TmpErrStat, TmpErrMsg, &
      Default=0.0_ReKi)
   if (Failed()) return
   call YamlGet(Doc, 'curled_wake_parameters:NumVortices', WD_InitInp%NumVortices, TmpErrStat, TmpErrMsg, Default=100)
   if (Failed()) return
   call YamlGet(Doc, 'curled_wake_parameters:sigma_D', WD_InitInp%sigma_D, TmpErrStat, TmpErrMsg, Default=0.2_ReKi)
   if (Failed()) return
   call YamlGet(Doc, 'curled_wake_parameters:FilterInit', WD_InitInp%FilterInit, TmpErrStat, TmpErrMsg, Default=1)
   if (Failed()) return
   call YamlGet(Doc, 'curled_wake_parameters:k_vCurl', WD_InitInp%k_vCurl, TmpErrStat, TmpErrMsg, Default=2.0_ReKi)
   if (Failed()) return
   call YamlGet(Doc, 'curled_wake_parameters:Mod_Projection', AWAE_InitInp%Mod_Projection, TmpErrStat, TmpErrMsg, &
      Default=-1)
   if (Failed()) return
   if (AWAE_InitInp%Mod_Projection == -1) then
      ! -1 means the user selected "default"
      if (WD_InitInp%Mod_Wake == Mod_Wake_Curl) then
         AWAE_InitInp%Mod_Projection = 2
      else
         AWAE_InitInp%Mod_Projection = 1
      end if
   end if

   !----------------------------------------------------------------------------------
   ! wake_added_turbulence (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'wake_added_turbulence:WAT', p%WAT, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'wake_added_turbulence:WAT_BoxFile', p%WAT_BoxFile, TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGet(Doc, 'wake_added_turbulence:WAT_NxNyNz', TmpIntAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (size(TmpIntAry) /= 3) then
      call SetErrStat(ErrID_Fatal, '>> "wake_added_turbulence:WAT_NxNyNz" must have exactly 3 entries.', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if
   p%WAT_NxNyNz = TmpIntAry

   call YamlGet(Doc, 'wake_added_turbulence:WAT_DxDyDz', TmpReAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (size(TmpReAry) /= 3) then
      call SetErrStat(ErrID_Fatal, '>> "wake_added_turbulence:WAT_DxDyDz" must have exactly 3 entries.', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if
   p%WAT_DxDyDz = TmpReAry

   call YamlGet(Doc, 'wake_added_turbulence:WAT_ScaleBox', p%WAT_ScaleBox, TmpErrStat, TmpErrMsg, Default=.true.)
   if (Failed()) return

   call GetR8AryWDefault(Doc, 'wake_added_turbulence:WAT_k_Def', 5, TmpRAry5, &
      (/ 0.6_ReKi, 0.0_ReKi, 0.0_ReKi, 2.0_ReKi, 1.00_ReKi /), TmpErrStat, TmpErrMsg)
   if (Failed()) return
   WD_InitInp%WAT_k_Def_k_c  = TmpRAry5(1)
   WD_InitInp%WAT_k_Def_FMin = TmpRAry5(2)
   WD_InitInp%WAT_k_Def_DMin = TmpRAry5(3)
   WD_InitInp%WAT_k_Def_DMax = TmpRAry5(4)
   WD_InitInp%WAT_k_Def_Exp  = TmpRAry5(5)

   call GetR8AryWDefault(Doc, 'wake_added_turbulence:WAT_k_Grad', 5, TmpRAry5, &
      (/ 3.0_ReKi, 0.0_ReKi, 0.0_ReKi, 12.0_ReKi, 0.65_ReKi /), TmpErrStat, TmpErrMsg)
   if (Failed()) return
   WD_InitInp%WAT_k_Grad_k_c  = TmpRAry5(1)
   WD_InitInp%WAT_k_Grad_FMin = TmpRAry5(2)
   WD_InitInp%WAT_k_Grad_DMin = TmpRAry5(3)
   WD_InitInp%WAT_k_Grad_DMax = TmpRAry5(4)
   WD_InitInp%WAT_k_Grad_Exp  = TmpRAry5(5)

   if (PathIsRelative(p%WAT_BoxFile)) p%WAT_BoxFile = trim(PriPath)//trim(p%WAT_BoxFile)
   if (p%WAT > 0_IntKi) WD_InitInp%WAT = .true.

   !----------------------------------------------------------------------------------
   ! visualization (required) -- OutDisWindZ/X/Y are lists; NOutDisWindXY/YZ/XZ derive
   ! from their lengths (counts are not separate inputs)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'visualization:WrDisWind', AWAE_InitInp%WrDisWind, TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGet(Doc, 'visualization:OutDisWindZ', TmpReAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   AWAE_InitInp%NOutDisWindXY = size(TmpReAry)
   AWAE_InitInp%OutDisWindZ = TmpReAry

   call YamlGet(Doc, 'visualization:OutDisWindX', TmpReAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   AWAE_InitInp%NOutDisWindYZ = size(TmpReAry)
   AWAE_InitInp%OutDisWindX = TmpReAry

   call YamlGet(Doc, 'visualization:OutDisWindY', TmpReAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   AWAE_InitInp%NOutDisWindXZ = size(TmpReAry)
   AWAE_InitInp%OutDisWindY = TmpReAry

   call YamlGet(Doc, 'visualization:WrDisDT', AWAE_InitInp%WrDisDT, TmpErrStat, TmpErrMsg, Default=p%DT_low)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! output (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'output:SumPrint', p%SumPrint, TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGet(Doc, 'output:ChkptTime', TmpTime, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (TmpTime > p%TMax) then
      p%n_ChkptTime = huge(p%n_ChkptTime)
   else
      p%n_ChkptTime = nint(TmpTime/p%DT_low)
   end if

   call YamlGet(Doc, 'output:TStart', p%TStart, TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGet(Doc, 'output:OutFileFmt', OutFileFmt, TmpErrStat, TmpErrMsg); if (Failed()) return
   select case (OutFileFmt)
      case (1_IntKi)
         p%WrBinOutFile = .false.
         p%WrTxtOutFile = .true.
      case (2_IntKi)
         p%WrBinOutFile = .true.
         p%WrTxtOutFile = .false.
      case (3_IntKi)
         p%WrBinOutFile = .true.
         p%WrTxtOutFile = .true.
      case default
         ! we'll check this later....
   end select
   if (OutFileFmt /= 1_IntKi) then   ! TODO: Only allow text format for now; add binary format later.
      call SetErrStat(ErrID_Fatal, "FAST.Farm's OutFileFmt must be 1.", ErrStat, ErrMsg, RoutineName)
      return
   end if

   call YamlGet(Doc, 'output:TabDelim', TabDelim, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (TabDelim) then
      p%Delim = TAB
   else
      p%Delim = ' '
   end if

   call YamlGet(Doc, 'output:OutFmt', p%OutFmt, TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGet(Doc, 'output:OutAllPlanes', WD_InitInp%OutAllPlanes, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   call YamlGet(Doc, 'output:OutRadii', TmpIntAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   p%NOutRadii = size(TmpIntAry)
   p%OutRadii = TmpIntAry

   call YamlGet(Doc, 'output:OutDist', TmpReAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   p%NOutDist = size(TmpReAry)
   p%OutDist = TmpReAry

   call YamlGet(Doc, 'output:WindVelX', TmpReAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   p%NWindVel = size(TmpReAry)
   p%WindVelX = TmpReAry

   call YamlGet(Doc, 'output:WindVelY', TmpReAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (size(TmpReAry) /= p%NWindVel) then
      call SetErrStat(ErrID_Fatal, '>> "output:WindVelY" must have the same length as "output:WindVelX" ('// &
         trim(Num2LStr(p%NWindVel))//').', ErrStat, ErrMsg, RoutineName)
      return
   end if
   p%WindVelY = TmpReAry

   call YamlGet(Doc, 'output:WindVelZ', TmpReAry, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (size(TmpReAry) /= p%NWindVel) then
      call SetErrStat(ErrID_Fatal, '>> "output:WindVelZ" must have the same length as "output:WindVelX" ('// &
         trim(Num2LStr(p%NWindVel))//').', ErrStat, ErrMsg, RoutineName)
      return
   end if
   p%WindVelZ = TmpReAry

   !----------------------------------------------------------------------------------
   ! output_channels (required) -- NumOuts derives from the OutList length
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'output_channels:OutList', TmpList, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (size(TmpList) > size(OutList)) then
      call SetErrStat(ErrID_Fatal, 'output_channels:OutList may contain at most '//trim(Num2LStr(size(OutList)))// &
         ' channels; found '//trim(Num2LStr(size(TmpList)))//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   OutList = ''
   p%NumOuts = size(TmpList)
   do i = 1, p%NumOuts
      OutList(i) = TmpList(i)
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseYamlDoc

!> turbines -- a list of block mappings, one per turbine; NumTurbines derives from the
!! list length. Columns WT_X/WT_Y/WT_Z/WT_FASTInFile are always present; the six
!! high-resolution-grid columns are read only when Mod_AmbWind is 2 or 3, mirroring the
!! text path's per-row column-count switch (FAST_Farm_IO.f90:749-754). This is the
!! anchors/merge showcase: a wind-farm layout is exactly the "define one turbine, copy
!! it with a single change" use case documented in yaml_input.rst.
subroutine ReadTurbines(Doc, PriPath, p, AWAE_InitInp, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   character(*),               intent(in   ) :: PriPath
   type(Farm_ParameterType),  intent(inout) :: p
   type(AWAE_InputFileType),  intent(inout) :: AWAE_InitInp
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                 intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ReadTurbines'
   integer(IntKi)            :: iSeq, iRow, i, N
   integer(IntKi)             :: TmpErrStat
   character(ErrMsgLen)        :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'turbines', iSeq, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   N = int(Yaml_NumChildren(Doc, iSeq))
   p%NumTurbines = N

   call AllocAry(p%WT_Position, 3, N, 'WT_Position', TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName); if (Failed()) return
   call AllocAry(p%WT_FASTInFile, N, 'WT_FASTInFile', TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName); if (Failed()) return
   call AllocAry(AWAE_InitInp%WT_Position, 3, N, 'AWAE_InitInp%WT_Position', TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName); if (Failed()) return

   select case (AWAE_InitInp%Mod_AmbWind)
   case (2,3)
      call AllocAry(AWAE_InitInp%X0_high, N, 'AWAE_InitInp%X0_high', TmpErrStat, TmpErrMsg); if (Failed()) return
      call AllocAry(AWAE_InitInp%Y0_high, N, 'AWAE_InitInp%Y0_high', TmpErrStat, TmpErrMsg); if (Failed()) return
      call AllocAry(AWAE_InitInp%Z0_high, N, 'AWAE_InitInp%Z0_high', TmpErrStat, TmpErrMsg); if (Failed()) return
      call AllocAry(AWAE_InitInp%dX_high, N, 'AWAE_InitInp%dX_high', TmpErrStat, TmpErrMsg); if (Failed()) return
      call AllocAry(AWAE_InitInp%dY_high, N, 'AWAE_InitInp%dY_high', TmpErrStat, TmpErrMsg); if (Failed()) return
      call AllocAry(AWAE_InitInp%dZ_high, N, 'AWAE_InitInp%dZ_high', TmpErrStat, TmpErrMsg); if (Failed()) return
   end select

   do i = 1, N
      iRow = Yaml_Child(Doc, iSeq, i)

      call YamlGet(Doc, 'WT_X', p%WT_Position(1,i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'WT_Y', p%WT_Position(2,i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'WT_Z', p%WT_Position(3,i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'WT_FASTInFile', p%WT_FASTInFile(i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return

      select case (AWAE_InitInp%Mod_AmbWind)
      case (2,3)
         call YamlGet(Doc, 'X0_High', AWAE_InitInp%X0_high(i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'Y0_High', AWAE_InitInp%Y0_high(i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'Z0_High', AWAE_InitInp%Z0_high(i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'dX_High', AWAE_InitInp%dX_high(i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'dY_High', AWAE_InitInp%dY_high(i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'dZ_High', AWAE_InitInp%dZ_high(i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      end select

      AWAE_InitInp%WT_Position(:,i) = p%WT_Position(:,i)
      if (PathIsRelative(p%WT_FASTInFile(i))) p%WT_FASTInFile(i) = trim(PriPath)//trim(p%WT_FASTInFile(i))
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadTurbines

!> Fetch a fixed-length (N-element) real array, accepting the literal scalar
!! "default"/"DEFAULT" in place of the list (filling DefaultAry) -- the array
!! counterpart of YamlGet's own Default= (which only special-cases scalar reads).
!! Used for the four "set of 5 parameters" keys (k_vAmb, k_vShr, WAT_k_Def,
!! WAT_k_Grad), mirroring the text path's ReadAryWDefault reads.
subroutine GetR8AryWDefault(Doc, Path, N, Ary, DefaultAry, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   character(*),   intent(in   ) :: Path
   integer(IntKi), intent(in   ) :: N
   real(ReKi),     intent(  out) :: Ary(N)
   real(ReKi),     intent(in   ) :: DefaultAry(N)
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'GetR8AryWDefault'
   integer(IntKi)              :: iNode
   character(:), allocatable   :: TmpStr
   real(ReKi), allocatable     :: TmpAry(:)
   integer(IntKi)               :: TmpErrStat
   character(ErrMsgLen)          :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, Path, iNode, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   if (Doc%Nodes(iNode)%Kind == YAML_SCALAR) then
      TmpStr = Doc%Nodes(iNode)%Scalar
      call Conv2UC(TmpStr)
      if (trim(TmpStr) == 'DEFAULT') then
         call Yaml_MarkUsed(Doc, iNode, .false.)
         Ary = DefaultAry
         return
      else
         call SetErrStat(ErrID_Fatal, '>> "'//trim(Path)//'" must be a list of '//trim(Num2LStr(N))// &
            ' number(s) or the literal "default".', ErrStat, ErrMsg, RoutineName)
         return
      end if
   end if

   call YamlGet(Doc, Path, TmpAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpAry) /= N) then
      call SetErrStat(ErrID_Fatal, '>> "'//trim(Path)//'" must have exactly '//trim(Num2LStr(N))//' entries.', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if
   Ary = TmpAry

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine GetR8AryWDefault

end module FAST_Farm_Yaml
