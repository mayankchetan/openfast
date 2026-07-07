!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of ElastoDyn.
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
!> Reader for the YAML form of the ElastoDyn primary input file. Fills the same
!! ED_InputFile structure as ReadPrimaryFile (the text path in ElastoDyn_IO.f90, a
!! sequential ReadVar/ReadAry-style reader), so everything downstream (blade/tower/
!! furling sub-file reads, ED_SetParameters, etc.) is shared between the two formats.
!!
!! Schema: sections mirror the text file's banners (simulation_control,
!! degrees_of_freedom, initial_conditions, turbine_configuration, mass_and_inertia,
!! blade, rotor_teeter, yaw_friction, drivetrain, furling, tower, output,
!! nodal_outputs). Keys keep their documented names.
!!
!! DT accepts the literal scalar "default" exactly like the text format (falls back to
!! the glue/driver-supplied interval). BlPitch/PreCone/TipMass/PBrIner/BlPIner -- three
!! separate keyword lines in the text format (index 1..MaxBl) -- are 3-entry lists.
!! BldFile/FurlFile/TwrFile are carried as path strings (second-order files: never
!! converted/inlined, resolved relative to the primary input file exactly like the
!! text path). NTwGages/NBlGages are NOT derived from list length here (unlike most
!! other modules' counted lists): the text format carries them as an explicit count
!! that may be less than the list length (a warning, not a list-length rule), so this
!! reader keeps that same explicit-count contract; TwrGagNd/BldGagNd lists must have at
!! least NTwGages/NBlGages entries. NumOuts derives from output:OutList's length.
!!
!! The optional nodal_outputs section (BldNd_BladesOut/BldNd_BlOutNd/BldNd_OutList,
!! mirroring ElastoDyn_AllBldNdOuts_IO's "additional nodal outputs" section) is simply
!! absent when not needed -- unlike the text path's tolerate-anything-malformed
!! fallback (which only exists to handle old decks predating that section), missing it
!! here just means BldNd_NumOuts = BldNd_BladesOut = 0.
!!
!! Unit conversions (deg->rad, rpm->rad/s, %->fraction) are applied in the same order
!! and on the same raw values as the text path, so verbatim numeric literals produce
!! bit-identical results.
module ElastoDyn_Yaml

   use NWTC_Library
   use YamlInput
   use ElastoDyn_Types
   use ElastoDyn_AllBldNdOuts_IO, only: BldNd_MaxOutPts

   implicit none
   private

   ! MaxBl/MaxOutPts/ED_Ver mirror the parameters defined in ElastoDyn_Parameters (both
   ! by name and by file, ElastoDyn_Parameters lives inside ElastoDyn_IO.f90 alongside
   ! MODULE ElastoDyn_IO). This module is USEd BY ElastoDyn_IO (at the funnel call site
   ! in ED_ReadInput), so it cannot in turn USE anything from that same file without
   ! creating a circular module dependency; these three constants are duplicated here
   ! instead. None of the three has changed since ElastoDyn's original implementation.
   integer(IntKi), parameter :: MaxBl      = 3
   integer(IntKi), parameter :: MaxOutPts  = 1003
   type(ProgDesc), parameter :: ED_Ver     = ProgDesc( 'ElastoDyn', '', '' )

   public :: ED_ParseYamlFile
   public :: ED_ParseYamlFileInfo

contains

!> Load and parse a YAML-format ElastoDyn primary input file. Mirrors the contract of
!! ReadPrimaryFile (the text path), including echo handling and the returned
!! BldFile/FurlFile/TwrFile path arrays (still read as separate text files downstream).
subroutine ED_ParseYamlFile(InputFileName, InputFileData, BldFile, FurlFile, TwrFile, &
                             OutFileRoot, Default_DT, UnEc, ErrStat, ErrMsg)
   character(*),               intent(in   ) :: InputFileName !< the .yaml primary input file
   type(ED_InputFile),         intent(inout) :: InputFileData !< the shared input-file structure
   character(*),               intent(  out) :: BldFile(MaxBl)!< name of the files containing blade inputs
   character(*),               intent(  out) :: FurlFile      !< name of the file containing furling inputs
   character(*),               intent(  out) :: TwrFile       !< name of the file containing tower inputs
   character(*),               intent(in   ) :: OutFileRoot   !< the rootname of the echo file, possibly opened here
   real(DbKi),                 intent(in   ) :: Default_DT    !< default DT supplied by the caller (glue code)
   integer(IntKi),             intent(  out) :: UnEc          !< I/O unit for echo file; > 0 if opened
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ED_ParseYamlFile'
   type(YamlDoc)           :: Doc
   character(1024)         :: PriPath
   logical                 :: Echo
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1

   call GetPath( InputFileName, PriPath )    ! sub-files (blade/tower/furling) resolve relative to this

   call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   Echo = .false.
   call YamlGet(Doc, 'simulation_control:Echo', Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (Echo) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included), which supersedes the text path's line-by-line echo
      call OpenEcho(UnEc, trim(OutFileRoot)//'.ech', TmpErrStat, TmpErrMsg, ED_Ver)
      if (Failed()) return
      write(UnEc, '(/,A,/)') 'Data from '//trim(ED_Ver%Name)//' primary input file "'//trim(InputFileName)//'":'
      call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   call ParseYamlDoc(Doc, PriPath, Default_DT, InputFileData, BldFile, FurlFile, TwrFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! typo guard: anything never looked up is reported (warning severity)
   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ED_ParseYamlFile

!> Parse YAML-format ElastoDyn input arriving as a FileInfoType -- the passed-data
!! channel used for inline module input from a YAML primary file (the glue code's
!! inline EDFile handover when CompElast selects full ElastoDyn). Per-line provenance
!! in the FileInfoType keeps error messages pointing at the original file and line.
subroutine ED_ParseYamlFileInfo(FileInfo, InputFileData, BldFile, FurlFile, TwrFile, &
                                 PriPath, OutFileRoot, Default_DT, UnEc, ErrStat, ErrMsg)
   type(FileInfoType),         intent(in   ) :: FileInfo
   type(ED_InputFile),         intent(inout) :: InputFileData
   character(*),               intent(  out) :: BldFile(MaxBl)
   character(*),               intent(  out) :: FurlFile
   character(*),               intent(  out) :: TwrFile
   character(*),               intent(in   ) :: PriPath       !< path sub-files resolve relative to (the .fst's directory)
   character(*),               intent(in   ) :: OutFileRoot
   real(DbKi),                 intent(in   ) :: Default_DT
   integer(IntKi),             intent(  out) :: UnEc
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ED_ParseYamlFileInfo'
   type(YamlDoc)           :: Doc
   logical                 :: Echo
   integer(IntKi)          :: i
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1

   call Yaml_LoadFileInfo(FileInfo, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   Echo = .false.
   call YamlGet(Doc, 'simulation_control:Echo', Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (Echo) then
      call OpenEcho(UnEc, trim(OutFileRoot)//'.ech', TmpErrStat, TmpErrMsg, ED_Ver)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for ElastoDyn primary input (passed YAML data)'
      do i = 1, FileInfo%NumLines
         write(UnEc, '(A)') trim(FileInfo%Lines(i))
      end do
   end if

   call ParseYamlDoc(Doc, PriPath, Default_DT, InputFileData, BldFile, FurlFile, TwrFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ED_ParseYamlFileInfo

!> Fill InputFileData (and the BldFile/FurlFile/TwrFile path arrays) from a parsed
!! document. Split from the file/FileInfo wrappers so both entry points share it
!! (mirrors the other modules' ParseYamlDoc split).
subroutine ParseYamlDoc(Doc, PriPath, Default_DT, InputFileData, BldFile, FurlFile, TwrFile, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   character(*),               intent(in   ) :: PriPath
   real(DbKi),                 intent(in   ) :: Default_DT
   type(ED_InputFile),         intent(inout) :: InputFileData
   character(*),               intent(  out) :: BldFile(MaxBl)
   character(*),               intent(  out) :: FurlFile
   character(*),               intent(  out) :: TwrFile
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'ED_ParseYamlDoc'
   character(:), allocatable  :: TmpList(:)
   real(ReKi),   allocatable  :: TmpReAry(:)
   integer(IntKi)             :: i
   integer(IntKi)             :: NGagList
   integer(IntKi), allocatable :: TmpGagAry(:)
   logical                    :: WasFound
   integer(IntKi)             :: iNodalOutputs
   integer(IntKi)             :: TmpErrStat
   character(ErrMsgLen)       :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   !----------------------------------------------------------------------------------
   ! simulation_control (required) -- DT accepts "default" (= the glue/driver-supplied
   ! interval, exactly like ParseVarWDefault's text-format "DEFAULT" handling)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'simulation_control:Method', InputFileData%Method, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'simulation_control:DT', InputFileData%DT, TmpErrStat, TmpErrMsg, Default=Default_DT)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! degrees_of_freedom (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'degrees_of_freedom:FlapDOF1',  InputFileData%FlapDOF1,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:FlapDOF2',  InputFileData%FlapDOF2,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:EdgeDOF',   InputFileData%EdgeDOF,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:PitchDOF',  InputFileData%PitchDOF,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:TeetDOF',   InputFileData%TeetDOF,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:DrTrDOF',   InputFileData%DrTrDOF,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:GenDOF',    InputFileData%GenDOF,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:YawDOF',    InputFileData%YawDOF,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:TwFADOF1',  InputFileData%TwFADOF1,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:TwFADOF2',  InputFileData%TwFADOF2,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:TwSSDOF1',  InputFileData%TwSSDOF1,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:TwSSDOF2',  InputFileData%TwSSDOF2,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:PtfmSgDOF', InputFileData%PtfmSgDOF, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:PtfmSwDOF', InputFileData%PtfmSwDOF, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:PtfmHvDOF', InputFileData%PtfmHvDOF, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:PtfmRDOF',  InputFileData%PtfmRDOF,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:PtfmPDOF',  InputFileData%PtfmPDOF,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:PtfmYDOF',  InputFileData%PtfmYDOF,  TmpErrStat, TmpErrMsg); if (Failed()) return

   !----------------------------------------------------------------------------------
   ! initial_conditions (required) -- angles deg->rad, RotSpeed rpm->rad/s, matching
   ! the text path's post-read conversions
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'initial_conditions:OoPDefl', InputFileData%OoPDefl, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'initial_conditions:IPDefl',  InputFileData%IPDefl,  TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGet(Doc, 'initial_conditions:BlPitch', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), MaxBl, 'initial_conditions:BlPitch')) return
   InputFileData%BlPitch = TmpReAry*D2R

   call YamlGet(Doc, 'initial_conditions:TeetDefl', InputFileData%TeetDefl, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%TeetDefl = InputFileData%TeetDefl*D2R
   call YamlGet(Doc, 'initial_conditions:Azimuth', InputFileData%Azimuth, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%Azimuth = InputFileData%Azimuth*D2R
   call YamlGet(Doc, 'initial_conditions:RotSpeed', InputFileData%RotSpeed, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%RotSpeed = InputFileData%RotSpeed*RPM2RPS
   call YamlGet(Doc, 'initial_conditions:NacYaw', InputFileData%NacYaw, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%NacYaw = InputFileData%NacYaw*D2R
   call YamlGet(Doc, 'initial_conditions:TTDspFA', InputFileData%TTDspFA, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'initial_conditions:TTDspSS', InputFileData%TTDspSS, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'initial_conditions:PtfmSurge', InputFileData%PtfmSurge, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'initial_conditions:PtfmSway', InputFileData%PtfmSway, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'initial_conditions:PtfmHeave', InputFileData%PtfmHeave, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'initial_conditions:PtfmRoll', InputFileData%PtfmRoll, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%PtfmRoll = InputFileData%PtfmRoll*D2R
   call YamlGet(Doc, 'initial_conditions:PtfmPitch', InputFileData%PtfmPitch, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%PtfmPitch = InputFileData%PtfmPitch*D2R
   call YamlGet(Doc, 'initial_conditions:PtfmYaw', InputFileData%PtfmYaw, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%PtfmYaw = InputFileData%PtfmYaw*D2R

   !----------------------------------------------------------------------------------
   ! turbine_configuration (required) -- BldFile is carried as a path list (second-order
   ! file, never inlined); relative paths resolve against the primary input file
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'turbine_configuration:NumBl', InputFileData%NumBl, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:TipRad', InputFileData%TipRad, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:HubRad', InputFileData%HubRad, TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGet(Doc, 'turbine_configuration:PreCone', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), MaxBl, 'turbine_configuration:PreCone')) return
   InputFileData%PreCone = TmpReAry*D2R

   call YamlGet(Doc, 'turbine_configuration:HubCM', InputFileData%HubCM, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:UndSling', InputFileData%UndSling, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:Delta3', InputFileData%Delta3, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%Delta3 = InputFileData%Delta3*D2R
   call YamlGet(Doc, 'turbine_configuration:AzimB1Up', InputFileData%AzimB1Up, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%AzimB1Up = InputFileData%AzimB1Up*D2R
   call YamlGet(Doc, 'turbine_configuration:OverHang', InputFileData%OverHang, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:ShftGagL', InputFileData%ShftGagL, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:ShftTilt', InputFileData%ShftTilt, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%ShftTilt = InputFileData%ShftTilt*D2R
   call YamlGet(Doc, 'turbine_configuration:NacCMxn', InputFileData%NacCMxn, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:NacCMyn', InputFileData%NacCMyn, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:NacCMzn', InputFileData%NacCMzn, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:NcIMUxn', InputFileData%NcIMUxn, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:NcIMUyn', InputFileData%NcIMUyn, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:NcIMUzn', InputFileData%NcIMUzn, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:Twr2Shft', InputFileData%Twr2Shft, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:TowerHt', InputFileData%TowerHt, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:TowerBsHt', InputFileData%TowerBsHt, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:PtfmCMxt', InputFileData%PtfmCMxt, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:PtfmCMyt', InputFileData%PtfmCMyt, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:PtfmCMzt', InputFileData%PtfmCMzt, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:PtfmRefxt', InputFileData%PtfmRefxt, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:PtfmRefyt', InputFileData%PtfmRefyt, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'turbine_configuration:PtfmRefzt', InputFileData%PtfmRefzt, TmpErrStat, TmpErrMsg); if (Failed()) return

   !----------------------------------------------------------------------------------
   ! mass_and_inertia (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'mass_and_inertia:TipMass', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), MaxBl, 'mass_and_inertia:TipMass')) return
   InputFileData%TipMass = TmpReAry

   call YamlGet(Doc, 'mass_and_inertia:PBrIner', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), MaxBl, 'mass_and_inertia:PBrIner')) return
   InputFileData%PBrIner = TmpReAry

   call YamlGet(Doc, 'mass_and_inertia:BlPIner', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), MaxBl, 'mass_and_inertia:BlPIner')) return
   InputFileData%BlPIner = TmpReAry

   call YamlGet(Doc, 'mass_and_inertia:HubMass', InputFileData%HubMass, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'mass_and_inertia:HubIner', InputFileData%HubIner, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'mass_and_inertia:HubIner_Teeter', InputFileData%HubIner_Teeter, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'mass_and_inertia:GenIner', InputFileData%GenIner, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'mass_and_inertia:NacMass', InputFileData%NacMass, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'mass_and_inertia:NacYIner', InputFileData%NacYIner, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'mass_and_inertia:YawBrMass', InputFileData%YawBrMass, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'mass_and_inertia:PtfmMass', InputFileData%PtfmMass, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'mass_and_inertia:PtfmRIner', InputFileData%PtfmRIner, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'mass_and_inertia:PtfmPIner', InputFileData%PtfmPIner, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'mass_and_inertia:PtfmYIner', InputFileData%PtfmYIner, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'mass_and_inertia:PtfmXYIner', InputFileData%PtfmXYIner, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'mass_and_inertia:PtfmYZIner', InputFileData%PtfmYZIner, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'mass_and_inertia:PtfmXZIner', InputFileData%PtfmXZIner, TmpErrStat, TmpErrMsg); if (Failed()) return

   !----------------------------------------------------------------------------------
   ! blade (required) -- BldFile: second-order file type, always referenced by path
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'blade:BldNodes', InputFileData%BldNodes, TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGet(Doc, 'blade:BldFile', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpList), MaxBl, 'blade:BldFile')) return
   do i = 1, MaxBl
      BldFile(i) = TmpList(i)
      if ( PathIsRelative( BldFile(i) ) ) BldFile(i) = trim(PriPath)//trim(BldFile(i))
   end do

   !----------------------------------------------------------------------------------
   ! rotor_teeter (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'rotor_teeter:TeetMod', InputFileData%TeetMod, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'rotor_teeter:TeetDmpP', InputFileData%TeetDmpP, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%TeetDmpP = InputFileData%TeetDmpP*D2R
   call YamlGet(Doc, 'rotor_teeter:TeetDmp', InputFileData%TeetDmp, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'rotor_teeter:TeetCDmp', InputFileData%TeetCDmp, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'rotor_teeter:TeetSStP', InputFileData%TeetSStP, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%TeetSStP = InputFileData%TeetSStP*D2R
   call YamlGet(Doc, 'rotor_teeter:TeetHStP', InputFileData%TeetHStP, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%TeetHStP = InputFileData%TeetHStP*D2R
   call YamlGet(Doc, 'rotor_teeter:TeetSSSp', InputFileData%TeetSSSp, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'rotor_teeter:TeetHSSp', InputFileData%TeetHSSp, TmpErrStat, TmpErrMsg); if (Failed()) return

   !----------------------------------------------------------------------------------
   ! yaw_friction (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'yaw_friction:YawFrctMod', InputFileData%YawFrctMod, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'yaw_friction:M_CSmax', InputFileData%M_CSmax, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'yaw_friction:M_FCSmax', InputFileData%M_FCSmax, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'yaw_friction:M_MCSmax', InputFileData%M_MCSmax, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'yaw_friction:M_CD', InputFileData%M_CD, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'yaw_friction:M_FCD', InputFileData%M_FCD, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'yaw_friction:M_MCD', InputFileData%M_MCD, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'yaw_friction:sig_v', InputFileData%sig_v, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'yaw_friction:sig_v2', InputFileData%sig_v2, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'yaw_friction:OmgCut', InputFileData%OmgCut, TmpErrStat, TmpErrMsg); if (Failed()) return

   !----------------------------------------------------------------------------------
   ! drivetrain (required) -- GBoxEff %->fraction
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'drivetrain:GBoxEff', InputFileData%GBoxEff, TmpErrStat, TmpErrMsg); if (Failed()) return
   InputFileData%GBoxEff = InputFileData%GBoxEff*0.01_ReKi
   call YamlGet(Doc, 'drivetrain:GBRatio', InputFileData%GBRatio, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'drivetrain:DTTorSpr', InputFileData%DTTorSpr, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'drivetrain:DTTorDmp', InputFileData%DTTorDmp, TmpErrStat, TmpErrMsg); if (Failed()) return

   !----------------------------------------------------------------------------------
   ! furling (required) -- FurlFile: second-order file type, always referenced by path
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'furling:Furling', InputFileData%Furling, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'furling:FurlFile', FurlFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   if ( PathIsRelative( FurlFile ) ) FurlFile = trim(PriPath)//trim(FurlFile)

   !----------------------------------------------------------------------------------
   ! tower (required) -- TwrFile: second-order file type, always referenced by path
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'tower:TwrNodes', InputFileData%TwrNodes, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'tower:TwrFile', TwrFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   if ( PathIsRelative( TwrFile ) ) TwrFile = trim(PriPath)//trim(TwrFile)

   !----------------------------------------------------------------------------------
   ! output (required) -- NumOuts derives from output:OutList's length; NTwGages/
   ! NBlGages are explicit counts (not list lengths -- the text format allows the count
   ! to be less than the list length), clamped to the TwrGagNd/BldGagNd array size
   ! exactly like the text path
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'output:SumPrint', InputFileData%SumPrint, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:OutFile', InputFileData%OutFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:TabDelim', InputFileData%TabDelim, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:OutFmt', InputFileData%OutFmt, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:Tstart', InputFileData%Tstart, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'output:DecFact', InputFileData%DecFact, TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGet(Doc, 'output:NTwGages', InputFileData%NTwGages, TmpErrStat, TmpErrMsg); if (Failed()) return
   if ( InputFileData%NTwGages > size(InputFileData%TwrGagNd) ) then
      call SetErrStat( ErrID_Warn, 'Number of tower strain gages exceeds '// &
                                   trim(Num2LStr(size(InputFileData%TwrGagNd)))//'.', ErrStat, ErrMsg, RoutineName )
      if (ErrStat >= AbortErrLev) return
      InputFileData%NTwGages = size(InputFileData%TwrGagNd)
   end if
   call YamlGet(Doc, 'output:TwrGagNd', TmpGagAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   NGagList = size(TmpGagAry)
   if (NGagList < InputFileData%NTwGages) then
      call SetErrStat(ErrID_Fatal, 'output:TwrGagNd must list at least output:NTwGages ('// &
         trim(Num2LStr(InputFileData%NTwGages))//') entries; found '//trim(Num2LStr(NGagList))//'.', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if
   InputFileData%TwrGagNd = 0
   if (InputFileData%NTwGages > 0) InputFileData%TwrGagNd(1:InputFileData%NTwGages) = TmpGagAry(1:InputFileData%NTwGages)

   call YamlGet(Doc, 'output:NBlGages', InputFileData%NBlGages, TmpErrStat, TmpErrMsg); if (Failed()) return
   if ( InputFileData%NBlGages > size(InputFileData%BldGagNd) ) then
      call SetErrStat( ErrID_Warn, 'Number of blade strain gages exceeds '// &
                                   trim(Num2LStr(size(InputFileData%BldGagNd)))//'.', ErrStat, ErrMsg, RoutineName )
      if (ErrStat >= AbortErrLev) return
      InputFileData%NBlGages = size(InputFileData%BldGagNd)
   end if
   call YamlGet(Doc, 'output:BldGagNd', TmpGagAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   NGagList = size(TmpGagAry)
   if (NGagList < InputFileData%NBlGages) then
      call SetErrStat(ErrID_Fatal, 'output:BldGagNd must list at least output:NBlGages ('// &
         trim(Num2LStr(InputFileData%NBlGages))//') entries; found '//trim(Num2LStr(NGagList))//'.', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if
   InputFileData%BldGagNd = 0
   if (InputFileData%NBlGages > 0) InputFileData%BldGagNd(1:InputFileData%NBlGages) = TmpGagAry(1:InputFileData%NBlGages)

   call YamlGet(Doc, 'output:OutList', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpList) > MaxOutPts) then
      call SetErrStat(ErrID_Fatal, 'output:OutList may contain at most '//trim(Num2LStr(MaxOutPts))// &
         ' channels; found '//trim(Num2LStr(size(TmpList)))//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   call AllocAry(InputFileData%OutList, MaxOutPts, "ElastoDyn Input File's Outlist", TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%OutList = ''
   InputFileData%NumOuts = size(TmpList)
   do i = 1, InputFileData%NumOuts
      if (len_trim(TmpList(i)) > ChanLen) then
         call SetErrStat(ErrID_Fatal, 'output:OutList entry "'//trim(TmpList(i))//'" is longer than the '// &
            trim(Num2LStr(ChanLen))//'-character channel-name limit.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      InputFileData%OutList(i) = TmpList(i)
   end do

   !----------------------------------------------------------------------------------
   ! nodal_outputs (optional) -- BldNd_NumOuts derives from nodal_outputs:OutList's
   ! length; absent entirely (rather than malformed) is the YAML equivalent of the text
   ! path's "section not found" fallback, so BldNd_NumOuts/BldNd_BladesOut stay 0
   !----------------------------------------------------------------------------------
   InputFileData%BldNd_NumOuts   = 0
   InputFileData%BldNd_BladesOut = 0
   InputFileData%BldNd_BlOutNd_Str = ''

   call YamlGetNode(Doc, 'nodal_outputs', iNodalOutputs, TmpErrStat, TmpErrMsg, Found=WasFound)
   if (Failed()) return
   if (WasFound) then
      call Yaml_MarkUsed(Doc, iNodalOutputs, .false.)   ! the mapping node itself; children looked up below
      call YamlGet(Doc, 'nodal_outputs:BldNd_BladesOut', InputFileData%BldNd_BladesOut, TmpErrStat, TmpErrMsg)
      if (Failed()) return
      call YamlGet(Doc, 'nodal_outputs:BldNd_BlOutNd', InputFileData%BldNd_BlOutNd_Str, TmpErrStat, TmpErrMsg)
      if (Failed()) return

      call YamlGet(Doc, 'nodal_outputs:OutList', TmpList, TmpErrStat, TmpErrMsg)
      if (Failed()) return
      if (size(TmpList) > BldNd_MaxOutPts) then
         call SetErrStat(ErrID_Fatal, 'nodal_outputs:OutList may contain at most '//trim(Num2LStr(BldNd_MaxOutPts))// &
            ' channels; found '//trim(Num2LStr(size(TmpList)))//'.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      call AllocAry(InputFileData%BldNd_OutList, BldNd_MaxOutPts, "BldNd_Outlist", TmpErrStat, TmpErrMsg)
      if (Failed()) return
      InputFileData%BldNd_OutList = ''
      InputFileData%BldNd_NumOuts = size(TmpList)
      do i = 1, InputFileData%BldNd_NumOuts
         InputFileData%BldNd_OutList(i) = TmpList(i)
      end do

      ! mirror the text path's segfault-guard: no blades requested -> no nodal outputs
      if (InputFileData%BldNd_BladesOut <= 0) InputFileData%BldNd_NumOuts = 0
   else
      call AllocAry(InputFileData%BldNd_OutList, BldNd_MaxOutPts, "BldNd_Outlist", TmpErrStat, TmpErrMsg)
      if (Failed()) return
      InputFileData%BldNd_OutList = ''
   end if

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

end subroutine ParseYamlDoc

end module ElastoDyn_Yaml
