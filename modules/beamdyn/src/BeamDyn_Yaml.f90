!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of BeamDyn.
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
!> Reader for the YAML form of the BeamDyn primary input file. Fills the same
!! BD_InputFile structure as BD_ReadPrimaryFile (the text path in BeamDyn_IO.f90, a
!! sequential ReadVar/ReadAry-style reader), so everything downstream (BD_ReadBladeFile,
!! BD_ValidateInputData, SetParameters, etc.) is shared between the two formats.
!!
!! Schema: sections mirror the text file's banners (simulation_control, geometry_parameter,
!! mesh_parameter, beam_sectional_parameter, outputs, nodal_outputs). Keys keep their
!! documented names.
!!
!! member_total/kp_total are NOT read as explicit counts (unlike the text format): per the
!! project-wide "counts derive from list lengths" rule, member_total = length of
!! geometry_parameter:kp_member (one entry per member, in member order -- the text format's
!! "member number" column is redundant bookkeeping the text reader itself only uses to
!! detect out-of-order entry; a YAML list is unambiguously ordered) and kp_total = number of
!! rows in geometry_parameter:key_points. BldFile is carried as a path string (a
!! second-order file: never converted/inlined, resolved relative to the primary input file
!! exactly like the text path).
!!
!! refine/n_fact/DTBeam/load_retries/NRMax/stop_tol/tngt_stf_fd/tngt_stf_comp/tngt_stf_pert/
!! tngt_stf_difftol all accept the literal scalar "default" (case-insensitive, matching
!! prefix only -- INDEX(UC(Line),"DEFAULT")==1 -- exactly like the text path's ad hoc
!! Conv2UC+INDEX check): each is read as a string first, and only parsed as its native type
!! when that string is not the "default" sentinel. DTBeam's default is the glue/driver
!! supplied interval (Default_DT, left untouched); the others fall back to the same hard
!! coded literals the text reader uses (refine=1, n_fact=5, load_retries=20, NRMax=10,
!! stop_tol=1.0D-05, tngt_stf_fd/tngt_stf_comp=.FALSE., tngt_stf_pert=1.0D-06,
!! tngt_stf_difftol=1.0D-01).
!!
!! NNodeOuts derives from output:OutNd's length (clamped to 9 with the same warning as the
!! text path if it's longer); NumOuts derives from output:OutList's length.
!!
!! The optional nodal_outputs section (BldNd_BlOutNd/BldNd_OutList) mirrors the text path's
!! separate, tolerate-anything-malformed "OutList for Blade node channels" section: it is
!! simply absent when not needed, and BldNd_NumOuts stays 0.
!!
!! There is no FileInfo/passed-data entry point: unlike ElastoDyn/ServoDyn, BeamDyn's
!! InitInputType has no PassedFileIsYaml/PassedPrimaryInputData channel, and the glue's
!! BDBldFile handling (FAST_Yaml.f90 GetBDBldFiles) only ever accepts a list of file paths
!! (never an inline mapping), for both text and YAML alike -- BD_ReadInput's InputFileName
!! is always a real file on disk. So this reader has a single file-based entry point,
!! funneled from BD_ReadInput exactly like BD_ReadPrimaryFile.
module BeamDyn_Yaml

   use NWTC_Library
   use YamlInput
   use BeamDyn_Types
   use BeamDyn_BldNdOuts_IO, only: BldNd_MaxOutPts

   implicit none
   private

   ! MaxOutPts/BeamDyn_Ver mirror the parameters defined in BeamDyn_IO (this module is USEd
   ! BY BeamDyn_IO at the funnel call site in BD_ReadInput, so it cannot in turn USE
   ! anything from that module without creating a circular module dependency; these two
   ! constants are duplicated here instead, matching ElastoDyn_Yaml's precedent). Neither
   ! has changed since BeamDyn's original implementation.
   integer(IntKi), parameter :: MaxOutPts   = 360
   type(ProgDesc), parameter :: BeamDyn_Ver = ProgDesc( 'BeamDyn', '', '' )

   public :: BD_ParseYamlFile

contains

!> Load and parse a YAML-format BeamDyn primary input file. Mirrors the contract of
!! BD_ReadPrimaryFile (the text path), including echo handling.
subroutine BD_ParseYamlFile(InputFileName, InputFileData, OutFileRoot, Default_DT, UnEc, ErrStat, ErrMsg)
   character(*),               intent(in   ) :: InputFileName !< the .yaml primary input file
   type(BD_InputFile),         intent(inout) :: InputFileData !< the shared input-file structure
   character(*),               intent(in   ) :: OutFileRoot   !< the rootname of the echo file, possibly opened here
   real(DbKi),                 intent(in   ) :: Default_DT    !< default DT supplied by the caller (glue/driver); DTBeam falls back to this
   integer(IntKi),             intent(  out) :: UnEc          !< I/O unit for echo file; > 0 if opened
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'BD_ParseYamlFile'
   type(YamlDoc)           :: Doc
   character(1024)         :: PriPath
   logical                 :: Echo
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1

   call GetPath( InputFileName, PriPath )    ! BldFile resolves relative to this

   call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   Echo = .false.
   call YamlGet(Doc, 'simulation_control:Echo', Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (Echo) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included), which supersedes the text path's line-by-line echo
      call OpenEcho(UnEc, trim(OutFileRoot)//'.ech', TmpErrStat, TmpErrMsg, BeamDyn_Ver)
      if (Failed()) return
      write(UnEc, '(/,A,/)') 'Data from '//trim(BeamDyn_Ver%Name)//' primary input file "'//trim(InputFileName)//'":'
      call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   call ParseYamlDoc(Doc, PriPath, Default_DT, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! typo guard: anything never looked up is reported (warning severity)
   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine BD_ParseYamlFile

!> Fill InputFileData from a parsed document. Split from the file-entry wrapper for
!! symmetry with the other modules' ParseYamlDoc split (BD has no FileInfo entry point).
subroutine ParseYamlDoc(Doc, PriPath, Default_DT, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   character(*),               intent(in   ) :: PriPath
   real(DbKi),                 intent(in   ) :: Default_DT
   type(BD_InputFile),         intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter     :: RoutineName = 'BD_ParseYamlDoc'
   character(200)               :: ChStr
   character(:), allocatable    :: TmpList(:)
   integer(IntKi), allocatable  :: TmpIntAry(:)
   real(R8Ki),     allocatable  :: TmpMat(:,:)
   integer(IntKi)               :: i
   integer(IntKi)                :: IOS
   logical                      :: WasFound
   integer(IntKi)                :: iNodalOutputs
   integer(IntKi)                :: TmpErrStat
   character(ErrMsgLen)          :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   !----------------------------------------------------------------------------------
   ! simulation_control (required) -- refine/n_fact/DTBeam/load_retries/NRMax/stop_tol/
   ! tngt_stf_fd/tngt_stf_comp/tngt_stf_pert/tngt_stf_difftol accept the literal scalar
   ! "default", read as a string first exactly like the text format's Line/Conv2UC/INDEX
   ! check (a prefix match, not full equality -- mirrored here for bit-for-bit parity)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'simulation_control:QuasiStaticInit', InputFileData%QuasiStaticInit, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'simulation_control:rhoinf', InputFileData%rhoinf, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'simulation_control:quadrature', InputFileData%quadrature, TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGet(Doc, 'simulation_control:refine', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if ( INDEX(ChStr, "DEFAULT") == 1 ) then
      InputFileData%refine = 1
   else
      read(ChStr, *, IOSTAT=IOS) InputFileData%refine
      call CheckIOS(IOS, '', 'refine', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if

   call YamlGet(Doc, 'simulation_control:n_fact', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if ( INDEX(ChStr, "DEFAULT") == 1 ) then
      InputFileData%n_fact = 5
   else
      read(ChStr, *, IOSTAT=IOS) InputFileData%n_fact
      call CheckIOS(IOS, '', 'n_fact', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if

   InputFileData%DTBeam = Default_DT   ! caller-supplied fallback; overwritten below unless "default"
   call YamlGet(Doc, 'simulation_control:DTBeam', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if ( INDEX(ChStr, "DEFAULT") /= 1 ) then
      read(ChStr, *, IOSTAT=IOS) InputFileData%DTBeam
      call CheckIOS(IOS, '', 'DTBeam', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if

   call YamlGet(Doc, 'simulation_control:load_retries', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if ( INDEX(ChStr, "DEFAULT") == 1 ) then
      InputFileData%load_retries = 20
   else
      read(ChStr, *, IOSTAT=IOS) InputFileData%load_retries
      call CheckIOS(IOS, '', 'load_retries', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if

   call YamlGet(Doc, 'simulation_control:NRMax', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if ( INDEX(ChStr, "DEFAULT") == 1 ) then
      InputFileData%NRMax = 10
   else
      read(ChStr, *, IOSTAT=IOS) InputFileData%NRMax
      call CheckIOS(IOS, '', 'NRMax', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if

   call YamlGet(Doc, 'simulation_control:stop_tol', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if ( INDEX(ChStr, "DEFAULT") == 1 ) then
      InputFileData%stop_tol = 1.0D-05
   else
      read(ChStr, *, IOSTAT=IOS) InputFileData%stop_tol
      call CheckIOS(IOS, '', 'stop_tol', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if

   call YamlGet(Doc, 'simulation_control:tngt_stf_fd', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if ( INDEX(ChStr, "DEFAULT") == 1 ) then
      InputFileData%tngt_stf_fd = .FALSE.
   else
      read(ChStr, *, IOSTAT=IOS) InputFileData%tngt_stf_fd
      call CheckIOS(IOS, '', 'tngt_stf_fd', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if
   if (InputFileData%tngt_stf_fd) call WrScr( 'Using finite difference to compute tangent stiffness matrix'//NewLine )

   call YamlGet(Doc, 'simulation_control:tngt_stf_comp', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if ( INDEX(ChStr, "DEFAULT") == 1 ) then
      InputFileData%tngt_stf_comp = .FALSE.
   else
      read(ChStr, *, IOSTAT=IOS) InputFileData%tngt_stf_comp
      call CheckIOS(IOS, '', 'tngt_stf_comp', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if
   if (InputFileData%tngt_stf_comp) call WrScr( 'WARNING: tngt_stf_comp set to true. Output will be verbose'//NewLine )

   call YamlGet(Doc, 'simulation_control:tngt_stf_pert', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if ( INDEX(ChStr, "DEFAULT") == 1 ) then
      InputFileData%tngt_stf_pert = 1.0D-06
   else
      read(ChStr, *, IOSTAT=IOS) InputFileData%tngt_stf_pert
      call CheckIOS(IOS, '', 'tngt_stf_pert', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if

   call YamlGet(Doc, 'simulation_control:tngt_stf_difftol', ChStr, TmpErrStat, TmpErrMsg, Default='DEFAULT'); if (Failed()) return
   call Conv2UC(ChStr)
   if ( INDEX(ChStr, "DEFAULT") == 1 ) then
      InputFileData%tngt_stf_difftol = 1.0D-01
   else
      read(ChStr, *, IOSTAT=IOS) InputFileData%tngt_stf_difftol
      call CheckIOS(IOS, '', 'tngt_stf_difftol', NumType, TmpErrStat, TmpErrMsg); if (Failed()) return
   end if

   call YamlGet(Doc, 'simulation_control:RotStates', InputFileData%RotStates, TmpErrStat, TmpErrMsg); if (Failed()) return

   !----------------------------------------------------------------------------------
   ! geometry_parameter (required) -- member_total/kp_total derive from list lengths
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'geometry_parameter:kp_member', TmpIntAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%member_total = size(TmpIntAry)

   call YamlGet(Doc, 'geometry_parameter:key_points', TmpMat, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%kp_total = size(TmpMat, 1)

   if (InputFileData%member_total<1 .or. InputFileData%kp_total<1) then
      call SetErrStat( ErrID_Fatal, "geometry_parameter:kp_member and geometry_parameter:key_points must each list "// &
                        "at least one entry", ErrStat, ErrMsg, RoutineName )
      return
   end if
   if (size(TmpMat, 2) /= 4) then
      call SetErrStat( ErrID_Fatal, "geometry_parameter:key_points rows must each have 4 entries (x, y, z, twist)", &
                        ErrStat, ErrMsg, RoutineName )
      return
   end if

   call AllocAry(InputFileData%kp_member, InputFileData%member_total, 'Number of key point in each member', TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%kp_member = TmpIntAry

   call AllocAry(InputFileData%kp_coordinate, InputFileData%kp_total, 4, 'Key point coordinates input array', TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%kp_coordinate(:,1) =  TmpMat(:,1) ! x
   InputFileData%kp_coordinate(:,2) =  TmpMat(:,2) ! y
   InputFileData%kp_coordinate(:,3) =  TmpMat(:,3) ! z
   InputFileData%kp_coordinate(:,4) = -TmpMat(:,4) ! initial twist

   !----------------------------------------------------------------------------------
   ! mesh_parameter (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'mesh_parameter:order_elem', InputFileData%order_elem, TmpErrStat, TmpErrMsg); if (Failed()) return

   !----------------------------------------------------------------------------------
   ! beam_sectional_parameter (required) -- BldFile: second-order file type, always
   ! referenced by path
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'beam_sectional_parameter:BldFile', InputFileData%BldFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   if ( PathIsRelative( InputFileData%BldFile ) ) InputFileData%BldFile = trim(PriPath)//trim(InputFileData%BldFile)

   !----------------------------------------------------------------------------------
   ! outputs (required) -- NNodeOuts derives from output:OutNd's length (clamped to 9
   ! with the same warning as the text path); NumOuts derives from output:OutList's length
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'outputs:SumPrint', InputFileData%SumPrint, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'outputs:OutFmt', InputFileData%OutFmt, TmpErrStat, TmpErrMsg); if (Failed()) return

   call YamlGet(Doc, 'outputs:OutNd', TmpIntAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%NNodeOuts = size(TmpIntAry)
   if ( InputFileData%NNodeOuts > size(InputFileData%OutNd) ) then
      call SetErrStat( ErrID_Warn, ' Warning: number of output nodes exceeds '// &
                        trim(Num2LStr(size(InputFileData%OutNd))) //'.', ErrStat, ErrMsg, RoutineName )
      if (ErrStat >= AbortErrLev) return
      InputFileData%NNodeOuts = size(InputFileData%OutNd)
   end if
   InputFileData%OutNd = 0
   if (InputFileData%NNodeOuts > 0) InputFileData%OutNd(1:InputFileData%NNodeOuts) = TmpIntAry(1:InputFileData%NNodeOuts)

   call YamlGet(Doc, 'outputs:OutList', TmpList, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpList) > MaxOutPts) then
      call SetErrStat(ErrID_Fatal, 'outputs:OutList may contain at most '//trim(Num2LStr(MaxOutPts))// &
         ' channels; found '//trim(Num2LStr(size(TmpList)))//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   call AllocAry(InputFileData%OutList, MaxOutPts, "Outlist", TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%OutList = ''
   InputFileData%NumOuts = size(TmpList)
   do i = 1, InputFileData%NumOuts
      if (len_trim(TmpList(i)) > ChanLen) then
         call SetErrStat(ErrID_Fatal, 'outputs:OutList entry "'//trim(TmpList(i))//'" is longer than the '// &
            trim(Num2LStr(ChanLen))//'-character channel-name limit.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      InputFileData%OutList(i) = TmpList(i)
   end do

   !----------------------------------------------------------------------------------
   ! nodal_outputs (optional) -- BldNd_NumOuts derives from nodal_outputs:OutList's
   ! length; absent entirely (rather than malformed) is the YAML equivalent of the text
   ! path's "section not found/ill-formed -> ignore it" fallback for the additional
   ! per-node outputs section, so BldNd_NumOuts stays 0
   !----------------------------------------------------------------------------------
   InputFileData%BldNd_NumOuts     = 0
   InputFileData%BldNd_BlOutNd_Str = ''

   call AllocAry(InputFileData%BldNd_OutList, BldNd_MaxOutPts, "BldNd_Outlist", TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%BldNd_OutList = ''

   call YamlGetNode(Doc, 'nodal_outputs', iNodalOutputs, TmpErrStat, TmpErrMsg, Found=WasFound)
   if (Failed()) return
   if (WasFound) then
      call Yaml_MarkUsed(Doc, iNodalOutputs, .false.)   ! the mapping node itself; children looked up below
      call YamlGet(Doc, 'nodal_outputs:BldNd_BlOutNd', InputFileData%BldNd_BlOutNd_Str, TmpErrStat, TmpErrMsg)
      if (Failed()) return

      call YamlGet(Doc, 'nodal_outputs:OutList', TmpList, TmpErrStat, TmpErrMsg)
      if (Failed()) return
      if (size(TmpList) > BldNd_MaxOutPts) then
         call SetErrStat(ErrID_Fatal, 'nodal_outputs:OutList may contain at most '//trim(Num2LStr(BldNd_MaxOutPts))// &
            ' channels; found '//trim(Num2LStr(size(TmpList)))//'.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      InputFileData%BldNd_NumOuts = size(TmpList)
      do i = 1, InputFileData%BldNd_NumOuts
         InputFileData%BldNd_OutList(i) = TmpList(i)
      end do
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseYamlDoc

end module BeamDyn_Yaml
