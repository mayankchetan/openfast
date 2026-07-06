!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of StrucCtrl.
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
!> Reader for the YAML form of a Structural Control (StC) input file. Fills the same
!! StC_InputFile structure as StC_ParseInputFileInfo (the text path in StrucCtrl.f90),
!! so validation (StC_ValidatePrimaryData) and everything downstream is shared between
!! the two formats. StC files are their own file type, referenced by path from the
!! ServoDyn deck (text or YAML) -- they are never inlined into a parent deck.
!!
!! Schema: sections mirror the text file's banners (general, degrees_of_freedom,
!! location, initial_conditions, configuration, mass_stiffness_damping,
!! user_defined_spring_forces, control, tlcd, prescribed_time_series). Keys keep their
!! documented names.
!!
!! StC_Z_PreLd is a variant string field exactly as in the text format: the literal
!! "gravity", "none", or a number, kept as raw text here and interpreted later
!! (StC_ValidatePrimaryData / StC_SetParameters). NKInpSt is not a YAML key: it derives
!! from the row count of the F_TBL matrix (each row is the text-format table's six
!! columns X, F_X, Y, F_Y, Z, F_Z). StC_CChan and PrescribedForcesFile mirror the text
!! reader's array-then-scalar fallback: each accepts either a per-instance list or a
!! single value broadcast to every mesh point (missing trailing PrescribedForcesFile
!! entries fall back to the first file with the same informational message as the text
!! path). Relative paths resolve against this StC input file, exactly as the text path.
module StrucCtrl_Yaml

   use NWTC_Library
   use YamlInput
   use StrucCtrl_Types

   implicit none
   private

   public :: StC_ParseYamlFile

contains

!> Load and parse a YAML-format StC input file. Mirrors the contract of ProcessComFile +
!! StC_ParseInputFileInfo (the text path), including echo handling: like the text path,
!! the echo unit is returned open (UnEcho > 0) so StC_Init can keep echoing the
!! prescribed-force time-series parse to it, and closes it.
subroutine StC_ParseYamlFile(InputFileName, PriPath, RootName, NumMeshPts, InputFileData, UnEcho, ErrStat, ErrMsg)
   character(*),               intent(in   ) :: InputFileName !< the .yaml StC input file
   character(*),               intent(in   ) :: PriPath       !< path of this StC input file (for relative sub-file paths)
   character(*),               intent(in   ) :: RootName      !< instance root name, for the echo file
   integer(IntKi),             intent(in   ) :: NumMeshPts    !< number of mesh points (blade StC: one per blade)
   type(StC_InputFile),        intent(inout) :: InputFileData !< the shared input-file structure
   integer(IntKi),             intent(  out) :: UnEcho        !< echo unit; left open (>0) when echoing, for StC_Init
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'StC_ParseYamlFile'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEcho  = -1

   call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   InputFileData%Echo = .false.
   call YamlGet(Doc, 'general:Echo', InputFileData%Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (InputFileData%Echo) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included), which supersedes the text path's line-by-line echo
      call OpenEcho(UnEcho, trim(RootName)//'.ech', TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEcho, '(A)') 'Echo file for StructCtrl input file: '//trim(InputFileName)
      call Yaml_LoadFile(InputFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEcho)
      if (Failed()) return
   end if

   call ParseYamlDoc(Doc, PriPath, NumMeshPts, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! typo guard: anything never looked up is reported (warning severity)
   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

   ! NOTE: UnEcho deliberately stays open on success -- StC_Init keeps using it for the
   ! prescribed-force time-series parse and closes it (same contract as the text path).

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
      if (Failed) call Cleanup()
   end function Failed

   subroutine Cleanup()
      if (UnEcho > -1_IntKi) close(UnEcho)
   end subroutine Cleanup

end subroutine StC_ParseYamlFile

!> Fill InputFileData from a parsed document.
subroutine ParseYamlDoc(Doc, PriPath, NumMeshPts, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   character(*),               intent(in   ) :: PriPath
   integer(IntKi),             intent(in   ) :: NumMeshPts
   type(StC_InputFile),        intent(inout) :: InputFileData
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter     :: RoutineName = 'StC_ParseYamlDoc'
   character(:),   allocatable :: TmpChList(:)
   integer(IntKi), allocatable :: TmpIntAry(:)
   real(ReKi),     allocatable :: Mat(:,:)
   integer(IntKi)              :: iNode
   integer(IntKi)              :: i
   integer(IntKi)              :: nGiven
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
   ! degrees_of_freedom (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'degrees_of_freedom:StC_DOF_MODE', InputFileData%StC_DOF_MODE, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:StC_X_DOF', InputFileData%StC_X_DOF, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:StC_Y_DOF', InputFileData%StC_Y_DOF, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'degrees_of_freedom:StC_Z_DOF', InputFileData%StC_Z_DOF, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! location (required) [relative to the reference origin of component attached to]
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'location:StC_P_X', InputFileData%StC_P_X, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'location:StC_P_Y', InputFileData%StC_P_Y, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'location:StC_P_Z', InputFileData%StC_P_Z, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! initial_conditions (required) -- StC_Z_PreLd is a variant string ("gravity",
   ! "none", or a number), kept as raw text exactly like the text path
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'initial_conditions:StC_X_DSP', InputFileData%StC_X_DSP, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'initial_conditions:StC_Y_DSP', InputFileData%StC_Y_DSP, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'initial_conditions:StC_Z_DSP', InputFileData%StC_Z_DSP, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'initial_conditions:StC_Z_PreLd', InputFileData%StC_Z_PreLdC, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! configuration (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'configuration:StC_X_PSP', InputFileData%StC_X_PSP, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'configuration:StC_X_NSP', InputFileData%StC_X_NSP, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'configuration:StC_Y_PSP', InputFileData%StC_Y_PSP, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'configuration:StC_Y_NSP', InputFileData%StC_Y_NSP, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'configuration:StC_Z_PSP', InputFileData%StC_Z_PSP, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'configuration:StC_Z_NSP', InputFileData%StC_Z_NSP, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! mass_stiffness_damping (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'mass_stiffness_damping:StC_X_M', InputFileData%StC_X_M, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_Y_M', InputFileData%StC_Y_M, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_Z_M', InputFileData%StC_Z_M, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_Omni_M', InputFileData%StC_Omni_M, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_X_K', InputFileData%StC_X_K, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_Y_K', InputFileData%StC_Y_K, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_Z_K', InputFileData%StC_Z_K, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_X_C', InputFileData%StC_X_C, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_Y_C', InputFileData%StC_Y_C, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_Z_C', InputFileData%StC_Z_C, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_X_KS', InputFileData%StC_X_KS, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_Y_KS', InputFileData%StC_Y_KS, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_Z_KS', InputFileData%StC_Z_KS, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_X_CS', InputFileData%StC_X_CS, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_Y_CS', InputFileData%StC_Y_CS, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'mass_stiffness_damping:StC_Z_CS', InputFileData%StC_Z_CS, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! user_defined_spring_forces (required) -- NKInpSt derives from the F_TBL row count;
   ! each row holds the text-format table's six columns (X, F_X, Y, F_Y, Z, F_Z)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'user_defined_spring_forces:Use_F_TBL', InputFileData%Use_F_TBL, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'user_defined_spring_forces:F_TBL', Mat, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   InputFileData%NKInpSt = size(Mat, 1)
   if (InputFileData%NKInpSt > 0) then
      if (size(Mat, 2) /= 6) then
         call SetErrStat(ErrID_Fatal, 'user_defined_spring_forces:F_TBL rows must have exactly 6 columns '// &
            '(X, F_X, Y, F_Y, Z, F_Z); found '//trim(Num2LStr(size(Mat, 2)))//'.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      call AllocAry( InputFileData%F_TBL, InputFileData%NKInpSt, 6, 'F_TBL', TmpErrStat, TmpErrMsg )
      if (Failed()) return
      InputFileData%F_TBL = Mat
   end if

   !----------------------------------------------------------------------------------
   ! control (required) -- StC_CChan accepts a per-instance list (NumMeshPts entries)
   ! or a single value broadcast to every instance, mirroring the text reader's
   ! array-then-scalar fallback
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'control:StC_CMODE', InputFileData%StC_CMODE, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   allocate( InputFileData%StC_CChan(NumMeshPts), STAT=TmpErrStat )    ! Blade TMD will possibly have independent TMD's for each instance
   if (TmpErrStat /= 0) then
      call SetErrStat(ErrID_Fatal, 'Error allocating InputFileData%StC_CChan(NumMeshPts)', ErrStat, ErrMsg, RoutineName)
      return
   end if
   allocate( InputFileData%PrescribedForcesFile(NumMeshPts), STAT=TmpErrStat )    ! Blade TMD may have individual prescribed force input files for each blade instance
   if (TmpErrStat /= 0) then
      call SetErrStat(ErrID_Fatal, 'Error allocating InputFileData%PrescribedForcesFile(NumMeshPts)', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call YamlGetNode(Doc, 'control:StC_CChan', iNode, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (Doc%Nodes(iNode)%Kind == YAML_SEQ) then
      call YamlGet(Doc, 'control:StC_CChan', TmpIntAry, TmpErrStat, TmpErrMsg)
      if (Failed()) return
      if (size(TmpIntAry) == NumMeshPts) then
         InputFileData%StC_CChan = TmpIntAry
      else if (size(TmpIntAry) == 1) then
         InputFileData%StC_CChan(:) = TmpIntAry(1)     ! Assign all the same, will check in validation
      else
         call SetErrStat(ErrID_Fatal, 'control:StC_CChan must be a single value or a list of '// &
            trim(Num2LStr(NumMeshPts))//' entries (one per mesh point); found '// &
            trim(Num2LStr(size(TmpIntAry)))//'.', ErrStat, ErrMsg, RoutineName)
         return
      end if
   else
      call YamlGet(Doc, 'control:StC_CChan', InputFileData%StC_CChan(1), TmpErrStat, TmpErrMsg)
      if (Failed()) return
      InputFileData%StC_CChan(:) = InputFileData%StC_CChan(1)     ! Assign all the same, will check in validation
   end if

   call YamlGet(Doc, 'control:StC_SA_MODE', InputFileData%StC_SA_MODE, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'control:StC_X_C_HIGH', InputFileData%StC_X_C_HIGH, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'control:StC_X_C_LOW', InputFileData%StC_X_C_LOW, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'control:StC_Y_C_HIGH', InputFileData%StC_Y_C_HIGH, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'control:StC_Y_C_LOW', InputFileData%StC_Y_C_LOW, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'control:StC_Z_C_HIGH', InputFileData%StC_Z_C_HIGH, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'control:StC_Z_C_LOW', InputFileData%StC_Z_C_LOW, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'control:StC_X_C_BRAKE', InputFileData%StC_X_C_BRAKE, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'control:StC_Y_C_BRAKE', InputFileData%StC_Y_C_BRAKE, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'control:StC_Z_C_BRAKE', InputFileData%StC_Z_C_BRAKE, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! tlcd (required) [used only when StC_DOF_MODE=3 (TLCD)]
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'tlcd:L_X', InputFileData%L_X, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'tlcd:B_X', InputFileData%B_X, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'tlcd:area_X', InputFileData%area_X, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'tlcd:area_ratio_X', InputFileData%area_ratio_X, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'tlcd:headLossCoeff_X', InputFileData%headLossCoeff_X, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'tlcd:rho_X', InputFileData%rho_X, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'tlcd:L_Y', InputFileData%L_Y, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'tlcd:B_Y', InputFileData%B_Y, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'tlcd:area_Y', InputFileData%area_Y, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'tlcd:area_ratio_Y', InputFileData%area_ratio_Y, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'tlcd:headLossCoeff_Y', InputFileData%headLossCoeff_Y, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'tlcd:rho_Y', InputFileData%rho_Y, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! prescribed_time_series (required) [used only when StC_DOF_MODE=4 (prescribed)]
   ! PrescribedForcesFile accepts one path (used for every instance) or a per-instance
   ! list; missing trailing entries fall back to the first file with the same
   ! informational message as the text path
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'prescribed_time_series:PrescribedForcesCoordSys', InputFileData%PrescribedForcesCoordSys, &
                TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGetNode(Doc, 'prescribed_time_series:PrescribedForcesFile', iNode, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (Doc%Nodes(iNode)%Kind == YAML_SEQ) then
      call YamlGet(Doc, 'prescribed_time_series:PrescribedForcesFile', TmpChList, TmpErrStat, TmpErrMsg)
      if (Failed()) return
      nGiven = size(TmpChList)
      if (nGiven < 1 .or. nGiven > NumMeshPts) then
         call SetErrStat(ErrID_Fatal, 'prescribed_time_series:PrescribedForcesFile must be a single path or a '// &
            'list of at most '//trim(Num2LStr(NumMeshPts))//' paths (one per mesh point); found '// &
            trim(Num2LStr(nGiven))//'.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      InputFileData%PrescribedForcesFile(1) = TmpChList(1)
   else
      nGiven = 1
      call YamlGet(Doc, 'prescribed_time_series:PrescribedForcesFile', InputFileData%PrescribedForcesFile(1), &
                   TmpErrStat, TmpErrMsg)
      if (Failed()) return
   end if
   if ( PathIsRelative( InputFileData%PrescribedForcesFile(1) ) ) &
      InputFileData%PrescribedForcesFile(1) = trim(PriPath)//trim(InputFileData%PrescribedForcesFile(1))
   do i = 2, NumMeshPts
      if (i <= nGiven) then
         InputFileData%PrescribedForcesFile(i) = TmpChList(i)
         if ( PathIsRelative( InputFileData%PrescribedForcesFile(i) ) ) &
            InputFileData%PrescribedForcesFile(i) = trim(PriPath)//trim(InputFileData%PrescribedForcesFile(i))
      else
         InputFileData%PrescribedForcesFile(i) = InputFileData%PrescribedForcesFile(1)
         call SetErrStat( ErrID_Info, "Using StC blade 1 time series force data for blade "//trim(Num2LStr(i)), &
            ErrStat, ErrMsg, RoutineName )
      end if
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseYamlDoc

end module StrucCtrl_Yaml
