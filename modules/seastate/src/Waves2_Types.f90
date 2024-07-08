!STARTOFREGISTRYGENERATEDFILE 'Waves2_Types.f90'
!
! WARNING This file is generated automatically by the FAST registry.
! Do not edit.  Your changes to this file will be lost.
!
! FAST Registry
!*********************************************************************************************************************************
! Waves2_Types
!.................................................................................................................................
! This file is part of Waves2.
!
! Copyright (C) 2012-2016 National Renewable Energy Laboratory
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
!
!
! W A R N I N G : This file was automatically generated from the FAST registry.  Changes made to this file may be lost.
!
!*********************************************************************************************************************************
!> This module contains the user-defined types needed in Waves2. It also contains copy, destroy, pack, and
!! unpack routines associated with each defined data type. This code is automatically generated by the FAST Registry.
MODULE Waves2_Types
!---------------------------------------------------------------------------------------------------------------------------------
USE NWTC_Library
IMPLICIT NONE
! =========  Waves2_InitInputType  =======
  TYPE, PUBLIC :: Waves2_InitInputType
    REAL(ReKi)  :: Gravity = 0.0_ReKi      !< Gravitational acceleration [(m/s^2)]
    INTEGER(IntKi) , DIMENSION(1:3)  :: nGrid = 0_IntKi      !< Grid dimensions [-]
    INTEGER(IntKi)  :: NWaveElevGrid = 0_IntKi      !< Number of grid points where the incident wave elevations can be output [-]
    INTEGER(IntKi)  :: NWaveKinGrid = 0_IntKi      !< Number of grid points where the incident wave kinematics will be computed [-]
    REAL(SiKi) , DIMENSION(:), ALLOCATABLE  :: WaveKinGridxi      !< xi-coordinates for grid points where the incident wave kinematics will be computed; these are relative to the mean sea level [(meters)]
    REAL(SiKi) , DIMENSION(:), ALLOCATABLE  :: WaveKinGridyi      !< yi-coordinates for grid points where the incident wave kinematics will be computed; these are relative to the mean sea level [(meters)]
    REAL(SiKi) , DIMENSION(:), ALLOCATABLE  :: WaveKinGridzi      !< zi-coordinates for grid points where the incident wave kinematics will be computed; these are relative to the mean sea level [(meters)]
    LOGICAL  :: WvDiffQTFF = .false.      !< Full difference QTF second order forces flag [(-)]
    LOGICAL  :: WvSumQTFF = .false.      !< Full sum QTF second order forces flag [(-)]
  END TYPE Waves2_InitInputType
! =======================
! =========  Waves2_InitOutputType  =======
  TYPE, PUBLIC :: Waves2_InitOutputType
    REAL(SiKi) , DIMENSION(:,:,:,:,:), ALLOCATABLE  :: WaveAcc2D      !< Instantaneous 2nd-order difference frequency correction for the acceleration     of incident waves in the xi- (1), yi- (2), and zi- (3) directions, respectively, at each of the NWaveKinGrid points where the incident wave kinematics will be computed [(m/s^2)]
    REAL(SiKi) , DIMENSION(:,:,:,:), ALLOCATABLE  :: WaveDynP2D      !< Instantaneous 2nd-order difference frequency correction for the dynamic pressure of incident waves                                                              , at each of the NWaveKinGrid points where the incident wave kinematics will be computed [(N/m^2)]
    REAL(SiKi) , DIMENSION(:,:,:,:,:), ALLOCATABLE  :: WaveAcc2S      !< Instantaneous 2nd-order sum        frequency correction for the acceleration     of incident waves in the xi- (1), yi- (2), and zi- (3) directions, respectively, at each of the NWaveKinGrid points where the incident wave kinematics will be computed [(m/s^2)]
    REAL(SiKi) , DIMENSION(:,:,:,:), ALLOCATABLE  :: WaveDynP2S      !< Instantaneous 2nd-order sum        frequency correction for the dynamic pressure of incident waves                                                              , at each of the NWaveKinGrid points where the incident wave kinematics will be computed [(N/m^2)]
    REAL(SiKi) , DIMENSION(:,:,:,:,:), ALLOCATABLE  :: WaveVel2D      !< Instantaneous 2nd-order difference frequency correction for the velocity         of incident waves in the xi- (1), yi- (2), and zi- (3) directions, respectively, at each of the NWaveKinGrid points where the incident wave kinematics will be computed (The values include both the velocity of incident waves and the velocity of current.) [(m/s)]
    REAL(SiKi) , DIMENSION(:,:,:,:,:), ALLOCATABLE  :: WaveVel2S      !< Instantaneous 2nd-order sum        frequency correction for the velocity         of incident waves in the xi- (1), yi- (2), and zi- (3) directions, respectively, at each of the NWaveKinGrid points where the incident wave kinematics will be computed (The values include both the velocity of incident waves and the velocity of current.) [(m/s)]
  END TYPE Waves2_InitOutputType
! =======================

contains

subroutine Waves2_CopyInitInput(SrcInitInputData, DstInitInputData, CtrlCode, ErrStat, ErrMsg)
   type(Waves2_InitInputType), intent(in) :: SrcInitInputData
   type(Waves2_InitInputType), intent(inout) :: DstInitInputData
   integer(IntKi),  intent(in   ) :: CtrlCode
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   integer(B8Ki)                  :: LB(1), UB(1)
   integer(IntKi)                 :: ErrStat2
   character(*), parameter        :: RoutineName = 'Waves2_CopyInitInput'
   ErrStat = ErrID_None
   ErrMsg  = ''
   DstInitInputData%Gravity = SrcInitInputData%Gravity
   DstInitInputData%nGrid = SrcInitInputData%nGrid
   DstInitInputData%NWaveElevGrid = SrcInitInputData%NWaveElevGrid
   DstInitInputData%NWaveKinGrid = SrcInitInputData%NWaveKinGrid
   if (allocated(SrcInitInputData%WaveKinGridxi)) then
      LB(1:1) = lbound(SrcInitInputData%WaveKinGridxi, kind=B8Ki)
      UB(1:1) = ubound(SrcInitInputData%WaveKinGridxi, kind=B8Ki)
      if (.not. allocated(DstInitInputData%WaveKinGridxi)) then
         allocate(DstInitInputData%WaveKinGridxi(LB(1):UB(1)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstInitInputData%WaveKinGridxi.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstInitInputData%WaveKinGridxi = SrcInitInputData%WaveKinGridxi
   end if
   if (allocated(SrcInitInputData%WaveKinGridyi)) then
      LB(1:1) = lbound(SrcInitInputData%WaveKinGridyi, kind=B8Ki)
      UB(1:1) = ubound(SrcInitInputData%WaveKinGridyi, kind=B8Ki)
      if (.not. allocated(DstInitInputData%WaveKinGridyi)) then
         allocate(DstInitInputData%WaveKinGridyi(LB(1):UB(1)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstInitInputData%WaveKinGridyi.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstInitInputData%WaveKinGridyi = SrcInitInputData%WaveKinGridyi
   end if
   if (allocated(SrcInitInputData%WaveKinGridzi)) then
      LB(1:1) = lbound(SrcInitInputData%WaveKinGridzi, kind=B8Ki)
      UB(1:1) = ubound(SrcInitInputData%WaveKinGridzi, kind=B8Ki)
      if (.not. allocated(DstInitInputData%WaveKinGridzi)) then
         allocate(DstInitInputData%WaveKinGridzi(LB(1):UB(1)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstInitInputData%WaveKinGridzi.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstInitInputData%WaveKinGridzi = SrcInitInputData%WaveKinGridzi
   end if
   DstInitInputData%WvDiffQTFF = SrcInitInputData%WvDiffQTFF
   DstInitInputData%WvSumQTFF = SrcInitInputData%WvSumQTFF
end subroutine

subroutine Waves2_DestroyInitInput(InitInputData, ErrStat, ErrMsg)
   type(Waves2_InitInputType), intent(inout) :: InitInputData
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   character(*), parameter        :: RoutineName = 'Waves2_DestroyInitInput'
   ErrStat = ErrID_None
   ErrMsg  = ''
   if (allocated(InitInputData%WaveKinGridxi)) then
      deallocate(InitInputData%WaveKinGridxi)
   end if
   if (allocated(InitInputData%WaveKinGridyi)) then
      deallocate(InitInputData%WaveKinGridyi)
   end if
   if (allocated(InitInputData%WaveKinGridzi)) then
      deallocate(InitInputData%WaveKinGridzi)
   end if
end subroutine

subroutine Waves2_PackInitInput(RF, Indata)
   type(RegFile), intent(inout) :: RF
   type(Waves2_InitInputType), intent(in) :: InData
   character(*), parameter         :: RoutineName = 'Waves2_PackInitInput'
   if (RF%ErrStat >= AbortErrLev) return
   call RegPack(RF, InData%Gravity)
   call RegPack(RF, InData%nGrid)
   call RegPack(RF, InData%NWaveElevGrid)
   call RegPack(RF, InData%NWaveKinGrid)
   call RegPackAlloc(RF, InData%WaveKinGridxi)
   call RegPackAlloc(RF, InData%WaveKinGridyi)
   call RegPackAlloc(RF, InData%WaveKinGridzi)
   call RegPack(RF, InData%WvDiffQTFF)
   call RegPack(RF, InData%WvSumQTFF)
   if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine Waves2_UnPackInitInput(RF, OutData)
   type(RegFile), intent(inout)    :: RF
   type(Waves2_InitInputType), intent(inout) :: OutData
   character(*), parameter            :: RoutineName = 'Waves2_UnPackInitInput'
   integer(B8Ki)   :: LB(1), UB(1)
   integer(IntKi)  :: stat
   logical         :: IsAllocAssoc
   if (RF%ErrStat /= ErrID_None) return
   call RegUnpack(RF, OutData%Gravity); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%nGrid); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%NWaveElevGrid); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%NWaveKinGrid); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%WaveKinGridxi); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%WaveKinGridyi); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%WaveKinGridzi); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%WvDiffQTFF); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%WvSumQTFF); if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine Waves2_CopyInitOutput(SrcInitOutputData, DstInitOutputData, CtrlCode, ErrStat, ErrMsg)
   type(Waves2_InitOutputType), intent(in) :: SrcInitOutputData
   type(Waves2_InitOutputType), intent(inout) :: DstInitOutputData
   integer(IntKi),  intent(in   ) :: CtrlCode
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   integer(B8Ki)                  :: LB(5), UB(5)
   integer(IntKi)                 :: ErrStat2
   character(*), parameter        :: RoutineName = 'Waves2_CopyInitOutput'
   ErrStat = ErrID_None
   ErrMsg  = ''
   if (allocated(SrcInitOutputData%WaveAcc2D)) then
      LB(1:5) = lbound(SrcInitOutputData%WaveAcc2D, kind=B8Ki)
      UB(1:5) = ubound(SrcInitOutputData%WaveAcc2D, kind=B8Ki)
      if (.not. allocated(DstInitOutputData%WaveAcc2D)) then
         allocate(DstInitOutputData%WaveAcc2D(LB(1):UB(1),LB(2):UB(2),LB(3):UB(3),LB(4):UB(4),LB(5):UB(5)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstInitOutputData%WaveAcc2D.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstInitOutputData%WaveAcc2D = SrcInitOutputData%WaveAcc2D
   end if
   if (allocated(SrcInitOutputData%WaveDynP2D)) then
      LB(1:4) = lbound(SrcInitOutputData%WaveDynP2D, kind=B8Ki)
      UB(1:4) = ubound(SrcInitOutputData%WaveDynP2D, kind=B8Ki)
      if (.not. allocated(DstInitOutputData%WaveDynP2D)) then
         allocate(DstInitOutputData%WaveDynP2D(LB(1):UB(1),LB(2):UB(2),LB(3):UB(3),LB(4):UB(4)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstInitOutputData%WaveDynP2D.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstInitOutputData%WaveDynP2D = SrcInitOutputData%WaveDynP2D
   end if
   if (allocated(SrcInitOutputData%WaveAcc2S)) then
      LB(1:5) = lbound(SrcInitOutputData%WaveAcc2S, kind=B8Ki)
      UB(1:5) = ubound(SrcInitOutputData%WaveAcc2S, kind=B8Ki)
      if (.not. allocated(DstInitOutputData%WaveAcc2S)) then
         allocate(DstInitOutputData%WaveAcc2S(LB(1):UB(1),LB(2):UB(2),LB(3):UB(3),LB(4):UB(4),LB(5):UB(5)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstInitOutputData%WaveAcc2S.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstInitOutputData%WaveAcc2S = SrcInitOutputData%WaveAcc2S
   end if
   if (allocated(SrcInitOutputData%WaveDynP2S)) then
      LB(1:4) = lbound(SrcInitOutputData%WaveDynP2S, kind=B8Ki)
      UB(1:4) = ubound(SrcInitOutputData%WaveDynP2S, kind=B8Ki)
      if (.not. allocated(DstInitOutputData%WaveDynP2S)) then
         allocate(DstInitOutputData%WaveDynP2S(LB(1):UB(1),LB(2):UB(2),LB(3):UB(3),LB(4):UB(4)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstInitOutputData%WaveDynP2S.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstInitOutputData%WaveDynP2S = SrcInitOutputData%WaveDynP2S
   end if
   if (allocated(SrcInitOutputData%WaveVel2D)) then
      LB(1:5) = lbound(SrcInitOutputData%WaveVel2D, kind=B8Ki)
      UB(1:5) = ubound(SrcInitOutputData%WaveVel2D, kind=B8Ki)
      if (.not. allocated(DstInitOutputData%WaveVel2D)) then
         allocate(DstInitOutputData%WaveVel2D(LB(1):UB(1),LB(2):UB(2),LB(3):UB(3),LB(4):UB(4),LB(5):UB(5)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstInitOutputData%WaveVel2D.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstInitOutputData%WaveVel2D = SrcInitOutputData%WaveVel2D
   end if
   if (allocated(SrcInitOutputData%WaveVel2S)) then
      LB(1:5) = lbound(SrcInitOutputData%WaveVel2S, kind=B8Ki)
      UB(1:5) = ubound(SrcInitOutputData%WaveVel2S, kind=B8Ki)
      if (.not. allocated(DstInitOutputData%WaveVel2S)) then
         allocate(DstInitOutputData%WaveVel2S(LB(1):UB(1),LB(2):UB(2),LB(3):UB(3),LB(4):UB(4),LB(5):UB(5)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstInitOutputData%WaveVel2S.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstInitOutputData%WaveVel2S = SrcInitOutputData%WaveVel2S
   end if
end subroutine

subroutine Waves2_DestroyInitOutput(InitOutputData, ErrStat, ErrMsg)
   type(Waves2_InitOutputType), intent(inout) :: InitOutputData
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   character(*), parameter        :: RoutineName = 'Waves2_DestroyInitOutput'
   ErrStat = ErrID_None
   ErrMsg  = ''
   if (allocated(InitOutputData%WaveAcc2D)) then
      deallocate(InitOutputData%WaveAcc2D)
   end if
   if (allocated(InitOutputData%WaveDynP2D)) then
      deallocate(InitOutputData%WaveDynP2D)
   end if
   if (allocated(InitOutputData%WaveAcc2S)) then
      deallocate(InitOutputData%WaveAcc2S)
   end if
   if (allocated(InitOutputData%WaveDynP2S)) then
      deallocate(InitOutputData%WaveDynP2S)
   end if
   if (allocated(InitOutputData%WaveVel2D)) then
      deallocate(InitOutputData%WaveVel2D)
   end if
   if (allocated(InitOutputData%WaveVel2S)) then
      deallocate(InitOutputData%WaveVel2S)
   end if
end subroutine

subroutine Waves2_PackInitOutput(RF, Indata)
   type(RegFile), intent(inout) :: RF
   type(Waves2_InitOutputType), intent(in) :: InData
   character(*), parameter         :: RoutineName = 'Waves2_PackInitOutput'
   if (RF%ErrStat >= AbortErrLev) return
   call RegPackAlloc(RF, InData%WaveAcc2D)
   call RegPackAlloc(RF, InData%WaveDynP2D)
   call RegPackAlloc(RF, InData%WaveAcc2S)
   call RegPackAlloc(RF, InData%WaveDynP2S)
   call RegPackAlloc(RF, InData%WaveVel2D)
   call RegPackAlloc(RF, InData%WaveVel2S)
   if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine Waves2_UnPackInitOutput(RF, OutData)
   type(RegFile), intent(inout)    :: RF
   type(Waves2_InitOutputType), intent(inout) :: OutData
   character(*), parameter            :: RoutineName = 'Waves2_UnPackInitOutput'
   integer(B8Ki)   :: LB(5), UB(5)
   integer(IntKi)  :: stat
   logical         :: IsAllocAssoc
   if (RF%ErrStat /= ErrID_None) return
   call RegUnpackAlloc(RF, OutData%WaveAcc2D); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%WaveDynP2D); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%WaveAcc2S); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%WaveDynP2S); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%WaveVel2D); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%WaveVel2S); if (RegCheckErr(RF, RoutineName)) return
end subroutine
END MODULE Waves2_Types
!ENDOFREGISTRYGENERATEDFILE
