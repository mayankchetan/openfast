!STARTOFREGISTRYGENERATEDFILE 'WAMIT2_Types.f90'
!
! WARNING This file is generated automatically by the FAST registry.
! Do not edit.  Your changes to this file will be lost.
!
! FAST Registry
!*********************************************************************************************************************************
! WAMIT2_Types
!.................................................................................................................................
! This file is part of WAMIT2.
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
!> This module contains the user-defined types needed in WAMIT2. It also contains copy, destroy, pack, and
!! unpack routines associated with each defined data type. This code is automatically generated by the FAST Registry.
MODULE WAMIT2_Types
!---------------------------------------------------------------------------------------------------------------------------------
USE SeaSt_WaveField_Types
USE NWTC_Library
IMPLICIT NONE
    INTEGER(IntKi), PUBLIC, PARAMETER  :: MaxWAMIT2Outputs                 = 6      !  [-]
! =========  WAMIT2_InitInputType  =======
  TYPE, PUBLIC :: WAMIT2_InitInputType
    LOGICAL  :: HasWAMIT = .false.      !< .TRUE. if using WAMIT model, .FALSE. otherwise [-]
    CHARACTER(1024)  :: WAMITFile      !< Root of the filename for WAMIT2 outputs [-]
    INTEGER(IntKi)  :: NBody = 0_IntKi      !< [>=1; only used when PotMod=1. If NBodyMod=1, the WAMIT data contains a vector of size 6*NBody x 1 and matrices of size 6*NBody x 6*NBody; if NBodyMod>1, there are NBody sets of WAMIT data each with a vector of size 6 x 1 and matrices of size 6 x 6] [-]
    INTEGER(IntKi)  :: NBodyMod = 0_IntKi      !< Body coupling model {1: include coupling terms between each body and NBody in HydroDyn equals NBODY in WAMIT, 2: neglect coupling terms between each body and NBODY=1 with XBODY=0 in WAMIT, 3: Neglect coupling terms between each body and NBODY=1 with XBODY=/0 in WAMIT} (switch) [only used when PotMod=1] [-]
    REAL(ReKi) , DIMENSION(:), ALLOCATABLE  :: PtfmRefxt      !< The xt offset of the body reference point(s) from (0,0,0)  [1 to NBody; only used when PotMod=1; must be 0.0 if NBodyMod=2 ] [(m)]
    REAL(ReKi) , DIMENSION(:), ALLOCATABLE  :: PtfmRefyt      !< The yt offset of the body reference point(s) from (0,0,0)  [1 to NBody; only used when PotMod=1; must be 0.0 if NBodyMod=2 ] [(m)]
    REAL(ReKi) , DIMENSION(:), ALLOCATABLE  :: PtfmRefzt      !< The zt offset of the body reference point(s) from (0,0,0)  [1 to NBody; only used when PotMod=1; must be 0.0 if NBodyMod=2 ] [(m)]
    REAL(R8Ki) , DIMENSION(:), ALLOCATABLE  :: PtfmRefztRot      !< The rotation about zt of the body reference frame(s) from xt/yt [radians]
    REAL(ReKi)  :: WAMITULEN = 0.0_ReKi      !< WAMIT unit length scale [-]
    REAL(ReKi)  :: Gravity = 0.0_ReKi      !< Supplied by Driver:  Gravitational acceleration [(m/s^2)]
    TYPE(SeaSt_WaveFieldType) , POINTER :: WaveField => NULL()      !< Pointer to wave field [-]
    INTEGER(IntKi)  :: MnDrift = 0_IntKi      !< Calculate the mean drift force {0: no mean drift; [7,8,9,10,11, or 12]: WAMIT file to use} [-]
    INTEGER(IntKi)  :: NewmanApp = 0_IntKi      !< Slow drift forces computed with Newman approximation from WAMIT file:{0: No slow drift; [7,8,9,10,11, or 12]: WAMIT file to use} [-]
    INTEGER(IntKi)  :: DiffQTF = 0_IntKi      !< Full Difference-Frequency forces computed with full QTF's from WAMIT file: {0: No diff-QTF; [10,11, or 12]: WAMIT file to use} [-]
    INTEGER(IntKi)  :: SumQTF = 0_IntKi      !< Full Sum-Frequency forces computed with full QTF's from WAMIT file: {0: No sum-QTF; [10,11, or 12]: WAMIT file to use} [-]
    LOGICAL  :: MnDriftF = .false.      !< Flag indicating mean drift force should be calculated [-]
    LOGICAL  :: NewmanAppF = .false.      !< Flag indicating Newman approximation should be calculated [-]
    LOGICAL  :: DiffQTFF = .false.      !< Flag indicating the full difference QTF should be calculated [-]
    LOGICAL  :: SumQTFF = .false.      !< Flag indicating the full    sum     QTF should be calculated [-]
  END TYPE WAMIT2_InitInputType
! =======================
! =========  WAMIT2_MiscVarType  =======
  TYPE, PUBLIC :: WAMIT2_MiscVarType
    INTEGER(IntKi) , DIMENSION(:), ALLOCATABLE  :: LastIndWave      !< Index for last interpolation step of 2nd order forces [-]
    REAL(ReKi) , DIMENSION(:), ALLOCATABLE  :: F_Waves2      !< 2nd order force from this timestep [-]
  END TYPE WAMIT2_MiscVarType
! =======================
! =========  WAMIT2_ParameterType  =======
  TYPE, PUBLIC :: WAMIT2_ParameterType
    INTEGER(IntKi)  :: NBody = 0_IntKi      !< [>=1; only used when PotMod=1. If NBodyMod=1, the WAMIT data contains a vector of size 6*NBody x 1 and matrices of size 6*NBody x 6*NBody; if NBodyMod>1, there are NBody sets of WAMIT data each with a vector of size 6 x 1 and matrices of size 6 x 6] [-]
    INTEGER(IntKi)  :: NBodyMod = 0_IntKi      !< Body coupling model {1: include coupling terms between each body and NBody in HydroDyn equals NBODY in WAMIT, 2: neglect coupling terms between each body and NBODY=1 with XBODY=0 in WAMIT, 3: Neglect coupling terms between each body and NBODY=1 with XBODY=/0 in WAMIT} (switch) [only used when PotMod=1] [-]
    REAL(SiKi) , DIMENSION(:,:), ALLOCATABLE  :: WaveExctn2      !< Time series of the resulting 2nd order force (first index is timestep, second index is load component) [(N)]
    LOGICAL , DIMENSION(1:6)  :: MnDriftDims = .false.      !< Flags for which dimensions to calculate in MnDrift   calculations [-]
    LOGICAL , DIMENSION(1:6)  :: NewmanAppDims = .false.      !< Flags for which dimensions to calculate in NewmanApp calculations [-]
    LOGICAL , DIMENSION(1:6)  :: DiffQTFDims = .false.      !< Flags for which dimensions to calculate in DiffQTF   calculations [-]
    LOGICAL , DIMENSION(1:6)  :: SumQTFDims = .false.      !< Flags for which dimensions to calculate in SumQTF    calculations [-]
    LOGICAL  :: MnDriftF = .false.      !< Flag indicating mean drift force should be calculated [-]
    LOGICAL  :: NewmanAppF = .false.      !< Flag indicating Newman approximation should be calculated [-]
    LOGICAL  :: DiffQTFF = .false.      !< Flag indicating the full difference QTF should be calculated [-]
    LOGICAL  :: SumQTFF = .false.      !< Flag indicating the full    sum     QTF should be calculated [-]
  END TYPE WAMIT2_ParameterType
! =======================
! =========  WAMIT2_OutputType  =======
  TYPE, PUBLIC :: WAMIT2_OutputType
    TYPE(MeshType)  :: Mesh      !< Loads at the platform reference point in the inertial frame [-]
  END TYPE WAMIT2_OutputType
! =======================
   integer(IntKi), public, parameter :: WAMIT2_y_Mesh                    =   1 ! WAMIT2%Mesh

contains

subroutine WAMIT2_CopyInitInput(SrcInitInputData, DstInitInputData, CtrlCode, ErrStat, ErrMsg)
   type(WAMIT2_InitInputType), intent(in) :: SrcInitInputData
   type(WAMIT2_InitInputType), intent(inout) :: DstInitInputData
   integer(IntKi),  intent(in   ) :: CtrlCode
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   integer(B8Ki)                  :: LB(1), UB(1)
   integer(IntKi)                 :: ErrStat2
   character(ErrMsgLen)           :: ErrMsg2
   character(*), parameter        :: RoutineName = 'WAMIT2_CopyInitInput'
   ErrStat = ErrID_None
   ErrMsg  = ''
   DstInitInputData%HasWAMIT = SrcInitInputData%HasWAMIT
   DstInitInputData%WAMITFile = SrcInitInputData%WAMITFile
   DstInitInputData%NBody = SrcInitInputData%NBody
   DstInitInputData%NBodyMod = SrcInitInputData%NBodyMod
   if (allocated(SrcInitInputData%PtfmRefxt)) then
      LB(1:1) = lbound(SrcInitInputData%PtfmRefxt, kind=B8Ki)
      UB(1:1) = ubound(SrcInitInputData%PtfmRefxt, kind=B8Ki)
      if (.not. allocated(DstInitInputData%PtfmRefxt)) then
         allocate(DstInitInputData%PtfmRefxt(LB(1):UB(1)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstInitInputData%PtfmRefxt.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstInitInputData%PtfmRefxt = SrcInitInputData%PtfmRefxt
   end if
   if (allocated(SrcInitInputData%PtfmRefyt)) then
      LB(1:1) = lbound(SrcInitInputData%PtfmRefyt, kind=B8Ki)
      UB(1:1) = ubound(SrcInitInputData%PtfmRefyt, kind=B8Ki)
      if (.not. allocated(DstInitInputData%PtfmRefyt)) then
         allocate(DstInitInputData%PtfmRefyt(LB(1):UB(1)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstInitInputData%PtfmRefyt.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstInitInputData%PtfmRefyt = SrcInitInputData%PtfmRefyt
   end if
   if (allocated(SrcInitInputData%PtfmRefzt)) then
      LB(1:1) = lbound(SrcInitInputData%PtfmRefzt, kind=B8Ki)
      UB(1:1) = ubound(SrcInitInputData%PtfmRefzt, kind=B8Ki)
      if (.not. allocated(DstInitInputData%PtfmRefzt)) then
         allocate(DstInitInputData%PtfmRefzt(LB(1):UB(1)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstInitInputData%PtfmRefzt.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstInitInputData%PtfmRefzt = SrcInitInputData%PtfmRefzt
   end if
   if (allocated(SrcInitInputData%PtfmRefztRot)) then
      LB(1:1) = lbound(SrcInitInputData%PtfmRefztRot, kind=B8Ki)
      UB(1:1) = ubound(SrcInitInputData%PtfmRefztRot, kind=B8Ki)
      if (.not. allocated(DstInitInputData%PtfmRefztRot)) then
         allocate(DstInitInputData%PtfmRefztRot(LB(1):UB(1)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstInitInputData%PtfmRefztRot.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstInitInputData%PtfmRefztRot = SrcInitInputData%PtfmRefztRot
   end if
   DstInitInputData%WAMITULEN = SrcInitInputData%WAMITULEN
   DstInitInputData%Gravity = SrcInitInputData%Gravity
   DstInitInputData%WaveField => SrcInitInputData%WaveField
   DstInitInputData%MnDrift = SrcInitInputData%MnDrift
   DstInitInputData%NewmanApp = SrcInitInputData%NewmanApp
   DstInitInputData%DiffQTF = SrcInitInputData%DiffQTF
   DstInitInputData%SumQTF = SrcInitInputData%SumQTF
   DstInitInputData%MnDriftF = SrcInitInputData%MnDriftF
   DstInitInputData%NewmanAppF = SrcInitInputData%NewmanAppF
   DstInitInputData%DiffQTFF = SrcInitInputData%DiffQTFF
   DstInitInputData%SumQTFF = SrcInitInputData%SumQTFF
end subroutine

subroutine WAMIT2_DestroyInitInput(InitInputData, ErrStat, ErrMsg)
   type(WAMIT2_InitInputType), intent(inout) :: InitInputData
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   integer(IntKi)                 :: ErrStat2
   character(ErrMsgLen)           :: ErrMsg2
   character(*), parameter        :: RoutineName = 'WAMIT2_DestroyInitInput'
   ErrStat = ErrID_None
   ErrMsg  = ''
   if (allocated(InitInputData%PtfmRefxt)) then
      deallocate(InitInputData%PtfmRefxt)
   end if
   if (allocated(InitInputData%PtfmRefyt)) then
      deallocate(InitInputData%PtfmRefyt)
   end if
   if (allocated(InitInputData%PtfmRefzt)) then
      deallocate(InitInputData%PtfmRefzt)
   end if
   if (allocated(InitInputData%PtfmRefztRot)) then
      deallocate(InitInputData%PtfmRefztRot)
   end if
   nullify(InitInputData%WaveField)
end subroutine

subroutine WAMIT2_PackInitInput(RF, Indata)
   type(RegFile), intent(inout) :: RF
   type(WAMIT2_InitInputType), intent(in) :: InData
   character(*), parameter         :: RoutineName = 'WAMIT2_PackInitInput'
   logical         :: PtrInIndex
   if (RF%ErrStat >= AbortErrLev) return
   call RegPack(RF, InData%HasWAMIT)
   call RegPack(RF, InData%WAMITFile)
   call RegPack(RF, InData%NBody)
   call RegPack(RF, InData%NBodyMod)
   call RegPackAlloc(RF, InData%PtfmRefxt)
   call RegPackAlloc(RF, InData%PtfmRefyt)
   call RegPackAlloc(RF, InData%PtfmRefzt)
   call RegPackAlloc(RF, InData%PtfmRefztRot)
   call RegPack(RF, InData%WAMITULEN)
   call RegPack(RF, InData%Gravity)
   call RegPack(RF, associated(InData%WaveField))
   if (associated(InData%WaveField)) then
      call RegPackPointer(RF, c_loc(InData%WaveField), PtrInIndex)
      if (.not. PtrInIndex) then
         call SeaSt_WaveField_PackSeaSt_WaveFieldType(RF, InData%WaveField) 
      end if
   end if
   call RegPack(RF, InData%MnDrift)
   call RegPack(RF, InData%NewmanApp)
   call RegPack(RF, InData%DiffQTF)
   call RegPack(RF, InData%SumQTF)
   call RegPack(RF, InData%MnDriftF)
   call RegPack(RF, InData%NewmanAppF)
   call RegPack(RF, InData%DiffQTFF)
   call RegPack(RF, InData%SumQTFF)
   if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine WAMIT2_UnPackInitInput(RF, OutData)
   type(RegFile), intent(inout)    :: RF
   type(WAMIT2_InitInputType), intent(inout) :: OutData
   character(*), parameter            :: RoutineName = 'WAMIT2_UnPackInitInput'
   integer(B8Ki)   :: LB(1), UB(1)
   integer(IntKi)  :: stat
   logical         :: IsAllocAssoc
   integer(B8Ki)   :: PtrIdx
   type(c_ptr)     :: Ptr
   if (RF%ErrStat /= ErrID_None) return
   call RegUnpack(RF, OutData%HasWAMIT); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%WAMITFile); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%NBody); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%NBodyMod); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%PtfmRefxt); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%PtfmRefyt); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%PtfmRefzt); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%PtfmRefztRot); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%WAMITULEN); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%Gravity); if (RegCheckErr(RF, RoutineName)) return
   if (associated(OutData%WaveField)) deallocate(OutData%WaveField)
   call RegUnpack(RF, IsAllocAssoc); if (RegCheckErr(RF, RoutineName)) return
   if (IsAllocAssoc) then
      call RegUnpackPointer(RF, Ptr, PtrIdx); if (RegCheckErr(RF, RoutineName)) return
      if (c_associated(Ptr)) then
         call c_f_pointer(Ptr, OutData%WaveField)
      else
         allocate(OutData%WaveField,stat=stat)
         if (stat /= 0) then 
            call SetErrStat(ErrID_Fatal, 'Error allocating OutData%WaveField.', RF%ErrStat, RF%ErrMsg, RoutineName)
            return
         end if
         RF%Pointers(PtrIdx) = c_loc(OutData%WaveField)
         call SeaSt_WaveField_UnpackSeaSt_WaveFieldType(RF, OutData%WaveField) ! WaveField 
      end if
   else
      OutData%WaveField => null()
   end if
   call RegUnpack(RF, OutData%MnDrift); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%NewmanApp); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%DiffQTF); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%SumQTF); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%MnDriftF); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%NewmanAppF); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%DiffQTFF); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%SumQTFF); if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine WAMIT2_CopyMisc(SrcMiscData, DstMiscData, CtrlCode, ErrStat, ErrMsg)
   type(WAMIT2_MiscVarType), intent(in) :: SrcMiscData
   type(WAMIT2_MiscVarType), intent(inout) :: DstMiscData
   integer(IntKi),  intent(in   ) :: CtrlCode
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   integer(B8Ki)                  :: LB(1), UB(1)
   integer(IntKi)                 :: ErrStat2
   character(*), parameter        :: RoutineName = 'WAMIT2_CopyMisc'
   ErrStat = ErrID_None
   ErrMsg  = ''
   if (allocated(SrcMiscData%LastIndWave)) then
      LB(1:1) = lbound(SrcMiscData%LastIndWave, kind=B8Ki)
      UB(1:1) = ubound(SrcMiscData%LastIndWave, kind=B8Ki)
      if (.not. allocated(DstMiscData%LastIndWave)) then
         allocate(DstMiscData%LastIndWave(LB(1):UB(1)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstMiscData%LastIndWave.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstMiscData%LastIndWave = SrcMiscData%LastIndWave
   end if
   if (allocated(SrcMiscData%F_Waves2)) then
      LB(1:1) = lbound(SrcMiscData%F_Waves2, kind=B8Ki)
      UB(1:1) = ubound(SrcMiscData%F_Waves2, kind=B8Ki)
      if (.not. allocated(DstMiscData%F_Waves2)) then
         allocate(DstMiscData%F_Waves2(LB(1):UB(1)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstMiscData%F_Waves2.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstMiscData%F_Waves2 = SrcMiscData%F_Waves2
   end if
end subroutine

subroutine WAMIT2_DestroyMisc(MiscData, ErrStat, ErrMsg)
   type(WAMIT2_MiscVarType), intent(inout) :: MiscData
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   character(*), parameter        :: RoutineName = 'WAMIT2_DestroyMisc'
   ErrStat = ErrID_None
   ErrMsg  = ''
   if (allocated(MiscData%LastIndWave)) then
      deallocate(MiscData%LastIndWave)
   end if
   if (allocated(MiscData%F_Waves2)) then
      deallocate(MiscData%F_Waves2)
   end if
end subroutine

subroutine WAMIT2_PackMisc(RF, Indata)
   type(RegFile), intent(inout) :: RF
   type(WAMIT2_MiscVarType), intent(in) :: InData
   character(*), parameter         :: RoutineName = 'WAMIT2_PackMisc'
   if (RF%ErrStat >= AbortErrLev) return
   call RegPackAlloc(RF, InData%LastIndWave)
   call RegPackAlloc(RF, InData%F_Waves2)
   if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine WAMIT2_UnPackMisc(RF, OutData)
   type(RegFile), intent(inout)    :: RF
   type(WAMIT2_MiscVarType), intent(inout) :: OutData
   character(*), parameter            :: RoutineName = 'WAMIT2_UnPackMisc'
   integer(B8Ki)   :: LB(1), UB(1)
   integer(IntKi)  :: stat
   logical         :: IsAllocAssoc
   if (RF%ErrStat /= ErrID_None) return
   call RegUnpackAlloc(RF, OutData%LastIndWave); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%F_Waves2); if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine WAMIT2_CopyParam(SrcParamData, DstParamData, CtrlCode, ErrStat, ErrMsg)
   type(WAMIT2_ParameterType), intent(in) :: SrcParamData
   type(WAMIT2_ParameterType), intent(inout) :: DstParamData
   integer(IntKi),  intent(in   ) :: CtrlCode
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   integer(B8Ki)                  :: LB(2), UB(2)
   integer(IntKi)                 :: ErrStat2
   character(*), parameter        :: RoutineName = 'WAMIT2_CopyParam'
   ErrStat = ErrID_None
   ErrMsg  = ''
   DstParamData%NBody = SrcParamData%NBody
   DstParamData%NBodyMod = SrcParamData%NBodyMod
   if (allocated(SrcParamData%WaveExctn2)) then
      LB(1:2) = lbound(SrcParamData%WaveExctn2, kind=B8Ki)
      UB(1:2) = ubound(SrcParamData%WaveExctn2, kind=B8Ki)
      if (.not. allocated(DstParamData%WaveExctn2)) then
         allocate(DstParamData%WaveExctn2(LB(1):UB(1),LB(2):UB(2)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstParamData%WaveExctn2.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstParamData%WaveExctn2 = SrcParamData%WaveExctn2
   end if
   DstParamData%MnDriftDims = SrcParamData%MnDriftDims
   DstParamData%NewmanAppDims = SrcParamData%NewmanAppDims
   DstParamData%DiffQTFDims = SrcParamData%DiffQTFDims
   DstParamData%SumQTFDims = SrcParamData%SumQTFDims
   DstParamData%MnDriftF = SrcParamData%MnDriftF
   DstParamData%NewmanAppF = SrcParamData%NewmanAppF
   DstParamData%DiffQTFF = SrcParamData%DiffQTFF
   DstParamData%SumQTFF = SrcParamData%SumQTFF
end subroutine

subroutine WAMIT2_DestroyParam(ParamData, ErrStat, ErrMsg)
   type(WAMIT2_ParameterType), intent(inout) :: ParamData
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   character(*), parameter        :: RoutineName = 'WAMIT2_DestroyParam'
   ErrStat = ErrID_None
   ErrMsg  = ''
   if (allocated(ParamData%WaveExctn2)) then
      deallocate(ParamData%WaveExctn2)
   end if
end subroutine

subroutine WAMIT2_PackParam(RF, Indata)
   type(RegFile), intent(inout) :: RF
   type(WAMIT2_ParameterType), intent(in) :: InData
   character(*), parameter         :: RoutineName = 'WAMIT2_PackParam'
   if (RF%ErrStat >= AbortErrLev) return
   call RegPack(RF, InData%NBody)
   call RegPack(RF, InData%NBodyMod)
   call RegPackAlloc(RF, InData%WaveExctn2)
   call RegPack(RF, InData%MnDriftDims)
   call RegPack(RF, InData%NewmanAppDims)
   call RegPack(RF, InData%DiffQTFDims)
   call RegPack(RF, InData%SumQTFDims)
   call RegPack(RF, InData%MnDriftF)
   call RegPack(RF, InData%NewmanAppF)
   call RegPack(RF, InData%DiffQTFF)
   call RegPack(RF, InData%SumQTFF)
   if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine WAMIT2_UnPackParam(RF, OutData)
   type(RegFile), intent(inout)    :: RF
   type(WAMIT2_ParameterType), intent(inout) :: OutData
   character(*), parameter            :: RoutineName = 'WAMIT2_UnPackParam'
   integer(B8Ki)   :: LB(2), UB(2)
   integer(IntKi)  :: stat
   logical         :: IsAllocAssoc
   if (RF%ErrStat /= ErrID_None) return
   call RegUnpack(RF, OutData%NBody); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%NBodyMod); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%WaveExctn2); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%MnDriftDims); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%NewmanAppDims); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%DiffQTFDims); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%SumQTFDims); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%MnDriftF); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%NewmanAppF); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%DiffQTFF); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%SumQTFF); if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine WAMIT2_CopyOutput(SrcOutputData, DstOutputData, CtrlCode, ErrStat, ErrMsg)
   type(WAMIT2_OutputType), intent(inout) :: SrcOutputData
   type(WAMIT2_OutputType), intent(inout) :: DstOutputData
   integer(IntKi),  intent(in   ) :: CtrlCode
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   integer(IntKi)                 :: ErrStat2
   character(ErrMsgLen)           :: ErrMsg2
   character(*), parameter        :: RoutineName = 'WAMIT2_CopyOutput'
   ErrStat = ErrID_None
   ErrMsg  = ''
   call MeshCopy(SrcOutputData%Mesh, DstOutputData%Mesh, CtrlCode, ErrStat2, ErrMsg2 )
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   if (ErrStat >= AbortErrLev) return
end subroutine

subroutine WAMIT2_DestroyOutput(OutputData, ErrStat, ErrMsg)
   type(WAMIT2_OutputType), intent(inout) :: OutputData
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   integer(IntKi)                 :: ErrStat2
   character(ErrMsgLen)           :: ErrMsg2
   character(*), parameter        :: RoutineName = 'WAMIT2_DestroyOutput'
   ErrStat = ErrID_None
   ErrMsg  = ''
   call MeshDestroy( OutputData%Mesh, ErrStat2, ErrMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
end subroutine

subroutine WAMIT2_PackOutput(RF, Indata)
   type(RegFile), intent(inout) :: RF
   type(WAMIT2_OutputType), intent(in) :: InData
   character(*), parameter         :: RoutineName = 'WAMIT2_PackOutput'
   if (RF%ErrStat >= AbortErrLev) return
   call MeshPack(RF, InData%Mesh) 
   if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine WAMIT2_UnPackOutput(RF, OutData)
   type(RegFile), intent(inout)    :: RF
   type(WAMIT2_OutputType), intent(inout) :: OutData
   character(*), parameter            :: RoutineName = 'WAMIT2_UnPackOutput'
   if (RF%ErrStat /= ErrID_None) return
   call MeshUnpack(RF, OutData%Mesh) ! Mesh 
end subroutine

subroutine WAMIT2_Output_ExtrapInterp(y, t, y_out, t_out, ErrStat, ErrMsg)
   !
   ! This subroutine calculates a extrapolated (or interpolated) Output y_out at time t_out, from previous/future time
   ! values of y (which has values associated with times in t).  Order of the interpolation is given by the size of y
   !
   !  expressions below based on either
   !
   !  f(t) = a
   !  f(t) = a + b * t, or
   !  f(t) = a + b * t + c * t**2
   !
   !  where a, b and c are determined as the solution to
   !  f(t1) = y1, f(t2) = y2, f(t3) = y3  (as appropriate)
   !
   !----------------------------------------------------------------------------------------------------------------------------------
   
   type(WAMIT2_OutputType), intent(inout)  :: y(:) ! Output at t1 > t2 > t3
   real(DbKi),                 intent(in   )  :: t(:)           ! Times associated with the Outputs
   type(WAMIT2_OutputType), intent(inout)  :: y_out ! Output at tin_out
   real(DbKi),                 intent(in   )  :: t_out           ! time to be extrap/interp'd to
   integer(IntKi),             intent(  out)  :: ErrStat         ! Error status of the operation
   character(*),               intent(  out)  :: ErrMsg          ! Error message if ErrStat /= ErrID_None
   ! local variables
   integer(IntKi)                             :: order           ! order of polynomial fit (max 2)
   integer(IntKi)                             :: ErrStat2        ! local errors
   character(ErrMsgLen)                       :: ErrMsg2         ! local errors
   character(*),    PARAMETER                 :: RoutineName = 'WAMIT2_Output_ExtrapInterp'
   
   ! Initialize ErrStat
   ErrStat = ErrID_None
   ErrMsg  = ''
   if (size(t) /= size(y)) then
      call SetErrStat(ErrID_Fatal, 'size(t) must equal size(y)', ErrStat, ErrMsg, RoutineName)
      return
   endif
   order = size(y) - 1
   select case (order)
   case (0)
      call WAMIT2_CopyOutput(y(1), y_out, MESH_UPDATECOPY, ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   case (1)
      call WAMIT2_Output_ExtrapInterp1(y(1), y(2), t, y_out, t_out, ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   case (2)
      call WAMIT2_Output_ExtrapInterp2(y(1), y(2), y(3), t, y_out, t_out, ErrStat2, ErrMsg2)
         call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   case default
      call SetErrStat(ErrID_Fatal, 'size(y) must be less than 4 (order must be less than 3).', ErrStat, ErrMsg, RoutineName)
      return
   end select
end subroutine

SUBROUTINE WAMIT2_Output_ExtrapInterp1(y1, y2, tin, y_out, tin_out, ErrStat, ErrMsg )
!
! This subroutine calculates a extrapolated (or interpolated) Output y_out at time t_out, from previous/future time
! values of y (which has values associated with times in t).  Order of the interpolation is 1.
!
!  f(t) = a + b * t, or
!
!  where a and b are determined as the solution to
!  f(t1) = y1, f(t2) = y2
!
!..................................................................................................................................

   TYPE(WAMIT2_OutputType), INTENT(INOUT)  :: y1    ! Output at t1 > t2
   TYPE(WAMIT2_OutputType), INTENT(INOUT)  :: y2    ! Output at t2 
   REAL(DbKi),         INTENT(IN   )          :: tin(2)   ! Times associated with the Outputs
   TYPE(WAMIT2_OutputType), INTENT(INOUT)  :: y_out ! Output at tin_out
   REAL(DbKi),         INTENT(IN   )          :: tin_out  ! time to be extrap/interp'd to
   INTEGER(IntKi),     INTENT(  OUT)          :: ErrStat  ! Error status of the operation
   CHARACTER(*),       INTENT(  OUT)          :: ErrMsg   ! Error message if ErrStat /= ErrID_None
   ! local variables
   REAL(DbKi)                                 :: t(2)     ! Times associated with the Outputs
   REAL(DbKi)                                 :: t_out    ! Time to which to be extrap/interpd
   CHARACTER(*),                    PARAMETER :: RoutineName = 'WAMIT2_Output_ExtrapInterp1'
   REAL(DbKi)                                 :: a1, a2   ! temporary for extrapolation/interpolation
   INTEGER(IntKi)                             :: ErrStat2 ! local errors
   CHARACTER(ErrMsgLen)                       :: ErrMsg2  ! local errors
   ! Initialize ErrStat
   ErrStat = ErrID_None
   ErrMsg  = ''
   ! we'll subtract a constant from the times to resolve some 
   ! numerical issues when t gets large (and to simplify the equations)
   t = tin - tin(1)
   t_out = tin_out - tin(1)
   
   IF (EqualRealNos(t(1), t(2))) THEN
      CALL SetErrStat(ErrID_Fatal, 't(1) must not equal t(2) to avoid a division-by-zero error.', ErrStat, ErrMsg, RoutineName)
      RETURN
   END IF
   
   ! Calculate weighting factors from Lagrange polynomial
   a1 = -(t_out - t(2))/t(2)
   a2 = t_out/t(2)
   
   CALL MeshExtrapInterp1(y1%Mesh, y2%Mesh, tin, y_out%Mesh, tin_out, ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg,RoutineName)
END SUBROUTINE

SUBROUTINE WAMIT2_Output_ExtrapInterp2(y1, y2, y3, tin, y_out, tin_out, ErrStat, ErrMsg )
!
! This subroutine calculates a extrapolated (or interpolated) Output y_out at time t_out, from previous/future time
! values of y (which has values associated with times in t).  Order of the interpolation is 2.
!
!  expressions below based on either
!
!  f(t) = a + b * t + c * t**2
!
!  where a, b and c are determined as the solution to
!  f(t1) = y1, f(t2) = y2, f(t3) = y3
!
!..................................................................................................................................

   TYPE(WAMIT2_OutputType), INTENT(INOUT)  :: y1      ! Output at t1 > t2 > t3
   TYPE(WAMIT2_OutputType), INTENT(INOUT)  :: y2      ! Output at t2 > t3
   TYPE(WAMIT2_OutputType), INTENT(INOUT)  :: y3      ! Output at t3
   REAL(DbKi),                 INTENT(IN   )  :: tin(3)    ! Times associated with the Outputs
   TYPE(WAMIT2_OutputType), INTENT(INOUT)  :: y_out     ! Output at tin_out
   REAL(DbKi),                 INTENT(IN   )  :: tin_out   ! time to be extrap/interp'd to
   INTEGER(IntKi),             INTENT(  OUT)  :: ErrStat   ! Error status of the operation
   CHARACTER(*),               INTENT(  OUT)  :: ErrMsg    ! Error message if ErrStat /= ErrID_None
   ! local variables
   REAL(DbKi)                                 :: t(3)      ! Times associated with the Outputs
   REAL(DbKi)                                 :: t_out     ! Time to which to be extrap/interpd
   INTEGER(IntKi)                             :: order     ! order of polynomial fit (max 2)
   REAL(DbKi)                                 :: a1,a2,a3 ! temporary for extrapolation/interpolation
   INTEGER(IntKi)                             :: ErrStat2 ! local errors
   CHARACTER(ErrMsgLen)                       :: ErrMsg2  ! local errors
   CHARACTER(*),            PARAMETER         :: RoutineName = 'WAMIT2_Output_ExtrapInterp2'
   ! Initialize ErrStat
   ErrStat = ErrID_None
   ErrMsg  = ''
   ! we'll subtract a constant from the times to resolve some 
   ! numerical issues when t gets large (and to simplify the equations)
   t = tin - tin(1)
   t_out = tin_out - tin(1)
   
   IF ( EqualRealNos( t(1), t(2) ) ) THEN
      CALL SetErrStat(ErrID_Fatal, 't(1) must not equal t(2) to avoid a division-by-zero error.', ErrStat, ErrMsg,RoutineName)
      RETURN
   ELSE IF ( EqualRealNos( t(2), t(3) ) ) THEN
      CALL SetErrStat(ErrID_Fatal, 't(2) must not equal t(3) to avoid a division-by-zero error.', ErrStat, ErrMsg,RoutineName)
      RETURN
   ELSE IF ( EqualRealNos( t(1), t(3) ) ) THEN
      CALL SetErrStat(ErrID_Fatal, 't(1) must not equal t(3) to avoid a division-by-zero error.', ErrStat, ErrMsg,RoutineName)
      RETURN
   END IF
   
   ! Calculate Lagrange polynomial coefficients
   a1 = (t_out - t(2))*(t_out - t(3))/((t(1) - t(2))*(t(1) - t(3)))
   a2 = (t_out - t(1))*(t_out - t(3))/((t(2) - t(1))*(t(2) - t(3)))
   a3 = (t_out - t(1))*(t_out - t(2))/((t(3) - t(1))*(t(3) - t(2)))
   CALL MeshExtrapInterp2(y1%Mesh, y2%Mesh, y3%Mesh, tin, y_out%Mesh, tin_out, ErrStat2, ErrMsg2)
      CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg,RoutineName)
END SUBROUTINE

function WAMIT2_OutputMeshPointer(y, DL) result(Mesh)
   type(WAMIT2_OutputType), target, intent(in) :: y
   type(DatLoc), intent(in)               :: DL
   type(MeshType), pointer                :: Mesh
   nullify(Mesh)
   select case (DL%Num)
   case (WAMIT2_y_Mesh)
       Mesh => y%Mesh
   end select
end function

subroutine WAMIT2_PackOutputAry(Vars, y, ValAry)
   type(WAMIT2_OutputType), intent(in)     :: y
   type(ModVarsType), intent(in)          :: Vars
   real(R8Ki), intent(inout)              :: ValAry(:)
   integer(IntKi)                         :: i
   do i = 1, size(Vars%y)
      associate (V => Vars%y(i), DL => Vars%y(i)%DL)
         select case (DL%Num)
         case (WAMIT2_y_Mesh)
            call MV_Pack(V, y%Mesh, ValAry)                                     ! Mesh
         case default
            ValAry(V%iLoc(1):V%iLoc(2)) = 0.0_R8Ki
         end select
      end associate
   end do
end subroutine

subroutine WAMIT2_UnpackOutputAry(Vars, ValAry, y)
   type(ModVarsType), intent(in)          :: Vars
   real(R8Ki), intent(in)                 :: ValAry(:)
   type(WAMIT2_OutputType), intent(inout)  :: y
   integer(IntKi)                         :: i
   do i = 1, size(Vars%y)
      associate (V => Vars%y(i), DL => Vars%y(i)%DL)
         select case (DL%Num)
         case (WAMIT2_y_Mesh)
            call MV_Unpack(V, ValAry, y%Mesh)                                   ! Mesh
         end select
      end associate
   end do
end subroutine

function WAMIT2_OutputFieldName(DL) result(Name)
   type(DatLoc), intent(in)      :: DL
   character(32)                 :: Name
   select case (DL%Num)
   case (WAMIT2_y_Mesh)
       Name = "y%Mesh"
   case default
       Name = "Unknown Field"
   end select
end function

END MODULE WAMIT2_Types

!ENDOFREGISTRYGENERATEDFILE
