!STARTOFREGISTRYGENERATEDFILE 'MAP_Fortran_Types.f90'
!
! WARNING This file is generated automatically by the FAST registry.
! Do not edit.  Your changes to this file will be lost.
!
! FAST Registry
!*********************************************************************************************************************************
! MAP_Fortran_Types
!.................................................................................................................................
! This file is part of MAP_Fortran.
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
!> This module contains the user-defined types needed in MAP_Fortran. It also contains copy, destroy, pack, and
!! unpack routines associated with each defined data type. This code is automatically generated by the FAST Registry.
MODULE MAP_Fortran_Types
!---------------------------------------------------------------------------------------------------------------------------------
USE NWTC_Library
IMPLICIT NONE
! =========  Lin_InitInputType  =======
  TYPE, PUBLIC :: Lin_InitInputType
    LOGICAL  :: linearize = .false.      !< Flag that tells this module if the glue code wants to linearize.  (fortran-only) [-]
  END TYPE Lin_InitInputType
! =======================
! =========  Lin_InitOutputType  =======
  TYPE, PUBLIC :: Lin_InitOutputType
    CHARACTER(200) , DIMENSION(:), ALLOCATABLE  :: LinNames_y      !< second line of output file contents: units (fortran-only) [-]
    CHARACTER(200) , DIMENSION(:), ALLOCATABLE  :: LinNames_u      !< Names of the inputs used in linearization (fortran-only) [-]
    LOGICAL , DIMENSION(:), ALLOCATABLE  :: IsLoad_u      !< Flag that tells FAST if the inputs used in linearization are loads (for preconditioning matrix) (fortran-only) [-]
  END TYPE Lin_InitOutputType
! =======================
! =========  Lin_ParamType  =======
  TYPE, PUBLIC :: Lin_ParamType
    INTEGER(IntKi) , DIMENSION(:,:), ALLOCATABLE  :: Jac_u_indx      !< matrix to help fill/pack the u vector in computing the jacobian (fortran-only) [-]
    REAL(R8Ki)  :: du = 0.0_R8Ki      !< determines size of the translational displacement perturbation for u (inputs) (fortran-only) [-]
    INTEGER(IntKi)  :: Jac_ny = 0_IntKi      !< number of outputs in jacobian matrix (fortran-only) [-]
  END TYPE Lin_ParamType
! =======================
CONTAINS

subroutine MAP_Fortran_CopyLin_InitInputType(SrcLin_InitInputTypeData, DstLin_InitInputTypeData, CtrlCode, ErrStat, ErrMsg)
   type(Lin_InitInputType), intent(in) :: SrcLin_InitInputTypeData
   type(Lin_InitInputType), intent(inout) :: DstLin_InitInputTypeData
   integer(IntKi),  intent(in   ) :: CtrlCode
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   character(*), parameter        :: RoutineName = 'MAP_Fortran_CopyLin_InitInputType'
   ErrStat = ErrID_None
   ErrMsg  = ''
   DstLin_InitInputTypeData%linearize = SrcLin_InitInputTypeData%linearize
end subroutine

subroutine MAP_Fortran_DestroyLin_InitInputType(Lin_InitInputTypeData, ErrStat, ErrMsg)
   type(Lin_InitInputType), intent(inout) :: Lin_InitInputTypeData
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   character(*), parameter        :: RoutineName = 'MAP_Fortran_DestroyLin_InitInputType'
   ErrStat = ErrID_None
   ErrMsg  = ''
end subroutine

subroutine MAP_Fortran_PackLin_InitInputType(RF, Indata)
   type(RegFile), intent(inout) :: RF
   type(Lin_InitInputType), intent(in) :: InData
   character(*), parameter         :: RoutineName = 'MAP_Fortran_PackLin_InitInputType'
   if (RF%ErrStat >= AbortErrLev) return
   call RegPack(RF, InData%linearize)
   if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine MAP_Fortran_UnPackLin_InitInputType(RF, OutData)
   type(RegFile), intent(inout)    :: RF
   type(Lin_InitInputType), intent(inout) :: OutData
   character(*), parameter            :: RoutineName = 'MAP_Fortran_UnPackLin_InitInputType'
   if (RF%ErrStat /= ErrID_None) return
   call RegUnpack(RF, OutData%linearize); if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine MAP_Fortran_CopyLin_InitOutputType(SrcLin_InitOutputTypeData, DstLin_InitOutputTypeData, CtrlCode, ErrStat, ErrMsg)
   type(Lin_InitOutputType), intent(in) :: SrcLin_InitOutputTypeData
   type(Lin_InitOutputType), intent(inout) :: DstLin_InitOutputTypeData
   integer(IntKi),  intent(in   ) :: CtrlCode
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   integer(B8Ki)                  :: LB(1), UB(1)
   integer(IntKi)                 :: ErrStat2
   character(*), parameter        :: RoutineName = 'MAP_Fortran_CopyLin_InitOutputType'
   ErrStat = ErrID_None
   ErrMsg  = ''
   if (allocated(SrcLin_InitOutputTypeData%LinNames_y)) then
      LB(1:1) = lbound(SrcLin_InitOutputTypeData%LinNames_y, kind=B8Ki)
      UB(1:1) = ubound(SrcLin_InitOutputTypeData%LinNames_y, kind=B8Ki)
      if (.not. allocated(DstLin_InitOutputTypeData%LinNames_y)) then
         allocate(DstLin_InitOutputTypeData%LinNames_y(LB(1):UB(1)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstLin_InitOutputTypeData%LinNames_y.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstLin_InitOutputTypeData%LinNames_y = SrcLin_InitOutputTypeData%LinNames_y
   end if
   if (allocated(SrcLin_InitOutputTypeData%LinNames_u)) then
      LB(1:1) = lbound(SrcLin_InitOutputTypeData%LinNames_u, kind=B8Ki)
      UB(1:1) = ubound(SrcLin_InitOutputTypeData%LinNames_u, kind=B8Ki)
      if (.not. allocated(DstLin_InitOutputTypeData%LinNames_u)) then
         allocate(DstLin_InitOutputTypeData%LinNames_u(LB(1):UB(1)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstLin_InitOutputTypeData%LinNames_u.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstLin_InitOutputTypeData%LinNames_u = SrcLin_InitOutputTypeData%LinNames_u
   end if
   if (allocated(SrcLin_InitOutputTypeData%IsLoad_u)) then
      LB(1:1) = lbound(SrcLin_InitOutputTypeData%IsLoad_u, kind=B8Ki)
      UB(1:1) = ubound(SrcLin_InitOutputTypeData%IsLoad_u, kind=B8Ki)
      if (.not. allocated(DstLin_InitOutputTypeData%IsLoad_u)) then
         allocate(DstLin_InitOutputTypeData%IsLoad_u(LB(1):UB(1)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstLin_InitOutputTypeData%IsLoad_u.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstLin_InitOutputTypeData%IsLoad_u = SrcLin_InitOutputTypeData%IsLoad_u
   end if
end subroutine

subroutine MAP_Fortran_DestroyLin_InitOutputType(Lin_InitOutputTypeData, ErrStat, ErrMsg)
   type(Lin_InitOutputType), intent(inout) :: Lin_InitOutputTypeData
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   character(*), parameter        :: RoutineName = 'MAP_Fortran_DestroyLin_InitOutputType'
   ErrStat = ErrID_None
   ErrMsg  = ''
   if (allocated(Lin_InitOutputTypeData%LinNames_y)) then
      deallocate(Lin_InitOutputTypeData%LinNames_y)
   end if
   if (allocated(Lin_InitOutputTypeData%LinNames_u)) then
      deallocate(Lin_InitOutputTypeData%LinNames_u)
   end if
   if (allocated(Lin_InitOutputTypeData%IsLoad_u)) then
      deallocate(Lin_InitOutputTypeData%IsLoad_u)
   end if
end subroutine

subroutine MAP_Fortran_PackLin_InitOutputType(RF, Indata)
   type(RegFile), intent(inout) :: RF
   type(Lin_InitOutputType), intent(in) :: InData
   character(*), parameter         :: RoutineName = 'MAP_Fortran_PackLin_InitOutputType'
   if (RF%ErrStat >= AbortErrLev) return
   call RegPackAlloc(RF, InData%LinNames_y)
   call RegPackAlloc(RF, InData%LinNames_u)
   call RegPackAlloc(RF, InData%IsLoad_u)
   if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine MAP_Fortran_UnPackLin_InitOutputType(RF, OutData)
   type(RegFile), intent(inout)    :: RF
   type(Lin_InitOutputType), intent(inout) :: OutData
   character(*), parameter            :: RoutineName = 'MAP_Fortran_UnPackLin_InitOutputType'
   integer(B8Ki)   :: LB(1), UB(1)
   integer(IntKi)  :: stat
   logical         :: IsAllocAssoc
   if (RF%ErrStat /= ErrID_None) return
   call RegUnpackAlloc(RF, OutData%LinNames_y); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%LinNames_u); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpackAlloc(RF, OutData%IsLoad_u); if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine MAP_Fortran_CopyLin_ParamType(SrcLin_ParamTypeData, DstLin_ParamTypeData, CtrlCode, ErrStat, ErrMsg)
   type(Lin_ParamType), intent(in) :: SrcLin_ParamTypeData
   type(Lin_ParamType), intent(inout) :: DstLin_ParamTypeData
   integer(IntKi),  intent(in   ) :: CtrlCode
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   integer(B8Ki)                  :: LB(2), UB(2)
   integer(IntKi)                 :: ErrStat2
   character(*), parameter        :: RoutineName = 'MAP_Fortran_CopyLin_ParamType'
   ErrStat = ErrID_None
   ErrMsg  = ''
   if (allocated(SrcLin_ParamTypeData%Jac_u_indx)) then
      LB(1:2) = lbound(SrcLin_ParamTypeData%Jac_u_indx, kind=B8Ki)
      UB(1:2) = ubound(SrcLin_ParamTypeData%Jac_u_indx, kind=B8Ki)
      if (.not. allocated(DstLin_ParamTypeData%Jac_u_indx)) then
         allocate(DstLin_ParamTypeData%Jac_u_indx(LB(1):UB(1),LB(2):UB(2)), stat=ErrStat2)
         if (ErrStat2 /= 0) then
            call SetErrStat(ErrID_Fatal, 'Error allocating DstLin_ParamTypeData%Jac_u_indx.', ErrStat, ErrMsg, RoutineName)
            return
         end if
      end if
      DstLin_ParamTypeData%Jac_u_indx = SrcLin_ParamTypeData%Jac_u_indx
   end if
   DstLin_ParamTypeData%du = SrcLin_ParamTypeData%du
   DstLin_ParamTypeData%Jac_ny = SrcLin_ParamTypeData%Jac_ny
end subroutine

subroutine MAP_Fortran_DestroyLin_ParamType(Lin_ParamTypeData, ErrStat, ErrMsg)
   type(Lin_ParamType), intent(inout) :: Lin_ParamTypeData
   integer(IntKi),  intent(  out) :: ErrStat
   character(*),    intent(  out) :: ErrMsg
   character(*), parameter        :: RoutineName = 'MAP_Fortran_DestroyLin_ParamType'
   ErrStat = ErrID_None
   ErrMsg  = ''
   if (allocated(Lin_ParamTypeData%Jac_u_indx)) then
      deallocate(Lin_ParamTypeData%Jac_u_indx)
   end if
end subroutine

subroutine MAP_Fortran_PackLin_ParamType(RF, Indata)
   type(RegFile), intent(inout) :: RF
   type(Lin_ParamType), intent(in) :: InData
   character(*), parameter         :: RoutineName = 'MAP_Fortran_PackLin_ParamType'
   if (RF%ErrStat >= AbortErrLev) return
   call RegPackAlloc(RF, InData%Jac_u_indx)
   call RegPack(RF, InData%du)
   call RegPack(RF, InData%Jac_ny)
   if (RegCheckErr(RF, RoutineName)) return
end subroutine

subroutine MAP_Fortran_UnPackLin_ParamType(RF, OutData)
   type(RegFile), intent(inout)    :: RF
   type(Lin_ParamType), intent(inout) :: OutData
   character(*), parameter            :: RoutineName = 'MAP_Fortran_UnPackLin_ParamType'
   integer(B8Ki)   :: LB(2), UB(2)
   integer(IntKi)  :: stat
   logical         :: IsAllocAssoc
   if (RF%ErrStat /= ErrID_None) return
   call RegUnpackAlloc(RF, OutData%Jac_u_indx); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%du); if (RegCheckErr(RF, RoutineName)) return
   call RegUnpack(RF, OutData%Jac_ny); if (RegCheckErr(RF, RoutineName)) return
end subroutine
END MODULE MAP_Fortran_Types
!ENDOFREGISTRYGENERATEDFILE
