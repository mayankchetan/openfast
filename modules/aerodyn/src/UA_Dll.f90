!> Fortran-side binding for the UnsteadyAero user-DLL interface (UA_Mod=9).
!! Mirrors modules/aerodyn/src/ua_dll_api.h exactly (ABI version 1).
module UA_Dll
   use ISO_C_BINDING
   use NWTC_Library
   use UnsteadyAero_Types
   use AirfoilInfo_Types
   implicit none
   private
   public :: UADll_Load, UADll_Init, UADll_UpdateStates, UADll_CalcOutput, &
             UADll_Pack, UADll_Unpack, UADll_End

   integer(C_INT32_T), parameter :: UA_DLL_ABI_VERSION = 1
   integer(IntKi),     parameter :: MSGLEN = 1024

   type, bind(C) :: UA_DllInfo_C
      integer(C_INT32_T) :: abi_version, struct_size
      character(kind=C_CHAR) :: model_name(64)
      integer(C_INT32_T) :: n_states_per_elem
      integer(C_INT32_T) :: caps            ! uint32 on C side; bit 0 = pack capability
      real(C_DOUBLE)     :: dt_min, dt_max
   end type

   type, bind(C) :: UA_DllPolar_C
      integer(C_INT32_T) :: abi_version, struct_size, n_alpha, pad
      type(C_PTR)        :: alpha, Cl, Cd, Cm
      real(C_DOUBLE)     :: alpha0, Cl_alpha, Re, UserProp
   end type

   type, bind(C) :: UA_DllInitInput_C
      integer(C_INT32_T) :: abi_version, struct_size
      real(C_DOUBLE)     :: dt, a_s, d_34_to_ac
      integer(C_INT32_T) :: n_blades, n_nodes_per_blade
      type(C_PTR)        :: chord, polar_id
      integer(C_INT32_T) :: n_polars, pad
      type(C_PTR)        :: polars, param_str
      integer(C_INT32_T) :: param_len, pad2
   end type

   type, bind(C) :: UA_DllElemInput_C
      real(C_DOUBLE) :: U, alpha34, Re, UserProp, v_ac_x, v_ac_y, omega
   end type
   type, bind(C) :: UA_DllElemOutput_C
      real(C_DOUBLE) :: Cn, Cc, Cl, Cd, Cm
   end type

   !> Local (non-registry) helper for marshalling ragged per-polar backing arrays.
   !! Used only as subroutine-local TARGET storage inside UADll_Init; never
   !! stored in Misc/Parameter (see controller note in the task-4 brief).
   type :: DllRealVec
      real(C_DOUBLE), allocatable :: v(:)
   end type

   abstract interface
      integer(C_INT32_T) function ua_getinfo_f(info, msg, msg_len) bind(C)
         import; type(UA_DllInfo_C) :: info
         character(kind=C_CHAR) :: msg(*); integer(C_INT32_T), value :: msg_len
      end function
      integer(C_INT32_T) function ua_init_f(ini, ctx, msg, msg_len) bind(C)
         import; type(UA_DllInitInput_C) :: ini
         type(C_PTR) :: ctx
         character(kind=C_CHAR) :: msg(*); integer(C_INT32_T), value :: msg_len
      end function
      integer(C_INT32_T) function ua_update_f(ctx, t, step, u_t, u_tp1, n, msg, msg_len) bind(C)
         import; type(C_PTR), value :: ctx
         real(C_DOUBLE), value :: t; integer(C_INT64_T), value :: step
         type(UA_DllElemInput_C) :: u_t(*), u_tp1(*)
         integer(C_INT32_T), value :: n
         character(kind=C_CHAR) :: msg(*); integer(C_INT32_T), value :: msg_len
      end function
      integer(C_INT32_T) function ua_output_f(ctx, t, u, n, y, msg, msg_len) bind(C)
         import; type(C_PTR), value :: ctx
         real(C_DOUBLE), value :: t
         type(UA_DllElemInput_C) :: u(*); integer(C_INT32_T), value :: n
         type(UA_DllElemOutput_C) :: y(*)
         character(kind=C_CHAR) :: msg(*); integer(C_INT32_T), value :: msg_len
      end function
      integer(C_INT32_T) function ua_pack_f(ctx, buf, n_bytes, msg, msg_len) bind(C)
         import; type(C_PTR), value :: ctx, buf
         integer(C_INT64_T) :: n_bytes
         character(kind=C_CHAR) :: msg(*); integer(C_INT32_T), value :: msg_len
      end function
      integer(C_INT32_T) function ua_unpack_f(ctx, buf, n_bytes, msg, msg_len) bind(C)
         import; type(C_PTR), value :: ctx, buf
         integer(C_INT64_T), value :: n_bytes
         character(kind=C_CHAR) :: msg(*); integer(C_INT32_T), value :: msg_len
      end function
      integer(C_INT32_T) function ua_end_f(ctx, msg, msg_len) bind(C)
         import; type(C_PTR), value :: ctx
         character(kind=C_CHAR) :: msg(*); integer(C_INT32_T), value :: msg_len
      end function
   end interface

contains

subroutine UADll_Load(p, FileName, ErrStat, ErrMsg)
   type(UA_ParameterType), intent(inout) :: p
   character(*),           intent(in   ) :: FileName
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg
   p%UA_DLL%FileName    = FileName
   p%UA_DLL%ProcName(1) = 'ua_dll_getinfo'
   p%UA_DLL%ProcName(2) = 'ua_dll_init'
   p%UA_DLL%ProcName(3) = 'ua_dll_update'
   p%UA_DLL%ProcName(4) = 'ua_dll_output'
   p%UA_DLL%ProcName(5) = 'ua_dll_pack'
   p%UA_DLL%ProcName(6) = 'ua_dll_unpack'
   p%UA_DLL%ProcName(7) = 'ua_dll_end'
   call LoadDynamicLib(p%UA_DLL, ErrStat, ErrMsg)   ! resolves all 7; fatal if any missing
end subroutine

! helper: run a call's msg buffer back into ErrStat/ErrMsg
subroutine SetFromDllRet(rc, cmsg, RoutineName, ErrStat, ErrMsg)
   integer(C_INT32_T), intent(in) :: rc
   character(kind=C_CHAR), intent(in) :: cmsg(MSGLEN)
   character(*), intent(in) :: RoutineName
   integer(IntKi), intent(out) :: ErrStat
   character(*),   intent(out) :: ErrMsg
   character(MSGLEN) :: fmsg
   integer :: i
   ErrStat = ErrID_None; ErrMsg = ''
   if (rc == 0) return
   fmsg = ''
   do i = 1, MSGLEN
      if (cmsg(i) == C_NULL_CHAR) exit
      fmsg(i:i) = cmsg(i)
   end do
   if (rc > 0) then
      ErrStat = ErrID_Warn
   else
      ErrStat = ErrID_Fatal
   end if
   ErrMsg = trim(RoutineName)//': UA DLL returned '//trim(Num2LStr(int(rc,IntKi)))//': '//trim(fmsg)
end subroutine

!> Builds UA_DllInitInput from InitInp/AFInfo/AFIndx, calls ua_dll_getinfo then
!! ua_dll_init, and stores the returned opaque context in m%UA_DLL_ctx.
!! All marshalling buffers below are subroutine-local: the ABI contract
!! guarantees the DLL deep-copies everything it needs during ua_dll_init, so
!! nothing here needs to persist past this call.
subroutine UADll_Init(p, InitInp, AFInfo, AFIndx, m, ErrStat, ErrMsg)
   type(UA_ParameterType),  intent(inout) :: p
   type(UA_InitInputType),  intent(in   ) :: InitInp
   type(AFI_ParameterType), intent(in   ) :: AFInfo(:)   !< one entry per distinct airfoil polar
   integer(IntKi),          intent(in   ) :: AFIndx(:,:) !< AFIndx(node,blade) -> index into AFInfo
   type(UA_MiscVarType),    intent(inout) :: m
   integer(IntKi),          intent(  out) :: ErrStat
   character(*),            intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'UADll_Init'

   procedure(ua_getinfo_f), pointer :: fGetInfo
   procedure(ua_init_f),    pointer :: fInit
   type(UA_DllInfo_C)                          :: info
   type(UA_DllInitInput_C)                     :: ini
   type(UA_DllPolar_C),  allocatable, target   :: dll_polars(:)
   real(C_DOUBLE),       allocatable, target   :: chordBuf(:)
   integer(C_INT32_T),   allocatable, target   :: polarIdBuf(:)
   character(kind=C_CHAR), allocatable, target :: paramBuf(:)
   type(DllRealVec),     allocatable, target   :: alphaVecs(:), clVecs(:), cdVecs(:), cmVecs(:)
   character(kind=C_CHAR) :: cmsg(MSGLEN)
   integer(C_INT32_T)     :: rc
   integer(IntKi)         :: nElem, nPolars, i, j, iElem, k, n_alpha, iChar
   integer(IntKi)         :: ErrStat2
   character(ErrMsgLen)   :: ErrMsg2
   character(1024)        :: paramStr

   ErrStat = ErrID_None
   ErrMsg  = ''

   nElem   = InitInp%numBlades * InitInp%nNodesPerBlade
   nPolars = size(AFInfo)

   !--------------------------------------------------------------
   ! 1) getinfo: verify ABI version + capability bits
   !--------------------------------------------------------------
   info%abi_version = UA_DLL_ABI_VERSION
   info%struct_size = int(storage_size(info)/8, C_INT32_T)
   call C_F_PROCPOINTER(p%UA_DLL%ProcAddr(1), fGetInfo)
   cmsg(1) = C_NULL_CHAR
   rc = fGetInfo(info, cmsg, int(MSGLEN, C_INT32_T))
   call SetFromDllRet(rc, cmsg, RoutineName//'(getinfo)', ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return

   if (info%abi_version /= UA_DLL_ABI_VERSION) then
      call SetErrStat(ErrID_Fatal, RoutineName//': UA DLL ABI version mismatch (DLL reports '// &
                       trim(Num2LStr(int(info%abi_version,IntKi)))//', expected '// &
                       trim(Num2LStr(int(UA_DLL_ABI_VERSION,IntKi)))//').', ErrStat, ErrMsg, RoutineName)
      return
   end if
   if (.not. BTEST(info%caps, 0)) then
      call SetErrStat(ErrID_Warn, RoutineName//': UA DLL "'//trim(p%UA_DLL%FileName)// &
                       '" does not report the pack/unpack capability bit. OpenFAST calls '// &
                       'ua_dll_pack/ua_dll_unpack unconditionally regardless of this bit; '// &
                       'restart correctness for this DLL depends entirely on the fidelity of '// &
                       'its pack/unpack implementation, which this bit does not guarantee.', &
                       ErrStat, ErrMsg, RoutineName)
   end if

   if (InitInp%dt > 0.0_DbKi) then
      if (info%dt_min > 0.0_C_DOUBLE .and. real(InitInp%dt,C_DOUBLE) < info%dt_min) then
         call SetErrStat(ErrID_Fatal, RoutineName//': requested time step ('// &
                          trim(Num2LStr(InitInp%dt))//' s) is smaller than the UA DLL''s '// &
                          'minimum supported dt ('//trim(Num2LStr(real(info%dt_min,DbKi)))//' s).', &
                          ErrStat, ErrMsg, RoutineName)
         return
      end if
      if (info%dt_max > 0.0_C_DOUBLE .and. real(InitInp%dt,C_DOUBLE) > info%dt_max) then
         call SetErrStat(ErrID_Fatal, RoutineName//': requested time step ('// &
                          trim(Num2LStr(InitInp%dt))//' s) is larger than the UA DLL''s '// &
                          'maximum supported dt ('//trim(Num2LStr(real(info%dt_max,DbKi)))//' s).', &
                          ErrStat, ErrMsg, RoutineName)
         return
      end if
   end if

   !--------------------------------------------------------------
   ! 2) marshal one polar per AFInfo entry (first table only)
   !--------------------------------------------------------------
   allocate(dll_polars(max(nPolars,1)))
   allocate(alphaVecs(nPolars), clVecs(nPolars), cdVecs(nPolars), cmVecs(nPolars))
   do k = 1, nPolars
      if (AFInfo(k)%NumTabs > 1) then
         call SetErrStat(ErrID_Fatal, RoutineName//': airfoil index '//trim(Num2LStr(k))// &
                          ' has '//trim(Num2LStr(AFInfo(k)%NumTabs))//' tables, but UA_Mod=9 '// &
                          'currently marshals only Table(1) to the UA DLL; multi-Re/UserProp '// &
                          'airfoil tables are not supported for UA_Mod=9.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      associate (tab => AFInfo(k)%Table(1))
         n_alpha = tab%NumAlf
         allocate(alphaVecs(k)%v(n_alpha))
         allocate(clVecs(k)%v(n_alpha))
         allocate(cdVecs(k)%v(n_alpha))
         alphaVecs(k)%v = real(tab%Alpha(1:n_alpha), C_DOUBLE)
         clVecs(k)%v    = real(tab%Coefs(1:n_alpha, AFInfo(k)%ColCl), C_DOUBLE)
         cdVecs(k)%v    = real(tab%Coefs(1:n_alpha, AFInfo(k)%ColCd), C_DOUBLE)

         dll_polars(k)%abi_version = UA_DLL_ABI_VERSION
         dll_polars(k)%struct_size = int(storage_size(dll_polars(k))/8, C_INT32_T)
         dll_polars(k)%n_alpha     = int(n_alpha, C_INT32_T)
         dll_polars(k)%pad         = 0_C_INT32_T
         dll_polars(k)%alpha       = C_LOC(alphaVecs(k)%v)
         dll_polars(k)%Cl          = C_LOC(clVecs(k)%v)
         dll_polars(k)%Cd          = C_LOC(cdVecs(k)%v)
         if (AFInfo(k)%ColCm > 0) then
            allocate(cmVecs(k)%v(n_alpha))
            cmVecs(k)%v      = real(tab%Coefs(1:n_alpha, AFInfo(k)%ColCm), C_DOUBLE)
            dll_polars(k)%Cm = C_LOC(cmVecs(k)%v)
         else
            dll_polars(k)%Cm = C_NULL_PTR
         end if
         dll_polars(k)%alpha0   = real(tab%UA_BL%alpha0,   C_DOUBLE)
         dll_polars(k)%Cl_alpha = real(tab%UA_BL%C_lalpha, C_DOUBLE)
         dll_polars(k)%Re       = real(tab%Re,             C_DOUBLE)
         dll_polars(k)%UserProp = real(tab%UserProp,       C_DOUBLE)
      end associate
   end do

   !--------------------------------------------------------------
   ! 3) marshal per-element chord + 0-based polar id
   !    (elem = iB*n_nodes_per_blade + iN, 0-based, per ua_dll_api.h)
   !--------------------------------------------------------------
   allocate(chordBuf(nElem), polarIdBuf(nElem))
   do j = 1, InitInp%numBlades
      do i = 1, InitInp%nNodesPerBlade
         iElem = (j-1)*InitInp%nNodesPerBlade + i
         chordBuf(iElem)   = real(InitInp%c(i,j), C_DOUBLE)
         polarIdBuf(iElem) = int(AFIndx(i,j) - 1, C_INT32_T)
      end do
   end do

   !--------------------------------------------------------------
   ! 4) param string (null-terminated; persists only through this call)
   !--------------------------------------------------------------
   paramStr = InitInp%UA_DLL_ParamFile
   allocate(paramBuf(len_trim(paramStr)+1))
   do iChar = 1, len_trim(paramStr)
      paramBuf(iChar) = paramStr(iChar:iChar)
   end do
   paramBuf(len_trim(paramStr)+1) = C_NULL_CHAR

   !--------------------------------------------------------------
   ! 5) assemble UA_DllInitInput_C and call ua_dll_init
   !--------------------------------------------------------------
   ini%abi_version = UA_DLL_ABI_VERSION
   ini%struct_size = int(storage_size(ini)/8, C_INT32_T)
   ini%dt          = real(InitInp%dt,         C_DOUBLE)
   ini%a_s         = real(InitInp%a_s,        C_DOUBLE)
   ini%d_34_to_ac  = real(InitInp%d_34_to_ac, C_DOUBLE)
   ini%n_blades          = int(InitInp%numBlades,      C_INT32_T)
   ini%n_nodes_per_blade = int(InitInp%nNodesPerBlade, C_INT32_T)
   ini%chord    = C_LOC(chordBuf)
   ini%polar_id = C_LOC(polarIdBuf)
   ini%n_polars = int(nPolars, C_INT32_T)
   ini%pad      = 0_C_INT32_T
   if (nPolars > 0) then
      ini%polars = C_LOC(dll_polars)
   else
      ini%polars = C_NULL_PTR
   end if
   ini%param_str = C_LOC(paramBuf)
   ini%param_len = int(len_trim(paramStr), C_INT32_T)
   ini%pad2      = 0_C_INT32_T

   call C_F_PROCPOINTER(p%UA_DLL%ProcAddr(2), fInit)
   cmsg(1) = C_NULL_CHAR
   rc = fInit(ini, m%UA_DLL_ctx, cmsg, int(MSGLEN, C_INT32_T))
   call SetFromDllRet(rc, cmsg, RoutineName//'(init)', ErrStat2, ErrMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   if (ErrStat >= AbortErrLev) return

end subroutine UADll_Init

!> Advances all element states from t to t+dt in a single batched call.
subroutine UADll_UpdateStates(p, m, t, step, u_t, u_tp1, ErrStat, ErrMsg)
   type(UA_ParameterType), intent(in   ) :: p
   type(UA_MiscVarType),   intent(inout) :: m
   real(DbKi),             intent(in   ) :: t
   integer(IntKi),         intent(in   ) :: step
   type(UA_InputType),     intent(in   ) :: u_t(:)
   type(UA_InputType),     intent(in   ) :: u_tp1(:)
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'UADll_UpdateStates'
   procedure(ua_update_f), pointer :: f
   type(UA_DllElemInput_C), allocatable :: dll_u_t(:), dll_u_tp1(:)
   character(kind=C_CHAR) :: cmsg(MSGLEN)
   integer(C_INT32_T) :: rc
   integer(IntKi) :: nElem, iElem

   ErrStat = ErrID_None
   ErrMsg  = ''

   nElem = size(u_t)
   if (size(u_tp1) /= nElem) then
      call SetErrStat(ErrID_Fatal, RoutineName//': u_t and u_tp1 must be the same size.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   allocate(dll_u_t(nElem), dll_u_tp1(nElem))
   do iElem = 1, nElem
      dll_u_t(iElem)%U        = real(u_t(iElem)%U,        C_DOUBLE)
      dll_u_t(iElem)%alpha34  = real(u_t(iElem)%alpha,     C_DOUBLE)
      dll_u_t(iElem)%Re       = real(u_t(iElem)%Re,        C_DOUBLE)
      dll_u_t(iElem)%UserProp = real(u_t(iElem)%UserProp,  C_DOUBLE)
      dll_u_t(iElem)%v_ac_x   = real(u_t(iElem)%v_ac(1),   C_DOUBLE)
      dll_u_t(iElem)%v_ac_y   = real(u_t(iElem)%v_ac(2),   C_DOUBLE)
      dll_u_t(iElem)%omega    = real(u_t(iElem)%omega,     C_DOUBLE)

      dll_u_tp1(iElem)%U        = real(u_tp1(iElem)%U,        C_DOUBLE)
      dll_u_tp1(iElem)%alpha34  = real(u_tp1(iElem)%alpha,     C_DOUBLE)
      dll_u_tp1(iElem)%Re       = real(u_tp1(iElem)%Re,        C_DOUBLE)
      dll_u_tp1(iElem)%UserProp = real(u_tp1(iElem)%UserProp,  C_DOUBLE)
      dll_u_tp1(iElem)%v_ac_x   = real(u_tp1(iElem)%v_ac(1),   C_DOUBLE)
      dll_u_tp1(iElem)%v_ac_y   = real(u_tp1(iElem)%v_ac(2),   C_DOUBLE)
      dll_u_tp1(iElem)%omega    = real(u_tp1(iElem)%omega,     C_DOUBLE)
   end do

   call C_F_PROCPOINTER(p%UA_DLL%ProcAddr(3), f)
   cmsg(1) = C_NULL_CHAR
   rc = f(m%UA_DLL_ctx, real(t,C_DOUBLE), int(step,C_INT64_T), dll_u_t, dll_u_tp1, &
          int(nElem,C_INT32_T), cmsg, int(MSGLEN,C_INT32_T))
   call SetFromDllRet(rc, cmsg, RoutineName, ErrStat, ErrMsg)

end subroutine UADll_UpdateStates

!> Outputs at time t for the current DLL-internal state. Must not modify ctx state.
subroutine UADll_CalcOutput(p, m, t, u, y, ErrStat, ErrMsg)
   type(UA_ParameterType), intent(in   ) :: p
   type(UA_MiscVarType),   intent(inout) :: m
   real(DbKi),             intent(in   ) :: t
   type(UA_InputType),     intent(in   ) :: u(:)
   type(UA_OutputType),    intent(inout) :: y(:)
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'UADll_CalcOutput'
   procedure(ua_output_f), pointer :: f
   type(UA_DllElemInput_C),  allocatable :: dll_u(:)
   type(UA_DllElemOutput_C), allocatable :: dll_y(:)
   character(kind=C_CHAR) :: cmsg(MSGLEN)
   integer(C_INT32_T) :: rc
   integer(IntKi) :: nElem, iElem

   ErrStat = ErrID_None
   ErrMsg  = ''

   nElem = size(u)
   if (size(y) /= nElem) then
      call SetErrStat(ErrID_Fatal, RoutineName//': u and y must be the same size.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   allocate(dll_u(nElem), dll_y(nElem))
   do iElem = 1, nElem
      dll_u(iElem)%U        = real(u(iElem)%U,        C_DOUBLE)
      dll_u(iElem)%alpha34  = real(u(iElem)%alpha,     C_DOUBLE)
      dll_u(iElem)%Re       = real(u(iElem)%Re,        C_DOUBLE)
      dll_u(iElem)%UserProp = real(u(iElem)%UserProp,  C_DOUBLE)
      dll_u(iElem)%v_ac_x   = real(u(iElem)%v_ac(1),   C_DOUBLE)
      dll_u(iElem)%v_ac_y   = real(u(iElem)%v_ac(2),   C_DOUBLE)
      dll_u(iElem)%omega    = real(u(iElem)%omega,     C_DOUBLE)
   end do

   call C_F_PROCPOINTER(p%UA_DLL%ProcAddr(4), f)
   cmsg(1) = C_NULL_CHAR
   rc = f(m%UA_DLL_ctx, real(t,C_DOUBLE), dll_u, int(nElem,C_INT32_T), dll_y, cmsg, int(MSGLEN,C_INT32_T))
   call SetFromDllRet(rc, cmsg, RoutineName, ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return

   do iElem = 1, nElem
      y(iElem)%Cn = real(dll_y(iElem)%Cn, ReKi)
      y(iElem)%Cc = real(dll_y(iElem)%Cc, ReKi)
      y(iElem)%Cl = real(dll_y(iElem)%Cl, ReKi)
      y(iElem)%Cd = real(dll_y(iElem)%Cd, ReKi)
      y(iElem)%Cm = real(dll_y(iElem)%Cm, ReKi)
   end do

end subroutine UADll_CalcOutput

!> Serializes DLL state into xd%UA_DLL_blob using the mandated two-call
!! protocol: first call with buf=NULL sizes the blob, second call writes it.
subroutine UADll_Pack(p, m, xd, ErrStat, ErrMsg)
   type(UA_ParameterType),     intent(in   ) :: p
   type(UA_MiscVarType),       intent(inout) :: m
   type(UA_DiscreteStateType), intent(inout), target :: xd
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'UADll_Pack'
   procedure(ua_pack_f), pointer :: f
   character(kind=C_CHAR) :: cmsg(MSGLEN)
   integer(C_INT32_T) :: rc
   integer(C_INT64_T) :: nBytes
   integer(IntKi)       :: ErrStat2
   character(ErrMsgLen) :: ErrMsg2

   ErrStat = ErrID_None
   ErrMsg  = ''

   call C_F_PROCPOINTER(p%UA_DLL%ProcAddr(5), f)

   ! call 1: size query (buf = NULL)
   nBytes = 0_C_INT64_T
   cmsg(1) = C_NULL_CHAR
   rc = f(m%UA_DLL_ctx, C_NULL_PTR, nBytes, cmsg, int(MSGLEN,C_INT32_T))
   call SetFromDllRet(rc, cmsg, RoutineName//'(size)', ErrStat, ErrMsg)
   if (ErrStat >= AbortErrLev) return

   if (allocated(xd%UA_DLL_blob)) deallocate(xd%UA_DLL_blob)
   call AllocAry(xd%UA_DLL_blob, int(nBytes,IntKi), 'xd%UA_DLL_blob', ErrStat2, ErrMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   if (ErrStat >= AbortErrLev) return

   ! call 2: write into the now-allocated blob
   if (nBytes > 0_C_INT64_T) then
      cmsg(1) = C_NULL_CHAR
      rc = f(m%UA_DLL_ctx, C_LOC(xd%UA_DLL_blob(1)), nBytes, cmsg, int(MSGLEN,C_INT32_T))
      call SetFromDllRet(rc, cmsg, RoutineName//'(write)', ErrStat2, ErrMsg2)
      call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   end if

end subroutine UADll_Pack

!> Restores DLL state from xd%UA_DLL_blob. m%UA_DLL_ctx must already hold a
!! context created by a prior UADll_Init call on the restarted run.
subroutine UADll_Unpack(p, m, xd, ErrStat, ErrMsg)
   type(UA_ParameterType),     intent(in   ) :: p
   type(UA_MiscVarType),       intent(inout) :: m
   type(UA_DiscreteStateType), intent(in   ), target :: xd
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'UADll_Unpack'
   procedure(ua_unpack_f), pointer :: f
   character(kind=C_CHAR) :: cmsg(MSGLEN)
   integer(C_INT32_T) :: rc
   integer(C_INT64_T) :: nBytes

   ErrStat = ErrID_None
   ErrMsg  = ''

   call C_F_PROCPOINTER(p%UA_DLL%ProcAddr(6), f)

   cmsg(1) = C_NULL_CHAR
   if (allocated(xd%UA_DLL_blob) .and. size(xd%UA_DLL_blob) > 0) then
      nBytes = int(size(xd%UA_DLL_blob), C_INT64_T)
      rc = f(m%UA_DLL_ctx, C_LOC(xd%UA_DLL_blob(1)), nBytes, cmsg, int(MSGLEN,C_INT32_T))
   else
      rc = f(m%UA_DLL_ctx, C_NULL_PTR, 0_C_INT64_T, cmsg, int(MSGLEN,C_INT32_T))
   end if
   call SetFromDllRet(rc, cmsg, RoutineName, ErrStat, ErrMsg)

end subroutine UADll_Unpack

!> Tears down the DLL-side context (if any) and unloads the library.
subroutine UADll_End(p, m, ErrStat, ErrMsg)
   type(UA_ParameterType), intent(inout) :: p
   type(UA_MiscVarType),   intent(inout) :: m
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'UADll_End'
   procedure(ua_end_f), pointer :: f
   character(kind=C_CHAR) :: cmsg(MSGLEN)
   integer(C_INT32_T) :: rc
   integer(IntKi)       :: ErrStat2
   character(ErrMsgLen) :: ErrMsg2

   ErrStat = ErrID_None
   ErrMsg  = ''

   if (C_ASSOCIATED(m%UA_DLL_ctx)) then
      call C_F_PROCPOINTER(p%UA_DLL%ProcAddr(7), f)
      cmsg(1) = C_NULL_CHAR
      rc = f(m%UA_DLL_ctx, cmsg, int(MSGLEN,C_INT32_T))
      call SetFromDllRet(rc, cmsg, RoutineName, ErrStat, ErrMsg)
      m%UA_DLL_ctx = C_NULL_PTR
   end if

   call FreeDynamicLib(p%UA_DLL, ErrStat2, ErrMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

end subroutine UADll_End

end module UA_Dll
