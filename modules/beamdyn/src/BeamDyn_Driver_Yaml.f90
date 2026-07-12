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
!> Reader for the YAML form of the BeamDyn *driver* input file. Fills the same
!! BD_InitInputType/BD_DriverInternalType outputs (plus the dt_global scalar) as
!! BD_ReadDvrFile's sequential ReadVar/ReadAry body (Driver_Beam_Subs.f90:72-233, a
!! class-B/sequential-reader driver, mirroring SubDyn/SeaState's precedent), so
!! everything downstream (BD_Init, the driver's own time-marching loop) is shared
!! between the two formats.
!!
!! Unlike SubDyn/SeaState's drivers, BD_InitInputType (BeamDyn's own registry type) and
!! BD_DriverInternalType (declared at module scope in BeamDyn_driver_subs, not
!! program-local) are both proper types this reader can take as ordinary dummy
!! arguments -- no parallel-array workaround needed.
!!
!! Schema: sections mirror the text file's banners --
!!   simulation_control:  DynamicSolve, t_initial, t_final, dt
!!   gravity_parameter:   gravity (list of 3: X,Y,Z)
!!   frame_parameter:     GlbPos (list of 3), RootOri (3x3 direction-cosine matrix,
!!                        row-major, each row a flow sequence), GlbRotBladeT0
!!   root_velocity_parameter: RootVel (list of 3: angular velocity X,Y,Z -- these feed
!!                        InitInputData%RootVel(4:6); RootVel(1:3) is a derived
!!                        cross-product, not read in either format)
!!   applied_force:       DistrLoad (list of 6), TipLoad (list of 6)
!!   multi_point_loads:   point_loads (list of row mappings: Eta, Fx, Fy, Fz, Mx, My,
!!                        Mz -- NumPointLoads derives from the list length, never a
!!                        separate key)
!!   primary_input_file:  InputFile
!!   outputs:             WrVTK, VTK_fps
!!
!! The text reader has no Echo option for the driver file (unlike BeamDyn's own primary
!! input file) -- BD_DriverInternalType/BD_InitInputType carry no such field here, so
!! this schema has no general:Echo section either (a true key-for-key match, not an
!! omission).
!!
!! GlbRotBladeT0 selects, exactly as the text path (Driver_Beam_Subs.f90:139-148), which
!! of InitInputData%GlbRot / DvrData%RootRelInit becomes RootOri and which becomes the
!! identity (via NWTC_Library's Eye): both are *outputs* of BD_ReadDvrFile, computed
!! inside the same routine (not left to the driver program), so this parser reproduces
!! that computation rather than leaving it to the funnel call site.
!!
!! InputFile is an externally-referenced file (second-order rule: stays path-valued),
!! resolved relative to the driver YAML file's own directory, exactly as the text path
!! resolves it relative to PriPath (Driver_Beam_Subs.f90:227). The BeamDyn blade file
!! referenced by that primary input file is untouched here (BeamDyn_Yaml.f90's concern).
!!
!! multi_point_loads:point_loads is emitted as block-mapping rows (YamlInput's
!! block-sequence reader rejects one-line flow mappings for named-field table rows);
!! an absent/empty list matches the text path's IOS/=0 backward-compatibility branch
!! (NumPointLoads=1, a single all-zero row) -- this reader always takes the modern,
!! explicit-list path, so there is no non-numeric "line" sniffing to reproduce.
!!
!! WrVTK is range-checked (0..2) exactly as the text path's own inline FIXME check
!! (Driver_Beam_Subs.f90:229-232).
module BeamDyn_Driver_Yaml

   use NWTC_Library
   use YamlInput
   use BeamDyn_driver_subs

   implicit none
   private

   public :: BDDvr_ParseYamlFile

contains

!> Load and parse a YAML-format BeamDyn driver input file, filling InitInputData
!! (BD_InitInputType) and DvrData (BD_DriverInternalType) plus dt, exactly as
!! BD_ReadDvrFile does for the text format.
subroutine BDDvr_ParseYamlFile(DvrFileName, dt, InitInputData, DvrData, ErrStat, ErrMsg)
   character(*),                 intent(in   ) :: DvrFileName   !< the .yaml driver input file
   real(DbKi),                   intent(  out) :: dt
   type(BD_InitInputType),       intent(  out) :: InitInputData
   type(BD_DriverInternalType),  intent(  out) :: DvrData
   integer(IntKi),                intent(  out) :: ErrStat
   character(*),                  intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'BDDvr_ParseYamlFile'
   type(YamlDoc)             :: Doc
   character(1024)           :: PriPath
   real(ReKi), allocatable   :: TmpReAry(:)
   real(R8Ki), allocatable   :: TmpRootOri(:,:)
   integer(IntKi)            :: TmpErrStat
   character(ErrMsgLen)      :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call WrScr( 'Opening BeamDyn Driver input file:  '//trim(DvrFileName) )
   call GetPath( DvrFileName, PriPath )   ! Input files will be relative to the path where the driver input file is located.

   call Yaml_LoadFile(DvrFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! simulation_control (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'simulation_control:DynamicSolve', DvrData%DynamicSolve, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'simulation_control:t_initial', DvrData%t_initial, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'simulation_control:t_final', DvrData%t_final, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'simulation_control:dt', dt, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! gravity_parameter (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'gravity_parameter:gravity', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), 3, 'gravity_parameter:gravity')) return
   InitInputData%gravity = TmpReAry

   !----------------------------------------------------------------------------------
   ! frame_parameter (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'frame_parameter:GlbPos', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), 3, 'frame_parameter:GlbPos')) return
   InitInputData%GlbPos = TmpReAry

   call YamlGet(Doc, 'frame_parameter:RootOri', TmpRootOri, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (size(TmpRootOri,1) /= 3 .or. size(TmpRootOri,2) /= 3) then
      call SetErrStat(ErrID_Fatal, '"frame_parameter:RootOri" must be a 3x3 matrix (one flow-sequence row per line).', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if
   InitInputData%RootOri = TmpRootOri

   call YamlGet(Doc, 'frame_parameter:GlbRotBladeT0', DvrData%GlbRotBladeT0, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! Use the initial blade root orientation as the GlbRot reference orientation for all
   ! calculations? Both InitInputData%GlbRot and DvrData%RootRelInit are outputs of the
   ! text reader too (Driver_Beam_Subs.f90:139-148) -- reproduced here identically.
   if ( DvrData%GlbRotBladeT0 ) then
      InitInputData%GlbRot = InitInputData%RootOri
      call eye( DvrData%RootRelInit, TmpErrStat, TmpErrMsg ); if (Failed()) return
   else
      DvrData%RootRelInit = InitInputData%RootOri
      call eye( InitInputData%GlbRot, TmpErrStat, TmpErrMsg ); if (Failed()) return
   end if

   !----------------------------------------------------------------------------------
   ! root_velocity_parameter (required) -- feeds RootVel(4:6); RootVel(1:3) is derived
   ! (cross product), exactly as the text path (Driver_Beam_Subs.f90:151-156).
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'root_velocity_parameter:RootVel', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), 3, 'root_velocity_parameter:RootVel')) return
   InitInputData%RootVel(4:6) = TmpReAry
   InitInputData%RootVel(1:3) = cross_product(InitInputData%RootVel(4:6), InitInputData%GlbPos(:))

   !----------------------------------------------------------------------------------
   ! applied_force (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'applied_force:DistrLoad', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), 6, 'applied_force:DistrLoad')) return
   DvrData%DistrLoad = TmpReAry

   call YamlGet(Doc, 'applied_force:TipLoad', TmpReAry, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (BadSize(size(TmpReAry), 6, 'applied_force:TipLoad')) return
   DvrData%TipLoad = TmpReAry

   !----------------------------------------------------------------------------------
   ! multi_point_loads (optional; default empty -- a modern-format concession, no
   ! IOS-sniffing backward-compatibility path; NumPointLoads derives from the list
   ! length)
   !----------------------------------------------------------------------------------
   call ParsePointLoads(Doc, DvrData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! primary_input_file (required) -- InputFile is an externally-referenced file
   ! (second-order rule: stays path-valued)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'primary_input_file:InputFile', InitInputData%InputFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if ( PathIsRelative( InitInputData%InputFile ) ) InitInputData%InputFile = trim(PriPath)//trim(InitInputData%InputFile)

   !----------------------------------------------------------------------------------
   ! outputs (required) -- WrVTK range-checked exactly as the text path's own inline
   ! check (Driver_Beam_Subs.f90:229-232).
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'outputs:WrVTK', DvrData%WrVTK, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'outputs:VTK_fps', DvrData%VTK_fps, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   if (DvrData%WrVTK < 0 .or. DvrData%WrVTK > 2) then
      call SetErrStat(ErrID_Fatal, 'WrVTK must be 0=none; 1=init; 2=animation', ErrStat, ErrMsg, RoutineName)
      return
   end if

   ! typo guard: anything never looked up is reported (warning severity)
   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

   logical function BadSize(NGiven, NExpect, Path)
      integer(IntKi), intent(in) :: NGiven
      integer(IntKi), intent(in) :: NExpect
      character(*),   intent(in) :: Path
      BadSize = (NGiven /= NExpect)
      if (BadSize) call SetErrStat(ErrID_Fatal, '"'//Path//'" must list exactly '//trim(Num2LStr(NExpect))// &
         ' value(s); found '//trim(Num2LStr(NGiven))//'.', ErrStat, ErrMsg, RoutineName)
   end function BadSize

end subroutine BDDvr_ParseYamlFile

!> multi_point_loads:point_loads -- an optional list of row mappings {Eta, Fx, Fy, Fz,
!! Mx, My, Mz}, one per constant point load applied along the blade span. Missing
!! section -> zero rows -> NumPointLoads=1 with a single all-zero row, mirroring the
!! text path's IOS/=0 branch (Driver_Beam_Subs.f90:206-210) without the non-numeric
!! "line" sniffing that branch exists to detect (this reader always takes the modern,
!! explicit-list path).
subroutine ParsePointLoads(Doc, DvrData, ErrStat, ErrMsg)
   type(YamlDoc),                intent(inout) :: Doc
   type(BD_DriverInternalType),  intent(inout) :: DvrData
   integer(IntKi),                intent(  out) :: ErrStat
   character(*),                  intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'BDDvr_ParsePointLoads'
   integer(IntKi)           :: iSeq, iRow, i, n
   logical                  :: SecFound
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'multi_point_loads:point_loads', iSeq, TmpErrStat, TmpErrMsg, Found=SecFound)
   if (Failed()) return

   n = 0
   if (SecFound) n = int(Yaml_NumChildren(Doc, iSeq))

   DvrData%NumPointLoads = max(1, n)
   call AllocAry(DvrData%MultiPointLoad, DvrData%NumPointLoads, 7, 'MultiPointLoad', TmpErrStat, TmpErrMsg)
   if (Failed()) return
   DvrData%MultiPointLoad = 0.0_BDKi      ! this must have at least one node, and it will be initialized to 0

   do i = 1, n
      iRow = Yaml_Child(Doc, iSeq, i)
      call YamlGet(Doc, 'Eta', DvrData%MultiPointLoad(i,1), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Fx',  DvrData%MultiPointLoad(i,2), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Fy',  DvrData%MultiPointLoad(i,3), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Fz',  DvrData%MultiPointLoad(i,4), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Mx',  DvrData%MultiPointLoad(i,5), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'My',  DvrData%MultiPointLoad(i,6), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'Mz',  DvrData%MultiPointLoad(i,7), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParsePointLoads

end module BeamDyn_Driver_Yaml
