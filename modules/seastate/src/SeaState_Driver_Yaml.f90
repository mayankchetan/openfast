!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of SeaState.
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
!> Reader for the YAML form of the SeaState *driver* input file. Fills the same fields
!! as ReadDriverInputFile's sequential ReadVar/ReadCom body (SeaState_DriverCode.f90,
!! this is a class-B/sequential-reader driver -- there is no FileInfoType/ParseVar
!! passed-file channel, and hence no derived type for the driver's own init-input
!! shared with a module -- it is declared inline inside PROGRAM SeaStateDriver), so
!! everything downstream (the driver's own time-marching loop) is shared between the
!! two formats.
!!
!! Because SeaSt_Drvr_InitInput is a program-local (host-associated) type rather than
!! one declared in a module, this reader cannot take it as a dummy argument (an
!! external/module procedure cannot type-match a program-internal derived type). It
!! instead takes each InitInp field as its own dummy argument; the funnel at the top of
!! ReadDriverInputFile (an internal subroutine of PROGRAM SeaStateDriver, so it *does*
!! have host access to the type) passes InitInp%<field> as actual arguments and copies
!! nothing else -- the same InitInp fields end up filled either way.
!!
!! Schema: sections mirror the text file's banners --
!!   general:                  Echo
!!   environmental_conditions: Gravity, WtrDens, WtrDpth, MSL2SWL
!!   seastate:                 SeaStateInputFile, OutRootName, WrWvKinMod, NSteps, TimeInterval
!!   wave_elevation_series:    WaveElevVis
!! Table-free (~10 scalars); no key accepts the "default" keyword (all are plain
!! ReadVar reads in the text path, unlike the SeaState *primary* file's WtrDens/WtrDpth/
!! MSL2SWL, which do). SeaStateInputFile is an externally-referenced file and stays
!! path-valued (second-order rule) -- never inlined here. WrWvKinMod is range-checked
!! 0-2, exactly as the text path range-checks it right after the read. WaveElevVisNx/
!! WaveElevVisNy are declared in SeaSt_Drvr_InitInput but are never read by the text
!! path either (commented-out FIXME in ReadDriverInputFile) -- not read here either, so
!! both formats leave them at their default-initialized value.
module SeaState_Driver_Yaml

   use NWTC_Library
   use YamlInput

   implicit none
   private

   public :: SeaStDvr_ParseYamlFile

contains

!> Load and parse a YAML-format SeaState driver input file, filling each InitInp field
!! passed in as its own dummy argument (see the module header for why).
subroutine SeaStDvr_ParseYamlFile(DvrFileName, Echo, Gravity, WtrDens, WtrDpth, MSL2SWL, &
                                   SeaStateInputFile, OutRootName, WrWvKinMod, NSteps, &
                                   TimeInterval, WaveElevVis, ErrStat, ErrMsg)
   character(*),   intent(in   ) :: DvrFileName        !< the .yaml driver input file
   logical,        intent(  out) :: Echo
   real(ReKi),     intent(  out) :: Gravity
   real(ReKi),     intent(  out) :: WtrDens
   real(ReKi),     intent(  out) :: WtrDpth
   real(ReKi),     intent(  out) :: MSL2SWL
   character(*),   intent(  out) :: SeaStateInputFile
   character(*),   intent(  out) :: OutRootName
   integer,        intent(  out) :: WrWvKinMod
   integer,        intent(  out) :: NSteps
   real(DbKi),     intent(  out) :: TimeInterval
   logical,        intent(  out) :: WaveElevVis
   integer,        intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SeaStDvr_ParseYamlFile'
   type(YamlDoc)            :: Doc
   character(1024)          :: RootName
   integer(IntKi)           :: UnEc
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1
   Echo    = .false.   ! initialize for error handling (cleanup path)

   call GetRoot(DvrFileName, RootName)

   call Yaml_LoadFile(DvrFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'general:Echo', Echo, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (Echo) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included), which supersedes the text path's line-by-line echo
      call OpenEcho(UnEc, trim(RootName)//'.ech', TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for SeaState driver input file: '//trim(DvrFileName)
      call Yaml_LoadFile(DvrFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   !----------------------------------------------------------------------------------
   ! environmental_conditions (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'environmental_conditions:Gravity', Gravity, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:WtrDens', WtrDens, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:WtrDpth', WtrDpth, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:MSL2SWL', MSL2SWL, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! seastate (required) -- SeaStateInputFile is an externally-referenced file
   ! (second-order rule: stays path-valued)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'seastate:SeaStateInputFile', SeaStateInputFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'seastate:OutRootName', OutRootName, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'seastate:WrWvKinMod', WrWvKinMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (WrWvKinMod < 0 .or. WrWvKinMod > 2) then
      call SetErrStat(ErrID_Fatal, ' WrWvKinMod parameter must be 0, 1, or 2', ErrStat, ErrMsg, RoutineName)
      call Cleanup()
      return
   end if
   call YamlGet(Doc, 'seastate:NSteps', NSteps, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'seastate:TimeInterval', TimeInterval, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! wave_elevation_series (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'wave_elevation_series:WaveElevVis', WaveElevVis, TmpErrStat, TmpErrMsg)
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

end subroutine SeaStDvr_ParseYamlFile

end module SeaState_Driver_Yaml
