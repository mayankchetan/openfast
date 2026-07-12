!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of InflowWind.
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
!> Reader for the YAML form of the InflowWind *driver* input file. Fills the same
!! IfWDriver_Flags/IfWDriver_Settings outputs as ReadDvrIptFile
!! (InflowWind_Driver_Subs.f90:700-1088, the text path), key-for-key, so everything
!! downstream (RetrieveArgs' command-line overrides, the driver's own time-marching
!! loop) is shared between the two formats. Unlike SeaState's driver (the first
!! class-B driver), IfWDriver_Flags/IfWDriver_Settings are proper types declared in
!! InflowWind_Driver_Types.f90, so this parser takes them as ordinary dummy arguments.
!!
!! Schema: sections mirror the text file's banners --
!!   general:              Echo
!!   driver_setup:         IfWIptFileName
!!   file_conversion:      WrHAWC, WrBladed, WrVTK, WrUniform
!!   interpolation_test:   NumTimeSteps, TStart, DT, Summary, SummaryFile, BoxExceedAllow
!!   points_file:          PointsFile, PointsFileName, CalcAccel
!!   gridded_data_output:  WindGrid, GridCtrCoord, GridDelta, GridN
!!   vtk_output:           NOutWindXY, OutWindZ
!!
!! NumTimeSteps and DT accept the literal scalar "default"/"DEFAULT" exactly like the
!! text format (a bare/quoted "DEFAULT" scalar selects NumTimeStepsDefault/DTDefault,
!! mirroring ReadDvrIptFile's NumTimeStepsChr/DTChr + Conv2UC + internal-READ handling:
!! fetched as a plain string here so the same Conv2UC/READ logic can run unchanged).
!!
!! IfWIptFileName (driver_setup) and PointsFileName (points_file) are externally
!! referenced files and stay path-valued (second-order rule); both are resolved
!! relative to the driver YAML file's own directory, exactly as the text path resolves
!! them relative to PriPath.
!!
!! gridded_data_output's GridCtrCoord/GridDelta/GridN are only required when WindGrid
!! is true (mirroring the text path's IF (DvrFlags%WindGrid) branch, ELSE branch skips
!! them as comments); the same XRange/YRange/ZRange/Dx/Dy/Dz derivation and range-check
!! logic (ReadDvrIptFile:906-1023) runs identically here for both branches.
!!
!! vtk_output's OutWindZ is only required (and read) when NOutWindXY > 0, exactly as
!! the text path (ReadDvrIptFile:1037-1043).
!!
!! The #ifdef UNUSED_INPUTFILE_LINES FFT section (ReadDvrIptFile:858-872) is dead code
!! in the text path (never compiled) and is not exposed here either. The commented-out
!! future-development NOutWindXZ/NOutWindYZ block (:1045-1062) is likewise never read
!! by either format.
module InflowWind_Driver_Yaml

   use NWTC_Library
   use YamlInput
   use InflowWind_Driver_Types

   implicit none
   private

   public :: IfWDvr_ParseYamlFile

contains

!> Load and parse a YAML-format InflowWind driver input file, filling the same
!! DvrFlags/DvrSettings fields the text path (ReadDvrIptFile) does.
subroutine IfWDvr_ParseYamlFile(DvrFileName, DvrFlags, DvrSettings, ProgInfo, ErrStat, ErrMsg)
   character(*),              intent(in   ) :: DvrFileName   !< the .yaml driver input file
   type(IfWDriver_Flags),     intent(inout) :: DvrFlags
   type(IfWDriver_Settings),  intent(inout) :: DvrSettings
   type(ProgDesc),            intent(in   ) :: ProgInfo
   integer(IntKi),            intent(  out) :: ErrStat
   character(*),              intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'IfWDvr_ParseYamlFile'
   type(YamlDoc)            :: Doc
   logical                  :: EchoFileContents
   character(1024)          :: PriPath
   integer(IntKi)           :: UnEc
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1

   call WrScr( 'Opening InflowWind Driver input file:  '//trim(DvrFileName) )
   call GetPath( DvrFileName, PriPath )   ! Input files will be relative to the path where the primary input file is located.

   call Yaml_LoadFile(DvrFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'general:Echo', EchoFileContents, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (EchoFileContents) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included), which supersedes the text path's line-by-line echo
      call OpenEcho(UnEc, trim(DvrFileName)//'.ech', TmpErrStat, TmpErrMsg, ProgInfo)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for InflowWind Driver input file: '//trim(DvrFileName)
      call Yaml_LoadFile(DvrFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   call ParseYamlDoc(Doc, PriPath, DvrFlags, DvrSettings, TmpErrStat, TmpErrMsg)
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

end subroutine IfWDvr_ParseYamlFile

!> Fill DvrFlags/DvrSettings from a parsed document. Split from the file wrapper so a
!! future FileInfoType-based entry point (if ever needed) could share it.
subroutine ParseYamlDoc(Doc, PriPath, DvrFlags, DvrSettings, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   character(*),                intent(in   ) :: PriPath
   type(IfWDriver_Flags),      intent(inout) :: DvrFlags
   type(IfWDriver_Settings),   intent(inout) :: DvrSettings
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'IfWDvr_ParseYamlDoc'
   character(1024)            :: InputChr
   integer(IntKi), allocatable:: TmpIntAry(:)
   real(ReKi), allocatable    :: TmpRealAry(:)
   real(ReKi)                 :: GridCtrCoord(3)
   real(ReKi)                 :: TmpRealAr3(3)
   integer(IntKi)             :: ios
   integer(IntKi)             :: iSec
   logical                    :: SecFound
   integer(IntKi)             :: TmpErrStat
   character(ErrMsgLen)       :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   !----------------------------------------------------------------------------------
   ! driver_setup (required) -- IfWIptFileName is an externally-referenced file
   ! (second-order rule: stays path-valued)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'driver_setup:IfWIptFileName', DvrSettings%IfWIptFileName, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   DvrFlags%IfWIptFile = .true.
   if ( PathIsRelative( DvrSettings%IfWIptFileName ) ) DvrSettings%IfWIptFileName = trim(PriPath)//trim(DvrSettings%IfWIptFileName)

   !----------------------------------------------------------------------------------
   ! file_conversion (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'file_conversion:WrHAWC', DvrFlags%WrHAWC, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'file_conversion:WrBladed', DvrFlags%WrBladed, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'file_conversion:WrVTK', DvrFlags%WrVTK, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'file_conversion:WrUniform', DvrFlags%WrUniform, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! interpolation_test (required) -- NumTimeSteps and DT accept the literal
   ! "default"/"DEFAULT", exactly like the text path. Default='DEFAULT' is passed so
   ! YamlGet's own "default" keyword handling (which is otherwise fatal without a
   ! Default=) hands the literal string back unchanged, letting the Conv2UC/internal-
   ! READ logic below (mirroring ReadDvrIptFile) decide.
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'interpolation_test:NumTimeSteps', InputChr, TmpErrStat, TmpErrMsg, Default='DEFAULT')
   if (Failed()) return
   call Conv2UC( InputChr )
   if ( trim(InputChr) == 'DEFAULT' ) then
      DvrFlags%NumTimeSteps        = .true.
      DvrFlags%NumTimeStepsDefault = .true.
   else
      read (InputChr,*,iostat=ios)  DvrSettings%NumTimeSteps
      if ( ios /= 0 ) then
         call CheckIOS ( ios, '', 'NumTimeSteps', NumType, TmpErrStat, TmpErrMsg )
         if (Failed()) return
      else
         DvrFlags%NumTimeSteps        = .true.
         DvrFlags%NumTimeStepsDefault = .false.
      end if
   end if

   call YamlGet(Doc, 'interpolation_test:TStart', DvrSettings%TStart, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   DvrFlags%TStart = .true.

   call YamlGet(Doc, 'interpolation_test:DT', InputChr, TmpErrStat, TmpErrMsg, Default='DEFAULT')
   if (Failed()) return
   call Conv2UC( InputChr )
   if ( trim(InputChr) == 'DEFAULT' ) then
      DvrFlags%DT        = .true.
      DvrFlags%DTDefault = .true.
   else
      read (InputChr,*,iostat=ios)  DvrSettings%DT
      if ( ios /= 0 ) then
         call CheckIOS ( ios, '', 'DT', NumType, TmpErrStat, TmpErrMsg )
         if (Failed()) return
      else
         DvrFlags%DT        = .true.
         DvrFlags%DTDefault = .false.
      end if
   end if

   call YamlGet(Doc, 'interpolation_test:Summary', DvrFlags%Summary, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'interpolation_test:SummaryFile', DvrFlags%SummaryFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'interpolation_test:BoxExceedAllow', DvrFlags%BoxExceedAllowF, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! points_file (required) -- PointsFileName is an externally-referenced file
   ! (second-order rule: stays path-valued)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'points_file:PointsFile', DvrFlags%PointsFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'points_file:PointsFileName', DvrSettings%PointsFileName, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if ( PathIsRelative( DvrSettings%PointsFileName ) ) DvrSettings%PointsFileName = trim(PriPath)//trim(DvrSettings%PointsFileName)
   call YamlGet(Doc, 'points_file:CalcAccel', DvrFlags%OutputAccel, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! gridded_data_output (required) -- GridCtrCoord/GridDelta/GridN are only required
   ! when WindGrid is true, exactly as the text path's IF (DvrFlags%WindGrid) branch
   ! (ReadDvrIptFile:896-1028); the ELSE branch there skips those lines as comments and
   ! sets the same defaults reproduced here.
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'gridded_data_output:WindGrid', DvrFlags%WindGrid, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   if ( DvrFlags%WindGrid ) then

      call YamlGetNode(Doc, 'gridded_data_output', iSec, TmpErrStat, TmpErrMsg)
      if (Failed()) return

      call YamlGet(Doc, 'GridCtrCoord', TmpRealAry, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      if ( size(TmpRealAry) /= 3 ) then
         call SetErrStat(ErrID_Fatal, 'GridCtrCoord must have exactly 3 entries.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      GridCtrCoord = TmpRealAry

      call YamlGet(Doc, 'GridDelta', TmpRealAry, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      if ( size(TmpRealAry) /= 3 ) then
         call SetErrStat(ErrID_Fatal, 'GridDelta must have exactly 3 entries.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      TmpRealAr3 = TmpRealAry

      call YamlGet(Doc, 'GridN', TmpIntAry, TmpErrStat, TmpErrMsg, From=iSec)
      if (Failed()) return
      if ( size(TmpIntAry) /= 3 ) then
         call SetErrStat(ErrID_Fatal, 'GridN must have exactly 3 entries.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      DvrSettings%GridN = TmpIntAry

         ! Save the DY and DZ values
      DvrSettings%GridDelta(1:3) =  abs(TmpRealAr3(1:3))
      DvrFlags%Dx                =  .true.               ! read in value for the X direction gridding
      DvrFlags%Dy                =  .true.               ! read in value for the Y direction gridding
      DvrFlags%Dz                =  .true.               ! read in value for the Z direction gridding

         ! Save the GridNY and GridNZ values
      DvrFlags%XRange        =  .true.                ! read in value for the X direction gridding
      DvrFlags%YRange        =  .true.                ! read in value for the Y direction gridding
      DvrFlags%ZRange        =  .true.                ! read in value for the Z direction gridding


         ! Check that valid values of Dx, Dy, and Dz were read in.
         ! Check GridDx
      if ( EqualRealNos(DvrSettings%GridDelta(1), 0.0_ReKi) ) then
         DvrFlags%Dx                =  .false.
         DvrFlags%XRange            =  .false.
         DvrSettings%GridDelta(1)   = 0.0_ReKi
         call SetErrStat(ErrID_Warn,' Grid spacing in X direction is 0.  Ignoring.',ErrStat,ErrMsg,RoutineName)
      end if

         ! Check GridDy
      if ( EqualRealNos(DvrSettings%GridDelta(2), 0.0_ReKi) ) then
         DvrFlags%Dy                =  .false.
         DvrFlags%YRange            =  .false.
         DvrSettings%GridDelta(2)   = 0.0_ReKi
         call SetErrStat(ErrID_Warn,' Grid spacing in Y direction is 0.  Ignoring.',ErrStat,ErrMsg,RoutineName)
      end if

         ! Check GridDz
      if ( EqualRealNos(DvrSettings%GridDelta(3), 0.0_ReKi) ) then
         DvrFlags%Dz                =  .false.
         DvrFlags%ZRange            =  .false.
         DvrSettings%GridDelta(3)   = 0.0_ReKi
         call SetErrStat(ErrID_Warn,' Grid spacing in Z direction is 0.  Ignoring.',ErrStat,ErrMsg,RoutineName)
      end if


         ! Now need to set the XRange, YRange, and ZRange values based on what we read in
         ! For XRange, check that we have an actual value for the number of points
      if ( (DvrSettings%GridN(1) <= 0) .or. (.not. DvrFlags%XRange) ) then
         DvrSettings%XRange   =  GridCtrCoord(1)
         DvrFlags%Dx          =  .false.
         DvrFlags%XRange      =  .false.

         if ( DvrSettings%GridN(1) < 0 )  then
            call SetErrStat(ErrID_Warn,' Negative number for number of grid points along X direction.  Ignoring.',ErrStat,ErrMsg, RoutineName)
         else
            call SetErrStat(ErrID_Warn,' No points along X direction.  Ignoring.',ErrStat,ErrMsg, RoutineName)
         end if

         DvrSettings%GridN(1) =  1_IntKi              ! Set to 1 for easier indexing.

      else
            ! Set the XRange values
         DvrSettings%XRange(1)   =  GridCtrCoord(1) - (real(DvrSettings%GridN(1) - 1_IntKi ) / 2.0_ReKi ) * DvrSettings%GridDelta(1)
         DvrSettings%XRange(2)   =  GridCtrCoord(1) + (real(DvrSettings%GridN(1) - 1_IntKi ) / 2.0_ReKi ) * DvrSettings%GridDelta(1)
         DvrFlags%XRange         =  .true.
      end if


         ! For YRange, check that we have an actual value for the number of points
      if ( (DvrSettings%GridN(2) <= 0) .or. (.not. DvrFlags%YRange) ) then
         DvrSettings%YRange   =  GridCtrCoord(2)
         DvrFlags%Dy          =  .false.
         DvrFlags%YRange      =  .false.

         if ( DvrSettings%GridN(2) < 0 )  then
            call SetErrStat(ErrID_Warn,' Negative number for number of grid points along Y direction.  Ignoring.',ErrStat,ErrMsg, RoutineName)
         else
            call SetErrStat(ErrID_Warn,' No points along Y direction.  Ignoring.',ErrStat,ErrMsg, RoutineName)
         end if

         DvrSettings%GridN(2) =  1_IntKi              ! Set to 1 for easier indexing.

      else
            ! Set the YRange values
         DvrSettings%YRange(1)   =  GridCtrCoord(2) - (real(DvrSettings%GridN(2) - 1_IntKi ) / 2.0_ReKi ) * DvrSettings%GridDelta(2)
         DvrSettings%YRange(2)   =  GridCtrCoord(2) + (real(DvrSettings%GridN(2) - 1_IntKi ) / 2.0_ReKi ) * DvrSettings%GridDelta(2)
         DvrFlags%YRange         =  .true.
      end if

         ! For ZRange, check that we have an actual value for the number of points, set to ctr point if negative or zero.
      if ( (DvrSettings%GridN(3) <= 0) .or. (.not. DvrFlags%ZRange) ) then
         DvrSettings%ZRange   =  abs(GridCtrCoord(3))       ! shouldn't have a negative value anyhow
         DvrFlags%Dz          =  .false.
         DvrFlags%ZRange      =  .false.

         if ( DvrSettings%GridN(3) < 0 )  then
            call SetErrStat(ErrID_Warn,' Negative number for number of grid points along Z direction.  Ignoring.',ErrStat,ErrMsg, RoutineName)
         else
            call SetErrStat(ErrID_Warn,' No points along Z direction.  Ignoring.',ErrStat,ErrMsg, RoutineName)
         end if

         DvrSettings%GridN(3) =  1_IntKi              ! Set to 1 for easier indexing.

      else
            ! Set the ZRange values
         DvrSettings%ZRange(1)   =  GridCtrCoord(3) - (real(DvrSettings%GridN(3) - 1_IntKi ) / 2.0_ReKi ) * DvrSettings%GridDelta(3)
         DvrSettings%ZRange(2)   =  GridCtrCoord(3) + (real(DvrSettings%GridN(3) - 1_IntKi ) / 2.0_ReKi ) * DvrSettings%GridDelta(3)
         DvrFlags%ZRange         =  .true.
      end if

   else ! not reading the gridded data section

      DvrSettings%GridDelta = 0.0_ReKi
      DvrFlags%Dx                =  .false.
      DvrFlags%Dy                =  .false.
      DvrFlags%Dz                =  .false.

      DvrSettings%GridN = 0.0_ReKi
      DvrFlags%XRange            =  .false.
      DvrFlags%YRange            =  .false.
      DvrFlags%ZRange            =  .false.

   end if

   !----------------------------------------------------------------------------------
   ! vtk_output (required) -- OutWindZ is only required (and read) when NOutWindXY > 0,
   ! exactly as the text path (ReadDvrIptFile:1037-1043)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'vtk_output:NOutWindXY', DvrSettings%NOutWindXY, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (DvrSettings%NOutWindXY > 0_IntKi) then
      call AllocAry( DvrSettings%OutWindZ, DvrSettings%NOutWindXY, "Z coordinates of XY planes for output", TmpErrStat, TmpErrMsg )
      if (Failed()) return
      call YamlGet(Doc, 'vtk_output:OutWindZ', TmpRealAry, TmpErrStat, TmpErrMsg)
      if (Failed()) return
      if ( size(TmpRealAry) /= DvrSettings%NOutWindXY ) then
         call SetErrStat(ErrID_Fatal, 'OutWindZ must have exactly NOutWindXY ('// &
            trim(Num2LStr(DvrSettings%NOutWindXY))//') entries.', ErrStat, ErrMsg, RoutineName)
         return
      end if
      DvrSettings%OutWindZ = TmpRealAry
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseYamlDoc

end module InflowWind_Driver_Yaml
