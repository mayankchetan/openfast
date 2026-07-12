!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of AeroDyn.
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
!> Reader for the YAML form of the AeroDyn *driver* input file. Fills the same `dvr`
!! (Dvr_SimData) fields as Dvr_ReadInputFile's ProcessComFile/ParseVar/ParseAry body
!! (AeroDyn_Driver_Subs.f90:963-1361, a class-A FileInfoType/ParseVar reader), key-for-key,
!! so everything downstream (ValidateInputs, the driver's own per-case time-marching loop)
!! is shared between the two formats.
!!
!! This module cannot 'use AeroDyn_Driver_Subs' (the funnel lives inside that module's
!! Dvr_ReadInputFile and must 'use AeroDyn_Driver_Yaml' to call this reader -- a mutual
!! 'use' between the two modules is not legal Fortran), so the handful of motion/analysis
!! -type id parameters and the myNaN sentinel are mirrored here as local parameters rather
!! than imported; their numeric values must stay in lockstep with AeroDyn_Driver_Subs.f90.
!!
!! Schema: sections mirror the text file's banners --
!!   general:                   Echo
!!   configuration:              MHK, analysisType, tMax, dt, AeroFile
!!   environmental_conditions:  FldDens, KinVisc, SpdSound, Patm, Pvap, WtrDpth
!!   inflow:                    compInflow, InflowFile, HWindSpeed, RefHt, PLExp -- the
!!                              last three are always present in the text file and always
!!                              read here too (mirrors the text path unconditionally
!!                              consuming those lines even when CompInflow==1, at which
!!                              point they are simply never used downstream)
!!   seastate:                  CompSeaSt, SeaStFile
!!   turbines:                  a list of block mappings, one per turbine (its length is
!!                              numTurbines -- counts derive from list length, never a
!!                              separate key the parser cross-checks). Each entry:
!!                                projMod (optional, default -1), basicHAWTFormat
!!                                basic geometry (basicHAWTFormat==true):
!!                                  baseOriginInit[3], numBlades, hubRad, hubHt, overhang,
!!                                  shftTilt (deg), precone (deg), twr2Shft
!!                                advanced geometry (basicHAWTFormat==false):
!!                                  baseOriginInit[3], baseOrientationInit[3] (deg),
!!                                  hasTower, HAWTprojection, twrOrigin_t[3], nacOrigin_t[3],
!!                                  hubOrigin_n[3], hubOrientation_n[3] (deg), numBlades,
!!                                  blades: a list (length numBlades) of block mappings
!!                                  merging the text format's separate geometry-loop and
!!                                  motion-loop passes into one row: origin_h[3],
!!                                  orientation_h[3] (deg), hubRad_bl, bldPitch (deg,
!!                                  only when bldMotionType==0), bldMotionFileName (only
!!                                  when bldMotionType==1)
!!                                base motion (always present, regardless of analysisType,
!!                                  exactly mirroring the text path's unconditional read
!!                                  followed by a conditional override):
!!                                  baseMotionType, degreeOfFreedom, amplitude, frequency
!!                                  (Hz), baseMotionFileName
!!                                RNA motion, basic format: nacYaw (deg), rotSpeed (rpm),
!!                                  bldPitch (deg)
!!                                RNA motion, advanced format: nacMotionType, nacYaw (deg,
!!                                  only when nacMotionType==0), nacMotionFileName (only
!!                                  when nacMotionType==1), rotMotionType, rotSpeed (rpm,
!!                                  only when rotMotionType==0), rotMotionFileName (only
!!                                  when rotMotionType==1), bldMotionType
!!   time_dependent_analysis:   TimeAnalysisFileName -- present only when
!!                              analysisType==2 (idAnalysisTimeD), mirroring the text
!!                              path's IF/ELSE (Dvr_ReadInputFile:1276-1288)
!!   combined_case_analysis:    cases -- a list of block-mapping rows via the converter's
!!                              _row_map helper (10 columns: HWindSpeed, PLExp, rotSpeed,
!!                              bldPitch, nacYaw, dT, tMax, DOF, amplitude, frequency),
!!                              present only when analysisType==3 (idAnalysisCombi);
!!                              its length is numCases (counts derive from list length).
!!                              Fatal, exactly as the text path, when analysisType==3 and
!!                              numTurbines>1.
!!   outputs:                   outFmt, outFileFmt, WrVTK, WrVTK_Type, VTKHubRad,
!!                              VTKNacDim[6]
!!
!! AeroFile/InflowFile/SeaStFile/baseMotionFileName/nacMotionFileName/rotMotionFileName/
!! bldMotionFileName/TimeAnalysisFileName are all externally-referenced files (second-order
!! rule: stay path-valued), resolved relative to the driver YAML file's own directory here,
!! exactly as the text path resolves them relative to PriPath. Each variable-motion file's
!! own referenced time-series table is read here with the exact same ReadDelimFile call the
!! text path makes, guaranteeing bit-identical numeric parsing; the same RPM2RPS/D2R
!! conversions are applied to the combined-case-table-adjacent TimeAnalysisFileName table.
module AeroDyn_Driver_Yaml

   use NWTC_Library
   use YamlInput
   use AeroDyn_Driver_Types

   implicit none
   private

   public :: AD_Dvr_ParseYamlFile

   ! Mirrors of AeroDyn_Driver_Subs' private parameters (see module header for why they
   ! cannot simply be imported via 'use AeroDyn_Driver_Subs').
   integer(IntKi), parameter :: idBaseMotionFixed   = 0
   integer(IntKi), parameter :: idBaseMotionSine    = 1
   integer(IntKi), parameter :: idBaseMotionGeneral = 2
   integer(IntKi), parameter :: idHubMotionConstant = 0
   integer(IntKi), parameter :: idHubMotionVariable = 1
   integer(IntKi), parameter :: idNacMotionConstant = 0
   integer(IntKi), parameter :: idNacMotionVariable = 1
   integer(IntKi), parameter :: idBldMotionConstant = 0
   integer(IntKi), parameter :: idBldMotionVariable = 1
   integer(IntKi), parameter :: idAnalysisRegular   = 1
   integer(IntKi), parameter :: idAnalysisTimeD     = 2
   integer(IntKi), parameter :: idAnalysisCombi     = 3
   real(ReKi),     parameter :: myNaN = -99.9_ReKi

contains

!> Load and parse a YAML-format AeroDyn driver input file, filling `dvr` directly (the
!! same output Dvr_ReadInputFile produces from the text path).
subroutine AD_Dvr_ParseYamlFile(DvrFileName, dvr, ErrStat, ErrMsg)
   character(*),               intent(in   ) :: DvrFileName   !< the .yaml driver input file
   type(Dvr_SimData), target,  intent(inout) :: dvr
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'AD_Dvr_ParseYamlFile'
   type(YamlDoc)            :: Doc
   logical                  :: EchoFileContents
   character(1024)          :: PriPath
   integer(IntKi)           :: UnEc
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1

   call GetPath(DvrFileName, PriPath)
   call GetRoot(DvrFileName, dvr%root)

   call Yaml_LoadFile(DvrFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'general:Echo', EchoFileContents, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (EchoFileContents) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included) -- same convention as AeroDyn_Yaml.f90's primary-file reader,
      ! which supersedes the text path's line-by-line echo.
      call OpenEcho(UnEc, trim(dvr%root)//'.ech', TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for AeroDyn driver input file: '//trim(DvrFileName)
      call Yaml_LoadFile(DvrFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   call ParseYamlDoc(Doc, PriPath, dvr, TmpErrStat, TmpErrMsg)
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

end subroutine AD_Dvr_ParseYamlFile

!> Fill `dvr` from a parsed document.
subroutine ParseYamlDoc(Doc, PriPath, dvr, ErrStat, ErrMsg)
   type(YamlDoc),               intent(inout) :: Doc
   character(*),                intent(in   ) :: PriPath
   type(Dvr_SimData), target,   intent(inout) :: dvr
   integer(IntKi),               intent(  out) :: ErrStat
   character(*),                 intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'AD_Dvr_ParseYamlDoc'
   real(SiKi)               :: hubRad_ReKi
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   !----------------------------------------------------------------------------------
   ! configuration (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'configuration:MHK', dvr%MHK, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'configuration:analysisType', dvr%analysisType, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'configuration:tMax', dvr%tMax, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'configuration:dt', dvr%dt, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'configuration:AeroFile', dvr%AD_InputFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (PathIsRelative(dvr%AD_InputFile)) dvr%AD_InputFile = trim(PriPath)//trim(dvr%AD_InputFile)

   !----------------------------------------------------------------------------------
   ! environmental_conditions (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'environmental_conditions:FldDens', dvr%FldDens, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:KinVisc', dvr%KinVisc, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:SpdSound', dvr%SpdSound, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:Patm', dvr%Patm, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:Pvap', dvr%Pvap, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:WtrDpth', dvr%WtrDpth, TmpErrStat, TmpErrMsg); if (Failed()) return
   dvr%MSL2SWL = 0.0_ReKi ! pass as zero since not set in AeroDyn driver input file

   !----------------------------------------------------------------------------------
   ! inflow (required) -- the last three fields are always present/read in the text
   ! path regardless of compInflow, so they are always required here too.
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'inflow:compInflow', dvr%IW_InitInp%compInflow, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'inflow:InflowFile', dvr%IW_InitInp%InputFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (PathIsRelative(dvr%IW_InitInp%InputFile)) dvr%IW_InitInp%InputFile = trim(PriPath)//trim(dvr%IW_InitInp%InputFile)
   if (dvr%IW_InitInp%compInflow /= 1) then
      call YamlGet(Doc, 'inflow:HWindSpeed', dvr%IW_InitInp%HWindSpeed, TmpErrStat, TmpErrMsg); if (Failed()) return
      call YamlGet(Doc, 'inflow:RefHt', dvr%IW_InitInp%RefHt, TmpErrStat, TmpErrMsg); if (Failed()) return
      call YamlGet(Doc, 'inflow:PLExp', dvr%IW_InitInp%PLExp, TmpErrStat, TmpErrMsg); if (Failed()) return
   else
      dvr%IW_InitInp%PLexp      = myNaN
      dvr%IW_InitInp%RefHt      = myNaN
      dvr%IW_InitInp%HWindSpeed = myNaN
   end if

   !----------------------------------------------------------------------------------
   ! seastate (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'seastate:CompSeaSt', dvr%SS_InitInp%CompSeaSt, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'seastate:SeaStFile', dvr%SS_InitInp%InputFile, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (PathIsRelative(dvr%SS_InitInp%InputFile)) dvr%SS_InitInp%InputFile = trim(PriPath)//trim(dvr%SS_InitInp%InputFile)

   !----------------------------------------------------------------------------------
   ! turbines (required; length = numTurbines)
   !----------------------------------------------------------------------------------
   call ParseTurbines(Doc, PriPath, dvr, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! time_dependent_analysis -- present only when analysisType == idAnalysisTimeD
   !----------------------------------------------------------------------------------
   if (dvr%analysisType == idAnalysisTimeD) then
      call ParseTimeSeries(Doc, PriPath, dvr, TmpErrStat, TmpErrMsg)
      if (Failed()) return
   end if

   !----------------------------------------------------------------------------------
   ! combined_case_analysis -- present only when analysisType == idAnalysisCombi
   !----------------------------------------------------------------------------------
   if (dvr%analysisType == idAnalysisCombi) then
      if (dvr%numTurbines > 1) then
         call SetErrStat(ErrID_Fatal, 'Combined case analyses only possible with zero or one turbine with `basicHAWT` format', &
            ErrStat, ErrMsg, RoutineName)
         return
      end if
      call ParseCombinedCases(Doc, dvr, TmpErrStat, TmpErrMsg)
      if (Failed()) return
   else
      dvr%numCases = 1 ! Only one case
      if (allocated(dvr%Cases)) deallocate(dvr%Cases)
   end if

   !----------------------------------------------------------------------------------
   ! outputs (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'outputs:outFmt', dvr%out%outFmt, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'outputs:outFileFmt', dvr%out%fileFmt, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'outputs:WrVTK', dvr%out%WrVTK, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'outputs:WrVTK_Type', dvr%out%WrVTK_Type, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'outputs:VTKHubRad', hubRad_ReKi, TmpErrStat, TmpErrMsg); if (Failed()) return
   call ReadVec3(Doc, 'outputs:VTKNacDim', dvr%out%VTKNacDim(1:3), TmpErrStat, TmpErrMsg); if (Failed()) return
   call ReadVec3(Doc, 'outputs:VTKNacDim', dvr%out%VTKNacDim(4:6), TmpErrStat, TmpErrMsg, Offset=3); if (Failed()) return
   dvr%out%VTKHubRad = real(hubRad_ReKi, SiKi)
   dvr%out%delim = ' ' ! TAB

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseYamlDoc

!> Read a YAML flow-sequence [x,y,z] (optionally a 3-element slice of a longer list, for
!! the 6-element VTKNacDim, via Offset) into a fixed real(ReKi) 3-vector. `From`, when
!! present, scopes the lookup to a specific list-row/mapping node (mirrors YamlGet's own
!! From convention).
subroutine ReadVec3(Doc, Path, Vec3, ErrStat, ErrMsg, Offset, From)
   type(YamlDoc),           intent(inout) :: Doc
   character(*),            intent(in   ) :: Path
   real(ReKi),               intent(  out) :: Vec3(3)
   integer(IntKi),           intent(  out) :: ErrStat
   character(*),             intent(  out) :: ErrMsg
   integer(IntKi), optional, intent(in   ) :: Offset
   integer(IntKi), optional, intent(in   ) :: From

   character(*), parameter :: RoutineName = 'ReadVec3'
   real(ReKi), allocatable  :: TmpVec(:)
   integer(IntKi)           :: iOff
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   iOff = 0
   if (present(Offset)) iOff = Offset

   if (present(From)) then
      call YamlGet(Doc, Path, TmpVec, TmpErrStat, TmpErrMsg, From=From)
   else
      call YamlGet(Doc, Path, TmpVec, TmpErrStat, TmpErrMsg)
   end if
   if (Failed()) return
   if (size(TmpVec) < iOff + 3) then
      call SetErrStat(ErrID_Fatal, '"'//Path//'" must have at least '//trim(Num2LStr(iOff+3))//' entries; found '// &
         trim(Num2LStr(size(TmpVec)))//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if
   Vec3 = TmpVec(iOff+1:iOff+3)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadVec3

!> turbines -- a list of block mappings, one per turbine; its length is numTurbines
!! (counts derive from list length).
subroutine ParseTurbines(Doc, PriPath, dvr, ErrStat, ErrMsg)
   type(YamlDoc),              intent(inout) :: Doc
   character(*),               intent(in   ) :: PriPath
   type(Dvr_SimData), target,  intent(inout) :: dvr
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ParseTurbines'
   integer(IntKi)           :: iSeq, iRow, iWT
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'turbines', iSeq, TmpErrStat, TmpErrMsg); if (Failed()) return

   dvr%numTurbines = int(Yaml_NumChildren(Doc, iSeq))
   allocate(dvr%WT(dvr%numTurbines), stat=TmpErrStat)
   if (TmpErrStat /= 0) then
      call SetErrStat(ErrID_Fatal, 'Error allocating dvr%WT.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   do iWT = 1, dvr%numTurbines
      iRow = Yaml_Child(Doc, iSeq, iWT)
      call ParseOneTurbine(Doc, iRow, PriPath, dvr%MHK, dvr%WtrDpth, dvr%analysisType, dvr%WT(iWT), TmpErrStat, TmpErrMsg)
      if (Failed()) return
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseTurbines

!> One `turbines` list entry -- fills a single WTData, mirroring the text path's
!! per-turbine loop body (Dvr_ReadInputFile:1064-1271) key-for-key.
subroutine ParseOneTurbine(Doc, iRow, PriPath, MHK, WtrDpth, analysisType, wt, ErrStat, ErrMsg)
   type(YamlDoc),           intent(inout) :: Doc
   integer(IntKi),           intent(in   ) :: iRow
   character(*),             intent(in   ) :: PriPath
   integer(IntKi),           intent(in   ) :: MHK
   real(ReKi),                intent(in   ) :: WtrDpth
   integer(IntKi),            intent(in   ) :: analysisType
   type(WTData),               intent(inout) :: wt
   integer(IntKi),              intent(  out) :: ErrStat
   character(*),                 intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ParseOneTurbine'
   integer(IntKi)           :: iB, iBladesSeq, iBladeRow
   logical                  :: Found
   real(ReKi)                :: hubRad, hubHt, overhang, shftTilt, precone, twr2Shft
   real(ReKi)                :: nacYaw, bldPitch, rotSpeed
   integer(IntKi)             :: bldMotionType
   integer(IntKi)              :: TmpErrStat
   character(ErrMsgLen)         :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   wt%hub%azimuth  = myNan
   wt%hub%rotSpeed = myNaN
   wt%nac%yaw      = myNaN

   call YamlGet(Doc, 'projMod', wt%projMod, TmpErrStat, TmpErrMsg, Default=-1_IntKi, From=iRow); if (Failed()) return
   call YamlGet(Doc, 'basicHAWTFormat', wt%basicHAWTFormat, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return

   if (wt%basicHAWTFormat) then
      !--------------------------------------------------------------------------------
      ! Basic geometry
      !--------------------------------------------------------------------------------
      call ReadVec3(Doc, 'baseOriginInit', wt%originInit, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      if (MHK == MHK_FixedBottom) wt%originInit(3) = wt%originInit(3) - WtrDpth

      call YamlGet(Doc, 'numBlades', wt%numBlades, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'hubRad', hubRad, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'hubHt', hubHt, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'overhang', overhang, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'shftTilt', shftTilt, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'precone', precone, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'twr2Shft', twr2Shft, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return

      shftTilt = -shftTilt * Pi / 180._ReKi ! deg 2 rad, NOTE: OpenFAST convention sign wrong around y
      precone  = precone * Pi / 180._ReKi   ! deg 2 rad

      wt%orientationInit(1:3) = 0.0_ReKi
      wt%hasTower             = .True.
      wt%HAWTprojection       = .True.
      wt%twr%origin_t         = 0.0_ReKi
      wt%nac%origin_t         = (/ 0.0_ReKi, 0.0_ReKi, hubHt - twr2Shft + overhang * sin(shftTilt) /)
      wt%hub%origin_n         = (/ overhang * cos(shftTilt), 0.0_ReKi, -overhang * sin(shftTilt) + twr2shft /)
      wt%hub%orientation_n    = (/ 0.0_ReKi, shftTilt, 0.0_ReKi /)

      allocate(wt%bld(wt%numBlades))
      do iB = 1, wt%numBlades
         wt%bld(iB)%pitch            = myNaN
         wt%bld(iB)%origin_h(1:3)    = 0.0_ReKi
         wt%bld(iB)%orientation_h(1) = (iB-1)*(2._ReKi*Pi)/wt%numBlades
         wt%bld(iB)%orientation_h(2) = precone
         wt%bld(iB)%orientation_h(3) = 0.0_ReKi
         wt%bld(iB)%hubRad_bl        = hubRad
      end do
   else
      !--------------------------------------------------------------------------------
      ! Advanced geometry
      !--------------------------------------------------------------------------------
      call ReadVec3(Doc, 'baseOriginInit', wt%originInit, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      if (MHK == MHK_FixedBottom) wt%originInit(3) = wt%originInit(3) - WtrDpth
      call ReadVec3(Doc, 'baseOrientationInit', wt%orientationInit, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'hasTower', wt%hasTower, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'HAWTprojection', wt%HAWTprojection, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call ReadVec3(Doc, 'twrOrigin_t', wt%twr%origin_t, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call ReadVec3(Doc, 'nacOrigin_t', wt%nac%origin_t, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call ReadVec3(Doc, 'hubOrigin_n', wt%hub%origin_n, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call ReadVec3(Doc, 'hubOrientation_n', wt%hub%orientation_n, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      wt%hub%orientation_n = wt%hub%orientation_n * D2R
      wt%orientationInit    = wt%orientationInit * D2R

      call YamlGet(Doc, 'numBlades', wt%numBlades, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      allocate(wt%bld(wt%numBlades), stat=TmpErrStat)
      if (TmpErrStat /= 0) then
         call SetErrStat(ErrID_Fatal, 'Error allocating wt%bld', ErrStat, ErrMsg, RoutineName)
         return
      end if

      if (wt%numBlades > 0) then
         call YamlGetNode(Doc, 'blades', iBladesSeq, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         if (int(Yaml_NumChildren(Doc, iBladesSeq)) /= wt%numBlades) then
            call SetErrStat(ErrID_Fatal, '"blades" must have exactly '//trim(Num2LStr(wt%numBlades))// &
               ' entries (== numBlades); found '//trim(Num2LStr(int(Yaml_NumChildren(Doc, iBladesSeq))))//'.', &
               ErrStat, ErrMsg, RoutineName)
            return
         end if
         do iB = 1, wt%numBlades
            wt%bld(iB)%pitch = myNaN
            iBladeRow = Yaml_Child(Doc, iBladesSeq, iB)
            call ReadVec3(Doc, 'origin_h', wt%bld(iB)%origin_h, TmpErrStat, TmpErrMsg, From=iBladeRow); if (Failed()) return
            call ReadVec3(Doc, 'orientation_h', wt%bld(iB)%orientation_h, TmpErrStat, TmpErrMsg, From=iBladeRow); if (Failed()) return
            wt%bld(iB)%orientation_h = wt%bld(iB)%orientation_h * Pi/180_ReKi
            call YamlGet(Doc, 'hubRad_bl', wt%bld(iB)%hubRad_bl, TmpErrStat, TmpErrMsg, From=iBladeRow); if (Failed()) return
         end do
      end if
   end if ! basic/advanced geometry

   !-----------------------------------------------------------------------------------
   ! Base motion (common to basic/advanced) -- always present/read, even when the values
   ! are about to be overridden below for a non-Regular analysisType (mirrors the text
   ! path's unconditional read followed by a conditional override).
   !-----------------------------------------------------------------------------------
   call YamlGet(Doc, 'baseMotionType', wt%motionType, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
   call YamlGet(Doc, 'degreeOfFreedom', wt%degreeOfFreedom, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
   call YamlGet(Doc, 'amplitude', wt%amplitude, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
   call YamlGet(Doc, 'frequency', wt%frequency, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
   call YamlGet(Doc, 'baseMotionFileName', wt%motionFileName, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
   wt%frequency = wt%frequency * 2 * pi ! Hz to rad/s

   if (analysisType == idAnalysisRegular) then
      if (wt%motionType == idBaseMotionGeneral) then
         if (PathIsRelative(wt%motionFileName)) wt%motionFileName = trim(PriPath)//trim(wt%motionFileName)
         call ReadDelimFile(wt%motionFileName, 19, wt%motion, TmpErrStat, TmpErrMsg, priPath=PriPath); if (Failed()) return
         wt%iMotion = 1
         ! text path additionally WrScr's a warning here when the motion file's last
         ! timestamp is earlier than tMax; purely informational (no effect on computed
         ! outputs), so intentionally not reproduced here.
      end if
   else
      wt%amplitude       = myNaN
      wt%frequency        = myNaN
      wt%motionType        = idBaseMotionFixed
      wt%degreeOfFreedom   = 0
   end if

   !-----------------------------------------------------------------------------------
   ! RNA Motion
   !-----------------------------------------------------------------------------------
   if (wt%basicHAWTFormat) then
      call YamlGet(Doc, 'nacYaw', nacyaw, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'rotSpeed', rotSpeed, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'bldPitch', bldPitch, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      if (analysisType /= idAnalysisRegular) then
         nacYaw   = myNaN
         rotSpeed = myNaN
         bldPitch = myNaN
      end if

      call SetSimpleMotionLocal(wt, rotSpeed, bldPitch, nacYaw, wt%degreeOfFreedom, wt%amplitude, wt%frequency)
   else
      if (wt%numBlades > 0) then
         call YamlGet(Doc, 'nacMotionType', wt%nac%motionType, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'nacYaw', wt%nac%yaw, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'nacMotionFileName', wt%nac%motionFileName, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         wt%nac%yaw = wt%nac%yaw * Pi/180_ReKi
         if (analysisType == idAnalysisRegular) then
            if (wt%nac%motionType == idNacMotionVariable) then
               if (PathIsRelative(wt%nac%motionFilename)) wt%nac%motionFilename = trim(PriPath)//trim(wt%nac%motionFilename)
               call ReadDelimFile(wt%nac%motionFilename, 4, wt%nac%motion, TmpErrStat, TmpErrMsg, priPath=PriPath); if (Failed()) return
               wt%nac%iMotion = 1
            end if
         else
            wt%nac%motionType = idNacMotionConstant
            wt%nac%yaw        = myNaN
         end if

         call YamlGet(Doc, 'rotMotionType', wt%hub%motionType, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'rotSpeed', wt%hub%rotSpeed, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         call YamlGet(Doc, 'rotMotionFileName', wt%hub%motionFileName, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         wt%hub%rotSpeed = wt%hub%rotSpeed * Pi/30_ReKi
         if (analysisType == idAnalysisRegular) then
            if (wt%hub%motionType == idHubMotionVariable) then
               if (PathIsRelative(wt%hub%motionFilename)) wt%hub%motionFilename = trim(PriPath)//trim(wt%hub%motionFilename)
               call ReadDelimFile(wt%hub%motionFilename, 4, wt%hub%motion, TmpErrStat, TmpErrMsg, priPath=PriPath); if (Failed()) return
               wt%hub%iMotion = 1
            end if
         else
            wt%hub%motionType = idHubMotionConstant
            wt%hub%rotSpeed   = myNaN
         end if

         call YamlGet(Doc, 'bldMotionType', bldMotionType, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
         do iB = 1, wt%numBlades
            wt%bld(iB)%motionType = bldMotionType
         end do
         if (analysisType == idAnalysisRegular) then
            call YamlGetNode(Doc, 'blades', iBladesSeq, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
            do iB = 1, wt%numBlades
               iBladeRow = Yaml_Child(Doc, iBladesSeq, iB)
               if (bldMotionType == idBldMotionConstant) then
                  call YamlGet(Doc, 'bldPitch', wt%bld(iB)%pitch, TmpErrStat, TmpErrMsg, From=iBladeRow); if (Failed()) return
                  wt%bld(iB)%pitch = wt%bld(iB)%pitch * Pi/180_ReKi
               else
                  call YamlGet(Doc, 'bldMotionFileName', wt%bld(iB)%motionFileName, TmpErrStat, TmpErrMsg, From=iBladeRow); if (Failed()) return
                  if (PathIsRelative(wt%bld(iB)%motionFilename)) wt%bld(iB)%motionFilename = trim(PriPath)//trim(wt%bld(iB)%motionFilename)
                  call ReadDelimFile(wt%bld(iB)%motionFilename, 4, wt%bld(iB)%motion, TmpErrStat, TmpErrMsg, priPath=PriPath); if (Failed()) return
                  wt%bld(iB)%iMotion = 1
               end if
            end do
         else
            do iB = 1, size(wt%bld)
               wt%bld(iB)%motionType = idBldMotionConstant
               wt%bld(iB)%pitch      = myNan
            end do
         end if
      end if ! numBlades>0
   end if ! basic/advanced rotor definition

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseOneTurbine

!> Local mirror of AeroDyn_Driver_Subs' setSimpleMotion (see module header for why it
!! cannot simply be imported). Fills the basic-format RNA motion fields.
subroutine SetSimpleMotionLocal(wt, rotSpeed, bldPitch, nacYaw, DOF, amplitude, frequency)
   type(WTData),   intent(inout) :: wt
   real(ReKi),     intent(in   ) :: rotSpeed  ! rpm
   real(ReKi),     intent(in   ) :: bldPitch  ! deg
   real(ReKi),     intent(in   ) :: nacYaw    ! deg
   integer(IntKi), intent(in   ) :: DOF
   real(ReKi),     intent(in   ) :: amplitude
   real(ReKi),     intent(in   ) :: frequency

   integer(IntKi) :: i
   wt%degreeofFreedom   = DOF
   wt%amplitude         = amplitude
   wt%frequency         = frequency
   wt%nac%motionType    = idNacMotionConstant
   wt%nac%yaw           = nacYaw * Pi / 180._ReKi
   wt%hub%motionType    = idHubMotionConstant
   wt%hub%rotSpeed      = rotSpeed * Pi / 30._ReKi
   if (allocated(wt%bld)) then
      do i = 1, size(wt%bld)
         wt%bld(i)%motionType = idBldMotionConstant
         wt%bld(i)%pitch      = bldPitch * Pi / 180._ReKi
      end do
   end if
end subroutine SetSimpleMotionLocal

!> time_dependent_analysis:TimeAnalysisFileName -- second-order rule: the referenced
!! file stays path-valued and is read with the exact same ReadDelimFile call the text
!! path makes (Dvr_ReadInputFile:1277-1285), including the RPM2RPS/D2R conversions.
subroutine ParseTimeSeries(Doc, PriPath, dvr, ErrStat, ErrMsg)
   type(YamlDoc),               intent(inout) :: Doc
   character(*),                intent(in   ) :: PriPath
   type(Dvr_SimData), target,   intent(inout) :: dvr
   integer(IntKi),               intent(  out) :: ErrStat
   character(*),                  intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ParseTimeSeries'
   character(1024)          :: TimeAnalysisFileName
   integer(IntKi)            :: TmpErrStat
   character(ErrMsgLen)       :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'time_dependent_analysis:TimeAnalysisFileName', TimeAnalysisFileName, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if (PathIsRelative(TimeAnalysisFileName)) TimeAnalysisFileName = trim(PriPath)//trim(TimeAnalysisFileName)

   call ReadDelimFile(TimeAnalysisFileName, 6, dvr%timeSeries, TmpErrStat, TmpErrMsg, priPath=PriPath)
   if (Failed()) return
   dvr%timeSeries(:,4) = real(dvr%timeSeries(:,4)*RPM2RPS, ReKi) ! rad/s
   dvr%timeSeries(:,5) = real(dvr%timeSeries(:,5)*D2R    , ReKi) ! rad
   dvr%timeSeries(:,6) = real(dvr%timeSeries(:,6)*D2R    , ReKi) ! rad
   dvr%iTimeSeries = 1

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseTimeSeries

!> combined_case_analysis:cases -- a list of block-mapping rows (10 columns: HWindSpeed,
!! PLExp, rotSpeed, bldPitch, nacYaw, dT, tMax, DOF, amplitude, frequency), emitted by the
!! converter's _row_map helper. Its length is numCases (counts derive from list length).
subroutine ParseCombinedCases(Doc, dvr, ErrStat, ErrMsg)
   type(YamlDoc),               intent(inout) :: Doc
   type(Dvr_SimData), target,   intent(inout) :: dvr
   integer(IntKi),                intent(  out) :: ErrStat
   character(*),                   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'ParseCombinedCases'
   integer(IntKi)           :: iSeq, iRow, iCase
   integer(IntKi)            :: TmpErrStat
   character(ErrMsgLen)       :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'combined_case_analysis:cases', iSeq, TmpErrStat, TmpErrMsg); if (Failed()) return

   dvr%numCases = int(Yaml_NumChildren(Doc, iSeq))
   if (dvr%numCases <= 0) then
      call SetErrStat(ErrID_Fatal, 'NumCases needs to be >0 for combined analyses', ErrStat, ErrMsg, RoutineName)
      return
   end if
   allocate(dvr%Cases(dvr%numCases))

   do iCase = 1, dvr%numCases
      iRow = Yaml_Child(Doc, iSeq, iCase)
      call YamlGet(Doc, 'HWindSpeed', dvr%Cases(iCase)%HWindSpeed, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'PLExp', dvr%Cases(iCase)%PLExp, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'rotSpeed', dvr%Cases(iCase)%rotSpeed, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'bldPitch', dvr%Cases(iCase)%bldPitch, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'nacYaw', dvr%Cases(iCase)%nacYaw, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'dT', dvr%Cases(iCase)%dT, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'tMax', dvr%Cases(iCase)%tMax, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'DOF', dvr%Cases(iCase)%DOF, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'amplitude', dvr%Cases(iCase)%amplitude, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'frequency', dvr%Cases(iCase)%frequency, TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseCombinedCases

end module AeroDyn_Driver_Yaml
