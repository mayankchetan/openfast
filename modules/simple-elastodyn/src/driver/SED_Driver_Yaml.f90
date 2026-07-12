!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of Simplified ElastoDyn (SED)
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
!> Reader for the YAML form of the Simplified ElastoDyn (SED) driver input file. Fills
!! the same SEDDriver_Flags/SEDDriver_Settings/CaseTime/CaseData outputs as
!! ParseDvrIptFile (SED_Driver_Subs.f90:372-563, the text path), key-for-key, so
!! everything downstream (UpdateSettingsWithCL, the driver's own time-marching loop) is
!! shared between the two formats. Mirrors AeroDisk_Driver_Yaml.f90's shape (the Wave 4
!! driver-parser pattern this file is a direct scale-down of).
!!
!! Schema: sections mirror the text file's banners --
!!   general:               Echo
!!   primary_file:          SEDIptFile, OutRootName
!!   output:                WrVTK
!!   case_analysis:         TStart, DT, NumTimeSteps, table
!! DT and NumTimeSteps accept the literal scalar "default"/"DEFAULT" exactly like the
!! text format (a bare/quoted "DEFAULT" scalar selects DTDefault/NumTimeStepsDefault,
!! mirroring ParseDvrIptFile's InputChr + Conv2UC + internal-READ handling: fetched as
!! a plain string here so the same Conv2UC/READ logic can run unchanged).
!!
!! case_analysis:table carries the combined case time/data series. Two forms:
!!   file: "<path>"   -- second-order rule: every r-test SED driver case sources this
!!                       table from a large "@filename"-included time-series file (e.g.
!!                       Free.csv, HSSBrk.csv; the text path's ProcessComFile/
!!                       FileInfoType inclusion mechanism splices it in transparently);
!!                       such tables are never inlined into YAML. Instead the
!!                       referenced file (same text layout: optional comment header,
!!                       then whitespace-delimited Time/AerTrq/HSSBrTrqC/GenTrq/
!!                       BlPitchCom/Yaw/YawRate rows) is read with the exact same
!!                       ProcessComFile+ParseAry machinery the text driver uses,
!!                       guaranteeing bit-identical numeric parsing. Resolved relative
!!                       to the YAML driver file's own directory.
!!   rows: [...]      -- a YAML list of block mappings (one per case timestep), each
!!                       keyed by column name (Time, AerTrq, HSSBrTrqC, GenTrq,
!!                       BlPitchCom, Yaw, YawRate) -- for a table given inline rather
!!                       than via "@include" (SubDyn/MoorDyn primary-table precedent,
!!                       Wave 3).
!! Exactly one of "file"/"rows" must be present.
!! HSSBrTrqC is forced positive (abs) and BlPitchCom/Yaw/YawRate (deg[/s] -> rad[/s])
!! conversions are applied identically to the text path (SED_Driver_Subs.f90:524-533)
!! regardless of which form is used.
module SED_Driver_Yaml

   use NWTC_Library
   use YamlInput
   use SED_Driver_Types

   implicit none
   private

   public :: SEDDvr_ParseYamlFile

contains

!> Load and parse a YAML-format SED driver input file.
subroutine SEDDvr_ParseYamlFile(DvrFileName, DvrFlags, DvrSettings, ProgInfo, CaseTime, CaseData, ErrStat, ErrMsg)
   character(1024),                   intent(in   ) :: DvrFileName    !< the .yaml driver input file
   type(SEDDriver_Flags),             intent(inout) :: DvrFlags
   type(SEDDriver_Settings),          intent(inout) :: DvrSettings
   type(ProgDesc),                    intent(in   ) :: ProgInfo       !< unused (kept for signature parity with ParseDvrIptFile)
   real(DbKi),          allocatable,  intent(  out) :: CaseTime(:)
   real(ReKi),          allocatable,  intent(  out) :: CaseData(:,:)
   integer(IntKi),                    intent(  out) :: ErrStat
   character(*),                      intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'SEDDvr_ParseYamlFile'
   type(YamlDoc)           :: Doc
   logical                 :: EchoFileContents
   character(1024)         :: RootName
   character(1024)         :: PriPath
   integer(IntKi)          :: UnEc
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1

   call GetRoot(DvrFileName, RootName)
   call GetPath(DvrFileName, PriPath)

   call Yaml_LoadFile(DvrFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'general:Echo', EchoFileContents, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   if (EchoFileContents) then
      ! reload with the echo unit: the echo is a verbatim copy of every physical file
      ! (comments included) -- same convention as SED_Yaml.f90's primary-file reader,
      ! which supersedes the text path's line-by-line echo.
      call OpenEcho(UnEc, trim(RootName)//'.ech', TmpErrStat, TmpErrMsg)
      if (Failed()) return
      write(UnEc, '(A)') 'Echo file for SED driver input file: '//trim(DvrFileName)
      call Yaml_LoadFile(DvrFileName, Doc, TmpErrStat, TmpErrMsg, UnEc=UnEc)
      if (Failed()) return
   end if

   call ParseYamlDoc(Doc, PriPath, DvrFlags, DvrSettings, CaseTime, CaseData, TmpErrStat, TmpErrMsg)
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

end subroutine SEDDvr_ParseYamlFile

!> Fill DvrFlags/DvrSettings/CaseTime/CaseData from a parsed document. Split from the
!! file wrapper so a future FileInfoType-based entry point (if ever needed) could share it.
subroutine ParseYamlDoc(Doc, PriPath, DvrFlags, DvrSettings, CaseTime, CaseData, ErrStat, ErrMsg)
   type(YamlDoc),                     intent(inout) :: Doc
   character(*),                      intent(in   ) :: PriPath
   type(SEDDriver_Flags),             intent(inout) :: DvrFlags
   type(SEDDriver_Settings),          intent(inout) :: DvrSettings
   real(DbKi),          allocatable,  intent(  out) :: CaseTime(:)
   real(ReKi),          allocatable,  intent(  out) :: CaseData(:,:)
   integer(IntKi),                    intent(  out) :: ErrStat
   character(*),                      intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'SEDDvr_ParseYamlDoc'
   character(1024)            :: InputChr
   logical                    :: EchoTmp
   integer(IntKi)             :: ios
   integer(IntKi)             :: TmpErrStat
   character(ErrMsgLen)       :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   ! general:Echo was already consulted (and, if true, acted on) by the caller before
   ! any possible echo-reload; touch it again here so it is marked Used on whichever
   ! Doc instance this routine actually receives (mirrors AeroDisk_Driver_Yaml.f90's
   ! ParseYamlDoc, which re-reads it for the same reason).
   call YamlGet(Doc, 'general:Echo', EchoTmp, TmpErrStat, TmpErrMsg, Default=.false.)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! primary_file (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'primary_file:SEDIptFile', DvrSettings%SEDIptFileName, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   DvrFlags%SEDIptFile = .true.

   call YamlGet(Doc, 'primary_file:OutRootName', DvrSettings%OutRootName, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   DvrFlags%OutRootName = .true.

   !----------------------------------------------------------------------------------
   ! output (required; no DvrFlags entry exists for WrVTK, mirroring ParseDvrIptFile
   ! which likewise never sets a flag for it)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'output:WrVTK', DvrSettings%WrVTK, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! case_analysis (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'case_analysis:TStart', DvrSettings%TStart, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   DvrFlags%TStart = .true.

   ! DT -- accepts the literal "default"/"DEFAULT", exactly like the text path.
   ! Default='DEFAULT' is passed so YamlGet's own "default" keyword handling (which is
   ! otherwise fatal without a Default=) hands the literal string back unchanged,
   ! letting the Conv2UC/internal-READ logic below (mirroring ParseDvrIptFile) decide.
   call YamlGet(Doc, 'case_analysis:DT', InputChr, TmpErrStat, TmpErrMsg, Default='DEFAULT')
   if (Failed()) return
   call Conv2UC(InputChr)
   if (trim(InputChr) == 'DEFAULT') then
      DvrFlags%DT        = .true.
      DvrFlags%DTDefault = .true.
   else
      read(InputChr, *, iostat=ios) DvrSettings%DT
      if (ios /= 0) then
         call CheckIOS(ios, '', 'DT', NumType, TmpErrStat, TmpErrMsg)
         if (Failed()) return
      else
         DvrFlags%DT        = .true.
         DvrFlags%DTDefault = .false.
      end if
   end if

   ! NumTimeSteps -- accepts the literal "default"/"DEFAULT", exactly like the text path
   call YamlGet(Doc, 'case_analysis:NumTimeSteps', InputChr, TmpErrStat, TmpErrMsg, Default='DEFAULT')
   if (Failed()) return
   call Conv2UC(InputChr)
   if (trim(InputChr) == 'DEFAULT') then
      DvrFlags%NumTimeSteps        = .false.
      DvrFlags%NumTimeStepsDefault = .true.
   else
      read(InputChr, *, iostat=ios) DvrSettings%NumTimeSteps
      if (ios /= 0) then
         call CheckIOS(ios, '', 'NumTimeSteps', NumType, TmpErrStat, TmpErrMsg)
         if (Failed()) return
      else
         DvrFlags%NumTimeSteps        = .true.
         DvrFlags%NumTimeStepsDefault = .false.
      end if
   end if

   call ReadCaseTable(Doc, PriPath, CaseTime, CaseData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseYamlDoc

!> Read case_analysis:table -- either a "file:" reference (second-order rule: read with
!! the same ProcessComFile+ParseAry machinery the text driver uses) or inline "rows:"
!! (a list of block mappings, one per timestep). Exactly one must be present.
subroutine ReadCaseTable(Doc, PriPath, CaseTime, CaseData, ErrStat, ErrMsg)
   type(YamlDoc),                    intent(inout) :: Doc
   character(*),                     intent(in   ) :: PriPath
   real(DbKi),         allocatable,  intent(  out) :: CaseTime(:)
   real(ReKi),         allocatable,  intent(  out) :: CaseData(:,:)
   integer(IntKi),                   intent(  out) :: ErrStat
   character(*),                     intent(  out) :: ErrMsg

   character(*), parameter   :: RoutineName = 'ReadCaseTable'
   integer(IntKi)            :: iSec, iSeq, iRow
   integer(IntKi)            :: TabLines, i
   logical                   :: FileFound, RowsFound
   character(1024)           :: CaseDataFile
   type(FileInfoType)        :: CaseFileInfo
   integer(IntKi)            :: CurLine
   real(DbKi)                :: TmpDb7(7)
   real(ReKi)                :: BlPitchComTmp, YawTmp, YawRateTmp
   integer(IntKi)            :: TmpErrStat
   character(ErrMsgLen)      :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGetNode(Doc, 'case_analysis:table', iSec, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'file', CaseDataFile, TmpErrStat, TmpErrMsg, Found=FileFound, From=iSec)
   if (Failed()) return

   if (FileFound) then
      !------------------------------------------------------------------------------
      ! second-order rule: the referenced file keeps its original text layout and is
      ! parsed with the exact same ProcessComFile+ParseAry calls the text driver uses.
      !------------------------------------------------------------------------------
      if (PathIsRelative(CaseDataFile)) CaseDataFile = trim(PriPath)//trim(CaseDataFile)

      call ProcessComFile(CaseDataFile, CaseFileInfo, TmpErrStat, TmpErrMsg)
      if (Failed()) return

      CurLine  = 1
      TabLines = CaseFileInfo%NumLines - CurLine + 1
      call AllocAry(CaseTime,    TabLines, 'CaseTime', TmpErrStat, TmpErrMsg); if (Failed()) return
      call AllocAry(CaseData, 6, TabLines, 'CaseData', TmpErrStat, TmpErrMsg); if (Failed()) return
      do i = 1, TabLines
         call ParseAry(CaseFileInfo, CurLine, 'Coordinates', TmpDb7, 7, TmpErrStat, TmpErrMsg)
         if (Failed()) return
         CaseTime(i)   = TmpDb7(1)
         CaseData(1,i) =     real(TmpDb7(2), ReKi)
         CaseData(2,i) = abs(real(TmpDb7(3), ReKi))    ! HSSBrTrqC should be positive valued
         CaseData(3,i) =     real(TmpDb7(4), ReKi)
         CaseData(4,i) = real(TmpDb7(5), ReKi) * D2R
         CaseData(5,i) = real(TmpDb7(6), ReKi) * D2R
         CaseData(6,i) = real(TmpDb7(7), ReKi) * D2R
      end do
      return
   end if

   !---------------------------------------------------------------------------------
   ! inline "rows:" -- a list of block mappings, one per timestep (SubDyn/MoorDyn
   ! primary-table precedent, Wave 3)
   !---------------------------------------------------------------------------------
   call YamlGetNode(Doc, 'rows', iSeq, TmpErrStat, TmpErrMsg, Found=RowsFound, From=iSec)
   if (Failed()) return
   if (.not. RowsFound) then
      call SetErrStat(ErrID_Fatal, 'case_analysis:table must contain either "file" or "rows".', &
         ErrStat, ErrMsg, RoutineName)
      return
   end if

   TabLines = int(Yaml_NumChildren(Doc, iSeq))
   call AllocAry(CaseTime,    TabLines, 'CaseTime', TmpErrStat, TmpErrMsg); if (Failed()) return
   call AllocAry(CaseData, 6, TabLines, 'CaseData', TmpErrStat, TmpErrMsg); if (Failed()) return

   do i = 1, TabLines
      iRow = Yaml_Child(Doc, iSeq, i)
      call YamlGet(Doc, 'Time', CaseTime(i), TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      call YamlGet(Doc, 'AerTrq', CaseData(1,i), TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      call YamlGet(Doc, 'HSSBrTrqC', CaseData(2,i), TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      CaseData(2,i) = abs(CaseData(2,i))    ! HSSBrTrqC should be positive valued
      call YamlGet(Doc, 'GenTrq', CaseData(3,i), TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      call YamlGet(Doc, 'BlPitchCom', BlPitchComTmp, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      CaseData(4,i) = BlPitchComTmp * D2R
      call YamlGet(Doc, 'Yaw', YawTmp, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      CaseData(5,i) = YawTmp * D2R
      call YamlGet(Doc, 'YawRate', YawRateTmp, TmpErrStat, TmpErrMsg, From=iRow)
      if (Failed()) return
      CaseData(6,i) = YawRateTmp * D2R
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ReadCaseTable

end module SED_Driver_Yaml
