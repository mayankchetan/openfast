!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of IceDyn.
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
!> Reader for the YAML form of the IceDyn primary input file. Fills the same IceD_InputFile
!! (InputFileData) scalar fields that the text-format IceD_ReadInput reader (in IceDyn.f90)
!! fills -- everything the text reader reads from the primary file itself, and nothing more.
!!
!! Funnel structure (mirrors FEAMooring's FEAM_ReadInput / SubDyn's SD_Input): the
!! format-specific reading branch inside IceD_ReadInput is what this module replaces. IceDyn's
!! primary-file reader has no cross-cutting post-processing step after the read (unlike
!! FEAMooring's deg->rad conversion or ExtPtfm's reduced/connection/forcing files), so nothing
!! is hoisted -- the funnel simply dispatches to this module and returns.
!!
!! Schema: top-level keys mirror the text file's section banners:
!!   structure_properties : LegPosX, LegPosY, StWidth (per-leg lists; NumLegs derives from
!!                           list length, per the project-wide counts-derive-from-lists rule)
!!   ice_models            : IceModel, IceSubModel
!!   ice_general           : IceVel, IceThks, WtDen, IceDen, InitLoc, InitTm, Seed1, Seed2
!!   ice_model_1           : Ikm, Ag, Qg, Rg, Tice, Poisson, WgAngle, EIce, SigNm
!!   ice_model_2           : Pitch, IceStr2, Delmax2
!!   ice_model_3           : ThkMean, ThkVar, VelMean, VelVar, TeMean, StrMean, StrVar,
!!                           DelMean, DelVar, PMean, PVar
!!   ice_model_4           : PrflMean, PrflSig, ZoneNo1, ZoneNo2, ZonePitch, IceStr, Delmax
!!   ice_model_5           : ConeAgl, ConeDwl, ConeDtp, RdupThk, mu, FlxStr, StrLim, StrRtLim
!!   ice_model_6           : FloeLth, FloeWth, CPrAr, dPrAr, Fdr, Kic, FspN
!!
!! Key names match the text file's variable-name comments (e.g. "IceVel", "WtDen"), not the
!! InputFileData field names (e.g. %v, %rhow) -- consistent with the project-wide convention of
!! using the text file's documented key names for YAML.
!!
!! No path-only / second-order external files: IceD_ReadInput reads every field directly from
!! the primary input file; there is no second data file to keep as a path.
!!
!! No OutList: unlike most modules, IceDyn's primary-file reader has no Section Header: OutList
!! block and never reads InputFileData%OutList/NumOuts, so this reader defines none either.
!!
!! UorD (upward/downward breaking cone flag) is a IceD_InputFile field that the text reader
!! never reads (it is set elsewhere / left at its type default of 0); this reader does not read
!! it either, matching the text path exactly.
module IceDyn_Yaml

   use NWTC_Library
   use IceDyn_Types
   use YamlInput

   implicit none
   private

   public :: IceD_ParseYamlFile
   public :: IceD_ParseYamlFileInfo

contains

!> Load and parse a YAML-format IceDyn primary input file.
subroutine IceD_ParseYamlFile(InputFile, PriPath, InputFileData, ErrStat, ErrMsg)
   character(*),          intent(in   ) :: InputFile     !< the .yaml primary input file
   character(*),          intent(in   ) :: PriPath       !< path of the primary file (unused; IceDyn has no second-order files)
   type(IceD_InputFile),  intent(inout) :: InputFileData
   integer(IntKi),        intent(  out) :: ErrStat
   character(*),          intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'IceD_ParseYamlFile'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call Yaml_LoadFile(InputFile, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call ParseYamlDoc(Doc, PriPath, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! typo guard: anything never looked up is reported (warning severity)
   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine IceD_ParseYamlFile

!> Parse YAML-format IceDyn input arriving as a FileInfoType -- the passed-data channel used
!! for inline module input from a YAML primary file (the glue code's inline IceFile handover
!! when CompIce selects IceDyn). Applied identically to every leg's InitInp by the caller.
subroutine IceD_ParseYamlFileInfo(FileInfoIn, PriPath, InputFileData, ErrStat, ErrMsg)
   type(FileInfoType),   intent(in   ) :: FileInfoIn    !< YAML text lines with provenance
   character(*),         intent(in   ) :: PriPath       !< path of the primary file (unused; IceDyn has no second-order files)
   type(IceD_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'IceD_ParseYamlFileInfo'
   type(YamlDoc)           :: Doc
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call Yaml_LoadFileInfo(FileInfoIn, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call ParseYamlDoc(Doc, PriPath, InputFileData, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine IceD_ParseYamlFileInfo

!> Fill InputFileData from a parsed document. Split from the two file-entry wrappers, mirroring
!! the project-wide ParseYamlDoc convention. PriPath is threaded through for parity with the
!! sibling YAML readers though IceDyn currently has no path-only field to resolve.
subroutine ParseYamlDoc(Doc, PriPath, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),         intent(inout) :: Doc
   character(*),          intent(in   ) :: PriPath
   type(IceD_InputFile),  intent(inout) :: InputFileData
   integer(IntKi),        intent(  out) :: ErrStat
   character(*),          intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'IceD_ParseYamlDoc'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call ParseStructureProperties(Doc, InputFileData, TmpErrStat, TmpErrMsg); if (Failed()) return
   call ParseIceModels(Doc, InputFileData, TmpErrStat, TmpErrMsg);           if (Failed()) return
   call ParseIceGeneral(Doc, InputFileData, TmpErrStat, TmpErrMsg);          if (Failed()) return
   call ParseIceModel1(Doc, InputFileData, TmpErrStat, TmpErrMsg);           if (Failed()) return
   call ParseIceModel2(Doc, InputFileData, TmpErrStat, TmpErrMsg);           if (Failed()) return
   call ParseIceModel3(Doc, InputFileData, TmpErrStat, TmpErrMsg);           if (Failed()) return
   call ParseIceModel4(Doc, InputFileData, TmpErrStat, TmpErrMsg);           if (Failed()) return
   call ParseIceModel5(Doc, InputFileData, TmpErrStat, TmpErrMsg);           if (Failed()) return
   call ParseIceModel6(Doc, InputFileData, TmpErrStat, TmpErrMsg);           if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseYamlDoc

!----------------------------------------------------------------------------------------
! section parsers
!----------------------------------------------------------------------------------------

!> structure_properties: LegPosX, LegPosY, StWidth -- three equal-length per-leg lists.
!! NumLegs is NOT a YAML key: it derives from len(LegPosX), per the project-wide rule.
subroutine ParseStructureProperties(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   type(IceD_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'IceD_ParseStructureProperties'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'structure_properties:LegPosX', InputFileData%LegPosX, TmpErrStat, TmpErrMsg); if (Failed()) return

   InputFileData%NumLegs = size(InputFileData%LegPosX)
   if (InputFileData%NumLegs < 0) then
      call SetErrStat(ErrID_Fatal, 'IceD_ParseYamlDoc: NumLegs must be a positive number.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call YamlGet(Doc, 'structure_properties:LegPosY', InputFileData%LegPosY, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (size(InputFileData%LegPosY) /= InputFileData%NumLegs) then
      call SetErrStat(ErrID_Fatal, 'structure_properties:LegPosY must have the same length ('// &
         trim(Num2LStr(InputFileData%NumLegs))//') as LegPosX.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call YamlGet(Doc, 'structure_properties:StWidth', InputFileData%StrWd, TmpErrStat, TmpErrMsg); if (Failed()) return
   if (size(InputFileData%StrWd) /= InputFileData%NumLegs) then
      call SetErrStat(ErrID_Fatal, 'structure_properties:StWidth must have the same length ('// &
         trim(Num2LStr(InputFileData%NumLegs))//') as LegPosX.', ErrStat, ErrMsg, RoutineName)
      return
   end if

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseStructureProperties

!> ice_models: IceModel, IceSubModel.
subroutine ParseIceModels(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   type(IceD_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'IceD_ParseIceModels'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'ice_models:IceModel',    InputFileData%IceModel,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_models:IceSubModel', InputFileData%IceSubModel, TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseIceModels

!> ice_general: IceVel, IceThks, WtDen, IceDen, InitLoc, InitTm, Seed1, Seed2.
subroutine ParseIceGeneral(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   type(IceD_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'IceD_ParseIceGeneral'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'ice_general:IceVel',  InputFileData%v,       TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_general:IceThks', InputFileData%h,       TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_general:WtDen',   InputFileData%rhow,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_general:IceDen',  InputFileData%rhoi,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_general:InitLoc', InputFileData%InitLoc, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_general:InitTm',  InputFileData%t0,      TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_general:Seed1',   InputFileData%Seed1,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_general:Seed2',   InputFileData%Seed2,   TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseIceGeneral

!> ice_model_1: Ikm, Ag, Qg, Rg, Tice, Poisson, WgAngle, EIce, SigNm.
subroutine ParseIceModel1(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   type(IceD_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'IceD_ParseIceModel1'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'ice_model_1:Ikm',     InputFileData%Ikm,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_1:Ag',      InputFileData%Ag,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_1:Qg',      InputFileData%Qg,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_1:Rg',      InputFileData%Rg,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_1:Tice',    InputFileData%Tice, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_1:Poisson', InputFileData%nu,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_1:WgAngle', InputFileData%phi,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_1:EIce',    InputFileData%Eice, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_1:SigNm',   InputFileData%SigNm,TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseIceModel1

!> ice_model_2: Pitch, IceStr2, Delmax2.
subroutine ParseIceModel2(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   type(IceD_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'IceD_ParseIceModel2'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'ice_model_2:Pitch',   InputFileData%Pitch,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_2:IceStr2', InputFileData%IceStr2, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_2:Delmax2', InputFileData%Delmax2, TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseIceModel2

!> ice_model_3: ThkMean, ThkVar, VelMean, VelVar, TeMean, StrMean, StrVar, DelMean, DelVar,
!! PMean, PVar.
subroutine ParseIceModel3(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   type(IceD_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'IceD_ParseIceModel3'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'ice_model_3:ThkMean', InputFileData%miuh,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_3:ThkVar',  InputFileData%varh,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_3:VelMean', InputFileData%miuv,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_3:VelVar',  InputFileData%varv,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_3:TeMean',  InputFileData%miut,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_3:StrMean', InputFileData%miubr,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_3:StrVar',  InputFileData%varbr,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_3:DelMean', InputFileData%miuDelm, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_3:DelVar',  InputFileData%varDelm, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_3:PMean',   InputFileData%miuP,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_3:PVar',    InputFileData%varP,    TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseIceModel3

!> ice_model_4: PrflMean, PrflSig, ZoneNo1, ZoneNo2, ZonePitch, IceStr, Delmax.
subroutine ParseIceModel4(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   type(IceD_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'IceD_ParseIceModel4'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'ice_model_4:PrflMean',  InputFileData%PrflMean,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_4:PrflSig',   InputFileData%PrflSig,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_4:ZoneNo1',   InputFileData%Zn1,       TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_4:ZoneNo2',   InputFileData%Zn2,       TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_4:ZonePitch', InputFileData%ZonePitch, TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_4:IceStr',    InputFileData%IceStr,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_4:Delmax',    InputFileData%Delmax,    TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseIceModel4

!> ice_model_5: ConeAgl, ConeDwl, ConeDtp, RdupThk, mu, FlxStr, StrLim, StrRtLim.
subroutine ParseIceModel5(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   type(IceD_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'IceD_ParseIceModel5'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'ice_model_5:ConeAgl',  InputFileData%alpha,    TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_5:ConeDwl',  InputFileData%Dwl,      TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_5:ConeDtp',  InputFileData%Dtp,      TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_5:RdupThk',  InputFileData%hr,       TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_5:mu',       InputFileData%mu,       TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_5:FlxStr',   InputFileData%sigf,     TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_5:StrLim',   InputFileData%StrLim,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_5:StrRtLim', InputFileData%StrRtLim, TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseIceModel5

!> ice_model_6: FloeLth, FloeWth, CPrAr, dPrAr, Fdr, Kic, FspN.
subroutine ParseIceModel6(Doc, InputFileData, ErrStat, ErrMsg)
   type(YamlDoc),        intent(inout) :: Doc
   type(IceD_InputFile), intent(inout) :: InputFileData
   integer(IntKi),       intent(  out) :: ErrStat
   character(*),         intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'IceD_ParseIceModel6'
   integer(IntKi)          :: TmpErrStat
   character(ErrMsgLen)    :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call YamlGet(Doc, 'ice_model_6:FloeLth', InputFileData%Ll,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_6:FloeWth', InputFileData%Lw,   TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_6:CPrAr',   InputFileData%Cpa,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_6:dPrAr',   InputFileData%dpa,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_6:Fdr',     InputFileData%Fdr,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_6:Kic',     InputFileData%Kic,  TmpErrStat, TmpErrMsg); if (Failed()) return
   call YamlGet(Doc, 'ice_model_6:FspN',    InputFileData%FspN, TmpErrStat, TmpErrMsg); if (Failed()) return

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseIceModel6

end module IceDyn_Yaml
