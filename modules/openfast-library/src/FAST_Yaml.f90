!**********************************************************************************************************************************
! Copyright (C) 2026 National Renewable Energy Laboratory
!
! This file is part of the OpenFAST glue code.
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
!> Reader for the YAML form of the OpenFAST primary (.fst) input file. Fills the same
!! FAST_ParameterType fields as FAST_ReadPrimaryFile (the text path), including the
!! same immediate conversions (module switches, output-format decoding, step counts),
!! so everything downstream is shared.
!!
!! Schema: sections mirror the text file's banners (simulation_control,
!! feature_switches, environment, input_files, output, linearization, visualization);
!! keys keep their documented names. Differences from the text format:
!!  - counts are derived from list lengths: NLinTimes from LinTimes, NRotors from the
!!    input_files:rotors sequence (1 + its length);
!!  - under input_files the *uniform value rule* applies: a string value is a file path
!!    (its own extension decides that file's format), a mapping value is that module's
!!    input written inline. Inline input is handed to the module through a serialized
!!    FileInfoType carrying true file:line provenance (see Yaml_Serialize). Modules
!!    gain inline support one at a time; supplying a mapping for an unsupported module
!!    is a clear fatal error.
module FAST_Yaml

   use FAST_Types
   use FAST_ModTypes
   use NWTC_Library
   use YamlInput

   implicit none
   private

   public :: FAST_ParseYamlPrimary

contains

!> YAML counterpart of FAST_ReadPrimaryFile. Inline module inputs (currently only
!! InflowFile) are serialized into m_FAST for hand-off at module init.
subroutine FAST_ParseYamlPrimary( InputFile, p, m_FAST, OverrideAbortErrLev, ErrStat, ErrMsg )
   character(*),             intent(in   ) :: InputFile           !< the .fst.yaml primary file
   type(FAST_ParameterType), intent(inout) :: p                   !< glue-code parameters
   type(FAST_MiscVarType),   intent(inout) :: m_FAST              !< misc vars (LinTimes, inline module input)
   logical,                  intent(in   ) :: OverrideAbortErrLev !< whether AbortLevel may change AbortErrLev
   integer(IntKi),           intent(  out) :: ErrStat
   character(*),             intent(  out) :: ErrMsg

   character(*), parameter    :: RoutineName = 'FAST_ParseYamlPrimary'
   type(YamlDoc)              :: Doc
   character(1024)            :: PriPath
   character(10)              :: AbortLevel
   character(30)              :: Line
   real(DbKi)                 :: TmpTime, TmpRate
   real(DbKi), allocatable    :: LinTimesTmp(:)
   character(:), allocatable  :: BDFiles(:)
   integer(IntKi)             :: OutFileFmt
   integer(IntKi)             :: iFiles, iRotors, iRot, iNode, i
   logical                    :: Echo, WasFound, TabDelim
   integer(IntKi)             :: UnEc
   integer(IntKi)             :: ErrStat2
   character(ErrMsgLen)       :: ErrMsg2

   ErrStat = ErrID_None
   ErrMsg  = ""
   UnEc    = -1
   call GetPath( InputFile, PriPath )

   p%NumSSCases   = 0
   p%RotSpeedInit = 0.0_ReKi

   call Yaml_LoadFile( InputFile, Doc, ErrStat2, ErrMsg2 ); if (Failed()) return

   ! echo: verbatim copy of every physical file, comments included
   Echo = .false.
   call YamlGet( Doc, 'simulation_control:Echo', Echo, ErrStat2, ErrMsg2, Default=.false. ); if (Failed()) return
   if (Echo) then
      call OpenEcho( UnEc, trim(p%OutFileRoot)//'.ech', ErrStat2, ErrMsg2 ); if (Failed()) return
      write(UnEc, '(A)') 'Echo file for OpenFAST primary input file: '//trim(InputFile)
      call Yaml_LoadFile( InputFile, Doc, ErrStat2, ErrMsg2, UnEc=UnEc ); if (Failed()) return
   end if

   !---------------------- header / description --------------------------------------
   p%FTitle = ''
   call YamlGet( Doc, 'description', p%FTitle, ErrStat2, ErrMsg2, Found=WasFound ); if (Failed()) return

   if (.not. p%CompAeroMaps) then
      call WrScr( trim(FAST_Ver%Name)//' input file heading:' )
      call WrScr( '    '//trim( p%FTitle ) )
      call WrScr('')
   end if

   !---------------------- SIMULATION CONTROL ----------------------------------------
   call YamlGet( Doc, 'simulation_control:AbortLevel', AbortLevel, ErrStat2, ErrMsg2, Default='FATAL' ); if (Failed()) return
   if (OverrideAbortErrLev) then
      call Conv2UC( AbortLevel )
      select case ( trim(AbortLevel) )
         case ( "WARNING" )
            AbortErrLev = ErrID_Warn
         case ( "SEVERE" )
            AbortErrLev = ErrID_Severe
         case ( "FATAL" )
            AbortErrLev = ErrID_Fatal
         case default
            call SetErrStat( ErrID_Fatal, 'Invalid AbortLevel specified in FAST input file. '// &
                             'Valid entries are "WARNING", "SEVERE", or "FATAL".', ErrStat, ErrMsg, RoutineName)
            call Cleanup()
            return
      end select
   end if

   call YamlGet( Doc, 'simulation_control:TMax',        p%TMax,        ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'simulation_control:DT',          p%DT,          ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'simulation_control:ModCoupling', p%ModCoupling, ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'simulation_control:InterpOrder', p%InterpOrder, ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'simulation_control:NumCrctn',    p%NumCrctn,    ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'simulation_control:RhoInf',      p%RhoInf,      ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'simulation_control:ConvTol',     p%ConvTol,     ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'simulation_control:MaxConvIter', p%MaxConvIter, ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'simulation_control:AutoRelax',   p%AutoRelax,   ErrStat2, ErrMsg2, Default=.true. ); if (Failed()) return
   if (p%AutoRelax) then
      call YamlGet( Doc, 'simulation_control:RelaxFactor', p%RelaxFactor, ErrStat2, ErrMsg2, Default=0.3_R8Ki ); if (Failed()) return
   else
      call YamlGet( Doc, 'simulation_control:RelaxFactor', p%RelaxFactor, ErrStat2, ErrMsg2, Default=0.7_R8Ki ); if (Failed()) return
   end if
   call YamlGet( Doc, 'simulation_control:DT_UJac',     p%DT_UJac,     ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'simulation_control:UJacSclFact', p%UJacSclFact, ErrStat2, ErrMsg2 ); if (Failed()) return

   !---------------------- FEATURE SWITCHES AND FLAGS --------------------------------
   ! NRotors is derived from the input_files:rotors sequence (rotor 1 + that list)
   call YamlGetNode( Doc, 'input_files:rotors', iRotors, ErrStat2, ErrMsg2, Found=WasFound ); if (Failed()) return
   if (.not. WasFound) iRotors = 0
   p%NRotors = 1
   if (iRotors > 0) p%NRotors = 1 + Yaml_NumChildren(Doc, iRotors)

   call YamlGet( Doc, 'feature_switches:CompElast',   p%CompElast,   ErrStat2, ErrMsg2 ); if (Failed()) return
   select case (p%CompElast)
   case (1)
      p%CompElast = Module_ED
   case (2)
      p%CompElast = Module_BD
   case (3)
      p%CompElast = Module_SED
   case default
      p%CompElast = Module_Unknown
   end select

   call YamlGet( Doc, 'feature_switches:CompInflow',  p%CompInflow,  ErrStat2, ErrMsg2 ); if (Failed()) return
   select case (p%CompInflow)
   case (0)
      p%CompInflow = Module_NONE
   case (1)
      p%CompInflow = Module_IfW
   case (2)
      p%CompInflow = Module_ExtInfw
   case default
      p%CompInflow = Module_Unknown
   end select

   call YamlGet( Doc, 'feature_switches:CompAero',    p%CompAero,    ErrStat2, ErrMsg2 ); if (Failed()) return
   select case (p%CompAero)
   case (0)
      p%CompAero = Module_NONE
   case (1)
      p%CompAero = Module_ADsk
   case (2)
      p%CompAero = Module_AD
   case (3)
      p%CompAero = Module_ExtLd
   case default
      p%CompAero = Module_Unknown
   end select

   call YamlGet( Doc, 'feature_switches:CompServo',   p%CompServo,   ErrStat2, ErrMsg2 ); if (Failed()) return
   select case (p%CompServo)
   case (0)
      p%CompServo = Module_NONE
   case (1)
      p%CompServo = Module_SrvD
   case default
      p%CompServo = Module_Unknown
   end select

   call YamlGet( Doc, 'feature_switches:CompSeaSt',   p%CompSeaSt,   ErrStat2, ErrMsg2 ); if (Failed()) return
   select case (p%CompSeaSt)
   case (0)
      p%CompSeaSt = Module_NONE
   case (1)
      p%CompSeaSt = Module_SeaSt
   case default
      p%CompSeaSt = Module_Unknown
   end select

   call YamlGet( Doc, 'feature_switches:CompHydro',   p%CompHydro,   ErrStat2, ErrMsg2 ); if (Failed()) return
   select case (p%CompHydro)
   case (0)
      p%CompHydro = Module_NONE
   case (1)
      p%CompHydro = Module_HD
   case default
      p%CompHydro = Module_Unknown
   end select

   call YamlGet( Doc, 'feature_switches:CompSub',     p%CompSub,     ErrStat2, ErrMsg2 ); if (Failed()) return
   select case (p%CompSub)
   case (0)
      p%CompSub = Module_NONE
   case (1)
      p%CompSub = Module_SD
   case default
      p%CompSub = Module_Unknown
   end select

   call YamlGet( Doc, 'feature_switches:CompMooring', p%CompMooring, ErrStat2, ErrMsg2 ); if (Failed()) return
   select case (p%CompMooring)
   case (0)
      p%CompMooring = Module_NONE
   case (1)
      p%CompMooring = Module_MAP
   case (2)
      p%CompMooring = Module_FEAM
   case (3)
      p%CompMooring = Module_MD
   case (4)
      p%CompMooring = Module_Orca
   case default
      p%CompMooring = Module_Unknown
   end select

   call YamlGet( Doc, 'feature_switches:CompIce',     p%CompIce,     ErrStat2, ErrMsg2 ); if (Failed()) return
   select case (p%CompIce)
   case (0)
      p%CompIce = Module_NONE
   case (1)
      p%CompIce = Module_IceF
   case (2)
      p%CompIce = Module_IceD
   case default
      p%CompIce = Module_Unknown
   end select

   call YamlGet( Doc, 'feature_switches:CompSoil',    p%CompSoil,    ErrStat2, ErrMsg2 ); if (Failed()) return
   select case (p%CompSoil)
   case (0)
      p%CompSoil = Module_NONE
   case (1)
      p%CompSoil = Module_SlD
   case default
      p%CompSoil = Module_Unknown
   end select

   call YamlGet( Doc, 'feature_switches:MHK',         p%MHK,         ErrStat2, ErrMsg2 ); if (Failed()) return

   call AllocAry( p%MirrorRotor, p%NRotors, "p%MirrorRotor", ErrStat2, ErrMsg2 ); if (Failed()) return
   p%MirrorRotor = .false.
   if (p%NRotors > 1) then
      block
         logical, allocatable :: MirrorTmp(:)
         call YamlGetLoAryLocal( Doc, 'feature_switches:MirrorRotor', MirrorTmp, ErrStat2, ErrMsg2 ); if (Failed()) return
         if (size(MirrorTmp) /= p%NRotors) then
            call SetErrStat( ErrID_Fatal, 'MirrorRotor must have exactly NRotors ('// &
               trim(Num2LStr(p%NRotors))//') entries.', ErrStat, ErrMsg, RoutineName)
            call Cleanup()
            return
         end if
         p%MirrorRotor = MirrorTmp
      end block
   else
      ! single rotor: MirrorRotor is optional; accept and ignore a one-entry list
      call YamlGetNode( Doc, 'feature_switches:MirrorRotor', iNode, ErrStat2, ErrMsg2, Found=WasFound ); if (Failed()) return
      if (WasFound) call Yaml_MarkUsed( Doc, iNode, .true. )
   end if

   !---------------------- ENVIRONMENTAL CONDITIONS ----------------------------------
   call YamlGet( Doc, 'environment:Gravity',  p%Gravity,  ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'environment:AirDens',  p%AirDens,  ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'environment:WtrDens',  p%WtrDens,  ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'environment:KinVisc',  p%KinVisc,  ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'environment:SpdSound', p%SpdSound, ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'environment:Patm',     p%Patm,     ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'environment:Pvap',     p%Pvap,     ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'environment:WtrDpth',  p%WtrDpth,  ErrStat2, ErrMsg2 ); if (Failed()) return
   call YamlGet( Doc, 'environment:MSL2SWL',  p%MSL2SWL,  ErrStat2, ErrMsg2 ); if (Failed()) return

   !---------------------- INPUT FILES (uniform value rule) --------------------------
   call AllocAry( p%EDFile,    p%NRotors,             "p%EDFile",    ErrStat2, ErrMsg2 ); if (Failed()) return
   call AllocAry( p%BDBldFile, MaxBladesBD, p%NRotors,"p%BDBldFile", ErrStat2, ErrMsg2 ); if (Failed()) return
   call AllocAry( p%ServoFile, p%NRotors,             "p%ServoFile", ErrStat2, ErrMsg2 ); if (Failed()) return
   p%EDFile    = ''
   p%BDBldFile = ''
   p%ServoFile = ''

   p%InflowFile  = ''
   p%AeroFile    = ''
   p%SeaStFile   = ''
   p%HydroFile   = ''
   p%SubFile     = ''
   p%MooringFile = ''
   p%IceFile     = ''
   p%SoilFile    = ''

   call YamlGetNode( Doc, 'input_files', iFiles, ErrStat2, ErrMsg2 ); if (Failed()) return

   ! each module file is required only when its feature switch enables the module;
   ! entries for disabled modules may be omitted entirely (no "unused" placeholders)
   call GetModFile( 'EDFile',      p%EDFile(1),    Required=.true. ); if (ErrStat >= AbortErrLev) return
   call GetBDBldFiles( iFiles, 1 );                                   if (ErrStat >= AbortErrLev) return
   call GetModFile( 'InflowFile',  p%InflowFile,   Required=(p%CompInflow  == Module_IfW), InlineTarget='InflowWind' ); if (ErrStat >= AbortErrLev) return
   call GetModFile( 'AeroFile',    p%AeroFile,     Required=(p%CompAero    /= Module_NONE) ); if (ErrStat >= AbortErrLev) return
   call GetModFile( 'ServoFile',   p%ServoFile(1), Required=(p%CompServo   == Module_SrvD) ); if (ErrStat >= AbortErrLev) return
   call GetModFile( 'SeaStFile',   p%SeaStFile,    Required=(p%CompSeaSt   == Module_SeaSt) ); if (ErrStat >= AbortErrLev) return
   call GetModFile( 'HydroFile',   p%HydroFile,    Required=(p%CompHydro   == Module_HD) ); if (ErrStat >= AbortErrLev) return
   call GetModFile( 'SubFile',     p%SubFile,      Required=(p%CompSub     /= Module_NONE) ); if (ErrStat >= AbortErrLev) return
   call GetModFile( 'MooringFile', p%MooringFile,  Required=(p%CompMooring /= Module_NONE) ); if (ErrStat >= AbortErrLev) return
   call GetModFile( 'IceFile',     p%IceFile,      Required=(p%CompIce     /= Module_NONE) ); if (ErrStat >= AbortErrLev) return
   call GetModFile( 'SoilFile',    p%SoilFile,     Required=(p%CompSoil    == Module_SlD) ); if (ErrStat >= AbortErrLev) return

   ! additional rotors (multirotor): sequence of mappings with EDFile/BDBldFile/ServoFile
   do iRot = 2, p%NRotors
      iNode = Yaml_Child( Doc, iRotors, iRot-1 )
      call Yaml_MarkUsed( Doc, iNode, .false. )
      call GetRotorFile( iNode, iRot, 'EDFile',    p%EDFile(iRot)    ); if (ErrStat >= AbortErrLev) return
      call GetBDBldFiles( iNode, iRot );                                if (ErrStat >= AbortErrLev) return
      call GetRotorFile( iNode, iRot, 'ServoFile', p%ServoFile(iRot) ); if (ErrStat >= AbortErrLev) return
   end do

   !---------------------- OUTPUT -----------------------------------------------------
   call YamlGet( Doc, 'output:SumPrint', p%SumPrint, ErrStat2, ErrMsg2 ); if (Failed()) return

   call YamlGet( Doc, 'output:SttsTime', TmpTime, ErrStat2, ErrMsg2 ); if (Failed()) return
   if (TmpTime > p%TMax) then
      p%n_SttsTime = huge(p%n_SttsTime)
   else
      p%n_SttsTime = nint( TmpTime / p%DT )
   end if

   call YamlGet( Doc, 'output:ChkptTime', TmpTime, ErrStat2, ErrMsg2 ); if (Failed()) return
   if (TmpTime > p%TMax) then
      p%n_ChkptTime = huge(p%n_ChkptTime)
   else
      p%n_ChkptTime = nint( TmpTime / p%DT )
   end if

   call YamlGet( Doc, 'output:DT_Out', p%DT_Out, ErrStat2, ErrMsg2, Default=p%DT ); if (Failed()) return
   p%n_DT_Out = nint( p%DT_Out / p%DT )

   call YamlGet( Doc, 'output:TStart', p%TStart, ErrStat2, ErrMsg2 ); if (Failed()) return

   call YamlGet( Doc, 'output:OutFileFmt', OutFileFmt, ErrStat2, ErrMsg2 ); if (Failed()) return
   if (OutFileFmt == 0) OutFileFmt = 5
   p%WrTxtOutFile = mod(OutFileFmt,2) == 1
   OutFileFmt = OutFileFmt / 2
   p%WrBinOutFile = mod(OutFileFmt,2) == 1
   OutFileFmt = OutFileFmt / 2
   if (mod(OutFileFmt,2) == 1) then
      if (p%WrBinOutFile) then
         call SetErrStat(ErrID_Warn,'Binary compressed file will not be generated because the uncompressed version was also requested.', ErrStat, ErrMsg, RoutineName)
      else
         p%WrBinOutFile = .true.
      end if
      p%WrBinMod = FileFmtID_NoCompressWithoutTime
   else
      p%WrBinMod = FileFmtID_ChanLen_In
   end if
   OutFileFmt = OutFileFmt / 2
   if (OutFileFmt /= 0) then
      call SetErrStat( ErrID_Fatal, "OutFileFmt must be 0, 1, 2, 3, 4, or 5.", ErrStat, ErrMsg, RoutineName)
      call Cleanup()
      return
   end if

   call YamlGet( Doc, 'output:TabDelim', TabDelim, ErrStat2, ErrMsg2 ); if (Failed()) return
   if ( TabDelim ) then
      p%Delim = char(9)
   else
      p%Delim = ' '
   end if

   call YamlGet( Doc, 'output:OutFmt', p%OutFmt, ErrStat2, ErrMsg2 ); if (Failed()) return

   !---------------------- LINEARIZATION ---------------------------------------------
   call YamlGet( Doc, 'linearization:Linearize',  p%Linearize,  ErrStat2, ErrMsg2, Default=.false. ); if (Failed()) return
   call YamlGet( Doc, 'linearization:CalcSteady', p%CalcSteady, ErrStat2, ErrMsg2, Default=.false. ); if (Failed()) return
   call YamlGet( Doc, 'linearization:TrimCase',   p%TrimCase,   ErrStat2, ErrMsg2, Default=3_IntKi ); if (Failed()) return
   call YamlGet( Doc, 'linearization:TrimTol',    p%TrimTol,    ErrStat2, ErrMsg2, Default=0.001_ReKi ); if (Failed()) return
   call YamlGet( Doc, 'linearization:TrimGain',   p%TrimGain,   ErrStat2, ErrMsg2, Default=0.01_ReKi ); if (Failed()) return
   call YamlGet( Doc, 'linearization:Twr_Kdmp',   p%Twr_Kdmp,   ErrStat2, ErrMsg2, Default=0.0_ReKi ); if (Failed()) return
   call YamlGet( Doc, 'linearization:Bld_Kdmp',   p%Bld_Kdmp,   ErrStat2, ErrMsg2, Default=0.0_ReKi ); if (Failed()) return

   ! NLinTimes is derived from the LinTimes list length
   call YamlGet( Doc, 'linearization:LinTimes', LinTimesTmp, ErrStat2, ErrMsg2, Found=WasFound ); if (Failed()) return
   if (WasFound) then
      p%NLinTimes = size(LinTimesTmp)
   else
      p%NLinTimes = 0
   end if
   if (.not. p%Linearize) then
      p%CalcSteady = .false.
      p%NLinTimes = 0
   end if
   if (.not. p%CalcSteady .and. p%NLinTimes >= 1) then
      if (.not. WasFound) then
         call SetErrStat( ErrID_Fatal, 'linearization:LinTimes is required when Linearize is true and CalcSteady is false.', &
                          ErrStat, ErrMsg, RoutineName)
         call Cleanup()
         return
      end if
      call AllocAry( m_FAST%Lin%LinTimes, p%NLinTimes, 'LinTimes', ErrStat2, ErrMsg2 ); if (Failed()) return
      m_FAST%Lin%LinTimes = LinTimesTmp
   end if

   call YamlGet( Doc, 'linearization:LinInputs',  p%LinInputs,  ErrStat2, ErrMsg2, Default=1_IntKi ); if (Failed()) return
   call YamlGet( Doc, 'linearization:LinOutputs', p%LinOutputs, ErrStat2, ErrMsg2, Default=1_IntKi ); if (Failed()) return
   call YamlGet( Doc, 'linearization:LinOutJac',  p%LinOutJac,  ErrStat2, ErrMsg2, Default=.false. ); if (Failed()) return
   call YamlGet( Doc, 'linearization:LinOutMod',  p%LinOutMod,  ErrStat2, ErrMsg2, Default=.false. ); if (Failed()) return

   !---------------------- VISUALIZATION ---------------------------------------------
   call YamlGet( Doc, 'visualization:WrVTK', p%WrVTK, ErrStat2, ErrMsg2, Default=0_IntKi ); if (Failed()) return
   if ( p%WrVTK < 0 .or. p%WrVTK > 3 ) p%WrVTK = VTK_Unknown

   call YamlGet( Doc, 'visualization:VTK_type', p%VTK_Type, ErrStat2, ErrMsg2, Default=0_IntKi ); if (Failed()) return
   if ( p%VTK_Type == 0 ) then
      p%VTK_Type = VTK_None
   elseif ( p%VTK_Type == 1 ) then
      p%VTK_Type = VTK_Surf
   elseif ( p%VTK_Type == 2 ) then
      p%VTK_Type = VTK_Basic
   elseif ( p%VTK_Type == 3 ) then
      p%VTK_Type = VTK_All
   elseif ( p%VTK_Type == 4 ) then
      p%VTK_Type = VTK_Old
   else
      p%VTK_Type = VTK_Unknown
   end if

   call YamlGet( Doc, 'visualization:VTK_fields', p%VTK_fields, ErrStat2, ErrMsg2, Default=.false. ); if (Failed()) return
   call YamlGet( Doc, 'visualization:VTK_fps',    p%VTK_fps,    ErrStat2, ErrMsg2, Default=15.0_DbKi ); if (Failed()) return

   if ( EqualRealNos(p%VTK_fps, 0.0_DbKi) ) then
      TmpTime = p%TMax + p%DT
   else
      TmpTime = 1.0_DbKi / p%VTK_fps
   end if
   if (p%WrVTK == VTK_ModeShapes) then
      p%n_VTKTime = 1
   else if (TmpTime > p%TMax) then
      p%n_VTKTime = nint( p%TMax / p%DT )
   else
      p%n_VTKTime = nint( TmpTime / p%DT )
      if (p%WrVTK == VTK_Animate) then
         TmpRate = p%n_VTKTime*p%DT
         if (.not. EqualRealNos(TmpRate, TmpTime)) then
            call SetErrStat(ErrID_Info, '1/VTK_fps is not an integer multiple of DT. FAST will output VTK information at '//&
                            trim(Num2LStr(1.0_DbKi/TmpRate))//' fps, the closest rate possible.', ErrStat, ErrMsg, RoutineName)
         end if
      end if
   end if

   ! typo guard: warn about anything never looked up
   call Yaml_WarnUnused( Doc, ErrStat2, ErrMsg2 )
   call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   call Cleanup()

contains

   logical function Failed()
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      Failed = ErrStat >= AbortErrLev
      if (Failed) call Cleanup()
   end function Failed

   subroutine Cleanup()
      if (UnEc > 0_IntKi) close(UnEc)
   end subroutine Cleanup

   !> Uniform value rule for one input_files entry: a string is a path (resolved
   !! relative to the .fst); a mapping is inline module input, legal only for modules
   !! that support it (currently InflowWind).
   subroutine GetModFile( KeyName, FileVar, Required, InlineTarget )
      character(*), intent(in   )           :: KeyName
      character(*), intent(inout)           :: FileVar
      logical,      intent(in   )           :: Required
      character(*), intent(in   ), optional :: InlineTarget

      integer(IntKi) :: iVal
      logical        :: EntryFound

      call YamlGetNode( Doc, 'input_files:'//KeyName, iVal, ErrStat2, ErrMsg2, Found=EntryFound )
      if (Failed()) return
      if (.not. EntryFound) then
         if (Required) then
            ErrStat2 = ErrID_Fatal
            ErrMsg2  = '>> The required key "input_files:'//KeyName//'" was not found.'
            if (Failed()) return
         end if
         return
      end if

      select case (Doc%Nodes(iVal)%Kind)
      case (YAML_SCALAR)
         call YamlGet( Doc, 'input_files:'//KeyName, FileVar, ErrStat2, ErrMsg2 )
         if (Failed()) return
         if ( PathIsRelative( FileVar ) ) FileVar = trim(PriPath)//trim(FileVar)
      case (YAML_MAP)
         if (present(InlineTarget)) then
            select case (InlineTarget)
            case ('InflowWind')
               call Yaml_MarkUsed( Doc, iVal, .true. )
               call Yaml_Serialize( Doc, iVal, m_FAST%IfWInlineFileInfo, ErrStat2, ErrMsg2 )
               if (Failed()) return
               m_FAST%IfWIsInline = .true.
               ! pseudo path: used only for PriPath derivation and messages downstream
               FileVar = trim(PriPath)//'inline_InflowWind.yaml'
            end select
         else
            ErrStat2 = ErrID_Fatal
            ErrMsg2  = '>> Module input "'//KeyName//'" does not (yet) support inline YAML input; '// &
                       'provide a file path instead.'
            if (Failed()) return
         end if
      case default
         ErrStat2 = ErrID_Fatal
         ErrMsg2  = '>> "input_files:'//KeyName//'" must be a file path or an inline mapping.'
         if (Failed()) return
      end select
   end subroutine GetModFile

   !> BDBldFile is a list of up to MaxBladesBD blade-file paths (unset entries stay '').
   subroutine GetBDBldFiles( iFrom, iRotLoc )
      integer(IntKi), intent(in) :: iFrom     !< node to look under (input_files or a rotor entry)
      integer(IntKi), intent(in) :: iRotLoc

      logical :: EntryFound
      integer(IntKi) :: iList, iItem
      integer :: k

      call YamlGetNode( Doc, 'BDBldFile', iList, ErrStat2, ErrMsg2, Found=EntryFound, From=iFrom )
      if (Failed()) return
      if (.not. EntryFound) then
         if (p%CompElast == Module_BD) then
            ErrStat2 = ErrID_Fatal
            ErrMsg2  = '>> "BDBldFile" (list of BeamDyn blade files) is required when CompElast = 2.'
            if (Failed()) return
         end if
         return
      end if
      if (Doc%Nodes(iList)%Kind /= YAML_SEQ) then
         ErrStat2 = ErrID_Fatal
         ErrMsg2  = '>> "BDBldFile" must be a list of file paths.'
         if (Failed()) return
      end if
      if (Yaml_NumChildren(Doc, iList) > MaxBladesBD) then
         ErrStat2 = ErrID_Fatal
         ErrMsg2  = '>> "BDBldFile" may list at most '//trim(Num2LStr(MaxBladesBD))//' files.'
         if (Failed()) return
      end if
      do k = 1, int(Yaml_NumChildren(Doc, iList))
         iItem = Yaml_Child( Doc, iList, int(k, IntKi) )
         call Yaml_MarkUsed( Doc, iItem, .false. )
         if (Doc%Nodes(iItem)%Kind /= YAML_SCALAR) then
            ErrStat2 = ErrID_Fatal
            ErrMsg2  = '>> Entries of "BDBldFile" must be file paths.'
            if (Failed()) return
         end if
         p%BDBldFile(k, iRotLoc) = Doc%Nodes(iItem)%Scalar
         if ( PathIsRelative( p%BDBldFile(k, iRotLoc) ) ) &
            p%BDBldFile(k, iRotLoc) = trim(PriPath)//trim(p%BDBldFile(k, iRotLoc))
      end do
   end subroutine GetBDBldFiles

   !> A per-rotor file entry (rotors 2..NRotors): plain path only.
   subroutine GetRotorFile( iRotNode, iRotLoc, KeyName, FileVar )
      integer(IntKi), intent(in   ) :: iRotNode
      integer(IntKi), intent(in   ) :: iRotLoc
      character(*),   intent(in   ) :: KeyName
      character(*),   intent(inout) :: FileVar

      call YamlGet( Doc, KeyName, FileVar, ErrStat2, ErrMsg2, From=iRotNode )
      if (ErrStat2 >= AbortErrLev) then
         ErrMsg2 = trim(ErrMsg2)//' (rotor '//trim(Num2LStr(iRotLoc))//' entry)'
      end if
      if (Failed()) return
      if ( PathIsRelative( FileVar ) ) FileVar = trim(PriPath)//trim(FileVar)
   end subroutine GetRotorFile

   !> Logical-array lookup (no logical-array specific exists in YamlGet yet).
   subroutine YamlGetLoAryLocal( DocL, Path, Ary, ErrStatL, ErrMsgL )
      type(YamlDoc),        intent(inout) :: DocL
      character(*),         intent(in   ) :: Path
      logical, allocatable, intent(  out) :: Ary(:)
      integer(IntKi),       intent(  out) :: ErrStatL
      character(*),         intent(  out) :: ErrMsgL

      character(:), allocatable :: Toks(:)
      character(8)              :: UTok
      integer                   :: k2

      call YamlGet( DocL, Path, Toks, ErrStatL, ErrMsgL )
      if (ErrStatL >= AbortErrLev) return
      allocate(Ary(size(Toks)))
      do k2 = 1, size(Toks)
         UTok = Toks(k2)
         call Conv2UC( UTok )
         select case (trim(UTok))
         case ('TRUE', 'T', '.TRUE.')
            Ary(k2) = .true.
         case ('FALSE', 'F', '.FALSE.')
            Ary(k2) = .false.
         case default
            ErrStatL = ErrID_Fatal
            ErrMsgL  = '>> "'//trim(Path)//'" entries must be true/false; found "'//trim(Toks(k2))//'".'
            return
         end select
      end do
   end subroutine YamlGetLoAryLocal

end subroutine FAST_ParseYamlPrimary

end module FAST_Yaml
