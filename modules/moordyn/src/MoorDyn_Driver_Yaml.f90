!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2026  National Renewable Energy Laboratory
!
!    This file is part of MoorDyn.
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
!> Reader for the YAML form of the MoorDyn *driver* input file. Fills the same fields as
!! ReadDriverInputFile's sequential ReadVar/ReadAry body (MoorDyn_Driver.f90:755-841, a
!! class-B/sequential-reader driver, mirroring SeaState/SubDyn's precedent), so
!! everything downstream (the driver's own time-marching loop) is shared between the
!! two formats.
!!
!! MD_Drvr_InitInput is a program-local (host-associated) type declared inside PROGRAM
!! MoorDyn_Driver, so this reader cannot take it as a dummy argument (an external/module
!! procedure cannot type-match a program-internal derived type) -- it instead takes each
!! InitInp field as its own dummy argument, exactly like SeaState_Driver_Yaml/
!! SubDyn_Driver_Yaml. FarmPositions is declared in the program-local type as a
!! fixed-shape REAL(DbKi) array (8,40); it is taken here as an assumed-shape dummy
!! (explicit interface via `use`, so this is legal) and the funnel passes
!! InitInp%FarmPositions directly -- no allocation/copy needed.
!!
!! Unlike SeaState/SubDyn's drivers, the MoorDyn driver never reads (or echoes) an Echo
!! flag at all -- UnEcho is set to -1 and left there ("echo is no longer used by MD",
!! MoorDyn_Driver.f90:138) -- so there is no general:Echo key and this reader never
!! opens an echo file.
!!
!! Schema: sections mirror the text file's banners --
!!   environmental_conditions: Gravity, rhoW, WtrDpth
!!   moordyn:                  MDInputFile, OutRootName, TMax, dtC
!!   inputs:                   InputsMod, InputsFile
!!   farm:                     NumTurbines, SeaStateFile (optional, default ""),
!!                              initial_positions (list of row mappings {ref_X, ref_Y,
!!                              surge_init, sway_init, heave_init, roll_init,
!!                              pitch_init, yaw_init} -- the text table's own column
!!                              labels). Its length must be exactly MAX(1, NumTurbines),
!!                              mirroring the text path's `do J=1,MAX(1,InitInp%FarmSize)`
!!                              loop (MoorDyn_Driver.f90:815-817) -- NumTurbines is a
!!                              real, independently-meaningful field (FarmSize==0 means
!!                              "normal, single-turbine OpenFAST mode" downstream, not
!!                              merely "0 rows"; the table still carries exactly 1 row
!!                              in that case), so it stays a required scalar key rather
!!                              than being inferred purely from the list length.
!!
!! MDInputFile/OutRootName/InputsFile/SeaStateFile are externally-referenced files
!! (second-order rule: stay path-valued) and are each resolved relative to the driver
!! YAML file's own directory here, exactly as the text path resolves them relative to
!! FilePath (MoorDyn_Driver.f90:827-839). SeaStateFile is optional (default ""), same
!! backwards-compatible convention as the text path's own '---' sentinel check
!! (MoorDyn_Driver.f90:802-810): an empty/absent SeaStateFile means SeaState is not
!! initialized.
module MoorDyn_Driver_Yaml

   use NWTC_Library
   use YamlInput

   implicit none
   private

   public :: MDDvr_ParseYamlFile

contains

!> Load and parse a YAML-format MoorDyn driver input file, filling each InitInp field
!! passed in as its own dummy argument (see the module header for why).
subroutine MDDvr_ParseYamlFile(DvrFileName, Gravity, rhoW, WtrDpth, MDInputFile, OutRootName, &
                                TMax, dtC, InputsMod, InputsFile, NumTurbines, SeaStateFile, &
                                FarmPositions, ErrStat, ErrMsg)
   character(*),   intent(in   ) :: DvrFileName        !< the .yaml driver input file
   real(DbKi),     intent(  out) :: Gravity
   real(DbKi),     intent(  out) :: rhoW
   real(DbKi),     intent(  out) :: WtrDpth
   character(*),   intent(  out) :: MDInputFile
   character(*),   intent(  out) :: OutRootName
   real(DbKi),     intent(  out) :: TMax
   real(DbKi),     intent(  out) :: dtC
   integer(IntKi), intent(  out) :: InputsMod
   character(*),   intent(  out) :: InputsFile
   integer(IntKi), intent(  out) :: NumTurbines
   character(*),   intent(  out) :: SeaStateFile
   real(DbKi),     intent(  out) :: FarmPositions(:,:)   !< (8,>=MAX(1,NumTurbines))
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'MDDvr_ParseYamlFile'
   type(YamlDoc)            :: Doc
   character(1024)          :: PriPath
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""

   call WrScr( 'Opening MoorDyn Driver input file:  '//trim(DvrFileName) )
   call GetPath( DvrFileName, PriPath )   ! Input files will be relative to the path where the driver input file is located.

   call Yaml_LoadFile(DvrFileName, Doc, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! environmental_conditions (required)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'environmental_conditions:Gravity', Gravity, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:rhoW', rhoW, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'environmental_conditions:WtrDpth', WtrDpth, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! moordyn (required) -- MDInputFile/OutRootName are externally-referenced/output
   ! files (second-order rule: stay path-valued)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'moordyn:MDInputFile', MDInputFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if ( PathIsRelative( MDInputFile ) ) MDInputFile = trim(PriPath)//trim(MDInputFile)

   call YamlGet(Doc, 'moordyn:OutRootName', OutRootName, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if ( PathIsRelative( OutRootName ) ) OutRootName = trim(PriPath)//trim(OutRootName)

   call YamlGet(Doc, 'moordyn:TMax', TMax, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'moordyn:dtC', dtC, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   !----------------------------------------------------------------------------------
   ! inputs (required) -- InputsFile is an externally-referenced file (second-order
   ! rule: stays path-valued)
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'inputs:InputsMod', InputsMod, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   call YamlGet(Doc, 'inputs:InputsFile', InputsFile, TmpErrStat, TmpErrMsg)
   if (Failed()) return
   if ( PathIsRelative( InputsFile ) ) InputsFile = trim(PriPath)//trim(InputsFile)

   !----------------------------------------------------------------------------------
   ! farm (required) -- NumTurbines/SeaStateFile/initial_positions
   !----------------------------------------------------------------------------------
   call YamlGet(Doc, 'farm:NumTurbines', NumTurbines, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   call YamlGet(Doc, 'farm:SeaStateFile', SeaStateFile, TmpErrStat, TmpErrMsg, Default='')
   if (Failed()) return
   if ( len_trim(SeaStateFile) > 0 ) then
      if ( PathIsRelative( SeaStateFile ) ) SeaStateFile = trim(PriPath)//trim(SeaStateFile)
   end if

   call ParseInitialPositions(Doc, NumTurbines, FarmPositions, TmpErrStat, TmpErrMsg)
   if (Failed()) return

   ! typo guard: anything never looked up is reported (warning severity)
   call Yaml_WarnUnused(Doc, TmpErrStat, TmpErrMsg)
   call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine MDDvr_ParseYamlFile

!> farm:initial_positions -- a list of {ref_X, ref_Y, surge_init, sway_init,
!! heave_init, roll_init, pitch_init, yaw_init} row mappings, one per turbine (or
!! exactly 1 when NumTurbines==0, the "normal, single-turbine OpenFAST mode" case),
!! mirroring the text path's `do J=1,MAX(1,InitInp%FarmSize)` ReadAry loop
!! (MoorDyn_Driver.f90:815-817).
subroutine ParseInitialPositions(Doc, NumTurbines, FarmPositions, ErrStat, ErrMsg)
   type(YamlDoc),  intent(inout) :: Doc
   integer(IntKi), intent(in   ) :: NumTurbines
   real(DbKi),     intent(  out) :: FarmPositions(:,:)
   integer(IntKi), intent(  out) :: ErrStat
   character(*),   intent(  out) :: ErrMsg

   character(*), parameter :: RoutineName = 'MDDvr_ParseInitialPositions'
   integer(IntKi)           :: iSeq, iRow, i, nRows, nExpected
   integer(IntKi)           :: TmpErrStat
   character(ErrMsgLen)     :: TmpErrMsg

   ErrStat = ErrID_None
   ErrMsg  = ""
   nExpected = max(1_IntKi, NumTurbines)

   call YamlGetNode(Doc, 'farm:initial_positions', iSeq, TmpErrStat, TmpErrMsg); if (Failed()) return

   nRows = int(Yaml_NumChildren(Doc, iSeq))
   if (nRows /= nExpected) then
      call SetErrStat(ErrID_Fatal, 'farm:initial_positions must list exactly MAX(1,NumTurbines) ('// &
         trim(Num2LStr(nExpected))//') row(s); got '//trim(Num2LStr(nRows))//'.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   do i = 1, nRows
      iRow = Yaml_Child(Doc, iSeq, i)
      call YamlGet(Doc, 'ref_X',       FarmPositions(1,i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'ref_Y',       FarmPositions(2,i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'surge_init',  FarmPositions(3,i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'sway_init',   FarmPositions(4,i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'heave_init',  FarmPositions(5,i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'roll_init',   FarmPositions(6,i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'pitch_init',  FarmPositions(7,i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
      call YamlGet(Doc, 'yaw_init',    FarmPositions(8,i), TmpErrStat, TmpErrMsg, From=iRow); if (Failed()) return
   end do

contains

   logical function Failed()
      call SetErrStat(TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed

end subroutine ParseInitialPositions

end module MoorDyn_Driver_Yaml
