! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

SUBMODULE(AbstractKernel_Class) IOMethods
USE BaseMethod
USE tomlf, ONLY:  &
& toml_error,  &
& toml_load,  &
& toml_parser_config,  &
& toml_serialize,  &
& toml_get => get_value, &
& toml_len => len, &
& toml_context,  &
& toml_terminal,  &
& toml_load,  &
& toml_array,  &
& toml_stat
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             KernelDisplay
!----------------------------------------------------------------------------

MODULE PROCEDURE ak_Display
IF (.NOT. obj%isInitiated) THEN
  CALL Display("The Kernel is not initiated, &
    & There if nothing to display")
  RETURN
END IF

CALL Display(TRIM(msg), unitno=unitno)
IF (LEN_TRIM(msg) .NE. 0) THEN
  CALL Display(TRIM(msg), unitno=unitno)
END IF

! name
CALL Display(obj%name, "name : ", unitno=unitno)

! isInitiated
CALL Display("isInitiated : TRUE", unitno=unitno)

! isCommonDomain
IF (obj%isCommonDomain) THEN
  CALL Display("isCommonDomain : TRUE", unitno=unitno)
ELSE
  CALL Display("isCommonDomain : FALSE", unitno=unitno)
END IF

! engine
CALL Display(obj%engine, "engine : ", unitno=unitno)

! coordinateSystem
CALL Display(obj%coordinateSystem, "coordinateSystem : ", unitno=unitno)

! maxIter
CALL Display(obj%maxIter, "maxIter : ", unitno=unitno)

! timeDependency
CALL Display(obj%timeDependency, "timeDependency : ", unitno=unitno)

! nsd
CALL Display(obj%nsd, "nsd : ", unitno=unitno)

! nnt
CALL Display(obj%nnt, "nnt : ", unitno=unitno)

! tdof
CALL Display(obj%tdof, "tdof : ", unitno=unitno)

! normRHS
CALL Display(obj%normRHS, "normRHS : ", unitno=unitno)

! dt
CALL Display(obj%dt, "dt : ", unitno=unitno)

! startTime
CALL Display(obj%startTime, "startTime : ", unitno=unitno)

! endTime
CALL Display(obj%endTime, "endTime : ", unitno=unitno)

! currentTime
CALL Display(obj%currentTime, "currentTime : ", unitno=unitno)

! lengthScale
CALL Display(obj%lengthScale, "lengthScale : ", unitno=unitno)

! currentTimeStep
CALL Display(obj%currentTimeStep, "currentTimeStep : ", unitno=unitno)

! totalTimeStep
CALL Display(obj%totalTimeStep, "totalTimeStep : ", unitno=unitno)

! postProcessOpt
CALL Display(obj%postProcessOpt, "postProcessOpt : ", unitno=unitno)

! gravity
CALL Display(obj%gravity, "gravity : ", unitno=unitno)

! iterData
CALL Display(obj%iterData, "iterData : ", unitno=unitno)

! elemToMatId
IF (ALLOCATED(obj%elemToMatId)) THEN
  CALL Display("elemToMatID : ALLOCATED", unitno=unitno)
ELSE
  CALL Display("elemToMatID : NOT ALLOCATED", unitno=unitno)
END IF

! linsol
IF (ASSOCIATED(obj%linsol)) THEN
  CALL Display("Linear Solver : ASSOCIATED", unitno=unitno)
ELSE
  CALL Display("Linear Solver : NOT ASSOCIATED", unitno=unitno)
END IF

! tanmat
IF (ASSOCIATED(obj%tanmat)) THEN
  CALL Display("Tangent matrix : ASSOCIATED", unitno=unitno)
  CALL obj%tanmat%Display("Tanmat", unitNo=unitNo)
ELSE
  CALL Display("Tangent matrix : NOT ASSOCIATED", unitno=unitno)
END IF

! baseContinuityForSpace
CALL Display( &
  & obj%baseContinuityForSpace%chars(), &
  & "baseContinuityForSpace : ", &
  & unitNo=unitNo)

! baseInterpolationForSpace
CALL Display( &
  & obj%baseInterpolationForSpace%chars(), &
  & "baseInterpolationForSpace : ", &
  & unitNo=unitNo)

! quadratureTypeForSpace
CALL Display( &
  & obj%quadratureTypeForSpace%chars(), &
  & "quadratureTypeForSpace : ", &
  & unitNo=unitNo)

! baseContinuityForTime
CALL Display( &
  & obj%baseContinuityForTime%chars(), &
  & "baseContinuityForTime : ", &
  & unitNo=unitNo)

! baseInterpolationForTime
CALL Display( &
  & obj%baseInterpolationForTime%chars(), &
  & "baseInterpolationForTime : ", &
  & unitNo=unitNo)

! quadratureTypeForTime
CALL Display( &
  & obj%quadratureTypeForTime%chars(), &
  & "quadratureTypeForTime : ", &
  & unitNo=unitNo)

! domainFile
CALL Display(obj%domainFile, &
  & "domainFile : ", unitNo=unitNo)

! dom
IF (ASSOCIATED(obj%dom)) THEN
  CALL Display("dom : ASSOCIATED", unitNo=unitNo)
ELSE
  CALL Display("dom : NOT ASSOCIATED", unitNo=unitNo)
END IF

! domains
IF (ALLOCATED(obj%domains)) THEN
  CALL Display("domains : ALLOCATED", unitNo=unitNo)
ELSE
  CALL Display("domains : NOT ALLOCATED", unitNo=unitNo)
END IF

! quadratureForSpace
IF (ALLOCATED(obj%quadratureForSpace)) THEN
  CALL Display("quadratureForSpace : ALLOCATED", unitNo=unitNo)
ELSE
  CALL Display("quadratureForSpace: NOT ALLOCATED", unitNo=unitNo)
END IF

! cellFE
IF (ALLOCATED(obj%cellFE)) THEN
  CALL Display("cellFE: ALLOCATED", unitNo=unitNo)
ELSE
  CALL Display("cellFE: NOT ALLOCATED", unitNo=unitNo)
END IF

! facetFE
IF (ALLOCATED(obj%facetFE)) THEN
  CALL Display("facetFE: ALLOCATED", unitNo=unitNo)
ELSE
  CALL Display("facetFE: NOT ALLOCATED", unitNo=unitNo)
END IF

! edgeFE
IF (ALLOCATED(obj%edgeFE)) THEN
  CALL Display("edgeFE: ALLOCATED", unitNo=unitNo)
ELSE
  CALL Display("edgeFE: NOT ALLOCATED", unitNo=unitNo)
END IF

END PROCEDURE ak_Display

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE ak_Export
CHARACTER(*), PARAMETER :: myName = "ak_Export"
TYPE(String) :: dsetname, strval

! check
IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'The object is not initiated, initiate it first!')
END IF

! print info
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
  & "[START] Exporting Kernel")

! check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file is not opened')
END IF

! check
IF (.NOT. hdf5%isWrite()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have Write permission')
END IF

! isCommonDomain
dsetname = TRIM(group)//"/isCommonDomain"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isCommonDomain)

! name
dsetname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%name)

! engine
dsetname = TRIM(group)//"/engine"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%engine)

! coordinateSystem
dsetname = TRIM(group)//"/coordinateSystem"
strval = KernelGetCoordinateSystemName(obj%coordinateSystem)
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

! maxIter
dsetname = TRIM(group)//"/maxIter"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%maxIter)

! timeDependency
dsetname = TRIM(group)//"/timeDependency"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%timeDependency)

! nsd
dsetname = TRIM(group)//"/nsd"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%nsd)

! nnt
dsetname = TRIM(group)//"/nnt"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%nnt)

! tdof
dsetname = TRIM(group)//"/tdof"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%tdof)

! normRHS
dsetname = TRIM(group)//"/normRHS"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%normRHS)

! dt
dsetname = TRIM(group)//"/dt"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%dt)

! startTime
dsetname = TRIM(group)//"/startTime"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%startTime)

! endTime
dsetname = TRIM(group)//"/endTime"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%endTime)

! currentTime
dsetname = TRIM(group)//"/currentTime"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%currentTime)

! currentTimeStep
dsetname = TRIM(group)//"/currentTimeStep"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%currentTimeStep)

! totalTimeStep
dsetname = TRIM(group)//"/totalTimeStep"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%totalTimeStep)

! lengthScale
dsetname = TRIM(group)//"/lengthScale"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%lengthScale)

! postProcessOpt
dsetname = TRIM(group)//"/postProcessOpt"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%postProcessOpt)

! gravity
dsetname = TRIM(group)//"/gravity"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%gravity)

! elemToMatId
IF (ALLOCATED(obj%elemToMatId)) THEN
  dsetname = TRIM(group)//"/elemToMatId"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%elemToMatId)
END IF

! linsol
IF (ASSOCIATED(obj%linsol)) THEN
  dsetname = TRIM(group)//"/linSolver"
  CALL obj%linsol%export(hdf5=hdf5, group=dsetname%chars())
ELSE
  CALL e%RaiseWarning(modName//'::'//myName//" - "// &
  & 'linsol is not ASSOCIATED')
END IF

! tanmat
IF (ASSOCIATED(obj%tanmat)) THEN
  dsetname = TRIM(group)//"/tanmat"
  CALL obj%tanmat%export(hdf5=hdf5, group=dsetname%chars())
ELSE
  CALL e%RaiseWarning(modName//'::'//myName//" - "// &
  & 'tanmat is not ASSOCIATED')
END IF

! baseContinuityForSpace
dsetname = TRIM(group)//"/baseContinuityForSpace"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
  & vals=obj%baseContinuityForSpace)

! baseInterpolationForSpace
dsetname = TRIM(group)//"/baseInterpolationForSpace"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
  & vals=obj%baseInterpolationForSpace)

! quadratureTypeForSpace
dsetname = TRIM(group)//"/quadratureTypeForSpace"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
  & vals=obj%quadratureTypeForSpace)

! baseContinuityForTime
dsetname = TRIM(group)//"/baseContinuityForTime"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
  & vals=obj%baseContinuityForTime)

! baseInterpolationForTime
dsetname = TRIM(group)//"/baseInterpolationForTime"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
  & vals=obj%baseInterpolationForTime)

! quadratureTypeForTime
dsetname = TRIM(group)//"/quadratureTypeForTime"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
  & vals=obj%quadratureTypeForTime)

CALL e%RaiseInformation(modName//"::"//myName//" - "// &
  & "[END] Exporting Kernel [OK!]")

END PROCEDURE ak_Export

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

MODULE PROCEDURE ak_Import
CHARACTER(*), PARAMETER :: myName = "ak_Import"
TYPE(String) :: dsetname

! Check
IF (obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'The object is already initiated, deallocate first!')
END IF

obj%isInitiated = .TRUE.

! Info
CALL e%RaiseInformation(modName//"::"//myName//" - "// &
  & "IMPORTING KERNEL")

! Check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file is not opened')
END IF

! Check
IF (.NOT. hdf5%isRead()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have read permission')
END IF

! name
dsetname = TRIM(group)//"/name"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'The dataSet name should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)

! CoordinateSystem
dsetname = TRIM(group)//"/CoordinateSystem"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'The dataSet CoordinateSystem should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%CoordinateSystem)
SELECT CASE (obj%CoordinateSystem)
CASE (KERNEL_1D_H, KERNEL_1D_V)
  obj%NSD = 1
CASE (KERNEL_2D, KERNEL_2D_AXISYM, KERNEL_PLANE_STRAIN, &
  & KERNEL_PLANE_STRESS)
  obj%NSD = 2
CASE DEFAULT
  obj%NSD = 3
END SELECT

! TimeDependency
dsetname = TRIM(group)//"/TimeDependency"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'The dataSet TimeDependency should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%TimeDependency)
dsetname = TRIM(group)//"/tDOF"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'The dataSet tDOF should be present')
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%tDOF)

! NNT
dsetname = TRIM(group)//"/nnt"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%nnt)
ELSE
  obj%NNT = 0
END IF

! dt
dsetname = TRIM(group)//"/normRHS"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%normRHS)
ELSE
  obj%normRHS = 0.0_DFP
END IF

SELECT CASE (obj%TimeDependency)
CASE (KERNEL_TRANSIENT)

  ! dt
  dsetname = TRIM(group)//"/dt"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'The dataSet dt should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%dt)
  END IF

  ! startTime
  dsetname = TRIM(group)//"/startTime"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'The dataSet startTime should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%startTime)
  END IF

  ! endTime
  dsetname = TRIM(group)//"/endTime"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'The dataSet endTime should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%endTime)
  END IF

  ! totalTimeStep
  dsetname = TRIM(group)//"/totalTimeStep"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'The dataSet totalTimeStep should be present')
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%totalTimeStep)
  END IF
CASE (KERNEL_STATIC, KERNEL_PSEUDOSTATIC)

  ! dt
  dsetname = TRIM(group)//"/dt"
  IF (hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%dt)
  ELSE
    obj%dt = 0.0
  END IF

  ! startTime
  dsetname = TRIM(group)//"/startTime"
  IF (hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%startTime)
  ELSE
    obj%startTime = 0.0
  END IF

  ! endTime
  dsetname = TRIM(group)//"/endTime"
  IF (hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%endTime)
  ELSE
    obj%endTime = 0.0
  END IF

  ! totalTimeStep
  dsetname = TRIM(group)//"/totalTimeStep"
  IF (hdf5%pathExists(dsetname%chars())) THEN
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%totalTimeStep)
  ELSE
    obj%totalTimeStep = 0
  END IF
END SELECT

! currentTime
dsetname = TRIM(group)//"/currentTime"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%currentTime)
ELSE
  obj%currentTime = obj%startTime
END IF

! currentTimeStep
dsetname = TRIM(group)//"/currentTimeStep"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%currentTimeStep)
ELSE
  obj%currentTimeStep = 1
END IF

! postProcessOpt
dsetname = TRIM(group)//"/postProcessOpt"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%postProcessOpt)
END IF

END PROCEDURE ak_Import

!----------------------------------------------------------------------------
!                                                               WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE ak_WriteData_hdf5
CHARACTER(*), PARAMETER :: myName = "ak_WriteData_hdf5"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & 'WIP: The kernel which you are using cannot writeData hdf5 format')
END PROCEDURE ak_WriteData_hdf5

!----------------------------------------------------------------------------
!                                                               WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE ak_WriteData_vtk
CHARACTER(*), PARAMETER :: myName = "ak_WriteData_vtk"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & 'WIP: The kernel which you are using cannot writeData vtk format')
END PROCEDURE ak_WriteData_vtk

!----------------------------------------------------------------------------
!                                                               WriteData
!----------------------------------------------------------------------------

! MODULE PROCEDURE ak_WriteData_xdmf
!   CHARACTER( LEN = * ), PARAMETER :: myName="ak_WriteData_xdmf"
!   CALL e%RaiseError(modName //'::'//myName// ' - '// &
!     & 'WIP: The kernel which you are using cannot writeData xdmf format')
! END PROCEDURE ak_WriteData_xdmf

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE ak_ImportParamFromToml
CHARACTER(*), PARAMETER :: myName = "ak_ImportParamFromToml()"
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: child1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ImportParamFromToml()')
#endif

CALL ak_ImportParamFromToml3( &
  & obj=obj,  &
  & param=param,  &
  & table=table,  &
  & child=child1,  &
  & filename=filename,  &
  & afile=afile,  &
  & tomlName=tomlName)

CALL ak_ImportParamFromToml2(obj=obj, param=param, table=child1,  &
  & tomlName=tomlName)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ImportParamFromToml()')
#endif
END PROCEDURE ak_ImportParamFromToml

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE ak_ImportParamFromToml3
CHARACTER(*), PARAMETER :: myName = "ak_ImportParamFromToml3()"
LOGICAL(LGT) :: isNotOpen, isNotRead
LOGICAL(LGT), PARAMETER :: color = .TRUE.
INTEGER(I4B), PARAMETER :: detail = 1
TYPE(toml_error), ALLOCATABLE :: error
TYPE(toml_context) :: context
TYPE(toml_terminal) :: terminal
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ImportParamFromToml3()')
#endif

terminal = toml_terminal(color)

IF (PRESENT(afile)) THEN
  isNotOpen = .NOT. afile%IsOpen()
  isNotRead = .NOT. afile%IsRead()

  IF (isNotRead .OR. isNotOpen) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: The file is not open or does not have '//  &
      & 'the access to read!')
  END IF

  CALL toml_load(table,  &
    & afile%GetUnitNo(),  &
    & context=context,  &
    & config=toml_parser_config(color=terminal, context_detail=detail), &
    & error=error  &
    & )

ELSEIF (PRESENT(filename)) THEN
  CALL toml_load(table,  &
    & filename,  &
    & context=context,  &
    & config=toml_parser_config(color=terminal, context_detail=detail), &
    & error=error  &
    & )
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ARG ERROR] :: either filename or afile should be present!')
  RETURN
END IF

IF (ALLOCATED(error)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Some error occured while parsing toml file'//  &
    & ' with following message: '//error%message)
END IF

child => NULL()
CALL toml_get(table, tomlName, child, origin=origin,  &
  & requested=.FALSE., stat=stat)

IF (.NOT. ASSOCIATED(child)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: following error occured while reading '//  &
    & 'the toml file :: cannot find kernel table')
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ImportParamFromToml3()')
#endif
END PROCEDURE ak_ImportParamFromToml3

!----------------------------------------------------------------------------
!                                                         ImportParamFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE ak_ImportParamFromToml2
CHARACTER(*), PARAMETER :: myName = "ak_ImportParamFromToml2"
TYPE(toml_array), POINTER :: array
TYPE(toml_table), POINTER :: linsolve_toml
CLASS(AbstractLinSolver_), POINTER :: temp_linsolve_ptr
TYPE(String) :: name, engine, coordinateSystem, domainFile,  &
& timeDependency, baseInterpolationForSpace,  &
& baseContinuityForSpace,  &
& quadratureTypeForSpace, ipTypeForSpace, basisTypeForSpace, &
& baseInterpolationForTime, baseContinuityForTime, quadratureTypeForTime,  &
& ipTypeForTime, basisTypeForTime

INTEGER(I4B) :: origin, stat, maxIter, nsd, nnt, tdof, &
& currentTimeStep, totalTimeStep, postProcessOpt, ii

REAL(DFP) :: dt, startTime, endTime, currentTime, alphaForSpace,  &
& betaForSpace, lambdaForSpace, alphaForTime, betaForTime, lambdaForTime,  &
& gravity(3)

LOGICAL(LGT) :: isCommonDomain

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ImportParamFromToml2()')
#endif

CALL toml_get(table, "baseContinuityForSpace", baseContinuityForSpace%raw,  &
  & DEFAULT_baseContinuityForSpace, origin=origin, stat=stat)

CALL toml_get(table, "baseContinuityForTime", baseContinuityForTime%raw,  &
  & DEFAULT_baseContinuityForTime, origin=origin, stat=stat)

CALL toml_get( &
  & table,  &
  & "baseInterpolationForSpace",  &
  & baseInterpolationForSpace%raw,  &
  & DEFAULT_baseInterpolationForSpace,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "baseInterpolationForTime",  &
  & baseInterpolationForTime%raw,  &
  & DEFAULT_baseInterpolationForTime,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "domainFile",  &
  & domainFile%raw,  &
  & origin=origin, stat=stat  &
  & )

IF (stat .NE. toml_stat%success) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: kernel.domainFile is missing. It should be string.')
  RETURN
END IF

CALL toml_get( &
  & table,  &
  & "engine",  &
  & engine%raw,  &
  & origin=origin, stat=stat  &
  & )

IF (stat .NE. toml_stat%success) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: kernel.engine is missing. It should be string.')
  RETURN
END IF

CALL toml_get( &
  & table,  &
  & "name",  &
  & name%raw,  &
  & obj%GetPrefix(),  &
  & origin=origin,  &
  & stat=stat)

CALL toml_get( &
  & table,  &
  & "quadratureTypeForSpace",  &
  & quadratureTypeForSpace%raw,  &
  & DEFAULT_quadratureTypeForSpace,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "quadratureTypeForTime",  &
  & quadratureTypeForTime%raw,  &
  & DEFAULT_quadratureTypeForTime,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "coordinateSystem",  &
  & coordinateSystem%raw,  &
  & DEFAULT_CoordinateSystem_Char,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "currentTimeStep",  &
  & currentTimeStep,  &
  & 1_I4B,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "maxIter",  &
  & maxIter,  &
  & DEFAULT_maxIter,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "nnt",  &
  & nnt,  &
  & DEFAULT_NNT,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "nsd",  &
  & nsd,  &
  & DEFAULT_NSD,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "postProcessOpt",  &
  & postProcessOpt,  &
  & DEFAULT_postProcessOpt,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "tdof",  &
  & tdof,  &
  & DEFAULT_tdof,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "timeDependency",  &
  & timeDependency%raw,  &
  & DEFAULT_TimeDependency_char,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "totalTimeStep",  &
  & totalTimeStep,  &
  & DEFAULT_TotalTimeStep,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "isCommonDomain",  &
  & isCommonDomain,  &
  & DEFAULT_isCommonDomain,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "currentTime",  &
  & currentTime,  &
  & DEFAULT_currentTime,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "dt",  &
  & dt,  &
  & DEFAULT_dt,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "endTime",  &
  & endTime,  &
  & DEFAULT_endTime,  &
  & origin=origin, stat=stat  &
  & )

array => NULL()

CALL toml_get( &
  & table,  &
  & "gravity",  &
  & array,  &
  & origin=origin, &
  & stat=stat &
  & )

IF (stat .EQ. toml_stat%success) THEN
  DO ii = 1, toml_len(array)
    CALL toml_get(array, ii, gravity(ii))
  END DO
  NULLIFY (array)
ELSE
  gravity = DEFAULT_gravity
END IF

CALL toml_get( &
  & table,  &
  & "startTime",  &
  & startTime,  &
  & DEFAULT_startTime,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "ipTypeForSpace",  &
  & ipTypeForSpace%raw,  &
  & DEFAULT_ipTypeForSpace_char,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "ipTypeForTime",  &
  & ipTypeForTime%raw,  &
  & DEFAULT_ipTypeForTime_char,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "basisTypeForSpace",  &
  & basisTypeForSpace%raw,  &
  & DEFAULT_basisTypeForSpace_char,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "basisTypeForTime",  &
  & basisTypeForTime%raw,  &
  & DEFAULT_basisTypeForTime_char,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "alphaForSpace",  &
  & alphaForSpace,  &
  & DEFAULT_alphaForSpace,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "alphaForTime",  &
  & alphaForTime,  &
  & DEFAULT_alphaForTime,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "betaForSpace",  &
  & betaForSpace,  &
  & DEFAULT_betaForSpace,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "betaForTime",  &
  & betaForTime,  &
  & DEFAULT_betaForTime,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "lambdaForSpace",  &
  & lambdaForSpace,  &
  & DEFAULT_lambdaForSpace,  &
  & origin=origin, stat=stat  &
  & )

CALL toml_get( &
  & table,  &
  & "lambdaForTime",  &
  & lambdaForTime,  &
  & DEFAULT_lambdaForTime,  &
  & origin=origin, stat=stat  &
  & )

! CALL Display(toml_serialize(table))

CALL SetAbstractKernelParam( &
& param=param,  &
& prefix=obj%GetPrefix(),  &
& baseContinuityForSpace=baseContinuityForSpace%chars(),  &
& baseContinuityForTime=baseContinuityForTime%chars(),  &
& baseInterpolationForSpace=baseInterpolationForSpace%chars(),  &
& baseInterpolationForTime=baseInterpolationForTime%chars(),  &
& domainFile=domainFile%chars(),  &
& engine=engine%chars(),  &
& name=name%chars(),  &
& quadratureTypeForSpace=quadratureTypeForSpace%chars(),  &
& quadratureTypeForTime=quadratureTypeForTime%chars(),  &
& coordinateSystem=typeCoordinateSystem%ToNumber(coordinateSystem%chars()),  &
& currentTimeStep=currentTimeStep,  &
& maxIter=maxIter,  &
& nnt=nnt,  &
& postProcessOpt=postProcessOpt,  &
& tdof=tdof,  &
& timeDependency=typeTimeDependency%ToNumber(timeDependency%chars()),  &
& totalTimeStep=totalTimeStep,  &
& isCommonDomain=isCommonDomain,  &
& currentTime=currentTime,  &
& dt=dt,  &
& endTime=endTime,  &
& gravity=gravity,  &
& startTime=startTime,  &
& ipTypeForSpace=BaseInterpolation_ToInteger(ipTypeForSpace%chars()),  &
& ipTypeForTime=BaseInterpolation_ToInteger(ipTypeForTime%chars()),  &
& basisTypeForSpace=BasisType_ToInteger(basisTypeForSpace%chars()),  &
& basisTypeForTime=BasisType_ToInteger(basisTypeForTime%chars()),  &
& alphaForSpace=alphaForSpace,  &
& alphaForTime=alphaForTime,  &
& betaForSpace=betaForSpace,  &
& betaForTime=betaForTime,  &
& lambdaForSpace=lambdaForSpace,  &
& lambdaForTime=lambdaForTime &
& )

! linesolve
linsolve_toml => NULL()
CALL toml_get(table, toml_linsolver_name, linsolve_toml, origin=origin,  &
  & requested=.FALSE., stat=stat)
IF (.NOT. ASSOCIATED(linsolve_toml)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: following error occured while reading '//  &
    & 'the toml file :: cannot find kernel.linSolver table')
END IF

temp_linsolve_ptr => LinearSolverFactory(engine%chars())
CALL temp_linsolve_ptr%ImportParamFromToml(param=param, table=linsolve_toml)
CALL temp_linsolve_ptr%DEALLOCATE()
NULLIFY (temp_linsolve_ptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ImportParamFromToml()')
#endif
END PROCEDURE ak_ImportParamFromToml2

!----------------------------------------------------------------------------
!                                                         ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE ak_ImportFromToml
CHARACTER(*), PARAMETER :: myName = "ak_ImportFromToml()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child class.')
END PROCEDURE ak_ImportFromToml

!----------------------------------------------------------------------------
!                                                            ExportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE ak_ExportToToml
CHARACTER(*), PARAMETER :: myName = "ak_ExportToToml"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine should be implemented by child class.')
END PROCEDURE ak_ExportToToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
