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

SUBMODULE(AbstractKernel_Class) ExportHDFMEthods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
TYPE(String) :: dsetname, strval
TYPE(CPUTime_) :: TypeCPUTime


IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

! check
IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: The object is not initiated, initiate it first!')
  RETURN
END IF

! check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: HDF5 file is not opened')
  RETURN
END IF

! check
IF (.NOT. hdf5%isWrite()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: HDF5 file does not have Write permission')
  RETURN
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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
  & currentTime=obj%currentTime, currentTimeStep=obj%currentTimeStep, &
  & methodName=myName))
END IF

END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ExportHDFMEthods
