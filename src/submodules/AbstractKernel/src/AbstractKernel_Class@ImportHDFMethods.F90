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

SUBMODULE(AbstractKernel_Class) ImportHDFMEthods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import"
TYPE(String) :: dsetname

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

! Check
IF (obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: The object is already initiated, deallocate first!')
  RETURN
END IF

obj%isInitiated = .TRUE.

! Check
IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: HDF5 file is not opened')
END IF

! Check
IF (.NOT. hdf5%isRead()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: HDF5 file does not have read permission')
END IF

! name
dsetname = TRIM(group)//"/name"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: The dataSet name should be present')
  RETURN
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)

! coordinateSystem
dsetname = TRIM(group)//"/coordinateSystem"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: dataSet coordinateSystem should be present')
  RETURN
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%coordinateSystem)
SELECT CASE (obj%coordinateSystem)
CASE (KERNEL_1D_H, KERNEL_1D_V)
  obj%NSD = 1
CASE (KERNEL_2D, KERNEL_2D_AXISYM, KERNEL_PLANE_STRAIN, &
  & KERNEL_PLANE_STRESS)
  obj%NSD = 2
CASE DEFAULT
  obj%NSD = 3
END SELECT

! TimeDependency
dsetname = TRIM(group)//"/timeDependency"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: dataSet timeDependency should be present')
  RETURN
END IF
CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%timeDependency)
dsetname = TRIM(group)//"/tDOF"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: dataSet tDOF should be present')
  RETURN
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
    & '[INTERNAL ERROR] :: dataSet dt should be present')
    RETURN
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%dt)
  END IF

  ! startTime
  dsetname = TRIM(group)//"/startTime"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: dataSet startTime should be present')
    RETURN
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%startTime)
  END IF

  ! endTime
  dsetname = TRIM(group)//"/endTime"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: dataSet endTime should be present')
    RETURN
  ELSE
    CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%endTime)
  END IF

  ! totalTimeStep
  dsetname = TRIM(group)//"/totalTimeStep"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: dataSet totalTimeStep should be present')
    RETURN
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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ImportHDFMEthods
