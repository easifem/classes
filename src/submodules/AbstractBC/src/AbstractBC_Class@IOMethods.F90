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

SUBMODULE(AbstractBC_Class) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Import
CHARACTER(*), PARAMETER :: myName = "bc_Import"
TYPE(String) :: dsetname, strval
INTEGER(I4B) :: ierr
REAL(DFP) :: real0
REAL(DFP), ALLOCATABLE :: real1(:), real2(:, :)

IF (obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The object is already initiated, deallocate first!')
END IF
obj%isInitiated = .TRUE.
obj%dom => dom

CALL e%raiseInformation(modName//"::"//myName//" - "// &
& "Importing Boundary condition")

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file is not opened')
END IF

IF (.NOT. hdf5%isRead()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'HDF5 file does not have read permission')
END IF

dsetname = TRIM(group)//"/name"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset name should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), &
    & vals=obj%name)
END IF

dsetname = TRIM(group)//"/idof"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset idof should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), &
    & vals=obj%idof)
END IF

dsetname = TRIM(group)//"/nodalValueType"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset nodalValueType should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), &
    & vals=strval)
END IF
SELECT CASE (TRIM(strval%chars()))
CASE ("CONSTANT")
  obj%nodalValueType = Constant
CASE ("SPACE")
  obj%nodalValueType = Space
CASE ("TIME")
  obj%nodalValueType = Time
CASE ("SPACETIME")
  obj%nodalValueType = SpaceTime
END SELECT

dsetname = TRIM(group)//"/useFunction"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset useFunction should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), &
    & vals=obj%useFunction)
END IF

dsetname = TRIM(group)//"/useExternal"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset useExternal should be present')
ELSE
  CALL hdf5%READ(dsetname=dsetname%chars(), &
    & vals=obj%useExternal)
END IF

dsetname = TRIM(group)//"/Boundary"
IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The dataset Boundary, which is a group, should be present')
ELSE
  CALL obj%boundary%IMPORT(hdf5=hdf5, group=dsetname%chars())
END IF

IF (.NOT. obj%UseFunction) THEN
  dsetname = TRIM(group)//"/NodalValue"
  IF (.NOT. hdf5%pathExists(dsetname%chars())) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The dataset NodalValue should be present')
  END IF
  SELECT CASE (obj%nodalValueType)
  CASE (Constant)
    CALL hdf5%READ(dsetname=dsetname%chars(), &
    & vals=real0)
    CALL Reallocate(obj%NodalValue, 1, 1)
    obj%NodalValue = real0
  CASE (Space, Time)
    CALL hdf5%READ(dsetname=dsetname%chars(), &
      & vals=real1)
    CALL Reallocate(obj%nodalValue, SIZE(real1), 1)
    obj%NodalValue(:, 1) = real1
  CASE (SpaceTime)
    CALL hdf5%READ(dsetname=dsetname%chars(), &
    & vals=real2)
    obj%NodalValue = real2
  END SELECT
END IF

IF (ALLOCATED(real1)) DEALLOCATE (real1)
IF (ALLOCATED(real2)) DEALLOCATE (real2)
END PROCEDURE bc_Import

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Export
CHARACTER(*), PARAMETER :: myName = "bc_Export"
TYPE(String) :: dsetname, strval
INTEGER(I4B) :: ierr
REAL(DFP) :: real0
REAL(DFP), ALLOCATABLE :: real1(:), real2(:, :)

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The object is not initiated, initiate it first!')
END IF

CALL e%raiseInformation(modName//"::"//myName//" - "// &
  & "Exporting Dirichlet Boundary Condition")

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file is not opened')
END IF

IF (.NOT. hdf5%isWrite()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'HDF5 file does not have write permission')
END IF

! WRITE name
dsetname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
  & vals=obj%name)

! WRITE idof
dsetname = TRIM(group)//"/idof"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
  & vals=obj%idof)

! WRITE nodalValueType
dsetname = TRIM(group)//"/nodalValueType"
SELECT CASE (obj%nodalValueType)
CASE (Constant)
  strval = "CONSTANT"
CASE (Space)
  strval = "SPACE"
CASE (Time)
  strval = "TIME"
CASE (SpaceTime)
  strval = "SPACETIME"
END SELECT
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
  & vals=strval)

! WRITE useFunction
dsetname = TRIM(group)//"/useFunction"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
  & vals=obj%useFunction)

! WRITE useExternal
dsetname = TRIM(group)//"/useExternal"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
  & vals=obj%useExternal)

! WRITE Boundary
dsetname = TRIM(group)//"/Boundary"
CALL obj%boundary%export(hdf5=hdf5, group=dsetname%chars())

! Read nodalValue
IF (.NOT. obj%UseFunction) THEN
  dsetname = TRIM(group)//"/NodalValue"
  IF (.NOT. ALLOCATED(obj%NodalValue)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'NodalValue is not allocated, it seems NodalValue is not set')
  END IF
  SELECT CASE (obj%nodalValueType)
  CASE (Constant)
    real0 = obj%NodalValue(1, 1)
    CALL hdf5%WRITE(dsetname=dsetname%chars(), &
      & vals=real0)
  CASE (Space, Time)
    real1 = obj%NodalValue(:, 1)
    CALL hdf5%WRITE(dsetname=dsetname%chars(), &
      & vals=real1)
  CASE (SpaceTime)
    real2 = obj%NodalValue
    CALL hdf5%WRITE(dsetname=dsetname%chars(), &
      & vals=real2)
  END SELECT
END IF

IF (ALLOCATED(real1)) DEALLOCATE (real1)
IF (ALLOCATED(real2)) DEALLOCATE (real2)
END PROCEDURE bc_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Display
TYPE(String) :: strval
REAL(DFP) :: real0
REAL(DFP), ALLOCATABLE :: real1(:), real2(:, :)

!> check
IF (.NOT. obj%isInitiated) THEN
  CALL Display("AbstractBC_::obj is not initiated, nothing to display", &
    & unitNo=unitNo)
END IF

CALL Display("# name : "//TRIM(obj%name%chars()), unitNo=unitNo)
CALL Display(obj%idof, "# idof : ", unitNo=unitNo)

SELECT CASE (obj%nodalValueType)
CASE (Constant)
  strval = "CONSTANT"
CASE (Space)
  strval = "SPACE"
CASE (Time)
  strval = "TIME"
CASE (SpaceTime)
  strval = "SPACETIME"
END SELECT
CALL Display("# nodalValueType : "//TRIM(strval%chars()), unitNo=unitNo)

CALL Display(obj%useFunction, "# useFunction : ", unitNo=unitNo)
CALL Display(obj%useExternal, "# useExternal : ", unitNo=unitNo)
CALL obj%Boundary%Display(msg="Boundary : ", unitNo=unitNo)

IF (.NOT. obj%UseFunction) THEN
  IF (.NOT. ALLOCATED(obj%NodalValue)) THEN
    CALL Display("# NodalValue : NOT ALLOCATED", unitNo=unitNo)
  ELSE
    SELECT CASE (obj%nodalValueType)
    CASE (Constant)
      real0 = obj%NodalValue(1, 1)
      CALL Display(real0, "# NodalValue : ", unitNo=unitNo)
    CASE (Space, Time)
      real1 = obj%NodalValue(:, 1)
      CALL Display(real1, "# NodalValue : ", unitNo=unitNo, orient="col")
    CASE (SpaceTime)
      real2 = obj%NodalValue(:, :)
      CALL Display(real2, "# NodalValue : ", unitNo=unitNo)
    END SELECT
  END IF
END IF

IF (ALLOCATED(real1)) DEALLOCATE (real1)
IF (ALLOCATED(real2)) DEALLOCATE (real2)
END PROCEDURE bc_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
