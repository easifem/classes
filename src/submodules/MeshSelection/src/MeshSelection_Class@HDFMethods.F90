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

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This module defines a data type for mesh selection

SUBMODULE(MeshSelection_Class) HDFMethods
USE Display_Method, ONLY: Display, ToString, EqualLine, &
                          BlankLines

USE BoundingBox_Method, ONLY: bb_Display => Display, bb_GetValue => GetValue

USE GlobalData, ONLY: CHAR_LF, stdout

USE IntVector_Method, ONLY: intvector_Display => Display, &
                            ASSIGNMENT(=), &
                            isAllocated

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
CHARACTER(*), PARAMETER :: myName = "obj_Import"
TYPE(String) :: dsetname
INTEGER(I4B), ALLOCATABLE :: intvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

IF (obj%isinit) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
         '[CONFIG ERROR] :: MeshSelection_::object is already initiated, '// &
                    'deallocate it first!')
END IF

obj%isinit = .TRUE.

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'HDF5 file is not opened')
END IF

!> check
IF (.NOT. hdf5%isRead()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'HDF5 file does not have read permission')
END IF

! READ isSelectionByMeshID
dsetname = TRIM(group)//"/isSelectionByMeshID"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%ms(1))
ELSE
  obj%ms(1) = .FALSE.
END IF

! READ isSelectionByElemNum
dsetname = TRIM(group)//"/isSelectionByElemNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%ms(2))
ELSE
  obj%ms(2) = .FALSE.
END IF

! READ isSelectionByNodeNum
dsetname = TRIM(group)//"/isSelectionByNodeNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%ms(3))
ELSE
  obj%ms(3) = .FALSE.
END IF

! READ isSelectionByBox
dsetname = TRIM(group)//"/isSelectionByBox"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%ms(4))
ELSE
  obj%ms(4) = .FALSE.
END IF

! READ PointMeshID
dsetname = TRIM(group)//"/PointMeshID"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%PointMeshID = intvec
  obj%ms(1) = .TRUE.
END IF

! READ CurveMeshID
dsetname = TRIM(group)//"/CurveMeshID"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%CurveMeshID = intvec
  obj%ms(1) = .TRUE.
END IF

! READ SurfaceMeshID
dsetname = TRIM(group)//"/SurfaceMeshID"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%SurfaceMeshID = intvec
  obj%ms(1) = .TRUE.
END IF

! READ VolumeMeshID
dsetname = TRIM(group)//"/VolumeMeshID"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%VolumeMeshID = intvec
  obj%ms(1) = .TRUE.
END IF

! READ PointElemNum
dsetname = TRIM(group)//"/PointElemNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%PointElemNum = intvec
  obj%ms(2) = .TRUE.
END IF

! READ CurveElemNum
dsetname = TRIM(group)//"/CurveElemNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%CurveElemNum = intvec
  obj%ms(2) = .TRUE.
END IF

! READ SurfaceElemNum
dsetname = TRIM(group)//"/SurfaceElemNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%SurfaceElemNum = intvec
  obj%ms(2) = .TRUE.
END IF

! READ VolumeElemNum
dsetname = TRIM(group)//"/VolumeElemNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%VolumeElemNum = intvec
  obj%ms(2) = .TRUE.
END IF

! READ NodeNum
dsetname = TRIM(group)//"/NodeNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%NodeNum = intvec
  obj%ms(3) = .TRUE.
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                   Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
TYPE(String) :: dsetname
INTEGER(I4B), ALLOCATABLE :: intvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

!> check
IF (.NOT. obj%isinit) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'The object is not initiated, allocate first!')
END IF
!> check

IF (.NOT. hdf5%isOpen()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'HDF5 file is not opened')
END IF
!> check

IF (.NOT. hdf5%isWrite()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'HDF5 file does not have write permission')
END IF

! READ isSelectionByMeshID
dsetname = TRIM(group)//"/isSelectionByMeshID"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
                vals=obj%ms(1))

! READ isSelectionByElemNum
dsetname = TRIM(group)//"/isSelectionByElemNum"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
                vals=obj%ms(2))

! READ isSelectionByNodeNum
dsetname = TRIM(group)//"/isSelectionByNodeNum"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
                vals=obj%ms(3))

! READ isSelectionByBox
dsetname = TRIM(group)//"/isSelectionByBox"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
                vals=obj%ms(4))

! READ PointMeshID
IF (isAllocated(obj%PointMeshID)) THEN
  dsetname = TRIM(group)//"/PointMeshID"
  intvec = obj%PointMeshID
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ CurveMeshID
IF (isAllocated(obj%CurveMeshID)) THEN
  dsetname = TRIM(group)//"/CurveMeshID"
  intvec = obj%CurveMeshID
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ SurfaceMeshID
IF (isAllocated(obj%SurfaceMeshID)) THEN
  dsetname = TRIM(group)//"/SurfaceMeshID"
  intvec = obj%SurfaceMeshID
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ VolumeMeshID
IF (isAllocated(obj%VolumeMeshID)) THEN
  dsetname = TRIM(group)//"/VolumeMeshID"
  intvec = obj%VolumeMeshID
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ PointElemNum
IF (isAllocated(obj%PointElemNum)) THEN
  dsetname = TRIM(group)//"/PointElemNum"
  intvec = obj%PointElemNum
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ CurveElemNum
IF (isAllocated(obj%CurveElemNum)) THEN
  dsetname = TRIM(group)//"/CurveElemNum"
  intvec = obj%CurveElemNum
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ SurfaceElemNum
IF (isAllocated(obj%SurfaceElemNum)) THEN
  dsetname = TRIM(group)//"/SurfaceElemNum"
  intvec = obj%SurfaceElemNum
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ VolumeElemNum
IF (isAllocated(obj%VolumeElemNum)) THEN
  dsetname = TRIM(group)//"/VolumeElemNum"
  intvec = obj%VolumeElemNum
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ NodeNum
IF (isAllocated(obj%NodeNum)) THEN
  dsetname = TRIM(group)//"/NodeNum"
  intvec = obj%NodeNum
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods
