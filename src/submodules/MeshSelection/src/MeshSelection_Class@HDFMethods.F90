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
USE BoundingBox_Method, ONLY: bb_Display => Display
USE BoundingBox_Method, ONLY: bb_GetValue => GetValue
USE Display_Method, ONLY: BlankLines
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: EqualLine
USE Display_Method, ONLY: ToString
USE GlobalData, ONLY: CHAR_LF
USE GlobalData, ONLY: stdout
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: intvector_Display => Display
USE IntVector_Method, ONLY: IsAllocated
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "MeshSelection_Class@HDFMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
LOGICAL(LGT) :: isok
#endif

TYPE(String) :: dsetname
INTEGER(I4B), ALLOCATABLE :: intvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
isok = .NOT. obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'MeshSelection_::object is already initiated, '// &
                  'deallocate it first!')
#endif

#ifdef DEBUG_VER
isok = hdf5%IsOpen()
CALL AssertError1(isok, myName, "file is not opened.")
#endif

#ifdef DEBUG_VER
isok = hdf5%IsRead()
CALL AssertError1(isok, myName, "file does not have read access.")
#endif

obj%isinit = .TRUE.

! READ isSelectionByMeshID
obj%ms = math%no
dsetname = TRIM(group)//"/isSelectionByMeshID"
IF (hdf5%pathExists(dsetname%Chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%Chars(), vals=obj%ms(1))
END IF

! READ isSelectionByElemNum
dsetname = TRIM(group)//"/isSelectionByElemNum"
IF (hdf5%pathExists(dsetname%Chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%Chars(), vals=obj%ms(2))
END IF

! READ isSelectionByNodeNum
dsetname = TRIM(group)//"/isSelectionByNodeNum"
IF (hdf5%pathExists(dsetname%Chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%Chars(), vals=obj%ms(3))
END IF

! READ isSelectionByBox
dsetname = TRIM(group)//"/isSelectionByBox"
IF (hdf5%pathExists(dsetname%Chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%Chars(), vals=obj%ms(4))
END IF

! READ PointMeshID
dsetname = TRIM(group)//"/PointMeshID"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%pointMeshID = intvec
  obj%ms(1) = .TRUE.
END IF

! READ CurveMeshID
dsetname = TRIM(group)//"/CurveMeshID"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=intvec)
  obj%curveMeshID = intvec
  obj%ms(1) = .TRUE.
END IF

! READ SurfaceMeshID
dsetname = TRIM(group)//"/SurfaceMeshID"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=intvec)
  obj%surfaceMeshID = intvec
  obj%ms(1) = .TRUE.
END IF

! READ VolumeMeshID
dsetname = TRIM(group)//"/VolumeMeshID"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=intvec)
  obj%volumeMeshID = intvec
  obj%ms(1) = .TRUE.
END IF

! READ PointElemNum
dsetname = TRIM(group)//"/PointElemNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=intvec)
  obj%pointElemNum = intvec
  obj%ms(2) = .TRUE.
END IF

! READ CurveElemNum
dsetname = TRIM(group)//"/CurveElemNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=intvec)
  obj%curveElemNum = intvec
  obj%ms(2) = .TRUE.
END IF

! READ SurfaceElemNum
dsetname = TRIM(group)//"/SurfaceElemNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=intvec)
  obj%surfaceElemNum = intvec
  obj%ms(2) = .TRUE.
END IF

! READ VolumeElemNum
dsetname = TRIM(group)//"/VolumeElemNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=intvec)
  obj%volumeElemNum = intvec
  obj%ms(2) = .TRUE.
END IF

! READ NodeNum
dsetname = TRIM(group)//"/NodeNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), vals=intvec)
  obj%nodeNum = intvec
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
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
#endif

TYPE(String) :: dsetname
INTEGER(I4B), ALLOCATABLE :: intvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%isinit) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

#ifdef DEBUG_VER
CALL AssertError1(hdf5%IsOpen(), myName, "file is not opened.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(hdf5%IsWrite(), myName, "file does not have write access.")
#endif

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
IF (IsAllocated(obj%pointMeshID)) THEN
  dsetname = TRIM(group)//"/PointMeshID"
  intvec = obj%pointMeshID
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ CurveMeshID
IF (IsAllocated(obj%curveMeshID)) THEN
  dsetname = TRIM(group)//"/CurveMeshID"
  intvec = obj%curveMeshID
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ SurfaceMeshID
IF (IsAllocated(obj%surfaceMeshID)) THEN
  dsetname = TRIM(group)//"/SurfaceMeshID"
  intvec = obj%surfaceMeshID
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ VolumeMeshID
IF (IsAllocated(obj%volumeMeshID)) THEN
  dsetname = TRIM(group)//"/VolumeMeshID"
  intvec = obj%volumeMeshID
  CALL hdf5%WRITE(dsetname=dsetname%Chars(), vals=intvec)
END IF

! READ PointElemNum
IF (IsAllocated(obj%pointElemNum)) THEN
  dsetname = TRIM(group)//"/PointElemNum"
  intvec = obj%pointElemNum
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ CurveElemNum
IF (IsAllocated(obj%curveElemNum)) THEN
  dsetname = TRIM(group)//"/CurveElemNum"
  intvec = obj%curveElemNum
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ SurfaceElemNum
IF (IsAllocated(obj%surfaceElemNum)) THEN
  dsetname = TRIM(group)//"/SurfaceElemNum"
  intvec = obj%surfaceElemNum
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ VolumeElemNum
IF (IsAllocated(obj%volumeElemNum)) THEN
  dsetname = TRIM(group)//"/VolumeElemNum"
  intvec = obj%volumeElemNum
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

! READ NodeNum
IF (IsAllocated(obj%nodeNum)) THEN
  dsetname = TRIM(group)//"/NodeNum"
  intvec = obj%nodeNum
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=intvec)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods
