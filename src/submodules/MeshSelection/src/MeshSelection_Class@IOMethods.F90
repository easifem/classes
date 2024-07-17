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

SUBMODULE(MeshSelection_Class) IOMethods
USE Display_Method, ONLY: Display, Tostring, EqualLine, &
                          BlankLines

USE BoundingBox_Method, ONLY: bb_Display => Display, bb_GetValue => GetValue

USE GlobalData, ONLY: CHAR_LF, stdout

USE IntVector_Method, ONLY: intvector_Display => Display, &
                            ASSIGNMENT(=), &
                            isAllocated

USE TomlUtility, ONLY: GetValue

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

IF (obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
         '[CONFIG ERROR] :: MeshSelection_::object is already initiated, '// &
                    'deallocate it first!')
END IF

obj%isInitiated = .TRUE.

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
                 vals=obj%isSelectionByMeshID)
ELSE
  obj%isSelectionByMeshID = .FALSE.
END IF

! READ isSelectionByElemNum
dsetname = TRIM(group)//"/isSelectionByElemNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%isSelectionByElemNum)
ELSE
  obj%isSelectionByElemNum = .FALSE.
END IF

! READ isSelectionByNodeNum
dsetname = TRIM(group)//"/isSelectionByNodeNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%isSelectionByNodeNum)
ELSE
  obj%isSelectionByNodeNum = .FALSE.
END IF

! READ isSelectionByBox
dsetname = TRIM(group)//"/isSelectionByBox"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=obj%isSelectionByBox)
ELSE
  obj%isSelectionByBox = .FALSE.
END IF

! READ PointMeshID
dsetname = TRIM(group)//"/PointMeshID"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%PointMeshID = intvec
  obj%isSelectionByMeshID = .TRUE.
END IF

! READ CurveMeshID
dsetname = TRIM(group)//"/CurveMeshID"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%CurveMeshID = intvec
  obj%isSelectionByMeshID = .TRUE.
END IF

! READ SurfaceMeshID
dsetname = TRIM(group)//"/SurfaceMeshID"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%SurfaceMeshID = intvec
  obj%isSelectionByMeshID = .TRUE.
END IF

! READ VolumeMeshID
dsetname = TRIM(group)//"/VolumeMeshID"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%VolumeMeshID = intvec
  obj%isSelectionByMeshID = .TRUE.
END IF

! READ PointElemNum
dsetname = TRIM(group)//"/PointElemNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%PointElemNum = intvec
  obj%isSelectionByElemNum = .TRUE.
END IF

! READ CurveElemNum
dsetname = TRIM(group)//"/CurveElemNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%CurveElemNum = intvec
  obj%isSelectionByElemNum = .TRUE.
END IF

! READ SurfaceElemNum
dsetname = TRIM(group)//"/SurfaceElemNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%SurfaceElemNum = intvec
  obj%isSelectionByElemNum = .TRUE.
END IF

! READ VolumeElemNum
dsetname = TRIM(group)//"/VolumeElemNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%VolumeElemNum = intvec
  obj%isSelectionByElemNum = .TRUE.
END IF

! READ NodeNum
dsetname = TRIM(group)//"/NodeNum"
IF (hdf5%pathExists(dsetname%chars())) THEN
  CALL hdf5%READ(dsetname=dsetname%chars(), &
                 vals=intvec)
  obj%NodeNum = intvec
  obj%isSelectionByNodeNum = .TRUE.
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
IF (.NOT. obj%isInitiated) THEN
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
                vals=obj%isSelectionByMeshID)

! READ isSelectionByElemNum
dsetname = TRIM(group)//"/isSelectionByElemNum"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
                vals=obj%isSelectionByElemNum)

! READ isSelectionByNodeNum
dsetname = TRIM(group)//"/isSelectionByNodeNum"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
                vals=obj%isSelectionByNodeNum)

! READ isSelectionByBox
dsetname = TRIM(group)//"/isSelectionByBox"
CALL hdf5%WRITE(dsetname=dsetname%chars(), &
                vals=obj%isSelectionByBox)

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
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: bool1
INTEGER(I4B) :: ii

CALL EqualLine(unitNo=unitNo)
CALL Display(msg, unitNo=unitNo)
CALL EqualLine(unitNo=unitNo)
IF (.NOT. obj%isInitiated) THEN
  CALL BlankLines(unitNo=unitNo, nol=1_I4B)
  CALL Display("The object is not initiated, nothing to show!", &
               unitNo=unitNo)
  RETURN
  CALL EqualLine(unitNo=unitNo)
END IF

CALL BlankLines(unitNo=unitNo, nol=1_I4B)
CALL Display(obj%isInitiated, "IsInitiated :", unitNo=unitNo)
CALL Display(obj%IsSelectionByMeshID, "IsSelectionByMeshID : ", &
             unitNo=unitNo)
CALL Display(obj%IsSelectionByElemNum, "IsSelectionByElemNum : ", &
             unitNo=unitNo)
CALL Display(obj%IsSelectionByNodeNum, "IsSelectionByNodeNum : ", &
             unitNo=unitNo)
CALL Display(obj%IsSelectionByBox, "IsSelectionByBox : ", &
             unitNo=unitNo)
bool1 = IsAllocated(obj%pointMeshID)
CALL Display(bool1, "PointMeshID ALLOCATED :", unitNo=unitNo)
bool1 = IsAllocated(obj%curveMeshID)
CALL Display(bool1, "CurveMeshID ALLOCATED :", unitNo=unitNo)
bool1 = IsAllocated(obj%surfaceMeshID)
CALL Display(bool1, "SurfaceMeshID ALLOCATED :", unitNo=unitNo)
bool1 = IsAllocated(obj%volumeMeshID)
CALL Display(bool1, "VolumeMeshID ALLOCATED :", unitNo=unitNo)
bool1 = IsAllocated(obj%pointElemNum)
CALL Display(bool1, "PointElemNum ALLOCATED :", unitNo=unitNo)
bool1 = IsAllocated(obj%curveElemNum)
CALL Display(bool1, "CurveElemNum ALLOCATED :", unitNo=unitNo)
bool1 = IsAllocated(obj%surfaceElemNum)
CALL Display(bool1, "SurfaceElemNum ALLOCATED :", unitNo=unitNo)
bool1 = IsAllocated(obj%volumeElemNum)
CALL Display(bool1, "VolumeElemNum ALLOCATED :", unitNo=unitNo)
bool1 = IsAllocated(obj%pointNodeNum)
CALL Display(bool1, "PointNodeNum ALLOCATED :", unitNo=unitNo)
bool1 = IsAllocated(obj%curveNodeNum)
CALL Display(bool1, "CurveNodeNum ALLOCATED :", unitNo=unitNo)
bool1 = IsAllocated(obj%surfaceNodeNum)
CALL Display(bool1, "SurfaceNodeNum ALLOCATED :", unitNo=unitNo)
bool1 = IsAllocated(obj%volumeNodeNum)
CALL Display(bool1, "VolumeNodeNum ALLOCATED :", unitNo=unitNo)
bool1 = ALLOCATED(obj%pointBox)
CALL Display(bool1, "PointBox ALLOCATED :", unitNo=unitNo)
bool1 = ALLOCATED(obj%curveBox)
CALL Display(bool1, "CurveBox ALLOCATED :", unitNo=unitNo)
bool1 = ALLOCATED(obj%surfaceBox)
CALL Display(bool1, "SurfaceBox ALLOCATED :", unitNo=unitNo)
bool1 = ALLOCATED(obj%volumeBox)
CALL Display(bool1, "VolumeBox ALLOCATED :", unitNo=unitNo)

bool1 = IsAllocated(obj%pointMeshID)
IF (bool1) THEN
  CALL BlankLines(unitNo=unitNo, nol=1_I4B)
  CALL intvector_Display(obj%pointMeshID, "PointMeshID : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%curveMeshID)
IF (bool1) THEN
  CALL BlankLines(unitNo=unitNo, nol=1_I4B)
  CALL intvector_Display(obj%curveMeshID, "CurveMeshID : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%surfaceMeshID)
IF (bool1) THEN
  CALL BlankLines(unitNo=unitNo, nol=1_I4B)
  CALL intvector_Display(obj%surfaceMeshID, "SurfaceMeshID : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%volumeMeshID)
IF (bool1) THEN
  CALL BlankLines(unitNo=unitNo, nol=1_I4B)
  CALL intvector_Display(obj%volumeMeshID, "VolumeMeshID : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%pointElemNum)
IF (bool1) THEN
  CALL BlankLines(unitNo=unitNo, nol=1_I4B)
  CALL intvector_Display(obj%pointElemNum, "PointElemNum : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%curveElemNum)
IF (bool1) THEN
  CALL BlankLines(unitNo=unitNo, nol=1_I4B)
  CALL intvector_Display(obj%curveElemNum, "CurveElemNum : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%surfaceElemNum)
IF (bool1) THEN
  CALL BlankLines(unitNo=unitNo, nol=1_I4B)
  CALL intvector_Display(obj%surfaceElemNum, "SurfaceElemNum : ", &
                         unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%volumeElemNum)
IF (bool1) THEN
  CALL BlankLines(unitNo=unitNo, nol=1_I4B)
  CALL intvector_Display(obj%volumeElemNum, "VolumeElemNum : ", unitNo=unitNo)
END IF
bool1 = IsAllocated(obj%pointNodeNum)
IF (bool1) THEN
  CALL BlankLines(unitNo=unitNo, nol=1_I4B)
  CALL intvector_Display(obj%pointNodeNum, "PointNodeNum : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%curveNodeNum)
IF (bool1) THEN
  CALL BlankLines(unitNo=unitNo, nol=1_I4B)
  CALL intvector_Display(obj%curveNodeNum, "CurveNodeNum : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%surfaceNodeNum)
IF (bool1) THEN
  CALL BlankLines(unitNo=unitNo, nol=1_I4B)
CALL intvector_Display(obj%surfaceNodeNum, "SurfaceNodeNum : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%volumeNodeNum)
IF (bool1) THEN
  CALL BlankLines(unitNo=unitNo, nol=1_I4B)
  CALL intvector_Display(obj%volumeNodeNum, "VolumeNodeNum : ", unitNo=unitNo)
END IF

bool1 = ALLOCATED(obj%pointBox)
IF (bool1) THEN
  DO ii = 1, SIZE(obj%pointBox)
    CALL BlankLines(unitNo=unitNo, nol=1_I4B)
    CALL bb_Display(obj%pointBox(ii), &
                    "PointBox("//tostring(ii)//") : ", &
                    unitNo=unitNo)
  END DO
END IF

bool1 = ALLOCATED(obj%curveBox)
IF (bool1) THEN
  DO ii = 1, SIZE(obj%curveBox)
    CALL BlankLines(unitNo=unitNo, nol=1_I4B)
    CALL bb_Display(obj%curveBox(ii), "curveBox("//tostring(ii)//") : ", &
                    unitNo=unitNo)
  END DO
END IF

bool1 = ALLOCATED(obj%surfaceBox)
IF (bool1) THEN
  DO ii = 1, SIZE(obj%surfaceBox)
    CALL BlankLines(unitNo=unitNo, nol=1_I4B)
    CALL bb_Display(obj%surfaceBox(ii), "surfaceBox("//tostring(ii)//") : ", &
                    unitNo=unitNo)
  END DO
END IF

bool1 = ALLOCATED(obj%volumeBox)
IF (bool1) THEN
  DO ii = 1, SIZE(obj%volumeBox)
    CALL BlankLines(unitNo=unitNo, nol=1_I4B)
    CALL bb_Display(obj%volumeBox(ii), "volumeBox("//tostring(ii)//") : ", &
                    unitNo=unitNo)
  END DO
END IF

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportParamFromToml
CHARACTER(*), PARAMETER :: myName = "obj_ImportParamFromToml()"
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isSelectionByElemNum, isSelectionByNodeNum, &
                isSelectionByBox, isSelectionByMeshID

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL toml_get(table, "isSelectionByMeshID", isSelectionByMeshID, &
              .FALSE., origin=origin, stat=stat)

CALL toml_get(table, "isSelectionByNodeNum", isSelectionByNodeNum, &
              .FALSE., origin=origin, stat=stat)

CALL toml_get(table, "isSelectionByBox", isSelectionByBox, &
              .FALSE., origin=origin, stat=stat)

CALL toml_get(table, "isSelectionByElemNum", isSelectionByElemNum, &
              .FALSE., origin=origin, stat=stat)

CALL SetMeshSelectionParam(param=param, prefix=obj%GetPrefix(), &
                           isSelectionByMeshID=isSelectionByMeshID, &
                           isSelectionByNodeNum=isSelectionByNodeNum, &
                           isSelectionByBox=isSelectionByBox, &
                           isSelectionByElemNum=isSelectionByElemNum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportParamFromToml

!----------------------------------------------------------------------------
!                                                         ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
TYPE(ParameterList_) :: param
INTEGER(I4B), ALLOCATABLE :: aintvec(:)
TYPE(BoundingBox_), ALLOCATABLE :: box(:)
LOGICAL(LGT) :: bool1, isFound
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL param%Initiate()
CALL obj%ImportParamFromToml(param=param, table=table)
CALL obj%Initiate(param)
CALL param%DEALLOCATE()

! meshID
node => NULL()
CALL toml_get(table, "meshID", node, origin=origin, &
              stat=stat, requested=.FALSE.)

bool1 = obj%isSelectionByMeshID .AND. (.NOT. ASSOCIATED(node))
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
             '[CONFIG ERROR] :: You have set isSelectionByMeshID = .TRUE.'// &
                    ' but you have not provided meshID table.')
  RETURN
END IF

bool1 = ASSOCIATED(node) .AND. (.NOT. obj%isSelectionByMeshID)
IF (bool1) obj%isSelectionByMeshID = .TRUE.

IF (ASSOCIATED(node)) THEN
  ! read points
  CALL GetValue(table=node, key="point", VALUE=aintvec, &
                origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=0_I4B, meshID=aintvec, dom=dom)

  ! read lines
  CALL GetValue(table=node, key="line", VALUE=aintvec, &
                origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=1_I4B, meshID=aintvec, dom=dom)

  ! read surfaces
  CALL GetValue(table=node, key="surface", VALUE=aintvec, &
                origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=2_I4B, meshID=aintvec, dom=dom)

  ! read volumes
  CALL GetValue(table=node, key="volume", VALUE=aintvec, &
                origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=3_I4B, meshID=aintvec, dom=dom)
END IF

! box
node => NULL()
CALL toml_get(table, "box", node, origin=origin, &
              stat=stat, requested=.FALSE.)

bool1 = obj%isSelectionByBox .AND. (.NOT. ASSOCIATED(node))
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                '[CONFIG ERROR] :: You have set isSelectionByBox = .TRUE.'// &
                    ' but you have not provided [box] table.')
  RETURN
END IF

bool1 = ASSOCIATED(node) .AND. (.NOT. obj%isSelectionByBox)
IF (bool1) obj%isSelectionByBox = .TRUE.

IF (ASSOCIATED(node)) THEN
  ! read points
  CALL bb_GetValue(table=node, key="point", VALUE=box, &
                   origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=0_I4B, box=box, dom=dom)

  ! read lines
  CALL bb_GetValue(table=node, key="line", VALUE=box, &
                   origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=1_I4B, box=box, dom=dom)

  ! read surfaces
  CALL bb_GetValue(table=node, key="surface", VALUE=box, &
                   origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=2_I4B, box=box, dom=dom)

  ! read volumes
  CALL bb_GetValue(table=node, key="volume", VALUE=box, &
                   origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=3_I4B, box=box, dom=dom)
END IF

! elemNum
node => NULL()
CALL toml_get(table, "elemNum", node, origin=origin, &
              stat=stat, requested=.FALSE.)

bool1 = obj%isSelectionByElemNum .AND. (.NOT. ASSOCIATED(node))
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
            '[CONFIG ERROR] :: You have set isSelectionByElemNum = .TRUE.'// &
                    ' but you have not provided [elemNum] table.')
  RETURN
END IF

bool1 = ASSOCIATED(node) .AND. (.NOT. obj%isSelectionByElemNum)
IF (bool1) obj%isSelectionByElemNum = .TRUE.

IF (ASSOCIATED(node)) THEN
  ! read points
  CALL GetValue(table=node, key="point", VALUE=aintvec, &
                origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=0_I4B, elemNum=aintvec, dom=dom)

  ! read lines
  CALL GetValue(table=node, key="line", VALUE=aintvec, &
                origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=1_I4B, elemNum=aintvec, dom=dom)

  ! read surfaces
  CALL GetValue(table=node, key="surface", VALUE=aintvec, &
                origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=2_I4B, elemNum=aintvec, dom=dom)

  ! read volumes
  CALL GetValue(table=node, key="volume", VALUE=aintvec, &
                origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=3_I4B, elemNum=aintvec, dom=dom)
END IF

! nodeNum
node => NULL()
CALL toml_get(table, "nodeNum", node, origin=origin, &
              stat=stat, requested=.FALSE.)

bool1 = obj%isSelectionByElemNum .AND. (.NOT. ASSOCIATED(node))
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
            '[CONFIG ERROR] :: You have set isSelectionByElemNum = .TRUE.'// &
                    ' but you have not provided [nodeNum] table.')
  RETURN
END IF

bool1 = ASSOCIATED(node) .AND. (.NOT. obj%isSelectionByElemNum)
IF (bool1) obj%isSelectionByElemNum = .TRUE.

IF (ASSOCIATED(node)) THEN
  ! read points
  CALL GetValue(table=node, key="point", VALUE=aintvec, &
                origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=0_I4B, nodeNum=aintvec, dom=dom)

  ! read lines
  CALL GetValue(table=node, key="line", VALUE=aintvec, &
                origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=1_I4B, nodeNum=aintvec, dom=dom)

  ! read surfaces
  CALL GetValue(table=node, key="surface", VALUE=aintvec, &
                origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=2_I4B, nodeNum=aintvec, dom=dom)

  ! read volumes
  CALL GetValue(table=node, key="volume", VALUE=aintvec, &
                origin=origin, stat=stat, isFound=isFound)
  IF (isFound) CALL obj%Add(dim=3_I4B, nodeNum=aintvec, dom=dom)
END IF

CALL obj%Set()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                         ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

IF (PRESENT(afile)) THEN
  CALL GetValue(table=table, afile=afile)
ELSEIF (PRESENT(filename)) THEN
  CALL GetValue(table=table, filename=filename)
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                 '[ARG ERROR] :: either filename or afile should be present!')
  RETURN
END IF

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

IF (.NOT. ASSOCIATED(node)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                '[CONFIG ERROR] :: following error occured while reading '// &
             'the toml file :: cannot find ['//tomlName//"] table in config.")
END IF

CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF, &
               unitNo=stdout)
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml2

END SUBMODULE IOMethods
