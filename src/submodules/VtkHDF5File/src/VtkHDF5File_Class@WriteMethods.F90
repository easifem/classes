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

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Write methods (mesh, steps, and data) of VtkHDF5File_

SUBMODULE(VtkHDF5File_Class) WriteMethods
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "VtkHDF5File_Class@WriteMethods.F90"
#endif

#include "./include/vtkhdf5_definitions.F90"

CONTAINS

!----------------------------------------------------------------------------
!                                                                 WriteGrid
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Append a grid as a new part and update the mesh offsets

MODULE PROCEDURE obj_WriteUnstructuredGrid
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteUnstructuredGrid()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: nP, nC, nIds, pointOffset, connOffset, cellOffset, &
                partOffset

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nP = SIZE(points, 2)
nC = SIZE(types)
nIds = SIZE(connectivity)

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  "VtkHDF5File_ is not initiated.")
CALL AssertError1(obj%IsWrite(), myName, &
                  "VtkHDF5File_ should be initiated with mode NEW.")
CALL AssertError2(SIZE(points, 1), 3_I4B, myName, &
                  "SIZE(points, 1) and 3")
CALL AssertError2(SIZE(offsets), nC + 1, myName, &
                  "SIZE(offsets) and SIZE(types) + 1")
CALL AssertError1(offsets(1) .EQ. 0, myName, &
                  "offsets(1) should be 0.")
CALL AssertError2(offsets(nC + 1), nIds, myName, &
                  "offsets(nCells + 1) and SIZE(connectivity)")
isok = ALL(offsets(2:) .GE. offsets(1:nC))
CALL AssertError1(isok, myName, &
                  "offsets should be non decreasing.")
IF (nIds .GT. 0) THEN
  isok = (MINVAL(connectivity) .GE. 0) .AND. &
         (MAXVAL(connectivity) .LT. nP)
  CALL AssertError1(isok, myName, &
                    "connectivity should be zero based point ids "// &
                    "in [0, nPoints - 1].")
END IF
#endif

CALL obj%Append(_ROOT_//"Points", points, pointOffset)
CALL obj%Append(_ROOT_//"Connectivity", connectivity, connOffset)
CALL obj%Append(_ROOT_//"Offsets", offsets)
CALL obj%Append(_ROOT_//"Types", types, cellOffset)
CALL obj%Append(_ROOT_//"NumberOfPoints", [nP], partOffset)
CALL obj%Append(_ROOT_//"NumberOfCells", [nC])
CALL obj%Append(_ROOT_//"NumberOfConnectivityIds", [nIds])

obj%nPoints = nP
obj%nCells = nC
obj%nIds = nIds
obj%pointOffset = pointOffset
obj%connOffset = connOffset
obj%cellOffset = cellOffset
obj%partOffset = partOffset
obj%isMeshWritten = math%yes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_WriteUnstructuredGrid

!----------------------------------------------------------------------------
!                                                                 BeginTimeStep
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_BeginTimeStep
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_BeginTimeStep()"
#endif

REAL(DFP) :: times(1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. (obj%IsInitiated() .AND. obj%isTransient)) THEN
#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'file is not initiated or not '// &
                    'transient.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
CALL AssertError1(.NOT. obj%isStepOpen, myName, &
                  'EndTimeStep should be called before the '// &
                  'next BeginTimeStep.')
#endif

obj%stepCounter = obj%stepCounter + 1
obj%isStepOpen = math%yes
obj%isSkipped = MOD(obj%stepCounter - 1, obj%frequency) .NE. 0

IF (.NOT. obj%isSkipped) THEN
  times(1) = time
  CALL obj%Append(_STEPS_//"Values", times)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_BeginTimeStep

!----------------------------------------------------------------------------
!                                                                   EndTimeStep
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_EndTimeStep
CHARACTER(*), PARAMETER :: myName = "obj_EndTimeStep()"
CHARACTER(:), ALLOCATABLE :: path
INTEGER(I4B) :: ii, nfill
INTEGER(I4B), ALLOCATABLE :: fill1(:), fill2(:, :)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. (obj%IsInitiated() .AND. obj%isTransient)) THEN
#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'file is not initiated or not '// &
                    'transient.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

#ifdef DEBUG_VER
CALL AssertError1(obj%isStepOpen, myName, &
                  'BeginTimeStep should be called before '// &
                  'EndTimeStep.')
#endif

obj%isStepOpen = math%no

IF (obj%isSkipped) THEN
  obj%isSkipped = math%no

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'step is skipped.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END]')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
CALL AssertError1(obj%isMeshWritten, myName, &
                  'WriteUnstructuredGrid should be called before '// &
                  'the first EndTimeStep.')
#endif

obj%nSteps = obj%nSteps + 1

! Geometry of the step
CALL obj%Append(_STEPS_//"PartOffsets", [obj%partOffset])
CALL obj%Append(_STEPS_//"NumberOfParts", [1_I4B])
CALL obj%Append(_STEPS_//"PointOffsets", [obj%pointOffset])
CALL obj%Append(_STEPS_//"CellOffsets", &
                RESHAPE([obj%cellOffset], [1, 1]))
CALL obj%Append(_STEPS_//"ConnectivityIdOffsets", &
                RESHAPE([obj%connOffset], [1, 1]))

! Data arrays of the step
DO ii = 1, obj%nArrays
  ASSOCIATE (arr => obj%arrays(ii))

    ! An array written for the first time in a later step: fill the
    ! previous steps (offset 0, and the current size of field data)
    nfill = obj%nSteps - 1 - arr%nStepsWritten

    path = _STEPS_//OffsetGroup(arr%loc)//"/"//arr%name%chars()
    IF (nfill .GT. 0) THEN
      ALLOCATE (fill1(nfill))
      fill1 = 0
      CALL obj%Append(path, fill1)
      DEALLOCATE (fill1)
    END IF
    CALL obj%Append(path, [arr%offset])

    IF (arr%loc .EQ. _LOC_FIELD_) THEN
      path = _STEPS_//"FieldDataSizes/"//arr%name%chars()
      IF (nfill .GT. 0) THEN
        ALLOCATE (fill2(2, nfill))
        fill2(1, :) = arr%ncomp
        fill2(2, :) = arr%ntuples
        CALL obj%Append(path, fill2)
        DEALLOCATE (fill2)
      END IF
      CALL obj%Append(path, RESHAPE([arr%ncomp, arr%ntuples], [2, 1]))
    END IF

    arr%nStepsWritten = obj%nSteps

  END ASSOCIATE
END DO

CALL obj%WriteAttribute("/VTKHDF/Steps", "NSteps", obj%nSteps)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_EndTimeStep

!----------------------------------------------------------------------------
!                                                            WritePointData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WritePointData1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WritePointData1()"
#endif
INTEGER(I4B) :: offset, ncomp, ntuples

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ncomp = 1
ntuples = SIZE(VALUE)

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  "VtkHDF5File_ is not initiated.")
CALL AssertError1(obj%IsWrite(), myName, &
                  "VtkHDF5File_ should be initiated with mode NEW.")
CALL AssertError1(obj%isMeshWritten, myName, &
                  "WriteUnstructuredGrid should be called before "// &
                  "writing point data.")
IF (obj%isTransient) THEN
  CALL AssertError1(obj%isStepOpen, myName, &
                    "Point data should be written between BeginTimeStep "// &
                    "and EndTimeStep in a transient file.")
END IF
CALL AssertError2(ntuples, obj%nPoints, myName, &
                  "number of tuples of "//NAME//" and nPoints")
#endif

! Nothing is written in a step which is skipped by frequency
IF (obj%isTransient .AND. obj%isSkipped) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] step is skipped.')
#endif
  RETURN
END IF

CALL obj%Append(_ROOT_//"PointData/"//NAME, VALUE, offset)
CALL obj%RegisterArray(_LOC_POINT_, NAME, offset, ncomp, ntuples)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_WritePointData1

!----------------------------------------------------------------------------
!                                                            WritePointData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WritePointData2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WritePointData2()"
#endif
INTEGER(I4B) :: offset, ncomp, ntuples

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ncomp = SIZE(VALUE, 1)
ntuples = SIZE(VALUE, 2)

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  "VtkHDF5File_ is not initiated.")
CALL AssertError1(obj%IsWrite(), myName, &
                  "VtkHDF5File_ should be initiated with mode NEW.")
CALL AssertError1(obj%isMeshWritten, myName, &
                  "WriteUnstructuredGrid should be called before "// &
                  "writing point data.")
IF (obj%isTransient) THEN
  CALL AssertError1(obj%isStepOpen, myName, &
                    "Point data should be written between BeginTimeStep "// &
                    "and EndTimeStep in a transient file.")
END IF
CALL AssertError2(ntuples, obj%nPoints, myName, &
                  "number of tuples of "//NAME//" and nPoints")
#endif

! Nothing is written in a step which is skipped by frequency
IF (obj%isTransient .AND. obj%isSkipped) THEN
#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'step is skipped.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL obj%Append(_ROOT_//"PointData/"//NAME, VALUE, offset)
CALL obj%RegisterArray(_LOC_POINT_, NAME, offset, ncomp, ntuples)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_WritePointData2

!----------------------------------------------------------------------------
!                                                             WriteCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteCellData1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteCellData1()"
#endif
INTEGER(I4B) :: offset, ncomp, ntuples

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ncomp = 1
ntuples = SIZE(vals)

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  "VtkHDF5File_ is not initiated.")
CALL AssertError1(obj%IsWrite(), myName, &
                  "VtkHDF5File_ should be initiated with mode NEW.")
CALL AssertError1(obj%isMeshWritten, myName, &
                  "WriteUnstructuredGrid should be called before "// &
                  "writing cell data.")
IF (obj%isTransient) THEN
  CALL AssertError1(obj%isStepOpen, myName, &
                    "Cell data should be written between BeginTimeStep "// &
                    "and EndTimeStep in a transient file.")
END IF
CALL AssertError2(ntuples, obj%nCells, myName, &
                  "number of tuples of "//NAME//" and nCells")
#endif

! Nothing is written in a step which is skipped by frequency
IF (obj%isTransient .AND. obj%isSkipped) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] step is skipped.')
#endif
  RETURN
END IF

CALL obj%Append(_ROOT_//"CellData/"//NAME, vals, offset)
CALL obj%RegisterArray(_LOC_CELL_, NAME, offset, ncomp, ntuples)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_WriteCellData1

!----------------------------------------------------------------------------
!                                                             WriteCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteCellData2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteCellData2()"
#endif
INTEGER(I4B) :: offset, ncomp, ntuples

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ncomp = SIZE(vals, 1)
ntuples = SIZE(vals, 2)

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  "VtkHDF5File_ is not initiated.")
CALL AssertError1(obj%IsWrite(), myName, &
                  "VtkHDF5File_ should be initiated with mode NEW.")
CALL AssertError1(obj%isMeshWritten, myName, &
                  "WriteUnstructuredGrid should be called before "// &
                  "writing cell data.")
IF (obj%isTransient) THEN
  CALL AssertError1(obj%isStepOpen, myName, &
                    "Cell data should be written between BeginTimeStep "// &
                    "and EndTimeStep in a transient file.")
END IF
CALL AssertError2(ntuples, obj%nCells, myName, &
                  "number of tuples of "//NAME//" and nCells")
#endif

! Nothing is written in a step which is skipped by frequency
IF (obj%isTransient .AND. obj%isSkipped) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] step is skipped.')
#endif
  RETURN
END IF

CALL obj%Append(_ROOT_//"CellData/"//NAME, vals, offset)
CALL obj%RegisterArray(_LOC_CELL_, NAME, offset, ncomp, ntuples)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_WriteCellData2

!----------------------------------------------------------------------------
!                                                            WriteFieldData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteFieldData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteFieldData()"
#endif
INTEGER(I4B) :: offset, ncomp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ncomp = SIZE(vals)

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  "VtkHDF5File_ is not initiated.")
CALL AssertError1(obj%IsWrite(), myName, &
                  "VtkHDF5File_ should be initiated with mode NEW.")
CALL AssertError1(obj%isMeshWritten, myName, &
                  "WriteUnstructuredGrid should be called before "// &
                  "writing field data.")
IF (obj%isTransient) THEN
  CALL AssertError1(obj%isStepOpen, myName, &
                    "Field data should be written between BeginTimeStep "// &
                    "and EndTimeStep in a transient file.")
END IF
#endif

! Nothing is written in a step which is skipped by frequency
IF (obj%isTransient .AND. obj%isSkipped) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] step is skipped.')
#endif
  RETURN
END IF

! vals is one tuple with ncomp components
CALL obj%Append(_ROOT_//"FieldData/"//NAME, &
                RESHAPE(vals, [ncomp, 1]), offset)
CALL obj%RegisterArray(_LOC_FIELD_, NAME, offset, ncomp, 1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_WriteFieldData

!----------------------------------------------------------------------------
!                                                             RegisterArray
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RegisterArray
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_RegisterArray()"
#endif
INTEGER(I4B), PARAMETER :: minCapacity = 8
INTEGER(I4B) :: ii, indx, n
TYPE(VtkDataArray_), ALLOCATABLE :: temp(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

indx = 0
loop1: DO ii = 1, obj%nArrays
  IF (obj%arrays(ii)%loc .EQ. loc .AND. &
      obj%arrays(ii)%name == NAME) THEN
    indx = ii
    EXIT loop1
  END IF
END DO loop1

IF (indx .EQ. 0) THEN
  n = obj%nArrays

  ! Allocate or double the capacity when it is full
  IF (.NOT. ALLOCATED(obj%arrays)) THEN
    ALLOCATE (obj%arrays(minCapacity))
  ELSE IF (n .EQ. SIZE(obj%arrays)) THEN
    ALLOCATE (temp(MAX(2 * n, minCapacity)))
    temp(1:n) = obj%arrays(1:n)
    CALL MOVE_ALLOC(temp, obj%arrays)
  END IF

  indx = n + 1
  obj%nArrays = indx
  obj%arrays(indx)%name = NAME
  obj%arrays(indx)%loc = loc
  obj%arrays(indx)%nStepsWritten = 0
END IF

obj%arrays(indx)%offset = offset
obj%arrays(indx)%ncomp = ncomp
obj%arrays(indx)%ntuples = ntuples

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_RegisterArray

!----------------------------------------------------------------------------
!                                                                 DataGroup
!----------------------------------------------------------------------------

FUNCTION DataGroup(loc) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: loc
  CHARACTER(:), ALLOCATABLE :: ans

  SELECT CASE (loc)
  CASE (_LOC_POINT_)
    ans = "PointData"
  CASE (_LOC_CELL_)
    ans = "CellData"
  CASE DEFAULT
    ans = "FieldData"
  END SELECT
END FUNCTION DataGroup

!----------------------------------------------------------------------------
!                                                               OffsetGroup
!----------------------------------------------------------------------------

FUNCTION OffsetGroup(loc) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: loc
  CHARACTER(:), ALLOCATABLE :: ans

  ans = DataGroup(loc)//"Offsets"
END FUNCTION OffsetGroup

!----------------------------------------------------------------------------
!                                                            Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE WriteMethods
