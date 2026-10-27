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
USE Display_Method, ONLY: ToString
USE ExceptionHandler_Class, ONLY: e
USE VTKMeshUtility, ONLY: VTKGetLagrangeGrid
USE VTKMeshUtility, ONLY: VTKGetLagrangeValues
USE VTKMeshUtility, ONLY: VTKGetLinearGrid
USE VTKMeshUtility, ONLY: VTKGetVertexValues
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
#endif

  INTEGER(I4B) :: nP, nC, nIds, pointOffset, connOffset, cellOffset, &
    partOffset

  nP = SIZE(points, 2)
  nC = SIZE(types)
  nIds = SIZE(connectivity)

  CALL obj%Append(_ROOT_ // "Points", points, pointOffset)
  CALL obj%Append(_ROOT_ // "Connectivity", connectivity, connOffset)
  CALL obj%Append(_ROOT_ // "Offsets", offsets)
  CALL obj%Append(_ROOT_ // "Types", types, cellOffset)
  CALL obj%Append(_ROOT_ // "NumberOfPoints", [nP], partOffset)
  CALL obj%Append(_ROOT_ // "NumberOfCells", [nC])
  CALL obj%Append(_ROOT_ // "NumberOfConnectivityIds", [nIds])

  obj%nPoints = nP
  obj%nCells = nC
  obj%nIds = nIds
  obj%pointOffset = pointOffset
  obj%connOffset = connOffset
  obj%cellOffset = cellOffset
  obj%partOffset = partOffset
  obj%isMeshWritten = math%yes

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteUnstructuredGrid

!----------------------------------------------------------------------------
!                                                                 BeginStep
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_BeginStep
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_BeginStep()"
#endif

  REAL(DFP) :: times(1)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (.NOT. (obj%IsInitiated() .AND. obj%isTransient)) THEN
#ifdef DEBUG_VER
    CALL e%RaiseDebug(modName // '::' // myName // ' - ' // &
                      'file is not initiated or not ' // &
                      'transient.')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] file is not initiated or not ' // &
                            'transient.')
#endif
    RETURN
  END IF

#ifdef DEBUG_VER
  CALL AssertError1(.NOT. obj%isStepOpen, myName, &
                    'EndStep should be called before the ' // &
                    'next BeginStep.')
#endif

  obj%stepCounter = obj%stepCounter + 1
  obj%isStepOpen = math%yes
  obj%isSkipped = MOD(obj%stepCounter - 1, obj%frequency) .NE. 0

  IF (.NOT. obj%isSkipped) THEN
    times(1) = time
    CALL obj%Append(_STEPS_ // "Values", times)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_BeginStep

!----------------------------------------------------------------------------
!                                                                   EndStep
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_EndStep
  CHARACTER(*), PARAMETER :: myName = "obj_EndStep()"
  CHARACTER(:), ALLOCATABLE :: path
  INTEGER(I4B) :: ii, nfill
  INTEGER(I4B), ALLOCATABLE :: fill1(:), fill2(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (.NOT. (obj%IsInitiated() .AND. obj%isTransient)) THEN
#ifdef DEBUG_VER
    CALL e%RaiseDebug(modName // '::' // myName // ' - ' // &
                      'file is not initiated or not ' // &
                      'transient.')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif
    RETURN
  END IF

#ifdef DEBUG_VER
  CALL AssertError1(obj%isStepOpen, myName, &
                    'BeginStep should be called before ' // &
                    'EndStep.')
#endif

  obj%isStepOpen = math%no

  IF (obj%isSkipped) THEN
    obj%isSkipped = math%no

#ifdef DEBUG_VER
    CALL e%RaiseDebug(modName // '::' // myName // ' - ' // &
                      'step is skipped.')
#endif

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END]')
#endif

    RETURN
  END IF

#ifdef DEBUG_VER
  CALL AssertError1(obj%isMeshWritten, myName, &
                    'WriteMesh should be called before ' // &
                    'the first EndStep.')
#endif

  obj%nSteps = obj%nSteps + 1

! Geometry of the step
  CALL obj%Append(_STEPS_ // "PartOffsets", [obj%partOffset])
  CALL obj%Append(_STEPS_ // "NumberOfParts", [1_I4B])
  CALL obj%Append(_STEPS_ // "PointOffsets", [obj%pointOffset])
  CALL obj%Append(_STEPS_ // "CellOffsets", &
                  RESHAPE([obj%cellOffset], [1, 1]))
  CALL obj%Append(_STEPS_ // "ConnectivityIdOffsets", &
                  RESHAPE([obj%connOffset], [1, 1]))

! Data arrays of the step
  DO ii = 1, obj%nArrays
    path = _STEPS_ // OffsetGroup(obj%arrayLoc(ii)) // "/" // &
      obj%arrayNames(ii)%chars()

    ! The array is written for the first time, fill the previous steps
    nfill = obj%nSteps - 1 - obj%GetExtent(path)
    IF (nfill .GT. 0) THEN
      ALLOCATE(fill1(nfill))
      fill1 = 0
      CALL obj%Append(path, fill1)
      DEALLOCATE(fill1)
    END IF

    CALL obj%Append(path, [obj%arrayOffset(ii)])

    IF (obj%arrayLoc(ii) .EQ. _LOC_FIELD_) THEN
      path = _STEPS_ // "FieldDataSizes/" // obj%arrayNames(ii)%chars()
      nfill = obj%nSteps - 1 - obj%GetExtent(path)
      IF (nfill .GT. 0) THEN
        ALLOCATE(fill2(2, nfill))
        fill2(1, :) = obj%arraySize(1, ii)
        fill2(2, :) = obj%arraySize(2, ii)
        CALL obj%Append(path, fill2)
        DEALLOCATE(fill2)
      END IF
      CALL obj%Append(path, RESHAPE(obj%arraySize(:, ii), [2, 1]))
    END IF
  END DO

  CALL obj%WriteAttribute("/VTKHDF/Steps", "NSteps", obj%nSteps)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_EndStep

!----------------------------------------------------------------------------
!                                                            WritePointData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WritePointData1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_WritePointData1()"
#endif
  REAL(DFP), ALLOCATABLE :: values(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (.NOT. CanWriteData(obj, myName)) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] data is not written.')
#endif
    RETURN
  END IF

  IF (obj%isLagrange) THEN
    CALL VTKGetLagrangeValues(field=field, values=values)
  ELSE
    CALL VTKGetVertexValues(field=field, nPoints=obj%nPoints, &
                            values=values)
  END IF

  IF (ALLOCATED(values)) THEN
    IF (SIZE(values, 1) .EQ. 1) THEN
      CALL WriteArray1(obj, _LOC_POINT_, NAME, values(1, :), myName)
    ELSE
      CALL WriteArray2(obj, _LOC_POINT_, NAME, values, myName)
    END IF
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (CanWriteData(obj, myName)) THEN
    CALL WriteArray1(obj, _LOC_POINT_, NAME, vals, myName)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WritePointData2

!----------------------------------------------------------------------------
!                                                            WritePointData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WritePointData3
  CHARACTER(*), PARAMETER :: myName = "obj_WritePointData3()"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (CanWriteData(obj, myName)) THEN
    CALL WriteArray2(obj, _LOC_POINT_, NAME, vals, myName)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WritePointData3

!----------------------------------------------------------------------------
!                                                             WriteCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteCellData1
  CHARACTER(*), PARAMETER :: myName = "obj_WriteCellData1()"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (CanWriteData(obj, myName)) THEN
    CALL WriteArray1(obj, _LOC_CELL_, NAME, vals, myName)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteCellData1

!----------------------------------------------------------------------------
!                                                             WriteCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteCellData2
  CHARACTER(*), PARAMETER :: myName = "obj_WriteCellData2()"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (CanWriteData(obj, myName)) THEN
    CALL WriteArray2(obj, _LOC_CELL_, NAME, vals, myName)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteCellData2

!----------------------------------------------------------------------------
!                                                            WriteFieldData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteFieldData
  CHARACTER(*), PARAMETER :: myName = "obj_WriteFieldData()"

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (CanWriteData(obj, myName)) THEN
    CALL WriteArray2(obj, _LOC_FIELD_, NAME, &
                     RESHAPE(vals, [SIZE(vals), 1]), myName)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteFieldData

!----------------------------------------------------------------------------
!                                                              CanWriteData
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Returns true if data can be written now
!
! Returns false (without error) in a skipped step.

FUNCTION CanWriteData(obj, myName) RESULT(ans)
  CLASS(VtkHDF5File_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: myName
  LOGICAL(LGT) :: ans

  ans = CheckWritable(obj, myName)
  IF (.NOT. ans) RETURN

  IF (.NOT. obj%isMeshWritten) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[ERROR] :: WriteMesh should be called before ' // &
                      'writing data.')
    ans = math%no
    RETURN
  END IF

  IF (.NOT. obj%isTransient) RETURN

  IF (.NOT. obj%isStepOpen) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[ERROR] :: Data should be written between ' // &
                      'BeginStep and EndStep in a transient file.')
    ans = math%no
    RETURN
  END IF

  ans = .NOT. obj%isSkipped
END FUNCTION CanWriteData

!----------------------------------------------------------------------------
!                                                             CheckWritable
!----------------------------------------------------------------------------

FUNCTION CheckWritable(obj, myName) RESULT(ans)
  CLASS(VtkHDF5File_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: myName
  LOGICAL(LGT) :: ans

  ans = obj%IsInitiated()
  IF (ans) ans = obj%IsWrite()

  IF (.NOT. ans) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[ERROR] :: VtkHDF5File_ should be initiated with ' // &
                      'mode NEW.')
  END IF
END FUNCTION CheckWritable

!----------------------------------------------------------------------------
!                                                               WriteArray1
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Append a rank-1 data array and register its offset

SUBROUTINE WriteArray1(obj, loc, name, vals, myName)
  CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: loc
  CHARACTER(*), INTENT(IN) :: name
  REAL(DFP), INTENT(IN) :: vals(:)
  CHARACTER(*), INTENT(IN) :: myName

  INTEGER(I4B) :: offset

  IF (.NOT. CheckSize(obj, loc, SIZE(vals), name, myName)) RETURN
  CALL obj%Append(_ROOT_ // DataGroup(loc) // "/" // name, vals, offset)
  CALL RegisterArray(obj, loc, name, offset, 1, SIZE(vals))
END SUBROUTINE WriteArray1

!----------------------------------------------------------------------------
!                                                               WriteArray2
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Append a rank-2 data array and register its offset

SUBROUTINE WriteArray2(obj, loc, name, vals, myName)
  CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: loc
  CHARACTER(*), INTENT(IN) :: name
  REAL(DFP), INTENT(IN) :: vals(:, :)
  CHARACTER(*), INTENT(IN) :: myName

  INTEGER(I4B) :: offset

  IF (.NOT. CheckSize(obj, loc, SIZE(vals, 2), name, myName)) RETURN
  CALL obj%Append(_ROOT_ // DataGroup(loc) // "/" // name, vals, offset)
  CALL RegisterArray(obj, loc, name, offset, SIZE(vals, 1), &
                     SIZE(vals, 2))
END SUBROUTINE WriteArray2

!----------------------------------------------------------------------------
!                                                                 CheckSize
!----------------------------------------------------------------------------

FUNCTION CheckSize(obj, loc, ntuples, name, myName) RESULT(ans)
  CLASS(VtkHDF5File_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: loc, ntuples
  CHARACTER(*), INTENT(IN) :: name, myName
  LOGICAL(LGT) :: ans

  INTEGER(I4B) :: nexpected

  SELECT CASE (loc)
  CASE (_LOC_POINT_)
    nexpected = obj%nPoints
  CASE (_LOC_CELL_)
    nexpected = obj%nCells
  CASE DEFAULT
    nexpected = ntuples
  END SELECT

  ans = ntuples .EQ. nexpected
  IF (.NOT. ans) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[ERROR] :: Size of ' // name // ' (' // &
                      ToString(ntuples) // ') does not match the mesh (' // &
                      ToString(nexpected) // ')')
  END IF
END FUNCTION CheckSize

!----------------------------------------------------------------------------
!                                                             RegisterArray
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Add a data array (if new) and set its latest offset and size

SUBROUTINE RegisterArray(obj, loc, name, offset, ncomp, ntuples)
  CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: loc
  CHARACTER(*), INTENT(IN) :: name
  INTEGER(I4B), INTENT(IN) :: offset, ncomp, ntuples

  INTEGER(I4B) :: ii, indx, n
  TYPE(String), ALLOCATABLE :: tempNames(:)
  INTEGER(I4B), ALLOCATABLE :: tempLoc(:), tempOffset(:), tempSize(:, :)

  indx = 0
  loop1: DO ii = 1, obj%nArrays
    IF (obj%arrayLoc(ii) .EQ. loc .AND. obj%arrayNames(ii) == name) THEN
      indx = ii
      EXIT loop1
    END IF
  END DO loop1

  IF (indx .EQ. 0) THEN
    n = obj%nArrays
    ALLOCATE(tempNames(n + 1), tempLoc(n + 1), tempOffset(n + 1), &
             tempSize(2, n + 1))
    IF (n .GT. 0) THEN
      tempNames(1:n) = obj%arrayNames(1:n)
      tempLoc(1:n) = obj%arrayLoc(1:n)
      tempOffset(1:n) = obj%arrayOffset(1:n)
      tempSize(:, 1:n) = obj%arraySize(:, 1:n)
    END IF
    tempNames(n + 1) = name
    tempLoc(n + 1) = loc
    CALL MOVE_ALLOC(tempNames, obj%arrayNames)
    CALL MOVE_ALLOC(tempLoc, obj%arrayLoc)
    CALL MOVE_ALLOC(tempOffset, obj%arrayOffset)
    CALL MOVE_ALLOC(tempSize, obj%arraySize)
    obj%nArrays = n + 1
    indx = n + 1
  END IF

  obj%arrayOffset(indx) = offset
  obj%arraySize(1, indx) = ncomp
  obj%arraySize(2, indx) = ntuples
END SUBROUTINE RegisterArray

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

  ans = DataGroup(loc) // "Offsets"
END FUNCTION OffsetGroup

!----------------------------------------------------------------------------
!                                                            Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE WriteMethods
