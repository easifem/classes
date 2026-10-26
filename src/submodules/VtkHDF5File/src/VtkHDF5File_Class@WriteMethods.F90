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

INTEGER(I4B), PARAMETER :: LOC_POINT = 1, LOC_CELL = 2, LOC_FIELD = 3
CHARACTER(*), PARAMETER :: ROOT = "/VTKHDF/", STEPS = "/VTKHDF/Steps/"

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "VtkHDF5File_Class@WriteMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 WriteMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteMesh1
  CHARACTER(*), PARAMETER :: myName = "obj_WriteMesh1()"
  REAL(DFP), ALLOCATABLE :: points(:, :)
  INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
  INTEGER(INT8), ALLOCATABLE :: types(:)

  IF (.NOT. CanWriteMesh(obj, myName)) RETURN

  CALL VTKGetLinearGrid(mesh=mesh, points=points, &
                        connectivity=connectivity, offsets=offsets, &
                        types=types)
  CALL WriteGrid(obj, points, connectivity, offsets, types)
  obj%isLagrange = math%no
END PROCEDURE obj_WriteMesh1

!----------------------------------------------------------------------------
!                                                                 WriteMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteMesh2
  CHARACTER(*), PARAMETER :: myName = "obj_WriteMesh2()"
  REAL(DFP), ALLOCATABLE :: points(:, :)
  INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
  INTEGER(INT8), ALLOCATABLE :: types(:)

  IF (.NOT. CanWriteMesh(obj, myName)) RETURN

  CALL VTKGetLagrangeGrid(fedof=fedof, geofedof=geofedof, points=points, &
                          connectivity=connectivity, offsets=offsets, &
                          types=types)
  CALL WriteGrid(obj, points, connectivity, offsets, types)
  obj%isLagrange = math%yes
END PROCEDURE obj_WriteMesh2

!----------------------------------------------------------------------------
!                                                                 BeginStep
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_BeginStep
  CHARACTER(*), PARAMETER :: myName = "obj_BeginStep()"

  IF (.NOT. obj%isVTKHDFInit) RETURN
  IF (.NOT. obj%isTransient) RETURN

  IF (obj%isStepOpen) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[ERROR] :: EndStep should be called before the ' // &
                      'next BeginStep.')
    RETURN
  END IF

  obj%stepCounter = obj%stepCounter + 1
  obj%isStepOpen = math%yes
  obj%isSkipped = MOD(obj%stepCounter - 1, obj%frequency) .NE. 0
  IF (obj%isSkipped) RETURN

  CALL obj%Append(STEPS // "Values", [time])
END PROCEDURE obj_BeginStep

!----------------------------------------------------------------------------
!                                                                   EndStep
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_EndStep
  CHARACTER(*), PARAMETER :: myName = "obj_EndStep()"
  CHARACTER(:), ALLOCATABLE :: path
  INTEGER(I4B) :: ii, nfill
  INTEGER(I4B), ALLOCATABLE :: fill1(:), fill2(:, :)

  IF (.NOT. obj%isVTKHDFInit) RETURN
  IF (.NOT. obj%isTransient) RETURN

  IF (.NOT. obj%isStepOpen) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[ERROR] :: BeginStep should be called before ' // &
                      'EndStep.')
    RETURN
  END IF

  obj%isStepOpen = math%no

  IF (obj%isSkipped) THEN
    obj%isSkipped = math%no
    RETURN
  END IF

  IF (.NOT. obj%isMeshWritten) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[ERROR] :: WriteMesh should be called before ' // &
                      'the first EndStep.')
    RETURN
  END IF

  obj%nSteps = obj%nSteps + 1

! Geometry of the step
  CALL obj%Append(STEPS // "PartOffsets", [obj%partOffset])
  CALL obj%Append(STEPS // "NumberOfParts", [1_I4B])
  CALL obj%Append(STEPS // "PointOffsets", [obj%pointOffset])
  CALL obj%Append(STEPS // "CellOffsets", &
                  RESHAPE([obj%cellOffset], [1, 1]))
  CALL obj%Append(STEPS // "ConnectivityIdOffsets", &
                  RESHAPE([obj%connOffset], [1, 1]))

! Data arrays of the step
  DO ii = 1, obj%nArrays
    path = STEPS // OffsetGroup(obj%arrayLoc(ii)) // "/" // &
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

    IF (obj%arrayLoc(ii) .EQ. LOC_FIELD) THEN
      path = STEPS // "FieldDataSizes/" // obj%arrayNames(ii)%chars()
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
END PROCEDURE obj_EndStep

!----------------------------------------------------------------------------
!                                                            WritePointData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WritePointData1
  CHARACTER(*), PARAMETER :: myName = "obj_WritePointData1()"
  REAL(DFP), ALLOCATABLE :: values(:, :)

  IF (.NOT. CanWriteData(obj, myName)) RETURN

  IF (obj%isLagrange) THEN
    CALL VTKGetLagrangeValues(field=field, values=values)
  ELSE
    CALL VTKGetVertexValues(field=field, nPoints=obj%nPoints, &
                            values=values)
  END IF

  IF (.NOT. ALLOCATED(values)) RETURN

  IF (SIZE(values, 1) .EQ. 1) THEN
    CALL WriteArray1(obj, LOC_POINT, NAME, values(1, :), myName)
  ELSE
    CALL WriteArray2(obj, LOC_POINT, NAME, values, myName)
  END IF
END PROCEDURE obj_WritePointData1

!----------------------------------------------------------------------------
!                                                            WritePointData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WritePointData2
  CHARACTER(*), PARAMETER :: myName = "obj_WritePointData2()"
  IF (.NOT. CanWriteData(obj, myName)) RETURN
  CALL WriteArray1(obj, LOC_POINT, NAME, vals, myName)
END PROCEDURE obj_WritePointData2

!----------------------------------------------------------------------------
!                                                            WritePointData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WritePointData3
  CHARACTER(*), PARAMETER :: myName = "obj_WritePointData3()"
  IF (.NOT. CanWriteData(obj, myName)) RETURN
  CALL WriteArray2(obj, LOC_POINT, NAME, vals, myName)
END PROCEDURE obj_WritePointData3

!----------------------------------------------------------------------------
!                                                             WriteCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteCellData1
  CHARACTER(*), PARAMETER :: myName = "obj_WriteCellData1()"
  IF (.NOT. CanWriteData(obj, myName)) RETURN
  CALL WriteArray1(obj, LOC_CELL, NAME, vals, myName)
END PROCEDURE obj_WriteCellData1

!----------------------------------------------------------------------------
!                                                             WriteCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteCellData2
  CHARACTER(*), PARAMETER :: myName = "obj_WriteCellData2()"
  IF (.NOT. CanWriteData(obj, myName)) RETURN
  CALL WriteArray2(obj, LOC_CELL, NAME, vals, myName)
END PROCEDURE obj_WriteCellData2

!----------------------------------------------------------------------------
!                                                            WriteFieldData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteFieldData
  CHARACTER(*), PARAMETER :: myName = "obj_WriteFieldData()"
  IF (.NOT. CanWriteData(obj, myName)) RETURN
  CALL WriteArray2(obj, LOC_FIELD, NAME, RESHAPE(vals, [SIZE(vals), 1]), &
                   myName)
END PROCEDURE obj_WriteFieldData

!----------------------------------------------------------------------------
!                                                                 WriteGrid
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Append a grid as a new part and update the mesh offsets

SUBROUTINE WriteGrid(obj, points, connectivity, offsets, types)
  CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(IN) :: points(:, :)
  INTEGER(I4B), INTENT(IN) :: connectivity(:)
  INTEGER(I4B), INTENT(IN) :: offsets(:)
  INTEGER(INT8), INTENT(IN) :: types(:)

  INTEGER(I4B) :: nP, nC, nIds, pointOffset, connOffset, cellOffset, &
    partOffset

  nP = SIZE(points, 2)
  nC = SIZE(types)
  nIds = SIZE(connectivity)

  CALL obj%Append(ROOT // "Points", points, pointOffset)
  CALL obj%Append(ROOT // "Connectivity", connectivity, connOffset)
  CALL obj%Append(ROOT // "Offsets", offsets)
  CALL obj%Append(ROOT // "Types", types, cellOffset)
  CALL obj%Append(ROOT // "NumberOfPoints", [nP], partOffset)
  CALL obj%Append(ROOT // "NumberOfCells", [nC])
  CALL obj%Append(ROOT // "NumberOfConnectivityIds", [nIds])

  obj%nPoints = nP
  obj%nCells = nC
  obj%nIds = nIds
  obj%pointOffset = pointOffset
  obj%connOffset = connOffset
  obj%cellOffset = cellOffset
  obj%partOffset = partOffset
  obj%isMeshWritten = math%yes
END SUBROUTINE WriteGrid

!----------------------------------------------------------------------------
!                                                              CanWriteMesh
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Returns true if a mesh can be written now

FUNCTION CanWriteMesh(obj, myName) RESULT(ans)
  CLASS(VtkHDF5File_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: myName
  LOGICAL(LGT) :: ans

  ans = CheckWritable(obj, myName)
  IF (.NOT. ans) RETURN

  IF (obj%isTransient .AND. obj%isStepOpen) ans = .NOT. obj%isSkipped
END FUNCTION CanWriteMesh

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

  ans = obj%isVTKHDFInit
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
  CALL obj%Append(ROOT // DataGroup(loc) // "/" // name, vals, offset)
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
  CALL obj%Append(ROOT // DataGroup(loc) // "/" // name, vals, offset)
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
  CASE (LOC_POINT)
    nexpected = obj%nPoints
  CASE (LOC_CELL)
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
  DO ii = 1, obj%nArrays
    IF (obj%arrayLoc(ii) .EQ. loc .AND. obj%arrayNames(ii) == name) THEN
      indx = ii
      EXIT
    END IF
  END DO

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
  CASE (LOC_POINT)
    ans = "PointData"
  CASE (LOC_CELL)
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
