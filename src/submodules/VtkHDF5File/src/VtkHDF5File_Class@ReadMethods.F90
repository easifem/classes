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

!> author: Shion Shimizu and Vikas Sharma
! date: 2026-10-24
! summary: Read methods of VtkHDF5File_

SUBMODULE(VtkHDF5File_Class) ReadMethods
USE Display_Method, ONLY: ToString
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE

!> Location of a step in the datasets
TYPE :: StepInfo_
  INTEGER(I4B) :: part = 0, pointOffset = 0, cellOffset = 0, &
                  connOffset = 0, nPoints = 0, nCells = 0, nIds = 0
  LOGICAL(LGT) :: hasStep = .FALSE.
END TYPE StepInfo_

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "VtkHDF5File_Class@ReadMethods.F90"
#endif

#include "./include/vtkhdf5_definitions.F90"

CONTAINS

!----------------------------------------------------------------------------
!                                                             GetTotalSteps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalSteps
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalSteps()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%readCache%isSet) CALL LoadReadCache(obj)
ans = obj%readCache%nSteps

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalSteps

!----------------------------------------------------------------------------
!                                                             GetTimeValues
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeValues
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTimeValues()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%readCache%isSet) CALL LoadReadCache(obj)
vals = obj%readCache%times

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTimeValues

!----------------------------------------------------------------------------
!                                                                  ReadMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadMesh
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadMesh()"
#endif
TYPE(StepInfo_) :: info

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

info = GetStepInfo(obj, step)

CALL obj%ReadSlab(_ROOT_//"Points", info%pointOffset, info%nPoints, points)
CALL obj%ReadSlab(_ROOT_//"Connectivity", info%connOffset, info%nIds, &
                  connectivity)
CALL obj%ReadSlab(_ROOT_//"Offsets", info%cellOffset + info%part, &
                  info%nCells + 1, offsets)
CALL obj%ReadSlab(_ROOT_//"Types", info%cellOffset, info%nCells, types)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadMesh

!----------------------------------------------------------------------------
!                                                             ReadPointData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadPointData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadPointData()"
#endif
TYPE(StepInfo_) :: info

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

info = GetStepInfo(obj, step)
CALL ReadData(obj, "PointData", NAME, info%pointOffset, info%nPoints, &
              info, step, vals)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadPointData

!----------------------------------------------------------------------------
!                                                              ReadCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadCellData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadCellData()"
#endif
TYPE(StepInfo_) :: info

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

info = GetStepInfo(obj, step)
CALL ReadData(obj, "CellData", NAME, info%cellOffset, info%nCells, &
              info, step, vals)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadCellData

!----------------------------------------------------------------------------
!                                                               GetStepInfo
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-28: values from obj%readCache (see LoadReadCache)
! summary: Part, geometry offsets, and sizes of a step
!
! If step is not present, or the file has no steps, the first part is
! returned.

FUNCTION GetStepInfo(obj, step) RESULT(ans)
  CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
  !! VTKHDF file
  INTEGER(I4B), INTENT(IN), OPTIONAL :: step
  !! step number (1 to NSteps)
  TYPE(StepInfo_) :: ans
  !! part, offsets and sizes of the step

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetStepInfo()"
  LOGICAL(LGT) :: isok
#endif
  INTEGER(I4B) :: nsteps, ipart

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (.NOT. obj%readCache%isSet) CALL LoadReadCache(obj)

  nsteps = 0
  IF (PRESENT(step)) nsteps = obj%readCache%nSteps

  IF (nsteps .GT. 0) THEN

#ifdef DEBUG_VER
    isok = (step .GE. 1) .AND. (step .LE. nsteps)
    CALL AssertError1(isok, myName, &
                      'step '//ToString(step)// &
                      ' is out of range [1, '//ToString(nsteps)//']')
#endif

    ans%hasStep = .TRUE.
    ans%part = obj%readCache%partOffsets(step)
    ans%pointOffset = obj%readCache%pointOffsets(step)
    ans%cellOffset = obj%readCache%cellOffsets(step)
    ans%connOffset = obj%readCache%connOffsets(step)
  END IF

  ipart = ans%part + 1

#ifdef DEBUG_VER
  isok = ipart .LE. SIZE(obj%readCache%nPoints)
  CALL AssertError1(isok, myName, &
                    'part '//ToString(ipart)//' not found, number of '// &
                    'parts is '//ToString(SIZE(obj%readCache%nPoints)))
#endif

  ans%nPoints = obj%readCache%nPoints(ipart)
  ans%nCells = obj%readCache%nCells(ipart)
  ans%nIds = obj%readCache%nIds(ipart)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END FUNCTION GetStepInfo

!----------------------------------------------------------------------------
!                                                                  ReadData
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-28: rank and offsets of the array from obj%readCache (see
!     GetReadArray); one component data is read with ReadSlab_ into
!     vals(1, :) without a temporary array
! summary: Read point or cell data of a step
!
! The offset of the data is taken from Steps/<group>Offsets/<name>.
! If it does not exist, the geometry offset is used (VTKHDF rule).

SUBROUTINE ReadData(obj, group, name, geoOffset, ntuples, info, step, &
                    vals)
  CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
  !! VTKHDF file
  CHARACTER(*), INTENT(IN) :: group
  !! "PointData" or "CellData"
  CHARACTER(*), INTENT(IN) :: name
  !! name of data array
  INTEGER(I4B), INTENT(IN) :: geoOffset
  !! geometry offset (points or cells) of the step
  INTEGER(I4B), INTENT(IN) :: ntuples
  !! number of tuples (points or cells) of the step
  TYPE(StepInfo_), INTENT(IN) :: info
  !! part, offsets and sizes of the step
  INTEGER(I4B), INTENT(IN), OPTIONAL :: step
  !! step number (1 to NSteps)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: vals(:, :)
  !! values, shape (nComp, ntuples)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadData()"
#endif
  CHARACTER(:), ALLOCATABLE :: path
  INTEGER(I4B) :: offset, indx, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  path = _ROOT_//group//"/"//name
  indx = GetReadArray(obj, group, name)

  offset = 0
  IF (info%hasStep) THEN
    IF (obj%readCache%arrays(indx)%hasOffsets) THEN
      offset = obj%readCache%arrays(indx)%offsets(step)
    ELSE
      offset = geoOffset
    END IF
  END IF

  IF (obj%readCache%arrays(indx)%RANK .EQ. 1) THEN
    ! one component: read directly in the row vals(1, :), which is
    ! contiguous since SIZE(vals, 1) = 1
    IF (ALLOCATED(vals)) THEN
      IF (SIZE(vals, 1) .NE. 1 .OR. SIZE(vals, 2) .NE. ntuples) THEN
        DEALLOCATE (vals)
      END IF
    END IF
    IF (.NOT. ALLOCATED(vals)) ALLOCATE (vals(1, ntuples))
    CALL obj%ReadSlab_(path, offset, ntuples, vals(1, :), tsize)
  ELSE
    CALL obj%ReadSlab(path, offset, ntuples, vals)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadData

!----------------------------------------------------------------------------
!                                                             LoadReadCache
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Read the Steps group and the NumberOf* datasets once
!
! Fills obj%readCache (see VtkReadCache_): NSteps, Steps/Values,
! Steps/PartOffsets, Steps/PointOffsets, Steps/CellOffsets,
! Steps/ConnectivityIdOffsets (SIZE nSteps), and NumberOfPoints,
! NumberOfCells, NumberOfConnectivityIds (SIZE number of parts). The
! data arrays of the cache are removed; they are added again by
! GetReadArray.

SUBROUTINE LoadReadCache(obj)
  CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
  !! VTKHDF file

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "LoadReadCache()"
#endif
  INTEGER(I4B) :: nsteps, nparts
  INTEGER(I4B), ALLOCATABLE :: ivals(:), ivals2(:, :)
  REAL(DFP), ALLOCATABLE :: rvals(:)
  INTEGER(I4B) :: izero(0)
  REAL(DFP) :: rzero(0)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ! NOTE: the data are read in local arrays and moved to the cache, so
  ! that a component of obj is not passed together with obj

  nsteps = 0
  IF (obj%PathExists("/VTKHDF/Steps")) THEN
    CALL obj%ReadAttribute("/VTKHDF/Steps", "NSteps", nsteps)
  END IF
  obj%readCache%nSteps = nsteps

  ! sizes of the parts
  nparts = obj%GetExtent(_ROOT_//"NumberOfPoints")
  IF (nparts .GT. 0) THEN
    CALL obj%ReadSlab(_ROOT_//"NumberOfPoints", 0, nparts, ivals)
    CALL MOVE_ALLOC(ivals, obj%readCache%nPoints)
    CALL obj%ReadSlab(_ROOT_//"NumberOfCells", 0, nparts, ivals)
    CALL MOVE_ALLOC(ivals, obj%readCache%nCells)
    CALL obj%ReadSlab(_ROOT_//"NumberOfConnectivityIds", 0, nparts, &
                      ivals)
    CALL MOVE_ALLOC(ivals, obj%readCache%nIds)
  ELSE
    obj%readCache%nPoints = izero
    obj%readCache%nCells = izero
    obj%readCache%nIds = izero
  END IF

  ! geometry of the steps
  IF (nsteps .GT. 0) THEN
    CALL obj%ReadSlab(_STEPS_//"Values", 0, nsteps, rvals)
    CALL MOVE_ALLOC(rvals, obj%readCache%times)
    CALL obj%ReadSlab(_STEPS_//"PartOffsets", 0, nsteps, ivals)
    CALL MOVE_ALLOC(ivals, obj%readCache%partOffsets)
    CALL obj%ReadSlab(_STEPS_//"PointOffsets", 0, nsteps, ivals)
    CALL MOVE_ALLOC(ivals, obj%readCache%pointOffsets)
    CALL obj%ReadSlab(_STEPS_//"CellOffsets", 0, nsteps, ivals2)
    obj%readCache%cellOffsets = ivals2(1, :)
    CALL obj%ReadSlab(_STEPS_//"ConnectivityIdOffsets", 0, nsteps, &
                      ivals2)
    obj%readCache%connOffsets = ivals2(1, :)
  ELSE
    obj%readCache%times = rzero
    obj%readCache%partOffsets = izero
    obj%readCache%pointOffsets = izero
    obj%readCache%cellOffsets = izero
    obj%readCache%connOffsets = izero
  END IF

  obj%readCache%nArrays = 0
  obj%readCache%isSet = .TRUE.

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE LoadReadCache

!----------------------------------------------------------------------------
!                                                              GetReadArray
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Index of a data array in the read cache (added if new)
!
! The array is searched in obj%readCache%arrays by group and name. If it
! is not found, a new entry is added: the rank of <group>/<name> is
! read, and the whole Steps/<group>Offsets/<name> (if it exists).

FUNCTION GetReadArray(obj, group, name) RESULT(ans)
  CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
  !! VTKHDF file, the read cache should be loaded
  CHARACTER(*), INTENT(IN) :: group
  !! "PointData" or "CellData"
  CHARACTER(*), INTENT(IN) :: name
  !! name of data array
  INTEGER(I4B) :: ans
  !! index of the array in obj%readCache%arrays

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetReadArray()"
  LOGICAL(LGT) :: isok
#endif
  INTEGER(I4B), PARAMETER :: minCapacity = 8
  CHARACTER(:), ALLOCATABLE :: path, offsetPath
  INTEGER(I4B) :: ii, n
  INTEGER(I4B), ALLOCATABLE :: ivals(:)
  LOGICAL(LGT) :: isOffsets
  TYPE(VtkReadDataArray_), ALLOCATABLE :: temp(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ans = 0
  loop1: DO ii = 1, obj%readCache%nArrays
    IF (obj%readCache%arrays(ii)%GROUP.EQ.GROUP .AND. &
        obj%readCache%arrays(ii)%NAME.EQ.NAME) THEN
      ans = ii
      EXIT loop1
    END IF
  END DO loop1

  IF (ans .EQ. 0) THEN
    path = _ROOT_//group//"/"//name

#ifdef DEBUG_VER
    isok = obj%PathExists(path)
    CALL AssertError1(isok, myName, 'Data array not found: '//path)
#endif

    n = obj%readCache%nArrays

    ! Allocate or double the capacity when it is full
    IF (.NOT. ALLOCATED(obj%readCache%arrays)) THEN
      ALLOCATE (obj%readCache%arrays(minCapacity))
    ELSE IF (n .EQ. SIZE(obj%readCache%arrays)) THEN
      ALLOCATE (temp(MAX(2 * n, minCapacity)))
      temp(1:n) = obj%readCache%arrays(1:n)
      CALL MOVE_ALLOC(temp, obj%readCache%arrays)
    END IF

    ans = n + 1
    obj%readCache%nArrays = ans

    obj%readCache%arrays(ans)%group = group
    obj%readCache%arrays(ans)%name = name
    obj%readCache%arrays(ans)%RANK = SIZE(obj%GetDataShape(path))

    ! read in a local array and move it, see LoadReadCache
    offsetPath = _STEPS_//group//"Offsets/"//name
    isOffsets = obj%PathExists(offsetPath)
    obj%readCache%arrays(ans)%hasOffsets = isOffsets
    IF (isOffsets) THEN
      CALL obj%ReadSlab(offsetPath, 0, obj%GetExtent(offsetPath), ivals)
      CALL MOVE_ALLOC(ivals, obj%readCache%arrays(ans)%offsets)
    ELSE IF (ALLOCATED(obj%readCache%arrays(ans)%offsets)) THEN
      DEALLOCATE (obj%readCache%arrays(ans)%offsets)
    END IF
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END FUNCTION GetReadArray

!----------------------------------------------------------------------------
!                                                            Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadMethods
