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

ans = 0
IF (obj%pathExists("/VTKHDF/Steps")) THEN
  CALL obj%ReadAttribute("/VTKHDF/Steps", "NSteps", ans)
END IF

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
INTEGER(I4B) :: n

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

n = obj%GetTotalSteps()
IF (n .EQ. 0) THEN
  IF (ALLOCATED(vals)) DEALLOCATE (vals)
  ALLOCATE (vals(0))
ELSE
  CALL obj%ReadSlab(_STEPS_//"Values", 0, n, vals)
END IF

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
! summary: Part, geometry offsets, and sizes of a step
!
! If step is not present, or the file has no steps, the first part is
! returned.

FUNCTION GetStepInfo(obj, step) RESULT(ans)
  CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN), OPTIONAL :: step
  TYPE(StepInfo_) :: ans

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetStepInfo()"
  LOGICAL(LGT) :: isok
#endif
  INTEGER(I4B) :: nsteps, istep
  INTEGER(I4B), ALLOCATABLE :: temp1(:), temp2(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nsteps = 0
  IF (PRESENT(step)) nsteps = obj%GetTotalSteps()

  IF (nsteps .GT. 0) THEN

#ifdef DEBUG_VER
    isok = (step .GE. 1) .AND. (step .LE. nsteps)
    CALL AssertError1(isok, myName, &
                      'step '//ToString(step)// &
                      ' is out of range [1, '//ToString(nsteps)//']')
#endif

    istep = step - 1
    ans%hasStep = .TRUE.

    CALL obj%ReadSlab(_STEPS_//"PartOffsets", istep, 1, temp1)
    ans%part = temp1(1)
    CALL obj%ReadSlab(_STEPS_//"PointOffsets", istep, 1, temp1)
    ans%pointOffset = temp1(1)
    CALL obj%ReadSlab(_STEPS_//"CellOffsets", istep, 1, temp2)
    ans%cellOffset = temp2(1, 1)
    CALL obj%ReadSlab(_STEPS_//"ConnectivityIdOffsets", istep, 1, temp2)
    ans%connOffset = temp2(1, 1)
  END IF

  CALL obj%ReadSlab(_ROOT_//"NumberOfPoints", ans%part, 1, temp1)
  ans%nPoints = temp1(1)
  CALL obj%ReadSlab(_ROOT_//"NumberOfCells", ans%part, 1, temp1)
  ans%nCells = temp1(1)
  CALL obj%ReadSlab(_ROOT_//"NumberOfConnectivityIds", ans%part, 1, temp1)
  ans%nIds = temp1(1)

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
! summary: Read point or cell data of a step
!
! The offset of the data is read from Steps/<group>Offsets/<name>.
! If it does not exist, the geometry offset is used (VTKHDF rule).

SUBROUTINE ReadData(obj, group, name, geoOffset, ntuples, info, step, &
                    vals)
  CLASS(VtkHDF5File_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: group, name
  INTEGER(I4B), INTENT(IN) :: geoOffset, ntuples
  TYPE(StepInfo_), INTENT(IN) :: info
  INTEGER(I4B), INTENT(IN), OPTIONAL :: step
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: vals(:, :)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadData()"
  LOGICAL(LGT) :: isok
#endif
  CHARACTER(:), ALLOCATABLE :: path, offsetPath
  INTEGER(I4B) :: offset
  INTEGER(I4B), ALLOCATABLE :: temp(:)
  REAL(DFP), ALLOCATABLE :: temp1(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  path = _ROOT_//group//"/"//name

#ifdef DEBUG_VER
  isok = obj%pathExists(path)
  CALL AssertError1(isok, myName, &
                    'Data array not found: '//path)
#endif

  offset = 0
  IF (info%hasStep) THEN
    offsetPath = _STEPS_//group//"Offsets/"//name
    IF (obj%pathExists(offsetPath)) THEN
      CALL obj%ReadSlab(offsetPath, step - 1, 1, temp)
      offset = temp(1)
    ELSE
      offset = geoOffset
    END IF
  END IF

  IF (SIZE(obj%getDataShape(path)) .EQ. 1) THEN
    CALL obj%ReadSlab(path, offset, ntuples, temp1)
    IF (ALLOCATED(vals)) DEALLOCATE (vals)
    ALLOCATE (vals(1, ntuples))
    vals(1, :) = temp1
  ELSE
    CALL obj%ReadSlab(path, offset, ntuples, vals)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadData

!----------------------------------------------------------------------------
!                                                            Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadMethods
