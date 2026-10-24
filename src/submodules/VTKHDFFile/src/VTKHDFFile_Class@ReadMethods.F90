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
! summary: Read methods of VTKHDFFile_

SUBMODULE(VTKHDFFile_Class) ReadMethods
USE Display_Method, ONLY: ToString
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE

CHARACTER(*), PARAMETER :: ROOT = "/VTKHDF/", STEPS = "/VTKHDF/Steps/"

!> Location of a step in the datasets
TYPE :: StepInfo_
  INTEGER(I4B) :: part = 0, pointOffset = 0, cellOffset = 0, &
                  connOffset = 0, nPoints = 0, nCells = 0, nIds = 0
  LOGICAL(LGT) :: hasStep = .FALSE.
END TYPE StepInfo_

CONTAINS

!----------------------------------------------------------------------------
!                                                             GetTotalSteps
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalSteps
ans = 0
IF (.NOT. obj%pathExists("/VTKHDF/Steps")) RETURN
CALL obj%ReadAttribute("/VTKHDF/Steps", "NSteps", ans)
END PROCEDURE obj_GetTotalSteps

!----------------------------------------------------------------------------
!                                                             GetTimeValues
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeValues
INTEGER(I4B) :: n

n = obj%GetTotalSteps()
IF (n .EQ. 0) THEN
  IF (ALLOCATED(vals)) DEALLOCATE (vals)
  ALLOCATE (vals(0))
  RETURN
END IF
CALL obj%ReadSlab(STEPS//"Values", 0, n, vals)
END PROCEDURE obj_GetTimeValues

!----------------------------------------------------------------------------
!                                                                  ReadMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadMesh
TYPE(StepInfo_) :: info

info = GetStepInfo(obj, step, "obj_ReadMesh()")

CALL obj%ReadSlab(ROOT//"Points", info%pointOffset, info%nPoints, points)
CALL obj%ReadSlab(ROOT//"Connectivity", info%connOffset, info%nIds, &
                  connectivity)
CALL obj%ReadSlab(ROOT//"Offsets", info%cellOffset + info%part, &
                  info%nCells + 1, offsets)
CALL obj%ReadSlab(ROOT//"Types", info%cellOffset, info%nCells, types)
END PROCEDURE obj_ReadMesh

!----------------------------------------------------------------------------
!                                                             ReadPointData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadPointData
TYPE(StepInfo_) :: info

info = GetStepInfo(obj, step, "obj_ReadPointData()")
CALL ReadData(obj, "PointData", name, info%pointOffset, info%nPoints, &
              info, step, vals)
END PROCEDURE obj_ReadPointData

!----------------------------------------------------------------------------
!                                                              ReadCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadCellData
TYPE(StepInfo_) :: info

info = GetStepInfo(obj, step, "obj_ReadCellData()")
CALL ReadData(obj, "CellData", name, info%cellOffset, info%nCells, &
              info, step, vals)
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

FUNCTION GetStepInfo(obj, step, myName) RESULT(ans)
  CLASS(VTKHDFFile_), INTENT(INOUT) :: obj
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: step
  CHARACTER(*), INTENT(IN) :: myName
  TYPE(StepInfo_) :: ans

  INTEGER(I4B) :: nsteps, istep
  INTEGER(I4B), ALLOCATABLE :: temp1(:), temp2(:, :)

  IF (PRESENT(step)) THEN
    nsteps = obj%GetTotalSteps()

    IF (nsteps .GT. 0) THEN
      IF (step .LT. 1 .OR. step .GT. nsteps) THEN
        CALL e%RaiseError(modName//'::'//myName//' - '// &
                          '[ERROR] :: step '//ToString(step)// &
                          ' is out of range [1, '//ToString(nsteps)//']')
        RETURN
      END IF

      istep = step - 1
      ans%hasStep = .TRUE.

      CALL obj%ReadSlab(STEPS//"PartOffsets", istep, 1, temp1)
      ans%part = temp1(1)
      CALL obj%ReadSlab(STEPS//"PointOffsets", istep, 1, temp1)
      ans%pointOffset = temp1(1)
      CALL obj%ReadSlab(STEPS//"CellOffsets", istep, 1, temp2)
      ans%cellOffset = temp2(1, 1)
      CALL obj%ReadSlab(STEPS//"ConnectivityIdOffsets", istep, 1, temp2)
      ans%connOffset = temp2(1, 1)
    END IF
  END IF

  CALL obj%ReadSlab(ROOT//"NumberOfPoints", ans%part, 1, temp1)
  ans%nPoints = temp1(1)
  CALL obj%ReadSlab(ROOT//"NumberOfCells", ans%part, 1, temp1)
  ans%nCells = temp1(1)
  CALL obj%ReadSlab(ROOT//"NumberOfConnectivityIds", ans%part, 1, temp1)
  ans%nIds = temp1(1)
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
  CLASS(VTKHDFFile_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: group, name
  INTEGER(I4B), INTENT(IN) :: geoOffset, ntuples
  TYPE(StepInfo_), INTENT(IN) :: info
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: step
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: vals(:, :)

  CHARACTER(*), PARAMETER :: myName = "ReadData()"
  CHARACTER(:), ALLOCATABLE :: path, offsetPath
  INTEGER(I4B) :: offset
  INTEGER(I4B), ALLOCATABLE :: temp(:)
  REAL(DFP), ALLOCATABLE :: temp1(:)

  path = ROOT//group//"/"//name

  IF (.NOT. obj%pathExists(path)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[ERROR] :: Data array not found: '//path)
    RETURN
  END IF

  offset = 0
  IF (info%hasStep) THEN
    offsetPath = STEPS//group//"Offsets/"//name
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
END SUBROUTINE ReadData

END SUBMODULE ReadMethods
