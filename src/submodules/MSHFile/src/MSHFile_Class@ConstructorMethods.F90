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

SUBMODULE(MSHFile_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_Final
CALL obj%DEALLOCATE()
END PROCEDURE msh_Final

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_Deallocate
INTEGER(I4B) :: ii
CHARACTER(*), PARAMETER :: myName="msh_Deallocate()"

CALL obj%FORMAT%DEALLOCATE()
CALL obj%PhysicalNames%DEALLOCATE()
CALL obj%Nodes%DEALLOCATE()
CALL obj%Elements%DEALLOCATE()
IF (ALLOCATED(obj%PointEntities)) THEN
  DO ii = 1, SIZE(obj%PointEntities)
    CALL obj%PointEntities(ii)%DEALLOCATE()
  END DO
  DEALLOCATE (obj%PointEntities)
END IF

IF (ALLOCATED(obj%CurveEntities)) THEN
  DO ii = 1, SIZE(obj%CurveEntities)
    CALL obj%CurveEntities(ii)%DEALLOCATE()
  END DO
  DEALLOCATE (obj%CurveEntities)
END IF

IF (ALLOCATED(obj%SurfaceEntities)) THEN
  DO ii = 1, SIZE(obj%SurfaceEntities)
    CALL obj%SurfaceEntities(ii)%DEALLOCATE()
  END DO
  DEALLOCATE (obj%SurfaceEntities)
END IF

IF (ALLOCATED(obj%VolumeEntities)) THEN
  DO ii = 1, SIZE(obj%VolumeEntities)
    CALL obj%VolumeEntities(ii)%DEALLOCATE()
  END DO
  DEALLOCATE (obj%VolumeEntities)
END IF

obj%nsd = 0
IF (ASSOCIATED(obj%buffer)) DEALLOCATE (obj%buffer)
NULLIFY (obj%buffer)
CALL TxtFileDeallocate(obj, Delete)
END PROCEDURE msh_Deallocate

!----------------------------------------------------------------------------
!                                                                        msh4
!----------------------------------------------------------------------------

! MODULE PROCEDURE msh_constuctor1
!   CALL ans%Initiate( Path, FileName, Extension, NSD )
! END PROCEDURE msh_constuctor1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_GetIntNodeNumber
CHARACTER(*), PARAMETER :: myName = "msh_GetIntNodeNumber"
INTEGER(I4B) :: n

SELECT CASE (dim)
CASE (0)

  IF (.NOT. ALLOCATED(obj%PointEntities)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'PointEntities not allocated')
  END IF

  n = SIZE(obj%PointEntities)
  IF (tag .GT. n) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'tag is out of bound')
  END IF

  ans = obj%PointEntities(tag)%GetIntNodeNumber()

CASE (1)

  IF (.NOT. ALLOCATED(obj%CurveEntities)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'CurveEntities not allocated')
  END IF

  n = SIZE(obj%CurveEntities)
  IF (tag .GT. n) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'tag is out of bound')
  END IF

  ans = obj%CurveEntities(tag)%GetIntNodeNumber()

CASE (2)

  IF (.NOT. ALLOCATED(obj%SurfaceEntities)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'SurfaceEntities not allocated')
  END IF

  n = SIZE(obj%SurfaceEntities)
  IF (tag .GT. n) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'tag is out of bound')
  END IF

  ans = obj%SurfaceEntities(tag)%GetIntNodeNumber()

CASE (3)

  IF (.NOT. ALLOCATED(obj%VolumeEntities)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'VolumeEntities not allocated')
  END IF

  n = SIZE(obj%VolumeEntities)
  IF (tag .GT. n) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'tag is out of bound')
  END IF

  ans = obj%VolumeEntities(tag)%GetIntNodeNumber()

END SELECT
END PROCEDURE msh_GetIntNodeNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE msh_SetNodeCoord
CHARACTER(*), PARAMETER :: myName = "msh_GetIntNodeNumber"
INTEGER(I4B) :: n

SELECT CASE (dim)
CASE (0)

  IF (.NOT. ALLOCATED(obj%PointEntities)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'PointEntities not allocated')
  END IF

  n = SIZE(obj%PointEntities)
  IF (tag .GT. n) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'tag is out of bound')
  END IF

  CALL obj%PointEntities(tag)%SetNodeCoord(NodeCoord)

CASE (1)

  IF (.NOT. ALLOCATED(obj%CurveEntities)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'CurveEntities not allocated')
  END IF

  n = SIZE(obj%CurveEntities)
  IF (tag .GT. n) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'tag is out of bound')
  END IF

  CALL obj%CurveEntities(tag)%SetNodeCoord(NodeCoord)

CASE (2)

  IF (.NOT. ALLOCATED(obj%SurfaceEntities)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'SurfaceEntities not allocated')
  END IF

  n = SIZE(obj%SurfaceEntities)
  IF (tag .GT. n) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'tag is out of bound')
  END IF

  CALL obj%SurfaceEntities(tag)%SetNodeCoord(NodeCoord)

CASE (3)

  IF (.NOT. ALLOCATED(obj%VolumeEntities)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'VolumeEntities not allocated')
  END IF

  n = SIZE(obj%VolumeEntities)
  IF (tag .GT. n) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'tag is out of bound')
  END IF

  CALL obj%VolumeEntities(tag)%SetNodeCoord(NodeCoord)

END SELECT
END PROCEDURE msh_SetNodeCoord

END SUBMODULE Methods
