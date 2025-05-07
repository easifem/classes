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
! date: 18 June 2021
! summary: This submodule contains methods for domain object

SUBMODULE(FEDomain_Class) GetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             GetMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshPointer1

IF (PRESENT(dim)) THEN
  SELECT CASE (dim)
  CASE (0)
    ans => obj%meshPoint
  CASE (1)
    ans => obj%meshCurve
  CASE (2)
    ans => obj%meshSurface
  CASE (3)
    ans => obj%meshVolume
  END SELECT
  RETURN
END IF

ans => obj%mesh

END PROCEDURE obj_GetMeshPointer1

! IF (PRESENT(meshVolume)) meshVolume => obj%meshVolume
! IF (PRESENT(meshSurface)) meshSurface => obj%meshSurface
! IF (PRESENT(meshCurve)) meshCurve => obj%meshCurve
! IF (PRESENT(meshPoint)) meshPoint => obj%meshPoint
! IF (PRESENT(mesh)) mesh => obj%mesh

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
