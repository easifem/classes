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

SUBMODULE(Domain_Class) ShapedataMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                        InitiateShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_InitiateElemSD
INTEGER(I4B) :: ii
CLASS(Mesh_), POINTER :: meshptr

DO ii = 1, obj%getTotalMesh(dim=dim)
  meshptr => obj%getMeshPointer(dim=dim, entitynum=ii)
  CALL meshptr%initiateElemSD(&
    & orderSpace=orderSpace(ii),  &
    & linSpaceElem=linSpaceElem, &
    & spaceElem=spaceElem, &
    & quadTypeSpace=quadTypeSpace, &
    & continuityTypeForSpace=continuityTypeForSpace, &
    & interpolTypeForSpace=interpolTypeForSpace, &
    & orderTime=orderTime, &
    & linTimeElem=linTimeElem, &
    & timeElem=timeElem, &
    & quadTypeTime=quadTypeTime, &
    & continuityTypeForTime=continuityTypeForTime, &
    & interpolTypeForTime=interpolTypeForTime, &
    & tvec=tvec)
END DO
NULLIFY(meshptr)
END PROCEDURE Domain_InitiateElemSD

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!
END SUBMODULE ShapedataMethods
