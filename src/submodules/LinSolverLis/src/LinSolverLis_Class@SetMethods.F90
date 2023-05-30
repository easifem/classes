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

SUBMODULE(LinSolverLis_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Set
CHARACTER(*), PARAMETER :: myName = "ls_Set"
INTEGER(I4B) :: s(2)
CHARACTER(100) :: engine

CALL Amat%GetParam(engine=engine)

IF (TRIM(engine) .NE. "LIS_OMP") THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'engine of Amat should be LIS_OMP, but give engine is = ' &
  & //TRIM(engine))
END IF

s = Amat%SHAPE()
CALL obj%SetParam( &
  & Amat=Amat, &
  & localNumColumn=s(2), &
  & localNumRow=s(1), &
  & globalNumRow=s(1), &
  & globalNumColumn=s(2))

END PROCEDURE ls_Set

END SUBMODULE SetMethods
