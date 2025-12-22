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
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set()"
CHARACTER(128) :: engine
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: s(2)

#ifdef DEBUG_VER
CALL amat%GetParam(engine=engine)

isok = TRIM(engine) .EQ. "LIS_OMP"
CALL AssertError1(isok, myName, &
      'engine of amat should be LIS_OMP, but give engine is = '//TRIM(engine))
#endif

s = amat%SHAPE()

CALL obj%SetParam(amat=amat, localNumColumn=s(2), localNumRow=s(1), &
                  globalNumRow=s(1), globalNumColumn=s(2))
END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
