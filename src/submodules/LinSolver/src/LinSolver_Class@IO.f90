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

SUBMODULE( LinSolver_Class ) IO
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Display
  INTEGER( I4B ) :: I
  I = INPUT( option=unitno, default=stdout )

  CALL Display( msg, "#", unitNo=I )
  CALL Display(  obj%isInitiated, "# isInitiated : ", unitNo=I )
  CALL Display(  obj%solverName, "# solverName : ", unitNo=I )
  CALL Display(  obj%preconditionOption, "# preconditionOption : ", unitNo=I )
  CALL Display(  obj%convergenceIn, "# convergenceIn : ", unitNo=I )
  CALL Display(  obj%convergenceType, "# convergenceType : ", unitNo=I )
  CALL Display(  obj%maxIter, "# maxIter : ", unitNo=I )
  CALL Display(  obj%relativeToRHS, "# relativeToRHS : ", unitNo=I )
  CALL Display(  obj%KrylovSubspaceSize, "# KrylovSubspaceSize : ", unitNo=I )
  CALL Display(  obj%atol, "# atol : ", unitNo=I )
  CALL Display(  obj%rtol, "# rtol : ", unitNo=I )
  CALL Display(  obj%ierr, "# ierr : ", unitNo=I )
  CALL Display(  obj%iter, "# iter : ", unitNo=I )
  IF( ASSOCIATED( obj%Amat ) ) THEN
    CALL Display( "Amat is associated", unitNo=I)
  ELSE
    CALL Display( "Amat is NOT associated", unitNo=I)
  END IF
  CALL Display(  obj%IPAR, "# IPAR : ", unitNo=I )
  CALL Display(  obj%FPAR, "# FPAR : ", unitNo=I )
  IF( ALLOCATED( obj%RES ) ) &
    & CALL Display(  obj%RES, "# RES : ", unitNo=I )
  IF( ALLOCATED( obj%W ) ) &
    & CALL Display(  obj%W, "# W : ", unitNo=I )
END PROCEDURE ls_Display

END SUBMODULE IO