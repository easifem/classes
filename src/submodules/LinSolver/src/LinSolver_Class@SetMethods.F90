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

SUBMODULE( LinSolver_Class ) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This subroutine allocates the workspace required for the linear solver
!
!### Introduction
!
! This routine allocates the workspace required for the linear solver

SUBROUTINE AllocateWorkSpace( W, IPAR, solverName, n )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: W( : )
  INTEGER( I4B ), INTENT( INOUT ) :: IPAR( : )
  INTEGER( I4B ), INTENT( IN ) :: solverName
  INTEGER( I4B ), INTENT( IN ) :: n

  INTEGER( I4B ) :: i, m

  SELECT CASE( solverName )
    CASE( LIS_CG, LIS_CGNR )
      i = 5 * n
    CASE( LIS_BICG )
      i = 7 * n
    CASE( LIS_DBICG )
      i = 11 * n
    CASE( LIS_BICGSTAB )
      i = 8 * n
    CASE( LIS_TFQMR )
      i = 11 * n
    CASE( LIS_ORTHOMIN, LIS_GMRES )
      m = INPUT( default=15, option=IPAR(5) )
      i = ( n + 3 ) * ( m + 2) + ( m + 1 ) * m / 2
    CASE( LIS_FGMRES )
      m = INPUT( default=15, option=IPAR(5) )
      i = 2 * n * ( m + 1 ) + ( m + 1 ) * m / 2 + 3 * m + 2
    CASE( LIS_DQGMRES )
      m = INPUT( default=15, option=IPAR(5) ) + 1
      i = n + m * ( 2 * n + 4 )
  END SELECT
  IPAR( 4 ) = i
  CALL Reallocate( W, i )
END SUBROUTINE AllocateWorkSpace

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Set
  CHARACTER( LEN = * ), PARAMETER :: myName="ls_Set"
  INTEGER( I4B ) :: s(2)
  obj%Amat => Amat
  SELECT TYPE( Amat )
  TYPE IS ( MatrixField_ )
    s = Amat%SHAPE()
  CLASS DEFAULT
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Type of Amat cannot be recognized, it should be MatrixField_ ')
  END SELECT
  obj%localNumRow = s(1)
  obj%localNumColumn = s(2)
  obj%globalNumRow = s(1)
  obj%globalNumColumn = s(2)
  CALL AllocateWorkSpace( W=obj%W, n=obj%globalNumRow, &
    & solverName=obj%solverName, IPAR=obj%IPAR)
END PROCEDURE ls_Set

END SUBMODULE SetMethods