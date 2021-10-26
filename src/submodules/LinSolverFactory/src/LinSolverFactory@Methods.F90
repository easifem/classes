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

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This modules is a factory for linear solvers

SUBMODULE (LinSolverFactory) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                          LinearSolverFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE LinearSolverFactory
  CHARACTER( LEN = * ), PARAMETER :: myName = "LinearSolverFactory"
  SELECT CASE( trim( engine ) )
  CASE( "NATIVE_SERIAL" )
    ALLOCATE( LinSolver_:: ans )
  CASE( "NATIVE_OMP" )
    CALL e%raiseError(modName//'::'//myName// &
    & 'NATIVE_OMP engine is not available currently!! We are working on it.')
  CASE( "NATIVE_MPI" )
    CALL e%raiseError(modName//'::'//myName// &
    & 'NATIVE_MPI engine is not available currently!! We are working on it.')
  CASE( "PETSC" )
    CALL e%raiseError(modName//'::'//myName// &
    & 'PETSC engine is not available currently!! We are working on it.')
  CASE( "LIS_SERIAL" )
    CALL e%raiseError(modName//'::'//myName// &
    & 'LIS_SERIAL engine is not available currently!! We are working on it.')
  CASE( "LIS_OMP" )
    CALL e%raiseError(modName//'::'//myName// &
      & 'LIS_OMP engine is not available currently!! We are working on it.')
  CASE( "LIS_MPI" )
    CALL e%raiseError(modName//'::'//myName// &
      & 'LIS_MPI engine is not available currently!! We are working on it.')
  END SELECT
END PROCEDURE LinearSolverFactory

END SUBMODULE Methods