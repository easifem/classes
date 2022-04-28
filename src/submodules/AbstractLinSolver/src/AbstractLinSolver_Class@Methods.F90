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

SUBMODULE ( AbstractLinSolver_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                   getPreconditionOption
!----------------------------------------------------------------------------

MODULE PROCEDURE als_getPreconditionOption
  ans = obj%preconditionOption
END PROCEDURE als_getPreconditionOption

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE als_setTolerance
  IF(PRESENT(atol)) obj%atol=atol
  IF(PRESENT(rtol)) obj%rtol=rtol
END PROCEDURE als_setTolerance

!----------------------------------------------------------------------------
!                                                 setAbstractLinSolverParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setAbstractLinSolverParam
  INTEGER( I4B ) :: ierr
  !!
  !! engine
  !!
  ierr = param%set( &
    & key=trim(prefix) // "/engine", &
    & value=trim(engine) )
  !!
  !! solverName
  !!
  IF( PRESENT( solverName ) ) &
    & ierr = param%set( &
    & key=trim(prefix) // "/solverName", &
    & value=solverName )
  !!
  !! preconditionOption
  !!
  IF( PRESENT( preconditionOption ) ) &
    & ierr = param%set( &
    & key=trim(prefix) // "/preconditionOption", &
    & value=preconditionOption )
  !!
  !! maxIter
  !!
  IF( PRESENT( maxIter ) ) &
    & ierr = param%set( &
    & key=trim(prefix) // "/maxIter", &
    & value=maxIter )
  !!
  !! rtol
  !!
  IF( PRESENT( rtol ) ) &
    & ierr = param%set( &
    & key=trim(prefix) // "/rtol", &
    & value=rtol )
  !!
  !! atol
  !!
  IF( PRESENT( atol ) ) &
    & ierr = param%set( &
    & key=trim(prefix) // "/atol", &
    & value=atol )
  !!
  !! convergenceIn
  !!
  IF( PRESENT( convergenceIn ) ) &
    & ierr = param%set( &
    & key=trim(prefix) // "/convergenceIn", &
    & value=convergenceIn )
  !!
  !! convergenceType
  !!
  IF( PRESENT( convergenceType ) ) &
    & ierr = param%set( &
    & key=trim(prefix) // "/convergenceType", &
    & value=convergenceType )
  !!
  !! relativeToRHS
  !!
  IF( PRESENT( relativeToRHS ) ) &
    & ierr = param%set( &
    & key=trim(prefix) // "/relativeToRHS", &
    & value=relativeToRHS )
  !!
  !! KrylovSubspaceSize
  !!
  IF( PRESENT( KrylovSubspaceSize ) ) &
    & ierr = param%set( &
    & key=trim(prefix) // "/KrylovSubspaceSize", &
    & value=KrylovSubspaceSize )
  !!
END PROCEDURE setAbstractLinSolverParam

!----------------------------------------------------------------------------
!                                                 getAbstractLinSolverParam
!----------------------------------------------------------------------------

MODULE PROCEDURE getAbstractLinSolverParam
  INTEGER( I4B ) :: ierr
  CHARACTER(LEN=:), ALLOCATABLE :: char_var
  !!
  !! engine
  !!
  IF( PRESENT( engine ) ) THEN
    !!
    ALLOCATE ( &
      & CHARACTER(LEN=param%DataSizeInBytes( &
      & key=TRIM(prefix) // "/engine")) :: char_var)
    ierr = param%get( &
      & key=TRIM(prefix) // "/engine", &
      & VALUE=char_var)
    engine = TRIM(char_var)
    DEALLOCATE (char_var)
    !!
  END IF
  !!
  IF( PRESENT( solverName ) ) &
    & ierr = param%get( key=TRIM(prefix) // "/solverName", value=solverName )
  !!
  IF( PRESENT( preconditionOption ) ) &
    & ierr = param%get( key=TRIM(prefix) // "/preconditionOption", &
    & value=preconditionOption )
  !!
  IF( PRESENT( maxIter ) ) &
    & ierr = param%get( key=TRIM(prefix) // "/maxIter", &
    & value=maxIter )
  !!
  IF( PRESENT( atol ) ) &
    & ierr = param%get( key=TRIM(prefix) // "/atol", &
    & value=atol )
  !!
  IF( PRESENT( rtol ) ) &
    & ierr = param%get( key=TRIM(prefix) // "/rtol", &
    & value=rtol )
  !!
  IF( PRESENT( convergenceIn ) ) &
    & ierr = param%get( key=TRIM(prefix) // "/convergenceIn", &
    & value=convergenceIn )
  !!
  IF( PRESENT( convergenceType ) ) &
    & ierr = param%get( key=TRIM(prefix) // "/convergenceType", &
    & value=convergenceType )
  !!
  IF( PRESENT( relativeToRHS ) ) &
    & ierr = param%get( key=TRIM(prefix) // "/relativeToRHS", &
    & value=relativeToRHS )
  !!
  IF( PRESENT( KrylovSubspaceSize ) ) &
    & ierr = param%get( key=TRIM(prefix) // "/KrylovSubspaceSize", &
    & value=KrylovSubspaceSize )
  !!
END PROCEDURE getAbstractLinSolverParam

!----------------------------------------------------------------------------
!                                                     setDirichletBCIndices
!----------------------------------------------------------------------------

MODULE PROCEDURE als_setDirichletBCIndices
  CALL Reallocate( obj%dbcIndx, SIZE( indx ) )
  obj%dbcIndx = indx
END PROCEDURE als_setDirichletBCIndices

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods