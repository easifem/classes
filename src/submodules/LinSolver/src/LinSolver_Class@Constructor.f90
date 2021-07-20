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

SUBMODULE( LinSolver_Class ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     setPreconditionOption
!----------------------------------------------------------------------------

SUBROUTINE setPreconditionOption( IPAR, PRECOND_TYPE )
  INTEGER( I4B ), INTENT( INOUT ) :: IPAR( : )
  INTEGER( I4B ), INTENT( IN ) :: PRECOND_TYPE
  SELECT CASE( PRECOND_TYPE )
  CASE( NO_PRECONDITION )
    IPAR(2) = 0
  CASE( LEFT_PRECONDITION )
    IPAR(2) = 1
  CASE( RIGHT_PRECONDITION )
    IPAR(2) = 2
  CASE( LEFT_RIGHT_PRECONDITION )
    IPAR(2) = 3
  END SELECT
END SUBROUTINE setPreconditionOption

!----------------------------------------------------------------------------
!                                                     setKrylovSubspaceSize
!----------------------------------------------------------------------------

SUBROUTINE setKrylovSubspaceSize( IPAR, m )
  INTEGER( I4B ), INTENT( INOUT ) :: IPAR( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: m
  IPAR( 5 ) = INPUT( default=15, option=m )
END SUBROUTINE setKrylovSubspaceSize

!----------------------------------------------------------------------------
!                                                                 setMatIter
!----------------------------------------------------------------------------

SUBROUTINE setMaxIter( IPAR, maxIter )
  INTEGER( I4B ), INTENT( INOUT ) :: IPAR( : )
  INTEGER( I4B ), INTENT( IN ) :: maxIter
  IPAR( 6 ) = maxIter
END SUBROUTINE setMaxIter

!----------------------------------------------------------------------------
!                                                          setConvergenceType
!----------------------------------------------------------------------------

SUBROUTINE setConvergenceType( IPAR, convergenceIn, convergenceType, &
  & relativeToRHS )
  INTEGER( I4B ), INTENT( INOUT ) :: IPAR( : )
  INTEGER( I4B ), INTENT( IN ) :: convergenceIn
  INTEGER( I4B ), INTENT( IN ) :: convergenceType
  LOGICAL( LGT ), INTENT( IN )  :: relativeToRHS
  !
  IPAR( 3 ) = 1
  SELECT CASE( convergenceType )
  CASE( absoluteConvergence )
    IF( convergenceIn .EQ. convergenceInSol ) THEN
      IPAR( 3 ) = -1
    ELSE IF( convergenceIn .EQ. convergenceInRes ) THEN
      IPAR( 3 ) = 1
    END IF
  CASE( relativeConvergence )
    IF( convergenceIn .EQ. convergenceInSol ) THEN
      IF( relativeToRHS ) THEN
        IPAR( 3 ) = -2
      ELSE
        IPAR( 3 ) = -1
      END IF
    ELSE IF( convergenceIn .EQ. convergenceInRes ) THEN
      IF( relativeToRHS ) THEN
        IPAR( 3 ) = 2
      ELSE
        IPAR( 3 ) = 1
      END IF
    END IF
  END SELECT
END SUBROUTINE setConvergenceType

!----------------------------------------------------------------------------
!                                                       setRelativeTolerance
!----------------------------------------------------------------------------

SUBROUTINE setRelativeTolerance( FPAR, tol )
  REAL( DFP ), INTENT( INOUT ) :: FPAR( : )
  REAL( DFP ), INTENT( IN ) :: tol
  FPAR( 1 ) = tol
END SUBROUTINE

!----------------------------------------------------------------------------
!                                                       setRelativeTolerance
!----------------------------------------------------------------------------

SUBROUTINE setAbsoluteTolerance( FPAR, tol )
  REAL( DFP ), INTENT( INOUT ) :: FPAR( : )
  REAL( DFP ), INTENT( IN ) :: tol
  FPAR( 2 ) = tol
END SUBROUTINE

!----------------------------------------------------------------------------
!                                                         setLinSolverParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setLinSolverParam
  INTEGER( I4B ) :: ierr
  ierr = param%set( key="solverName", value=solverName )
  ierr = param%set( key="preconditionOption", value=preconditionOption )
  ierr = param%set( key="convergenceIn", value=convergenceIn )
  ierr = param%set( key="convergenceType", value=convergenceType )
  ierr = param%set( key="maxIter", value=maxIter )
  IF( PRESENT( relativeToRHS ) ) THEN
    ierr = param%set( key="relativeToRHS", value=.FALSE. )
  ELSE
    ierr = param%set( key="relativeToRHS", value=relativeToRHS )
  END IF
  IF( PRESENT( KrylovSubspaceSize ) ) THEN
    ierr = param%set( key="KrylovSubspaceSize", value=15_I4B )
  ELSE
    ierr = param%set( key="KrylovSubspaceSize", value=KrylovSubspaceSize )
  END IF
  IF( PRESENT( rtol ) ) THEN
    ierr = param%set( key="rtol", value=REAL(1.0E-8, DFP) )
  ELSE
    ierr = param%set( key="rtol", value=rtol )
  END IF
  IF( PRESENT( atol ) ) THEN
    ierr = param%set( key="atol", value=REAL(1.0E-8, DFP) )
  ELSE
    ierr = param%set( key="atol", value=atol )
  END IF
END PROCEDURE setLinSolverParam

!----------------------------------------------------------------------------
!                                                         getLinSolverParam
!----------------------------------------------------------------------------

MODULE PROCEDURE getLinSolverParam
  INTEGER( I4B ) :: ierr
  ierr = param%get( key="solverName", value=solverName )
  ierr = param%get( key="preconditionOption", value=preconditionOption )
  ierr = param%get( key="convergenceIn", value=convergenceIn )
  ierr = param%get( key="convergenceType", value=convergenceType )
  ierr = param%get( key="maxIter", value=maxIter )
  ierr = param%get( key="relativeToRHS", value=relativeToRHS )
  ierr = param%get( key="KrylovSubspaceSize", value=KrylovSubspaceSize )
  ierr = param%get( key="rtol", value=rtol )
  ierr = param%get( key="atol", value=atol )
END PROCEDURE getLinSolverParam

!----------------------------------------------------------------------------
!                                                       checkEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_checkEssentialParam
  CHARACTER( LEN = * ), PARAMETER :: myName = "ls_checkEssentialParam"

  IF( .NOT. param%isPresent(key="solverName") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'solverName should be present in param')

  IF( .NOT. param%isPresent(key="preconditionOption") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'preconditionOption should be present in param')

  IF( .NOT. param%isPresent(key="convergenceIn") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'convergenceIn should be present in param')

  IF( .NOT. param%isPresent(key="convergenceType") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'convergenceType should be present in param')

  IF( .NOT. param%isPresent(key="maxIter") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'maxIter should be present in param')

  IF( .NOT. param%isPresent(key="relativeToRHS") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'relativeToRHS should be present in param')

  IF( .NOT. param%isPresent(key="KrylovSubspaceSize") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'KrylovSubspaceSize should be present in param')

  IF( .NOT. param%isPresent(key="rtol") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'rtol should be present in param')

  IF( .NOT. param%isPresent(key="atol") ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'atol should be present in param')
END PROCEDURE ls_checkEssentialParam

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Initiate
  INTEGER( I4B ) :: solverName, preconditionOption, convergenceIn, &
    & convergenceType, maxIter, KrylovSubspaceSize, relativeToRHS
  REAL( DFP ) :: rtol, atol
  LOGICAL( LGT ) :: relativeToRHS
  CALL obj%checkEssentialParam(param)
  CALL getLinSolverParam( param=param, solverName=solverName, &
    & preconditionOption=preconditionOption, convergenceIn=convergenceIn, &
    & convergenceType=convergenceType, maxIter=maxIter, &
    & relativeToRHS=relativeToRHS, KrylovSubspaceSize=KrylovSubspaceSize, &
    & rtol=rtol, atol=atol )
  obj%isInitiated = .TRUE.
  obj%ierr = 0
  obj%iter=0
  obj%solverName=solverName
  obj%preconditionOption = preconditionOption
  obj%convergenceIn = convergenceIn
  obj%convergenceType = convergenceType
  obj%maxIter = maxIter
  obj%relativeToRHS = relativeToRHS
  obj%KrylovSubspaceSize = KrylovSubspaceSize
  obj%atol = atol
  obj%rtol = rtol
  obj%IPAR = 0
  CALL setPreconditionOption( obj%IPAR, preconditionOption )
  CALL setConvergenceType( obj%IPAR, convergenceIn, convergenceType, &
  & relativeToRHS )
  obj%IPAR( 5 ) = KrylovSubspaceSize
  CALL setMaxIter( obj%IPAR, maxIter )
  obj%FPAR = 0.0_DFP
  CALL setRelativeTolerance( obj%FPAR, rtol )
  CALL setAbsoluteTolerance( obj%FPAR, atol )
  CALL Reallocate(obj%RES, maxIter)
END PROCEDURE ls_Initiate

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_DeallocateData
  obj%IPAR=0
  obj%FPAR=0.0_DFP
  obj%isInitiated = .FALSE.
  obj%ierr = 0
  obj%iter=0
  obj%solverName=0
  obj%preconditionOption = 0
  obj%convergenceIn = 0
  obj%convergenceType = 0
  obj%maxIter = 0
  obj%relativeToRHS = .FALSE.
  obj%KrylovSubspaceSize = 15
  obj%atol = 1.0E-8
  obj%rtol = 1.0E-8
  obj%globalNumColumn = 0
  obj%globalNumRow = 0
  obj%localNumColumn = 0
  obj%localNumRow = 0
  IF( ALLOCATED( obj%RES ) ) DEALLOCATE( obj%RES )
  IF( ALLOCATED( obj%W ) ) DEALLOCATE( obj%W )
  NULLIFY( obj%Amat, obj%Pmat )
END PROCEDURE ls_DeallocateData

END SUBMODULE Constructor