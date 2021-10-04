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

SUBMODULE(LinSolver_Class) SolveMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

#define _debug_linsolve_

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Solve
  CHARACTER( LEN = * ), PARAMETER :: myName = "ls_Solve"
  REAL( DFP ), POINTER :: rhsVar( : ), solVar( : )

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Linear solver is not initiated, initiate first!')

  rhsVar => rhs%getPointer()
  solVar => sol%getPointer()

  SELECT CASE( obj%solverName )
  CASE( LIS_CG )
    CALL LS_SOLVE_CG( obj, sol=solVar, rhs=rhsVar )
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Unknown linear solver encountered')
  END SELECT

  rhsVar => NULL()
  solVar => NULL()
END PROCEDURE ls_Solve

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

SUBROUTINE LS_SOLVE_CG( obj, sol, rhs )
  CLASS( LinSolver_ ), TARGET, INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( INOUT ) :: sol( : )
  REAL( DFP ), INTENT( INOUT ) :: rhs( : )
  ! Internal variables
  CHARACTER( LEN = * ), PARAMETER :: myName = "LS_SOLVE_CG"
  INTEGER( I4B ) :: n, x1, x2, y1, y2, unitNo
  REAL( DFP ) :: val
  !
  obj%IPAR(1) = 0; obj%ierr = 0; obj%FPAR(11) = 0.0_DFP
  obj%iter = 0; n = obj%globalNumRow

#ifdef _debug_linsolve_
CALL Display( "LINEAR ITERATIVE SOLVER CG METHOD" )
IF( obj%preconditionOption .EQ. NO_PRECONDITION ) THEN
  CALL Display( "NO PRECONDITONER" )
ELSE IF( obj%preconditionOption .EQ. LEFT_PRECONDITION ) THEN
  CALL Display( "LEFT PRECONDITONER" )
ELSE IF( obj%preconditionOption .EQ. RIGHT_PRECONDITION ) THEN
  CALL Display( "RIGHT PRECONDITONER" )
ELSE IF( obj%preconditionOption .EQ. LEFT_RIGHT_PRECONDITION ) THEN
  CALL Display( "LEFT RIGHT PRECONDITONER" )
END IF
#endif

  DO

#ifdef _debug_linsolve_
CALL EqualLine()
CALL Display( obj%iter, "ITERATION = ")
CALL Display( obj%FPAR( 6 ), "CURRENT RESIDUAL = ")
#endif

    CALL CG( n, rhs, sol, obj%IPAR, obj%FPAR, obj%W )
    obj%ierr = obj%IPAR( 1 )
    obj%iter = obj%ipar( 7 )
    obj%RES( obj%iter ) = obj%FPAR( 6 ) !> Current residual
    x1 = obj%ipar( 8 ); x2 = x1 + n - 1
    y1 = obj%ipar( 9 ); y2 = y1 + n - 1
    IF( obj%ierr .EQ. 0 ) THEN
      IF( e%isLogActive() ) THEN
        unitNo=e%getLogFileUnit()
      ELSE
        unitNo=stdout
      END IF
      CALL e%raiseInformation(modName//'::'//myName// " - "// &
        & "CONVERGENCE IS ACHIEVED IN CONJUGATE GRADIENT METHOD" )
      CALL EqualLine(unitNo=unitNo)
      CALL Display( obj%iter, "Number of Matrix-Vector Multiplication = ",&
        & unitNo=unitNo)
      CALL Display( obj%fpar(3), "Initial residual/error norm = ",&
        & unitNo=unitNo)
      CALL Display( obj%fpar(4), "Target residual/error norm = ",&
        & unitNo=unitNo)
      CALL Display( obj%fpar(6), "Current residual/error norm = ",&
        & unitNo=unitNo)
      CALL Display( obj%fpar(5), "Current residual norm = ",&
        & unitNo=unitNo)
      CALL Display( obj%fpar(7), "Convergence rate = ",&
        & unitNo=unitNo)
      CALL EqualLine(unitNo=unitNo)
      EXIT
    ELSE IF( obj%ierr .GT. 0 ) THEN
      CALL PERFORM_TASK( obj%Amat, y=obj%W(y1:y2), x=obj%W(x1:x2), &
        & ierr=obj%ierr, myName=myName )
    ELSE IF ( obj%ierr .LT. 0 ) THEN
      CALL CHECKERROR( IPAR=obj%IPAR, FPAR=obj%FPAR, myName=myName )
    END IF
  END DO
END SUBROUTINE LS_SOLVE_CG

!----------------------------------------------------------------------------
!                                                           PerformMatVec
!----------------------------------------------------------------------------

SUBROUTINE PERFORM_TASK( Amat, y, x, ierr, myName )
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: Amat
  REAL( DFP ), INTENT( INOUT ) :: y ( : )
  REAL( DFP ), INTENT( IN ) :: x ( : )
  INTEGER( I4B ), INTENT( IN ) :: ierr
  CHARACTER( LEN = * ), INTENT( IN ) :: myName
  !-------------------------------------------
  SELECT CASE( ierr )
  CASE( 1 )

#ifdef _debug_linsolve_
CALL Display( "CALLING MATVEC")
#endif

    CALL Amat%Matvec( y=y, x=x )
  CASE( 2 )

#ifdef _debug_linsolve_
CALL Display( "CALLING MATVEC WITH TRANSPOSE TRUE")
#endif

    CALL Amat%Matvec( y=y, x=x, transp=.TRUE. )
  CASE( 3, 5 )

#ifdef _debug_linsolve_
CALL Display( "CALLING LUSOLVE")
#endif

    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are inside the Amat
    CALL Amat%LUSOLVE( sol=y, rhs=x )
  CASE( 4, 6 )

#ifdef _debug_linsolve_
CALL Display( "CALLING LUTSOLVE")
#endif

    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are inside the Amat
    CALL Amat%LUSOLVE( sol=y, rhs=x, transp=.TRUE. )
  END SELECT
END SUBROUTINE PERFORM_TASK

!----------------------------------------------------------------------------
!                                                                  CHECKERR
!----------------------------------------------------------------------------

SUBROUTINE CHECKERROR( IPAR, FPAR, myName )
  INTEGER( I4B ), INTENT( IN ) :: IPAR( : )
  REAL( DFP ), INTENT( IN ) :: FPAR( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: myName
  ! internal variable
  INTEGER( I4B ) :: ierr, unitNo

  !
  ierr = IPAR( 1 )
  SELECT CASE( ierr )
  CASE( -1 )
    IF( e%isLogActive() ) THEN
      unitNo=e%getLogFileUnit()
    ELSE
      unitNo=stdout
    END IF
    CALL EqualLine(unitNo=unitNo)
    CALL Display( IPAR(7), "Number of Matrix-Vector Multiplication = ",&
      & unitNo=unitNo)
    CALL Display( FPAR(3), "Initial residual/error norm = ",&
      & unitNo=unitNo)
    CALL Display( FPAR(4), "Target residual/error norm = ",&
      & unitNo=unitNo)
    CALL Display( FPAR(6), "Current residual/error norm = ",&
      & unitNo=unitNo)
    CALL Display( FPAR(5), "Current residual norm = ",&
      & unitNo=unitNo)
    CALL Display( FPAR(7), "Convergence rate = ",&
      & unitNo=unitNo)
    CALL EqualLine(unitNo=unitNo)
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & "TERMINATION BECAUSE ITERATION NUMBER IS GREATER THAN THE PRESET LIMIT" )
  CASE( -2 )
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & "Return due to insufficient work space" )
  CASE( -3 )
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & "Return due to anticipated break-down / divide by zero" )
  CASE( -4 )
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & "The values of `fpar(1)` and `fpar(2)` are both <= 0, the valid &
      & ranges are `0 <= fpar(1) < 1`, `0 <= fpar(2)`, and they can not be &
      & zero at the same time" )
  CASE( -9 )
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & "While trying to detect a break-down, an abnormal number is detected")
  CASE( -10 )
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & "Return due to some non-numerical reasons, e.g. invalid floating-point numbers etc")
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & "Unknown error encountered. Cannot read the error message")
  END SELECT
END SUBROUTINE CHECKERROR

END SUBMODULE SolveMethods