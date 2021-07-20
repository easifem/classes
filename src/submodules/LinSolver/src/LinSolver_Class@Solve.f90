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

SUBMODULE( LinSolver_Class ) Solve
USE BaseMethod
IMPLICIT NONE
CONTAINS

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
  INTEGER( I4B ) :: n, x1, x2, y1, y2
  REAL( DFP ) :: val
  !
  obj%IPAR(1) = 0; obj%ierr = 0; obj%FPAR(11) = 0.0_DFP
  obj%iter = 0; n = obj%globalNumRow
  DO
    CALL CG( n, rhs, sol, obj%IPAR, obj%FPAR, obj%W )
    obj%ierr = obj%IPAR( 1 )
    IF( obj%ipar( 7 ) - obj%iter .GT. 0 ) THEN
      obj%iter = obj % ipar( 7 )
      obj%RES( obj%iter ) = obj%FPAR( 6 ) !> Current residual
    END IF
    x1 = obj%ipar( 8 ); x2 = x1 + n
    y1 = obj%ipar( 9 ); y2 = y1 + n
    IF( obj%ierr .EQ. 0 ) THEN
      CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & "Convergence is achieved" )
      EXIT
    ELSE IF( obj%ierr .GT. 0 ) THEN
      CALL PERFORM_TASK( obj%Amat, y=obj%W(y1:y2), x=obj%W(x1:x2), &
      & ierr=obj%ierr )
    ELSE IF ( obj%ierr .LT. 0 ) THEN
      CALL CHECKERROR( IPAR=obj%IPAR, myName=myName )
    END IF
  END DO
END SUBROUTINE LS_SOLVE_CG

!----------------------------------------------------------------------------
!                                                           PerformMatVec
!----------------------------------------------------------------------------

SUBROUTINE PERFORM_TASK( Amat, y, x, ierr )
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: Amat
  REAL( DFP ), INTENT( INOUT ) :: y ( : )
  REAL( DFP ), INTENT( IN ) :: x ( : )
  INTEGER( I4B ), INTENT( IN ) :: ierr
  SELECT CASE( ierr )
  CASE( 1 )
    CALL Amat%Matvec( y=y, x=x )
  CASE( 2 )
    CALL Amat%Matvec( y=y, x=x, transp=.TRUE. )
  CASE( 3, 5 )
    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are inside the Amat
    CALL Amat%LUSOLVE( sol=y, rhs=x )
  CASE( 4, 6 )
    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are inside the Amat
    CALL Amat%LUTSOLVE( sol=y, rhs=x )
  END SELECT
END SUBROUTINE PERFORM_TASK

!----------------------------------------------------------------------------
!                                                                  CHECKERR
!----------------------------------------------------------------------------

SUBROUTINE CHECKERROR( IPAR, myName )
  INTEGER( I4B ), INTENT( IN ) :: IPAR( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: myName
  ! internal variable
  INTEGER( I4B ) :: ierr

  !
  ierr = IPAR( 1 )
  SELECT CASE( ierr )
  CASE( -1 )
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & "Termination because iteration number is greater than the preset limit" )
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

END SUBMODULE Solve