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

!----------------------------------------------------------------------------
!                                                           PerformMatVec
!----------------------------------------------------------------------------

SUBROUTINE PERFORM_TASK(Amat, y, x, dbcIndx, ierr, myName)
  ! intent of dummy variables
  CLASS(AbstractMatrixField_), INTENT(INOUT) :: Amat
  REAL(DFP), INTENT(INOUT) :: y(:)
  REAL(DFP), INTENT(IN) :: x(:)
  INTEGER(I4B), INTENT(IN) :: dbcIndx(:)
  INTEGER(I4B), INTENT(IN) :: ierr
  CHARACTER(*), INTENT(IN) :: myName
  !
  ! main
  !
  SELECT CASE (ierr)
  CASE (1)
    !
    ! MatVec, y=Ax
    !
    CALL display(__FILE__//"  debug: I am disabling y(dbcIndx)=0.0")
    CALL Amat%Matvec(y=y, x=x)
    ! IF (SIZE(dbcIndx) .GT. 0) THEN
    !   y(dbcIndx) = 0.0_DFP
    ! END IF
    !
  CASE (2)
    !
    ! Transposed MatVec
    !
    CALL display(__FILE__//"  debug: I am disabling y(dbcIndx)=0.0")
    CALL Amat%Matvec(y=y, x=x, isTranspose=.TRUE.)
    ! IF (SIZE(dbcIndx) .GT. 0) THEN
    !   y(dbcIndx) = 0.0_DFP
    ! END IF
    !
  CASE (3, 5)
    !
    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are inside the Amat
    !
    CALL Amat%LUSOLVE(sol=y, rhs=x, isTranspose=.FALSE.)
    CALL display(__FILE__//"  debug: ILU Solve")
    !
  CASE (4, 6)
    !
    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are inside the Amat
    !
    CALL Amat%LUSOLVE(sol=y, rhs=x, isTranspose=.TRUE.)
    CALL display(__FILE__//"  debug: Transpose ILU Solve")
    !
  END SELECT
  !
END SUBROUTINE PERFORM_TASK

!----------------------------------------------------------------------------
!                                                                  CHECKERR
!----------------------------------------------------------------------------

SUBROUTINE CHECKERROR(IPAR, FPAR, myName)
  INTEGER(I4B), INTENT(IN) :: IPAR(:)
  REAL(DFP), INTENT(IN) :: FPAR(:)
  CHARACTER(*), INTENT(IN) :: myName
  !
  ! internal variable
  !
  INTEGER(I4B) :: ierr, unitNo
  !
  !
  !
  ierr = IPAR(1)
  !
  SELECT CASE (ierr)
    !
    !
    !
  CASE (-1)
    !
    IF (e%isLogActive()) THEN
      unitNo = e%getLogFileUnit()
    ELSE
      unitNo = stdout
    END IF
    !
    CALL EqualLine(unitNo=unitNo)
    !
    CALL Display(IPAR(7), "# Number of Matrix-Vector Multiplication = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(3), "# Initial residual/error norm = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(4), "# Target residual/error norm = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(6), "# Current residual/error norm = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(5), "# Current residual norm = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(7), "# Convergence rate = ",&
      & unitNo=unitNo)
    !
    CALL EqualLine(unitNo=unitNo)
    !
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "Termination because iteration number exceeds the limit")
    !
    !
    !
  CASE (-2)
    !
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "Return due to insufficient work space")
    !
    !
    !
  CASE (-3)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "Return due to anticipated break-down / divide by zero")
    !
    !
    !
  CASE (-4)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "The values of `fpar(1)` and `fpar(2)` are both <= 0, &
      & the valid ranges are `0 <= fpar(1) < 1`, `0 <= fpar(2)`, &
      & and they can not be zero at the same time")
    !
    !
    !
  CASE (-9)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "While trying to detect a break-down, &
      & an abnormal number is detected")
    !
    !
    !
  CASE (-10)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "Return due to some non-numerical reasons, &
      & e.g. invalid floating-point numbers etc")
    !
    !
    !
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "Unknown error encountered. Cannot read the error message")
    !
    !
    !
  END SELECT
END SUBROUTINE CHECKERROR

!----------------------------------------------------------------------------
!                                                        DisplayConvergence
!----------------------------------------------------------------------------

SUBROUTINE DisplayConvergence(myName, iter, FPAR)
  CHARACTER(*), INTENT(IN) :: myName
  INTEGER(I4B), INTENT(IN) :: iter
  REAL(DFP), INTENT(IN) :: FPAR(:)
  !
  ! main
  !
  INTEGER(I4B) :: unitno
  IF (e%isLogActive()) THEN
    unitno = e%getLogFileUnit()
  ELSE
    unitno = stdout
  END IF
  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'Convergence is achieved 🎖')
  CALL EqualLine(unitNo=unitNo)
  CALL Display(iter, "# Number of Matrix-Vector Multiplication = ",&
    & unitno=unitno)
  CALL Display(fpar(3), "# Initial residual/error norm = ",&
    & unitno=unitno)
  CALL Display(fpar(4), "# Target residual/error norm = ",&
    & unitno=unitno)
  CALL Display(fpar(6), "# Current residual/error norm = ",&
    & unitno=unitno)
  CALL Display(fpar(5), "# Current residual norm = ",&
    & unitno=unitno)
  CALL Display(fpar(7), "# Convergence rate = ",&
    & unitno=unitno)
  CALL EqualLine(unitNo=unitNo)
END SUBROUTINE DisplayConvergence

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Solve
!
CHARACTER(*), PARAMETER :: myName = "ls_Solve"
REAL(DFP), POINTER :: rhsvar(:), solvar(:)
REAL(DFP), ALLOCATABLE :: diag(:)
INTEGER(I4B) :: ii
!
! main
!
! check
!
IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Linear solver is not initiated, initiate first!')
END IF
!
SELECT CASE (obj%solverName)
CASE (LIS_GMRES)

  rhsvar => rhs%getPointer()
  solvar => sol%getPointer()
  !
  ! Applying dirichlet boundary condition
  !
  CALL display(__FILE__//"  debug: I am disabling rhsvar(obj%dbcIndx) = 0.0_DFP")
  ! IF (SIZE(obj%dbcIndx) .GT. 0) rhsvar(obj%dbcIndx) = 0.0_DFP
  !
  ! Applying diagonal precondition
  !
  ! IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
  !
  ! CALL e%raiseInformation(modName//'::'//myName//" - "// &
  !   & 'No precondition = Diagonal precondition !')
  !
  CALL obj%Amat%GetDiagonal(diag=diag)
  ! CALL obj%Amat%DiagonalScaling( side='BOTH' )
  CALL obj%Amat%DiagonalScaling(side='BOTH', diag=diag)
  !
  DO ii = 1, SIZE(diag)
    rhsvar(ii) = rhsvar(ii) / SQRT(ABS(diag(ii)))
    solvar(ii) = solvar(ii) * SQRT(ABS(diag(ii)))
  END DO
  !
  ! END IF
  !
  CALL LS_SOLVE_GMRES(obj, sol=solvar, rhs=rhsvar)
  !
  ! Applying diagnoal precondition
  !
  ! IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
  DO ii = 1, SIZE(diag)
    solvar(ii) = solvar(ii) / SQRT(ABS(diag(ii)))
  END DO
  DEALLOCATE (diag)
  ! END IF
  !
  rhsvar => NULL(); solvar => NULL()
  !
  !
  !
CASE (LIS_CG)
  rhsvar => rhs%getPointer()
  solvar => sol%getPointer()
  !
  ! Applying dirichlet boundary condition
  !
  IF (SIZE(obj%dbcIndx) .GT. 0) rhsvar(obj%dbcIndx) = 0.0_DFP
  !
  ! Applying diagonal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    !
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'No precondition = Diagonal precondition !')
    !
    CALL obj%Amat%GetDiagonal(diag=diag)
    ! CALL obj%Amat%DiagonalScaling( side='BOTH' )
    CALL obj%Amat%DiagonalScaling(side='BOTH', diag=diag)
    !
    DO ii = 1, SIZE(diag)
      rhsvar(ii) = rhsvar(ii) / SQRT(ABS(diag(ii)))
      solvar(ii) = solvar(ii) * SQRT(ABS(diag(ii)))
    END DO
    !
  END IF
  !
  CALL LS_SOLVE_CG(obj, sol=solvar, rhs=rhsvar)
  !
  ! Applying diagnoal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    DO ii = 1, SIZE(diag)
      solvar(ii) = solvar(ii) / SQRT(ABS(diag(ii)))
    END DO
    DEALLOCATE (diag)
  END IF
  !
  rhsvar => NULL(); solvar => NULL()
  !
  !
  !
CASE (LIS_CGNR)

  rhsvar => rhs%getPointer()
  solvar => sol%getPointer()
  !
  ! Applying dirichlet boundary condition
  !
  IF (SIZE(obj%dbcIndx) .GT. 0) rhsvar(obj%dbcIndx) = 0.0_DFP
  !
  ! Applying diagonal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    !
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'No precondition = Diagonal precondition !')
    !
    CALL obj%Amat%GetDiagonal(diag=diag)
    ! CALL obj%Amat%DiagonalScaling( side='BOTH' )
    CALL obj%Amat%DiagonalScaling(side='BOTH', diag=diag)
    !
    DO ii = 1, SIZE(diag)
      rhsvar(ii) = rhsvar(ii) / SQRT(ABS(diag(ii)))
      solvar(ii) = solvar(ii) * SQRT(ABS(diag(ii)))
    END DO
    !
  END IF
  !
  CALL LS_SOLVE_CGNR(obj, sol=solvar, rhs=rhsvar)
  !
  ! Applying diagnoal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    DO ii = 1, SIZE(diag)
      solvar(ii) = solvar(ii) / SQRT(ABS(diag(ii)))
    END DO
    DEALLOCATE (diag)
  END IF
  !
  rhsvar => NULL(); solvar => NULL()
  !
  !
  !
CASE (LIS_BCG)

  rhsvar => rhs%getPointer()
  solvar => sol%getPointer()
  !
  ! Applying dirichlet boundary condition
  !
  IF (SIZE(obj%dbcIndx) .GT. 0) rhsvar(obj%dbcIndx) = 0.0_DFP
  !
  ! Applying diagonal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    !
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'No precondition = Diagonal precondition !')
    !
    CALL obj%Amat%GetDiagonal(diag=diag)
    ! CALL obj%Amat%DiagonalScaling( side='BOTH' )
    CALL obj%Amat%DiagonalScaling(side='BOTH', diag=diag)
    !
    DO ii = 1, SIZE(diag)
      rhsvar(ii) = rhsvar(ii) / SQRT(ABS(diag(ii)))
      solvar(ii) = solvar(ii) * SQRT(ABS(diag(ii)))
    END DO
    !
  END IF
  !
  CALL LS_SOLVE_BCG(obj, sol=solvar, rhs=rhsvar)
  !
  ! Applying diagnoal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    DO ii = 1, SIZE(diag)
      solvar(ii) = solvar(ii) / SQRT(ABS(diag(ii)))
    END DO
    DEALLOCATE (diag)
  END IF
  !
  rhsvar => NULL(); solvar => NULL()
  !
  !
  !
CASE (LIS_DBCG)

  rhsvar => rhs%getPointer()
  solvar => sol%getPointer()
  !
  ! Applying dirichlet boundary condition
  !
  IF (SIZE(obj%dbcIndx) .GT. 0) rhsvar(obj%dbcIndx) = 0.0_DFP
  !
  ! Applying diagonal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    !
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'No precondition = Diagonal precondition !')
    !
    CALL obj%Amat%GetDiagonal(diag=diag)
    ! CALL obj%Amat%DiagonalScaling( side='BOTH' )
    CALL obj%Amat%DiagonalScaling(side='BOTH', diag=diag)
    !
    DO ii = 1, SIZE(diag)
      rhsvar(ii) = rhsvar(ii) / SQRT(ABS(diag(ii)))
      solvar(ii) = solvar(ii) * SQRT(ABS(diag(ii)))
    END DO
    !
  END IF
  !
  CALL LS_SOLVE_DBCG(obj, sol=solvar, rhs=rhsvar)
  !
  ! Applying diagnoal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    DO ii = 1, SIZE(diag)
      solvar(ii) = solvar(ii) / SQRT(ABS(diag(ii)))
    END DO
    DEALLOCATE (diag)
  END IF
  !
  rhsvar => NULL(); solvar => NULL()
  !
  !
  !
CASE (LIS_BCGSTAB)

  rhsvar => rhs%getPointer()
  solvar => sol%getPointer()
  !
  ! Applying dirichlet boundary condition
  !
  IF (SIZE(obj%dbcIndx) .GT. 0) rhsvar(obj%dbcIndx) = 0.0_DFP
  !
  ! Applying diagonal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    !
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'No precondition = Diagonal precondition !')
    !
    CALL obj%Amat%GetDiagonal(diag=diag)
    ! CALL obj%Amat%DiagonalScaling( side='BOTH' )
    CALL obj%Amat%DiagonalScaling(side='BOTH', diag=diag)
    !
    DO ii = 1, SIZE(diag)
      rhsvar(ii) = rhsvar(ii) / SQRT(ABS(diag(ii)))
      solvar(ii) = solvar(ii) * SQRT(ABS(diag(ii)))
    END DO
    !
  END IF
  !
  CALL LS_SOLVE_BCGSTAB(obj, sol=solvar, rhs=rhsvar)
  !
  ! Applying diagnoal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    DO ii = 1, SIZE(diag)
      solvar(ii) = solvar(ii) / SQRT(ABS(diag(ii)))
    END DO
    DEALLOCATE (diag)
  END IF
  !
  rhsvar => NULL(); solvar => NULL()
  !
  !
  !
CASE (LIS_TFQMR)

  rhsvar => rhs%getPointer()
  solvar => sol%getPointer()
  !
  ! Applying dirichlet boundary condition
  !
  IF (SIZE(obj%dbcIndx) .GT. 0) rhsvar(obj%dbcIndx) = 0.0_DFP
  !
  ! Applying diagonal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    !
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'No precondition = Diagonal precondition !')
    !
    CALL obj%Amat%GetDiagonal(diag=diag)
    ! CALL obj%Amat%DiagonalScaling( side='BOTH' )
    CALL obj%Amat%DiagonalScaling(side='BOTH', diag=diag)
    !
    DO ii = 1, SIZE(diag)
      rhsvar(ii) = rhsvar(ii) / SQRT(ABS(diag(ii)))
      solvar(ii) = solvar(ii) * SQRT(ABS(diag(ii)))
    END DO
    !
  END IF
  !
  CALL LS_SOLVE_TFQMR(obj, sol=solvar, rhs=rhsvar)
  !
  ! Applying diagnoal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    DO ii = 1, SIZE(diag)
      solvar(ii) = solvar(ii) / SQRT(ABS(diag(ii)))
    END DO
    DEALLOCATE (diag)
  END IF
  !
  rhsvar => NULL(); solvar => NULL()
  !
  !
  !
CASE (LIS_FOM)

  rhsvar => rhs%getPointer()
  solvar => sol%getPointer()
  !
  ! Applying dirichlet boundary condition
  !
  IF (SIZE(obj%dbcIndx) .GT. 0) rhsvar(obj%dbcIndx) = 0.0_DFP
  !
  ! Applying diagonal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    !
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'No precondition = Diagonal precondition !')
    !
    CALL obj%Amat%GetDiagonal(diag=diag)
    ! CALL obj%Amat%DiagonalScaling( side='BOTH' )
    CALL obj%Amat%DiagonalScaling(side='BOTH', diag=diag)
    !
    DO ii = 1, SIZE(diag)
      rhsvar(ii) = rhsvar(ii) / SQRT(ABS(diag(ii)))
      solvar(ii) = solvar(ii) * SQRT(ABS(diag(ii)))
    END DO
    !
  END IF
  !
  CALL LS_SOLVE_FOM(obj, sol=solvar, rhs=rhsvar)
  !
  ! Applying diagnoal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    DO ii = 1, SIZE(diag)
      solvar(ii) = solvar(ii) / SQRT(ABS(diag(ii)))
    END DO
    DEALLOCATE (diag)
  END IF
  !
  rhsvar => NULL(); solvar => NULL()
  !
  !
  !
CASE (LIS_FGMRES)

  rhsvar => rhs%getPointer()
  solvar => sol%getPointer()
  !
  ! Applying dirichlet boundary condition
  !
  IF (SIZE(obj%dbcIndx) .GT. 0) rhsvar(obj%dbcIndx) = 0.0_DFP
  !
  ! Applying diagonal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    !
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'No precondition = Diagonal precondition !')
    !
    CALL obj%Amat%GetDiagonal(diag=diag)
    ! CALL obj%Amat%DiagonalScaling( side='BOTH' )
    CALL obj%Amat%DiagonalScaling(side='BOTH', diag=diag)
    !
    DO ii = 1, SIZE(diag)
      rhsvar(ii) = rhsvar(ii) / SQRT(ABS(diag(ii)))
      solvar(ii) = solvar(ii) * SQRT(ABS(diag(ii)))
    END DO
    !
  END IF
  !
  CALL LS_SOLVE_FGMRES(obj, sol=solvar, rhs=rhsvar)
  !
  ! Applying diagnoal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    DO ii = 1, SIZE(diag)
      solvar(ii) = solvar(ii) / SQRT(ABS(diag(ii)))
    END DO
    DEALLOCATE (diag)
  END IF
  !
  rhsvar => NULL(); solvar => NULL()
  !
  !
  !
CASE (LIS_DQGMRES)

  rhsvar => rhs%getPointer()
  solvar => sol%getPointer()
  !
  ! Applying dirichlet boundary condition
  !
  IF (SIZE(obj%dbcIndx) .GT. 0) rhsvar(obj%dbcIndx) = 0.0_DFP
  !
  ! Applying diagonal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    !
    CALL e%raiseInformation(modName//'::'//myName//" - "// &
      & 'No precondition = Diagonal precondition !')
    !
    CALL obj%Amat%GetDiagonal(diag=diag)
    ! CALL obj%Amat%DiagonalScaling( side='BOTH' )
    CALL obj%Amat%DiagonalScaling(side='BOTH', diag=diag)
    !
    DO ii = 1, SIZE(diag)
      rhsvar(ii) = rhsvar(ii) / SQRT(ABS(diag(ii)))
      solvar(ii) = solvar(ii) * SQRT(ABS(diag(ii)))
    END DO
    !
  END IF
  !
  CALL LS_SOLVE_DQGMRES(obj, sol=solvar, rhs=rhsvar)
  !
  ! Applying diagnoal precondition
  !
  IF (obj%preconditionOption .EQ. NO_PRECONDITION) THEN
    DO ii = 1, SIZE(diag)
      solvar(ii) = solvar(ii) / SQRT(ABS(diag(ii)))
    END DO
    DEALLOCATE (diag)
  END IF
  !
  rhsvar => NULL(); solvar => NULL()

CASE (1000)
  rhsvar => rhs%getPointer()
  solvar => sol%getPointer()

  CALL obj%Amat%LUSolve( &
    & rhs=rhsvar, &
    & sol=solvar)

  rhsvar => NULL(); solvar => NULL()
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Unknown linear solver encountered')

END SELECT
!
END PROCEDURE ls_Solve

!----------------------------------------------------------------------------
!                                                               LS_SOLVE_CG
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_CG
#define _LIS_NAME CG
#define _MY_NAME "LS_SOLVE_CG"
#include "./LIS_SOLVE.inc"

!----------------------------------------------------------------------------
!                                                               LS_SOLVE_CGNR
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_CGNR
#define _LIS_NAME CGNR
#define _MY_NAME "LS_SOLVE_CGNR"
#include "./LIS_SOLVE.inc"

!----------------------------------------------------------------------------
!                                                               LS_SOLVE_BCG
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_BCG
#define _LIS_NAME BCG
#define _MY_NAME "LS_SOLVE_BCG"
#include "./LIS_SOLVE.inc"

!----------------------------------------------------------------------------
!                                                               LS_SOLVE_DBCG
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_DBCG
#define _LIS_NAME DBCG
#define _MY_NAME "LS_SOLVE_DBCG"
#include "./LIS_SOLVE.inc"

!----------------------------------------------------------------------------
!                                                          LS_SOLVE_BCGSTAB
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_BCGSTAB
#define _LIS_NAME BCGSTAB
#define _MY_NAME "LS_SOLVE_BCGSTAB"
#include "./LIS_SOLVE.inc"

!----------------------------------------------------------------------------
!                                                          LS_SOLVE_TFQMR
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_TFQMR
#define _LIS_NAME TFQMR
#define _MY_NAME "LS_SOLVE_TFQMR"
#include "./LIS_SOLVE.inc"

!----------------------------------------------------------------------------
!                                                              LS_SOLVE_FOM
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_FOM
#define _LIS_NAME FOM
#define _MY_NAME "LS_SOLVE_FOM"
#include "./LIS_SOLVE.inc"

!----------------------------------------------------------------------------
!                                                              LS_SOLVE_GMRES
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_GMRES
#define _LIS_NAME GMRES
#define _MY_NAME "LS_SOLVE_GMRES"

#include "./LIS_SOLVE.inc"

!----------------------------------------------------------------------------
!                                                           LS_SOLVE_FGMRES
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_FGMRES
#define _LIS_NAME FGMRES
#define _MY_NAME "LS_SOLVE_FGMRES"
#include "./LIS_SOLVE.inc"

!----------------------------------------------------------------------------
!                                                           LS_SOLVE_DQGMRES
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_DQGMRES
#define _LIS_NAME DQGMRES
#define _MY_NAME "LS_SOLVE_DQGMRES"
#include "./LIS_SOLVE.inc"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SolveMethods
