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
USE Display_Method, ONLY: Display, EqualLine, Blanklines, ToString

USE MatrixField_Class, ONLY: MatrixField_

USE BaseType, ONLY: TypeSolverNameOpt

USE CSRMatrix_Method, ONLY: LinSolve, MatVec

USE SuperLU_Types, ONLY: yes_no_t

USE GlobalData, ONLY: stdout

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                           PerformMatVec
!----------------------------------------------------------------------------

SUBROUTINE PERFORM_TASK(amat, y, x, ierr)
  ! intent of dummy variables
  CLASS(AbstractMatrixField_), INTENT(INOUT) :: amat
  REAL(DFP), INTENT(INOUT) :: y(:)
  REAL(DFP), INTENT(IN) :: x(:)
  INTEGER(I4B), INTENT(IN) :: ierr

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "PERFORM_TASK()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  SELECT CASE (ierr)

  CASE (1)

    CALL amat%Matvec(y=y, x=x, isTranspose=.FALSE.)

  CASE (2)

    CALL amat%Matvec(y=y, x=x, isTranspose=.TRUE.)

  CASE (3, 5)

    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are inside the Amat
    CALL amat%ILUSOLVE(sol=y, rhs=x, isTranspose=.FALSE.)

  CASE (4, 6)

    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are inside the Amat
    CALL amat%ILUSOLVE(sol=y, rhs=x, isTranspose=.TRUE.)

  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE PERFORM_TASK

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE PERFORM_TASK_PRECOND(amat, y, x, precond, ierr)

  ! intent of dummy variables
  CLASS(AbstractMatrixField_), INTENT(INOUT) :: amat
  TYPE(CSRMatrix_), INTENT(IN) :: precond
  REAL(DFP), INTENT(INOUT) :: y(:)
  REAL(DFP), INTENT(IN) :: x(:)
  INTEGER(I4B), INTENT(IN) :: ierr

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "PERFORM_TASK()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  SELECT CASE (ierr)

  CASE (1)

    CALL amat%Matvec(y=y, x=x, isTranspose=.FALSE.)

  CASE (2)

    CALL amat%Matvec(y=y, x=x, isTranspose=.TRUE.)

  CASE (3, 5)

    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are given precond
    CALL Matvec(obj=precond, x=x, y=y, isTranspose=.FALSE.)

  CASE (4, 6)

    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are given precond
    CALL Matvec(obj=precond, x=x, y=y, isTranspose=.TRUE.)

  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE PERFORM_TASK_PRECOND

!----------------------------------------------------------------------------
!                                                                  CHECKERR
!----------------------------------------------------------------------------

SUBROUTINE CHECKERROR(IPAR, FPAR, myName)
  INTEGER(I4B), INTENT(IN) :: IPAR(:)
  REAL(DFP), INTENT(IN) :: FPAR(:)
  CHARACTER(*), INTENT(IN) :: myName
  ! internal variable
  INTEGER(I4B) :: ierr, unitNo

  ierr = IPAR(1)

  SELECT CASE (ierr)

  CASE (-1)

    IF (e%isLogActive()) THEN
      unitNo = e%getLogFileUnit()
    ELSE
      unitNo = stdout
    END IF

    CALL EqualLine(unitNo=unitNo)

    CALL Display(IPAR(7), "Number of Matrix-Vector Multiplication: ", &
                 unitNo=unitNo)

    CALL Display(FPAR(3), "Initial residual/error norm: ", &
                 unitNo=unitNo)

    CALL Display(FPAR(4), "Target residual/error norm: ", &
                 unitNo=unitNo)

    CALL Display(FPAR(6), "Current residual/error norm: ", &
                 unitNo=unitNo)

    CALL Display(FPAR(5), "Current residual norm: ", &
                 unitNo=unitNo)

    CALL Display(FPAR(7), "Convergence rate: ", &
                 unitNo=unitNo)

    CALL EqualLine(unitNo=unitNo)

    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      "[INTERNAL ERROR] :: Termination because iteration "// &
                      "number exceeds the limit")
    RETURN

  CASE (-2)
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                  "[INTERNAL ERROR] :: Return due to insufficient work space")
    RETURN

  CASE (-3)
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      "[INTERNAL ERROR] :: Return due to anticipated &
                      & break-down / divide by zero")
    RETURN

  CASE (-4)
    CALL e%RaiseError(modName//'::'//myName//" - "// &
       "[INTERNAL ERROR] :: values of `fpar(1)` and `fpar(2)` &
      & are both <= 0,valid ranges are `0<=fpar(1)<1`, `0<=fpar(2)`, &
      & and they can not be zero at the same time")
    RETURN

  CASE (-9)
    CALL e%RaiseError(modName//'::'//myName//" - "// &
       "[INTERNAL ERROR] :: While trying to detect a break-down, &
      & an abnormal number is detected")
    RETURN

  CASE (-10)
    CALL e%RaiseError(modName//'::'//myName//" - "// &
       "[INTERNAL ERROR] :: Return due to some non-numerical reasons, &
      & e.g. invalid floating-point numbers etc")
    RETURN

  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//" - "// &
       "[INTERNAL ERROR] :: Unknown error encountered. &
       & Cannot read the error message")
  END SELECT

END SUBROUTINE CHECKERROR

!----------------------------------------------------------------------------
!                                                        DisplayConvergence
!----------------------------------------------------------------------------

SUBROUTINE DisplayConvergence(myName, iter, FPAR)
  CHARACTER(*), INTENT(IN) :: myName
  INTEGER(I4B), INTENT(IN) :: iter
  REAL(DFP), INTENT(IN) :: FPAR(:)

  ! internal variable
  INTEGER(I4B) :: unitno

  IF (e%isLogActive()) THEN
    unitno = e%getLogFileUnit()
  ELSE
    unitno = stdout
  END IF

  CALL e%RaiseInformation(modName//'::'//myName//" - "// &
                          'Convergence is achieved')

  CALL Blanklines(nol=2, unitno=unitno)
  CALL EqualLine(unitNo=unitNo)

  CALL Display(iter, "Number of Matrix-Vector Multiplication: ", &
               unitno=unitno)
  CALL Display(fpar(3), "Initial residual/error norm: ", &
               unitno=unitno)
  CALL Display(fpar(4), "Target residual/error norm: ", &
               unitno=unitno)
  CALL Display(fpar(6), "Current residual/error norm: ", &
               unitno=unitno)
  CALL Display(fpar(5), "Current residual norm: ", &
               unitno=unitno)
  CALL Display(fpar(7), "Convergence rate: ", &
               unitno=unitno)
  CALL EqualLine(unitNo=unitNo)
END SUBROUTINE DisplayConvergence

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Solve
CHARACTER(*), PARAMETER :: myName = "obj_Solve()"
REAL(DFP), POINTER :: rhsvar(:), solvar(:)
INTEGER(I4B) :: info
INTEGER(I4B) :: solverName
LOGICAL(LGT) :: isok
CLASS(AbstractMatrixField_), POINTER :: amat

CALL obj%GetParam(isInitiated=isok, solverName=solverName, amat=amat)

CALL AssertError1(isok, myname, 'Linear solver is not initiated!')

isok = ASSOCIATED(amat)
CALL AssertError1(isok, myname, 'Amat is not associated')

SELECT CASE (solverName)

CASE (TypeSolverNameOpt%GMRES)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_GMRES(obj=obj, sol=solvar, rhs=rhsvar)
  NULLIFY (rhsvar, solvar)

CASE (TypeSolverNameOpt%CG)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_CG(obj=obj, sol=solvar, rhs=rhsvar)
  NULLIFY (rhsvar, solvar)

CASE (TypeSolverNameOpt%CGNR)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_CGNR(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL(); solvar => NULL()

CASE (TypeSolverNameOpt%BCG)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_BCG(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL(); solvar => NULL()

CASE (TypeSolverNameOpt%DBCG)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_DBCG(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL(); solvar => NULL()

CASE (TypeSolverNameOpt%BCGSTAB)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_BCGSTAB(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL(); solvar => NULL()

CASE (TypeSolverNameOpt%TFQMR)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_TFQMR(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL(); solvar => NULL()

CASE (TypeSolverNameOpt%FOM)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_FOM(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL(); solvar => NULL()

CASE (TypeSolverNameOpt%FGMRES)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_FGMRES(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL(); solvar => NULL()

CASE (TypeSolverNameOpt%DQGMRES)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_DQGMRES(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL(); solvar => NULL()

CASE (TypeSolverNameOpt%SUPERLU)

  SELECT TYPE (amat)

  CLASS IS (MatrixField_)

    rhsvar => rhs%GetPointer()
    solvar => sol%GetPointer()

    CALL LinSolve(A=amat%mat, B=rhsvar, X=solvar, isTranspose=.FALSE., &
                  isFactored=.FALSE., PrintStat=yes_no_t%YES, info=info)

    NULLIFY (rhsvar, solvar)

    isok = info .EQ. 0
    CALL AssertError1(isok, myName, 'Failure in LinSolve()')

  CLASS DEFAULT

    CALL AssertError1(.FALSE., myName, 'No case found for obj%Amat type')
    RETURN

  END SELECT

CASE DEFAULT

  CALL AssertError1(.FALSE., myName, 'No case found for linear solver')
  RETURN
END SELECT

END PROCEDURE obj_Solve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Solve_precond
CHARACTER(*), PARAMETER :: myName = "obj_Solve()"
REAL(DFP), POINTER :: rhsvar(:), solvar(:)
INTEGER(I4B) :: info
INTEGER(I4B) :: solverName
LOGICAL(LGT) :: isok
CLASS(AbstractMatrixField_), POINTER :: amat

CALL obj%GetParam(isInitiated=isok, solverName=solverName, amat=amat)

CALL AssertError1(isok, myname, 'Linear solver is not initiated!')

isok = ASSOCIATED(amat)
CALL AssertError1(isok, myname, 'Amat is not associated')

SELECT CASE (solverName)

CASE (TypeSolverNameOpt%GMRES)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_GMRES_precond(obj=obj, sol=solvar, rhs=rhsvar, &
                              precond=precond)
  NULLIFY (rhsvar, solvar)

CASE DEFAULT

  CALL AssertError1(.FALSE., myName, 'No case found for linear solver')
  RETURN
END SELECT

END PROCEDURE obj_Solve_precond

!----------------------------------------------------------------------------
!                                                               LS_SOLVE_CG
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_CG
#define _LIS_NAME CG
#define _MY_NAME "LS_SOLVE_CG"
#include "./LIS_SOLVE.F90"

!----------------------------------------------------------------------------
!                                                               LS_SOLVE_CGNR
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_CGNR
#define _LIS_NAME CGNR
#define _MY_NAME "LS_SOLVE_CGNR"
#include "./LIS_SOLVE.F90"

!----------------------------------------------------------------------------
!                                                               LS_SOLVE_BCG
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_BCG
#define _LIS_NAME BCG
#define _MY_NAME "LS_SOLVE_BCG"
#include "./LIS_SOLVE.F90"

!----------------------------------------------------------------------------
!                                                               LS_SOLVE_DBCG
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_DBCG
#define _LIS_NAME DBCG
#define _MY_NAME "LS_SOLVE_DBCG"
#include "./LIS_SOLVE.F90"

!----------------------------------------------------------------------------
!                                                          LS_SOLVE_BCGSTAB
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_BCGSTAB
#define _LIS_NAME BCGSTAB
#define _MY_NAME "LS_SOLVE_BCGSTAB"
#include "./LIS_SOLVE.F90"

!----------------------------------------------------------------------------
!                                                          LS_SOLVE_TFQMR
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_TFQMR
#define _LIS_NAME TFQMR
#define _MY_NAME "LS_SOLVE_TFQMR"
#include "./LIS_SOLVE.F90"

!----------------------------------------------------------------------------
!                                                              LS_SOLVE_FOM
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_FOM
#define _LIS_NAME FOM
#define _MY_NAME "LS_SOLVE_FOM"
#include "./LIS_SOLVE.F90"

!----------------------------------------------------------------------------
!                                                              LS_SOLVE_GMRES
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_GMRES
#define _LIS_NAME GMRES
#define _MY_NAME "LS_SOLVE_GMRES"

#include "./LIS_SOLVE.F90"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_GMRES_PRECOND
#define _LIS_NAME GMRES
#define _MY_NAME "LS_SOLVE_GMRES_PRECOND"

#include "./LIS_SOLVE_PRECOND.F90"

!----------------------------------------------------------------------------
!                                                           LS_SOLVE_FGMRES
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_FGMRES
#define _LIS_NAME FGMRES
#define _MY_NAME "LS_SOLVE_FGMRES"
#include "./LIS_SOLVE.F90"

!----------------------------------------------------------------------------
!                                                           LS_SOLVE_DQGMRES
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME LS_SOLVE_DQGMRES
#define _LIS_NAME DQGMRES
#define _MY_NAME "LS_SOLVE_DQGMRES"
#include "./LIS_SOLVE.F90"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SolveMethods
