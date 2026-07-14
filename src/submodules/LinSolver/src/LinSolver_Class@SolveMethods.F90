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
USE BaseType, ONLY: TypeSolverNameOpt
USE BaseType, ONLY: math => TypeMathOpt
USE CSRMatrix_Method, ONLY: LinSolve
USE Display_Method, ONLY: Blanklines
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: EqualLine
USE Display_Method, ONLY: ToString
USE GlobalData, ONLY: stdout
USE MatrixField_Class, ONLY: MatrixField_
USE SuperLU_Types, ONLY: yes_no_t
IMPLICIT NONE

CHARACTER(*), PARAMETER :: modName = "LinSolver_Class@SolveMethods.F90"

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
    CALL amat%Matvec(y=y, x=x, isTranspose=math%no)

  CASE (2)
    CALL amat%Matvec(y=y, x=x, isTranspose=math%yes)

  CASE (3, 5)
    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are inside the Amat
    CALL amat%ILUSOLVE(sol=y, rhs=x, isTranspose=math%no)

  CASE (4, 6)
    ! LEFT/RIGHT PRECONDITIONER SOLVER
    ! The preconditioners are inside the Amat
    CALL amat%ILUSOLVE(sol=y, rhs=x, isTranspose=math%yes)

  CASE DEFAULT

#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, &
                      "no case found for ierr")
#endif

  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE PERFORM_TASK

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
    CALL Display(FPAR(3), "Initial residual/error norm: ", unitNo=unitNo)
    CALL Display(FPAR(4), "Target residual/error norm: ", unitNo=unitNo)
    CALL Display(FPAR(6), "Current residual/error norm: ", unitNo=unitNo)
    CALL Display(FPAR(5), "Current residual norm: ", unitNo=unitNo)
    CALL Display(FPAR(7), "Convergence rate: ", unitNo=unitNo)
    CALL EqualLine(unitNo=unitNo)

    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      "Termination because iteration "// &
                      "number exceeds the limit")

  CASE (-2)
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      "Return due to insufficient work space")

  CASE (-3)
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      "[INTERNAL ERROR] :: Return due to anticipated &
                      & break-down / divide by zero")

  CASE (-4)
    CALL e%RaiseError(modName//'::'//myName//" - "// &
       "[INTERNAL ERROR] :: values of `fpar(1)` and `fpar(2)` &
      & are both <= 0,valid ranges are `0<=fpar(1)<1`, `0<=fpar(2)`, &
      & and they can not be zero at the same time")

  CASE (-9)
    CALL e%RaiseError(modName//'::'//myName//" - "// &
       "[INTERNAL ERROR] :: While trying to detect a break-down, &
      & an abnormal number is detected")

  CASE (-10)
    CALL e%RaiseError(modName//'::'//myName//" - "// &
       "[INTERNAL ERROR] :: Return due to some non-numerical reasons, &
      & e.g. invalid floating-point numbers etc")

  CASE DEFAULT
#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, &
                      "Unknown error encountered")
#endif

  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE CHECKERROR

!----------------------------------------------------------------------------
!                                                        DisplayConvergence
!----------------------------------------------------------------------------

SUBROUTINE DisplayConvergence(iter, FPAR, verbosity)
  INTEGER(I4B), INTENT(IN) :: iter
  REAL(DFP), INTENT(IN) :: FPAR(:)
  INTEGER(I4B), INTENT(IN) :: verbosity

  ! internal variable
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "DisplayConvergence()"
#endif
  INTEGER(I4B) :: unitno

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (e%isLogActive()) THEN
    unitno = e%getLogFileUnit()
  ELSE
    unitno = stdout
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//" - "// &
                    'Convergence is achieved')
#endif

  IF (verbosity .NE. 0) THEN
    CALL Blanklines(nol=2, unitno=unitno)
    CALL EqualLine(unitno=unitno)

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
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%GetParam(isInitiated=isok, solverName=solverName, amat=amat)

#ifdef DEBUG_VER
CALL AssertError1(isok, myname, 'Linear solver is not initiated!')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(amat)
CALL AssertError1(isok, myname, 'Amat is not associated')
#endif

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
  rhsvar => NULL()
  solvar => NULL()

CASE (TypeSolverNameOpt%BCG)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_BCG(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL()
  solvar => NULL()

CASE (TypeSolverNameOpt%DBCG)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_DBCG(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL()
  solvar => NULL()

CASE (TypeSolverNameOpt%BCGSTAB)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_BCGSTAB(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL()
  solvar => NULL()

CASE (TypeSolverNameOpt%TFQMR)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_TFQMR(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL()
  solvar => NULL()

CASE (TypeSolverNameOpt%FOM)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_FOM(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL()
  solvar => NULL()

CASE (TypeSolverNameOpt%FGMRES)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_FGMRES(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL()
  solvar => NULL()

CASE (TypeSolverNameOpt%DQGMRES)

  rhsvar => rhs%GetPointer()
  solvar => sol%GetPointer()
  CALL LS_SOLVE_DQGMRES(obj=obj, sol=solvar, rhs=rhsvar)
  rhsvar => NULL()
  solvar => NULL()

CASE (TypeSolverNameOpt%SUPERLU)

  SELECT TYPE (amat)

  CLASS IS (MatrixField_)

    rhsvar => rhs%GetPointer()
    solvar => sol%GetPointer()

    CALL LinSolve(A=amat%mat, B=rhsvar, X=solvar, isTranspose=math%no, &
                  isFactored=math%no, PrintStat=yes_no_t%yes, info=info)

    NULLIFY (rhsvar, solvar)

#ifdef DEBUG_VER
    isok = info .EQ. 0
    CALL AssertError1(isok, myName, 'Failure in LinSolve()')
#endif

#ifdef DEBUG_VER
  CLASS DEFAULT
    CALL AssertError1(math%no, myName, 'No case found for obj%Amat type')
#endif

  END SELECT

CASE DEFAULT

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, 'No case found for linear solver')
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Solve

!----------------------------------------------------------------------------
!                                                               LS_SOLVE_CG
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME_ LS_SOLVE_CG
#define _LIS_NAME_ CG
#define _MY_NAME_ "LS_SOLVE_CG"
! #include "./include/LIS_SOLVE.F90"

SUBROUTINE _SUBROUTINE_NAME_(obj, sol, rhs)
  CLASS(LinSolver_), TARGET, INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: sol(:)
  REAL(DFP), INTENT(INOUT) :: rhs(:)

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = _MY_NAME_
  INTEGER(I4B) :: n
  CLASS(AbstractMatrixField_), POINTER :: amat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  obj%IPAR(1) = math%zero_i
  obj%FPAR(11) = math%zero
  CALL obj%GetParam(globalNumRow=n, amat=amat)
  obj%IPAR(7) = math%one_i

  main_loop: DO

    CALL _LIS_NAME_(n, rhs, sol, obj%ipar, obj%fpar, obj%w)

    IF (obj%IPAR(1) .GT. math%zero_i) THEN

      CALL PERFORM_TASK(amat=amat, &
                        y=obj%W(obj%IPAR(9):obj%IPAR(9) + n - 1), &
                        x=obj%W(obj%IPAR(8):obj%IPAR(8) + n - 1), &
                        ierr=obj%IPAR(1))

    ELSE IF (obj%IPAR(1) .LT. math%zero_i) THEN

      IF (obj%fpar(6) .LE. obj%fpar(4)) EXIT main_loop

      CALL CHECKERROR(IPAR=obj%ipar, FPAR=obj%fpar, myName=myName)
      EXIT main_loop

    ELSE IF (obj%ipar(1) .EQ. math%zero_i) THEN

      CALL obj%SetParam(ierr=obj%ipar(1), iter=obj%ipar(7))

      CALL DisplayConvergence(iter=obj%ipar(7), fpar=obj%FPAR, &
                              verbosity=obj%verbosity)
      EXIT main_loop

    END IF

  END DO main_loop

  ! Initial residual/error norm
  CALL obj%SetParam(error0=obj%fpar(3), tol=obj%fpar(4), &
                    error=obj%fpar(6), normRes=obj%fpar(5))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE _SUBROUTINE_NAME_

#undef _SUBROUTINE_NAME_
#undef _LIS_NAME_
#undef _MY_NAME_

!----------------------------------------------------------------------------
!                                                               LS_SOLVE_CGNR
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME_ LS_SOLVE_CGNR
#define _LIS_NAME_ CGNR
#define _MY_NAME_ "LS_SOLVE_CGNR"
#include "./include/LIS_SOLVE.F90"
#undef _SUBROUTINE_NAME_
#undef _LIS_NAME_
#undef _MY_NAME_

!----------------------------------------------------------------------------
!                                                               LS_SOLVE_BCG
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME_ LS_SOLVE_BCG
#define _LIS_NAME_ BCG
#define _MY_NAME_ "LS_SOLVE_BCG"
#include "./include/LIS_SOLVE.F90"
#undef _SUBROUTINE_NAME_
#undef _LIS_NAME_
#undef _MY_NAME_

!----------------------------------------------------------------------------
!                                                               LS_SOLVE_DBCG
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME_ LS_SOLVE_DBCG
#define _LIS_NAME_ DBCG
#define _MY_NAME_ "LS_SOLVE_DBCG"
#include "./include/LIS_SOLVE.F90"
#undef _SUBROUTINE_NAME_
#undef _LIS_NAME_
#undef _MY_NAME_

!----------------------------------------------------------------------------
!                                                          LS_SOLVE_BCGSTAB
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME_ LS_SOLVE_BCGSTAB
#define _LIS_NAME_ BCGSTAB
#define _MY_NAME_ "LS_SOLVE_BCGSTAB"
#include "./include/LIS_SOLVE.F90"
#undef _SUBROUTINE_NAME_
#undef _LIS_NAME_
#undef _MY_NAME_

!----------------------------------------------------------------------------
!                                                          LS_SOLVE_TFQMR
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME_ LS_SOLVE_TFQMR
#define _LIS_NAME_ TFQMR
#define _MY_NAME_ "LS_SOLVE_TFQMR"
#include "./include/LIS_SOLVE.F90"
#undef _SUBROUTINE_NAME_
#undef _LIS_NAME_
#undef _MY_NAME_

!----------------------------------------------------------------------------
!                                                              LS_SOLVE_FOM
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME_ LS_SOLVE_FOM
#define _LIS_NAME_ FOM
#define _MY_NAME_ "LS_SOLVE_FOM"
#include "./include/LIS_SOLVE.F90"
#undef _SUBROUTINE_NAME_
#undef _LIS_NAME_
#undef _MY_NAME_

!----------------------------------------------------------------------------
!                                                              LS_SOLVE_GMRES
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME_ LS_SOLVE_GMRES
#define _LIS_NAME_ GMRES
#define _MY_NAME_ "LS_SOLVE_GMRES"
#include "./include/LIS_SOLVE.F90"
#undef _SUBROUTINE_NAME_
#undef _LIS_NAME_
#undef _MY_NAME_

!----------------------------------------------------------------------------
!                                                           LS_SOLVE_FGMRES
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME_ LS_SOLVE_FGMRES
#define _LIS_NAME_ FGMRES
#define _MY_NAME_ "LS_SOLVE_FGMRES"
#include "./include/LIS_SOLVE.F90"
#undef _SUBROUTINE_NAME_
#undef _LIS_NAME_
#undef _MY_NAME_

!----------------------------------------------------------------------------
!                                                           LS_SOLVE_DQGMRES
!----------------------------------------------------------------------------

#define _SUBROUTINE_NAME_ LS_SOLVE_DQGMRES
#define _LIS_NAME_ DQGMRES
#define _MY_NAME_ "LS_SOLVE_DQGMRES"
#include "./include/LIS_SOLVE.F90"
#undef _SUBROUTINE_NAME_
#undef _LIS_NAME_
#undef _MY_NAME_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SolveMethods
