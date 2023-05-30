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
! date: 15 July 2021
! summary: This module defines an abstract class for tangent matrix field.

MODULE AbstractMatrixField_Class
USE GlobalData
USE BaseType
USE AbstractField_Class
USE AbstractNodeField_Class
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "AbstractMatrixField_Class"

!----------------------------------------------------------------------------
!                                                     AbstractMatrixField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This abstract class is defined to handle the finite element matrix
!
!{!pages/AbstractMatrixField_.md!}

TYPE, ABSTRACT, EXTENDS(AbstractField_) :: AbstractMatrixField_
  LOGICAL(LGT) :: isPmatInitiated = .FALSE.
  !! True if precondition matrix is initiated
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Display => amField_Display
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => amField_Deallocate
  PROCEDURE(amField_Size), DEFERRED, PUBLIC, PASS(obj) :: Size
  PROCEDURE(amField_Shape), DEFERRED, PUBLIC, PASS(obj) :: Shape
  !
  ! @MatVecMethods
  !
  PROCEDURE(amField_Matvec1), DEFERRED, PASS(obj) :: Matvec1
  !! Matrix vector multiplication, here vector is fortran array
  PROCEDURE(amField_Matvec2), DEFERRED, PASS(obj) :: Matvec2
  !! Matrix vector multiplication, here vector is AbstractNodeField_
  GENERIC, PUBLIC :: Matvec => Matvec1, Matvec2
  !
  ! @ILUSolveMethods
  !
  PROCEDURE(amField_ILUSOLVE1), DEFERRED, PASS(obj) :: ILUSOLVE1
  !! Matrix vector multiplication, here vector is fortran array
  PROCEDURE(amField_ILUSOLVE2), DEFERRED, PASS(obj) :: ILUSOLVE2
  !! Matrix vector multiplication, here vector is AbstractNodeField_
  GENERIC, PUBLIC :: ILUSOLVE => ILUSOLVE1, ILUSOLVE2
  !! Generic LU Solve
  !
  !
  !
  PROCEDURE, PUBLIC, PASS(obj) :: isPreconditionSet => &
    & amField_isPreconditionSet
  !! True if prcondition is set
  PROCEDURE(amField_setPrecondition), DEFERRED, PUBLIC, PASS(obj) :: &
    & setPrecondition
  !! Build precondition matrix
  PROCEDURE(amField_getPrecondition), DEFERRED, PUBLIC, PASS(obj) :: &
    & getPrecondition
  !! Get the precondition matrix
  PROCEDURE(amField_reversePermutation), DEFERRED, PUBLIC, PASS(obj) :: &
    & reversePermutation
  !
  PROCEDURE(amField_DiagonalScaling), DEFERRED, PUBLIC, PASS(obj) :: &
    & DiagonalScaling
  ! DiagonalScaling
  PROCEDURE(amField_GetDiagonal), DEFERRED, PUBLIC, PASS(obj) :: &
    & GetDiagonal
  ! Get Diagonal
  PROCEDURE, PUBLIC, PASS(obj) :: &
    & SymSchurLargestEigenVal => amField_SymSchurLargestEigenVal
  ! SymSchurLargestEigenVal
  PROCEDURE, PUBLIC, PASS(obj) :: &
    & SymLargestEigenVal => amField_SymLargestEigenVal
  ! SymLargestEigenVal
  PROCEDURE(amField_ApplyDBC), DEFERRED, PUBLIC, PASS(obj) :: ApplyDBC
  ! ApplyDBC
  PROCEDURE, PUBLIC, PASS(obj) :: SPY => amField_SPY
  ! SPY
  !
  ! @SetMethods
  !
  PROCEDURE(amField_set1), DEFERRED, PASS(obj) :: set1
  PROCEDURE(amField_set2), DEFERRED, PASS(obj) :: set2
  PROCEDURE(amField_set3), DEFERRED, PASS(obj) :: set3
  PROCEDURE(amField_set4), DEFERRED, PASS(obj) :: set4
  PROCEDURE(amField_set5), DEFERRED, PASS(obj) :: set5
  PROCEDURE(amField_set6), DEFERRED, PASS(obj) :: set6
  PROCEDURE(amField_set7), DEFERRED, PASS(obj) :: set7
  PROCEDURE(amField_set8), DEFERRED, PASS(obj) :: set8
  PROCEDURE(amField_set9), DEFERRED, PASS(obj) :: set9
  PROCEDURE(amField_set10), DEFERRED, PASS(obj) :: set10
  GENERIC, PUBLIC :: set => set1, set2, set3, set4, set5, &
    & set6, set7, set8, set9, set10
  !
  ! @SetRow
  !
  PROCEDURE(amField_setRow1), DEFERRED, PASS(obj) :: setRow1
  PROCEDURE(amField_setRow2), DEFERRED, PASS(obj) :: setRow2
  PROCEDURE(amField_setRow3), DEFERRED, PASS(obj) :: setRow3
  PROCEDURE(amField_setRow4), DEFERRED, PASS(obj) :: setRow4
  PROCEDURE(amField_setRow5), DEFERRED, PASS(obj) :: setRow5
  PROCEDURE(amField_setRow6), DEFERRED, PASS(obj) :: setRow6
  PROCEDURE(amField_setRow7), DEFERRED, PASS(obj) :: setRow7
  GENERIC, PUBLIC :: setRow => setRow1, setRow2, setRow3, &
    & setRow4, setRow5, setRow6, setRow7
  !
  ! @GetColumn
  !
  PROCEDURE(amField_getColumn1), DEFERRED, PASS(obj) :: getColumn1
  PROCEDURE(amField_getColumn2), DEFERRED, PASS(obj) :: getColumn2
  PROCEDURE(amField_getColumn3), DEFERRED, PASS(obj) :: getColumn3
  PROCEDURE(amField_getColumn4), DEFERRED, PASS(obj) :: getColumn4
  PROCEDURE(amField_getColumn5), DEFERRED, PASS(obj) :: getColumn5
  PROCEDURE(amField_getColumn6), DEFERRED, PASS(obj) :: getColumn6
  PROCEDURE(amField_getColumn7), DEFERRED, PASS(obj) :: getColumn7
  GENERIC, PUBLIC :: getColumn => getColumn1, getColumn2, &
    & getColumn3, getColumn4, getColumn5, getColumn6, getColumn7
  !
  ! @GetRow
  !
  PROCEDURE(amField_getRow1), DEFERRED, PASS(obj) :: getRow1
  PROCEDURE(amField_getRow2), DEFERRED, PASS(obj) :: getRow2
  PROCEDURE(amField_getRow3), DEFERRED, PASS(obj) :: getRow3
  PROCEDURE(amField_getRow4), DEFERRED, PASS(obj) :: getRow4
  PROCEDURE(amField_getRow5), DEFERRED, PASS(obj) :: getRow5
  PROCEDURE(amField_getRow6), DEFERRED, PASS(obj) :: getRow6
  PROCEDURE(amField_getRow7), DEFERRED, PASS(obj) :: getRow7
  GENERIC, PUBLIC :: getRow => getRow1, getRow2, &
    & getRow3, getRow4, getRow5, getRow6, getRow7
  !
  ! @SetColumn
  !
  PROCEDURE(amField_setColumn1), DEFERRED, PASS(obj) :: setColumn1
  PROCEDURE(amField_setColumn2), DEFERRED, PASS(obj) :: setColumn2
  PROCEDURE(amField_setColumn3), DEFERRED, PASS(obj) :: setColumn3
  PROCEDURE(amField_setColumn4), DEFERRED, PASS(obj) :: setColumn4
  PROCEDURE(amField_setColumn5), DEFERRED, PASS(obj) :: setColumn5
  PROCEDURE(amField_setColumn6), DEFERRED, PASS(obj) :: setColumn6
  PROCEDURE(amField_setColumn7), DEFERRED, PASS(obj) :: setColumn7
  GENERIC, PUBLIC :: setColumn => setColumn1, setColumn2, &
    & setColumn3, setColumn4, setColumn5, setColumn6, setColumn7
END TYPE AbstractMatrixField_

PUBLIC :: AbstractMatrixField_

!----------------------------------------------------------------------------
!                                                                Display
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE amField_Display(obj, msg, unitNo)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE amField_Display
END INTERFACE

INTERFACE AbstractMatrixFieldDisplay
  MODULE PROCEDURE amField_Display
END INTERFACE AbstractMatrixFieldDisplay

PUBLIC :: AbstractMatrixFieldDisplay

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE amField_Deallocate(obj)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
  END SUBROUTINE amField_Deallocate
END INTERFACE

INTERFACE AbstractMatrixFieldDeallocate
  MODULE PROCEDURE amField_Deallocate
END INTERFACE AbstractMatrixFieldDeallocate

PUBLIC :: AbstractMatrixFieldDeallocate

!----------------------------------------------------------------------------
!                                                                      SIZE
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  FUNCTION amField_Size(obj, dim) RESULT(ans)
    IMPORT :: AbstractMatrixField_, I4B
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B) :: ans
  END FUNCTION amField_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      SHAPE
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  FUNCTION amField_Shape(obj) RESULT(ans)
    IMPORT :: AbstractMatrixField_, I4B
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans(2)
  END FUNCTION amField_Shape
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Matvec
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine returns the maxtrix vector multiplication
!
!# Introduction
!
! This routine returns the matrix vector multiplication. Here, input vector
! is a native fortran vector. The output vector is also a native fortran
! vector. It should be noted that the output vector should be allocated
! outside and it should have same length as the input vector.

ABSTRACT INTERFACE
  SUBROUTINE amField_Matvec1(obj, x, y, isTranspose, addContribution, &
    & scale)
    IMPORT :: AbstractMatrixField_, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: y(:)
    !! Output vector y=Ax
    REAL(DFP), INTENT(IN) :: x(:)
    !! Input vector in y=Ax
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! True if we have to use TRANSPOSE of matrix
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! Default is FALSE
    !! if true then we do not set y = 0, and perform
    !! y = y + matvec(obj, x)
    !! if false, then we perform y = matvec(obj, x)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
  END SUBROUTINE amField_Matvec1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      AMUX
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine returns the maxtrix vector multiplication
!
!# Introduction
!
! This routine returns the matrix vector multiplication. Here, input vector
! is an abstract node field.
! The output vector is also an abstract node field.

ABSTRACT INTERFACE
  SUBROUTINE amField_Matvec2(obj, x, y, isTranspose, addContribution, &
    & scale)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, LGT, DFP
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: y
    !! Output vector y=Ax
    CLASS(AbstractNodeField_), INTENT(IN) :: x
    !! Input vector in y=Ax
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! True if we have to use TRANSPOSE of matrix
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! Default is FALSE
    !! if true then we do not set y = 0, and perform
    !! y = y + matvec(obj, x)
    !! if false, then we perform y = matvec(obj, x)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
  END SUBROUTINE amField_Matvec2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ILUSOLVE
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine solves (LU) sol = rhs
!
!# Introduction
!
! If transp is absent or it is false then:
! This routine solves (LU) sol = rhs
! sol and rhs are fortran real vector
! The LU decomposition is stored inside the AbstractMatrixField_.
! Note that sol should be allocated by the user, and size of sol should be
! same as the size of rhs
!
! If transp is present and it is true then:
!
! If transp is present and it is true then this subroutine solves
! (LU)^T sol = rhs

ABSTRACT INTERFACE
  SUBROUTINE amField_ILUSOLVE1(obj, sol, rhs, isTranspose)
    IMPORT :: AbstractMatrixField_, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: sol(:)
    !! Output vector y=Ax
    REAL(DFP), INTENT(IN) :: rhs(:)
    !! Input vector in y=Ax
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
  END SUBROUTINE amField_ILUSOLVE1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   LUSOLVE
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine solves (LU) sol = rhs
!
!# Introduction
!
! If transp is not present or it is false then:
! This routine solves (LU) sol = rhs
! sol and rhs are [[AbstractNodeField_]]
! The LU decomposition is stored inside the AbstractMatrixField_.
!
! If transp is present and it is true then this subroutine solves
! (LU)^T sol = rhs

ABSTRACT INTERFACE
  SUBROUTINE amField_ILUSOLVE2(obj, sol, rhs, isTranspose)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: sol
    !! Output vector
    CLASS(AbstractNodeField_), INTENT(IN) :: rhs
    !! Input vector, rhs
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
  END SUBROUTINE amField_ILUSOLVE2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           setPrecondition
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine sets the precondition

ABSTRACT INTERFACE
  SUBROUTINE amField_setPrecondition(obj, param, dbcPtrs)
    IMPORT :: AbstractMatrixField_, ParameterList_, I4B
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), OPTIONAL, INTENT(IN) :: param
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcPtrs(:)
  END SUBROUTINE amField_setPrecondition
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getPrecondition
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine returns the precondition

ABSTRACT INTERFACE
  SUBROUTINE amField_getPrecondition(obj, Pmat)
    IMPORT :: AbstractMatrixField_
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: Pmat
  END SUBROUTINE amField_getPrecondition
END INTERFACE

!----------------------------------------------------------------------------
!                                                        reversePermutation
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine fix the solution
!
!# Introduction
! In sparse solver, it is common to use row or column permutations.
! This is done to improve the sparsity of ILU decomposition.
! In case of column permutation, the solution needs to be permutated
! In case of row permulation, the rhs needs to be permutated

ABSTRACT INTERFACE
  SUBROUTINE amField_reversePermutation(obj, rhs, sol)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: rhs
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: sol
  END SUBROUTINE amField_reversePermutation
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isPreconditionSet@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION amField_isPreconditionSet(obj) RESULT(Ans)
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION amField_isPreconditionSet
END INTERFACE

!----------------------------------------------------------------------------
!                                                              getDiagonal
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: REturns the diagnoal

ABSTRACT INTERFACE
  SUBROUTINE amField_GetDiagonal(obj, diag)
    IMPORT :: AbstractMatrixField_, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: diag(:)
  END SUBROUTINE amField_GetDiagonal
END INTERFACE

!----------------------------------------------------------------------------
!                                                              getDiagonal
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: Perform diagonal scaling to the matrix

ABSTRACT INTERFACE
  SUBROUTINE amField_DiagonalScaling(obj, side, diag, OPERATOR)
    IMPORT :: AbstractMatrixField_, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: side
    REAL(DFP), OPTIONAL, INTENT(IN) :: diag(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: OPERATOR
  END SUBROUTINE amField_DiagonalScaling
END INTERFACE

!----------------------------------------------------------------------------
!                                                              SPY@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine creates spy figure

INTERFACE
  MODULE SUBROUTINE amField_SPY(obj, filename, ext)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: filename
    CHARACTER(*), INTENT(IN) :: ext
  END SUBROUTINE amField_SPY
END INTERFACE

!----------------------------------------------------------------------------
!                                  SymSchurLargestEigenVal@SpectralMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-30
! summary: SymSchurLargestEigenVal

INTERFACE
  MODULE FUNCTION amField_SymSchurLargestEigenVal(obj, B, nev, which, NCV, &
      & maxIter, tol) RESULT(ans)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! CSRMatrix, symmetric
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: B
    !! B matrix, possibly rectangle
    INTEGER(I4B), INTENT(IN) :: nev
    !! number of eigenvalues requested
    CHARACTER(*), OPTIONAL, INTENT(IN) :: which
    !! `which = "LM"` ⇨ absolute largest eigenvalue
    !! `which = "LA"` ⇨ algebraic largest eigenvalue
    !! default is "LA"
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: NCV
    !! Number of Lanczos vectors generated
    !! It must be greater than 1 and smaller than `size(mat,1)`
    !! Default is `NCV = MIN(n, MAX(2*nev+1, 20))`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    !! Maximum number of iteration default = `N*10`
    REAL(DFP), OPTIONAL, INTENT(IN) :: tol
    !! tolerance, default = 0.0
    REAL(DFP) :: ans(nev)
    !! first k, largest eigenvalue
  END FUNCTION amField_SymSchurLargestEigenVal
END INTERFACE

!----------------------------------------------------------------------------
!                                  SymSchurLargestEigenVal@SpectralMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-30
! summary: SymSchurLargestEigenVal

INTERFACE
  MODULE FUNCTION amField_SymLargestEigenVal(obj, nev, which, NCV, &
      & maxIter, tol) RESULT(ans)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! CSRMatrix, symmetric
    INTEGER(I4B), INTENT(IN) :: nev
    !! number of eigenvalues requested
    CHARACTER(*), OPTIONAL, INTENT(IN) :: which
    !! `which = "LM"` ⇨ absolute largest eigenvalue
    !! `which = "LA"` ⇨ algebraic largest eigenvalue
    !! default is "LA"
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: NCV
    !! Number of Lanczos vectors generated
    !! It must be greater than 1 and smaller than `size(mat,1)`
    !! Default is `NCV = MIN(n, MAX(2*nev+1, 20))`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    !! Maximum number of iteration default = `N*10`
    REAL(DFP), OPTIONAL, INTENT(IN) :: tol
    !! tolerance, default = 0.0
    REAL(DFP) :: ans(nev)
    !! first k, largest eigenvalue
  END FUNCTION amField_SymLargestEigenVal
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_ApplyDBC(obj, dbcPtrs)
    IMPORT :: AbstractMatrixField_, I4B
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dbcPtrs(:)
  END SUBROUTINE amField_ApplyDBC
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_set1(obj, globalNode, VALUE, storageFMT, scale, &
    & addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: storageFMT
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_set2(obj, globalNode, VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_set3(obj, iNodeNum, jNodeNum, idof, jdof, VALUE, &
    & scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_set4(obj, iNodeNum, jNodeNum, ivar, jvar, VALUE, &
    & scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_set5(obj, iNodeNum, jNodeNum, ivar, jvar, idof,  &
    & jdof, VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_set6(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & idof, jdof, VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_set7(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & ispacecompo, itimecompo, jspacecompo, jtimecompo, &
    & VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    INTEGER(I4B), INTENT(IN) :: itimecompo
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_set8(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & ispacecompo, itimecompo, jspacecompo, jtimecompo, &
    & VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    INTEGER(I4B), INTENT(IN) :: itimecompo
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_set9(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & ispacecompo, itimecompo, jspacecompo, jtimecompo, &
    & VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    INTEGER(I4B), INTENT(IN) :: itimecompo(:)
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    INTEGER(I4B), INTENT(IN) :: jtimecompo(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_set10(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & ispacecompo, itimecompo, jspacecompo, jtimecompo, &
    & VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: ispacecompo(:)
    INTEGER(I4B), INTENT(IN) :: itimecompo
    INTEGER(I4B), INTENT(IN) :: jspacecompo(:)
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_setRow1(obj, globalNode, idof, scalarVal, vecVal, &
    & nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_setRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_setRow2(obj, globalNode, ivar, idof, scalarVal, vecVal, &
    & nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_setRow2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_setRow3(obj, globalNode, ivar, spacecompo, timecompo, &
    & scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_setRow3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_setRow4(obj, globalNode, ivar, spacecompo, timecompo, &
    & scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_setRow4
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_setRow5(obj, globalNode, ivar, spacecompo, timecompo, &
    & scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_setRow5
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_setRow6(obj, globalNode, ivar, spacecompo, timecompo, &
    & scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_setRow6
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_setRow7(obj, globalNode, ivar, spacecompo, timecompo, &
    & scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_setRow7
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
!
! This routine sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_setColumn1(obj, globalNode, idof, scalarVal, vecVal, &
    & nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_setColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
!
! This routine sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_setColumn2(obj, globalNode, ivar, idof, &
    & scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_setColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
!
! This routine sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_setColumn3(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_setColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
!
! This routine sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_setColumn4(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_setColumn4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
!
! This routine sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_setColumn5(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_setColumn5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
!
! This routine sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_setColumn6(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_setColumn6
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
!
! This routine sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_setColumn7(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_setColumn7
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix in rank1 fortran
! vector
!
!# Introduction
! This routine returns the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the row is returned inside the node field

ABSTRACT INTERFACE
  SUBROUTINE amField_getRow1(obj, globalNode, idof, VALUE, nodeFieldVal, &
    & scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_getRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix in rank1 fortran
! vector
!
!# Introduction
! This routine returns the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the row is returned inside the node field

ABSTRACT INTERFACE
  SUBROUTINE amField_getRow2(obj, globalNode, ivar, idof, VALUE, &
    & nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_getRow2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix in rank1 fortran
! vector
!
!# Introduction
! This routine returns the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the row is returned inside the node field

ABSTRACT INTERFACE
  SUBROUTINE amField_getRow3(obj, globalNode, ivar, spacecompo, timecompo, &
    & VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_getRow3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix in rank1 fortran
! vector
!
!# Introduction
! This routine returns the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the row is returned inside the node field

ABSTRACT INTERFACE
  SUBROUTINE amField_getRow4(obj, globalNode, ivar, spacecompo, timecompo, &
    & VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_getRow4
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix in rank1 fortran
! vector
!
!# Introduction
! This routine returns the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the row is returned inside the node field

ABSTRACT INTERFACE
  SUBROUTINE amField_getRow5(obj, globalNode, ivar, spacecompo, timecompo, &
    & VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_getRow5
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix in rank1 fortran
! vector
!
!# Introduction
! This routine returns the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the row is returned inside the node field

ABSTRACT INTERFACE
  SUBROUTINE amField_getRow6(obj, globalNode, ivar, spacecompo, timecompo, &
    & VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_getRow6
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getRow@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the row of a sparse matrix in rank1 fortran
! vector
!
!# Introduction
! This routine returns the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the row is returned inside the node field

ABSTRACT INTERFACE
  SUBROUTINE amField_getRow7(obj, globalNode, ivar, spacecompo, timecompo, &
    & VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_getRow7
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
! vector
!
!# Introduction
! This routine returns the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node
! field

ABSTRACT INTERFACE
  SUBROUTINE amField_getColumn1(obj, globalNode, idof, VALUE, nodeFieldVal, &
    & scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_getColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
! vector
!
!# Introduction
! This routine returns the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node
! field

ABSTRACT INTERFACE
  SUBROUTINE amField_getColumn2(obj, globalNode, ivar, idof, &
      & VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_getColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
! vector
!
!# Introduction
! This routine returns the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node
! field

ABSTRACT INTERFACE
  SUBROUTINE amField_getColumn3(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_getColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
! vector
!
!# Introduction
! This routine returns the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node
! field

ABSTRACT INTERFACE
  SUBROUTINE amField_getColumn4(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_getColumn4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
! vector
!
!# Introduction
! This routine returns the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node
! field

ABSTRACT INTERFACE
  SUBROUTINE amField_getColumn5(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_getColumn5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
! vector
!
!# Introduction
! This routine returns the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node
! field

ABSTRACT INTERFACE
  SUBROUTINE amField_getColumn6(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_getColumn6
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
! vector
!
!# Introduction
! This routine returns the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `value` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node
! field

ABSTRACT INTERFACE
  SUBROUTINE amField_getColumn7(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_getColumn7
END INTERFACE

END MODULE AbstractMatrixField_Class
