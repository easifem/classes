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
USE BaSetype
USE AbstractField_Class
USE AbstractNodeField_Class
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "AbstractMatrixField_Class"
PUBLIC :: AbstractMatrixField_
PUBLIC :: AbstractMatrixFieldDisplay
PUBLIC :: AbstractMatrixFieldDeallocate

!----------------------------------------------------------------------------
!                                                     AbstractMatrixField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This abstract class is defined to handle the finite element matrix
!
!{!pages/docs-api/AbstractMatrixField/AbstractMatrixField_.md!}

TYPE, ABSTRACT, EXTENDS(AbstractField_) :: AbstractMatrixField_
  LOGICAL(LGT) :: isPmatInitiated = .FALSE.
  !! True if precondition matrix is initiated
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Display => amField_Display
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => amField_Deallocate
  PROCEDURE(amField_Size), DEFERRED, PUBLIC, PASS(obj) :: Size
  PROCEDURE(amField_Shape), DEFERRED, PUBLIC, PASS(obj) :: Shape

  ! @MatVecMethods
  PROCEDURE(amField_Matvec1), DEFERRED, PASS(obj) :: Matvec1
  !! Matrix vector multiplication, here vector is fortran array
  PROCEDURE(amField_Matvec2), DEFERRED, PASS(obj) :: Matvec2
  !! Matrix vector multiplication, here vector is AbstractNodeField_
  GENERIC, PUBLIC :: Matvec => Matvec1, Matvec2

  ! @ILUSolveMethods
  PROCEDURE(amField_ILUSOLVE1), DEFERRED, PASS(obj) :: ILUSOLVE1
  !! Matrix vector multiplication, here vector is fortran array
  PROCEDURE(amField_ILUSOLVE2), DEFERRED, PASS(obj) :: ILUSOLVE2
  !! Matrix vector multiplication, here vector is AbstractNodeField_
  GENERIC, PUBLIC :: ILUSOLVE => ILUSOLVE1, ILUSOLVE2
  !! Generic LU Solve

  PROCEDURE, PUBLIC, PASS(obj) :: isPreconditionSet => &
    & amField_isPreconditionSet
  !! True if prcondition is Set
  PROCEDURE(amField_SetPrecondition), DEFERRED, PUBLIC, PASS(obj) :: &
    & SetPrecondition
  !! Build precondition matrix
  PROCEDURE(amField_GetPrecondition), DEFERRED, PUBLIC, PASS(obj) :: &
    & GetPrecondition
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
  PROCEDURE(amField_GetDBCSubMat), DEFERRED, PUBLIC, PASS(obj) ::  &
    & GetDBCSubMat
  ! ApplyDBC
  PROCEDURE(amField_ApplyDBCToRHS), DEFERRED, PUBLIC, PASS(obj) ::  &
    & ApplyDBCtoRHS
  PROCEDURE, PUBLIC, PASS(obj) :: SPY => amField_SPY
  ! SPY

  ! Set:
  ! @SetMethods
  PROCEDURE(amField_Set1), DEFERRED, PASS(obj) :: set1
  PROCEDURE(amField_Set2), DEFERRED, PASS(obj) :: set2
  PROCEDURE(amField_Set3), DEFERRED, PASS(obj) :: set3
  PROCEDURE(amField_Set4), DEFERRED, PASS(obj) :: set4
  PROCEDURE(amField_Set5), DEFERRED, PASS(obj) :: set5
  PROCEDURE(amField_Set6), DEFERRED, PASS(obj) :: set6
  PROCEDURE(amField_Set7), DEFERRED, PASS(obj) :: set7
  PROCEDURE(amField_Set8), DEFERRED, PASS(obj) :: set8
  PROCEDURE(amField_Set9), DEFERRED, PASS(obj) :: set9
  PROCEDURE(amField_Set10), DEFERRED, PASS(obj) :: set10
  GENERIC, PUBLIC :: Set => set1, set2, set3, set4, set5, &
    & Set6, set7, set8, set9, set10

  ! Set:
  ! @SetRow
  PROCEDURE(amField_SetRow1), DEFERRED, PASS(obj) :: setRow1
  PROCEDURE(amField_SetRow2), DEFERRED, PASS(obj) :: setRow2
  PROCEDURE(amField_SetRow3), DEFERRED, PASS(obj) :: setRow3
  PROCEDURE(amField_SetRow4), DEFERRED, PASS(obj) :: setRow4
  PROCEDURE(amField_SetRow5), DEFERRED, PASS(obj) :: setRow5
  PROCEDURE(amField_SetRow6), DEFERRED, PASS(obj) :: setRow6
  PROCEDURE(amField_SetRow7), DEFERRED, PASS(obj) :: setRow7
  GENERIC, PUBLIC :: SetRow => setRow1, setRow2, setRow3, &
    & SetRow4, setRow5, setRow6, setRow7

  ! Get:
  ! @GetColumn
  PROCEDURE(amField_GetColumn1), DEFERRED, PASS(obj) :: getColumn1
  PROCEDURE(amField_GetColumn2), DEFERRED, PASS(obj) :: getColumn2
  PROCEDURE(amField_GetColumn3), DEFERRED, PASS(obj) :: getColumn3
  PROCEDURE(amField_GetColumn4), DEFERRED, PASS(obj) :: getColumn4
  PROCEDURE(amField_GetColumn5), DEFERRED, PASS(obj) :: getColumn5
  PROCEDURE(amField_GetColumn6), DEFERRED, PASS(obj) :: getColumn6
  PROCEDURE(amField_GetColumn7), DEFERRED, PASS(obj) :: getColumn7
  GENERIC, PUBLIC :: GetColumn => getColumn1, getColumn2, &
    & GetColumn3, getColumn4, getColumn5, getColumn6, getColumn7

  ! Get:
  ! @GetRow
  PROCEDURE(amField_GetRow1), DEFERRED, PASS(obj) :: getRow1
  PROCEDURE(amField_GetRow2), DEFERRED, PASS(obj) :: getRow2
  PROCEDURE(amField_GetRow3), DEFERRED, PASS(obj) :: getRow3
  PROCEDURE(amField_GetRow4), DEFERRED, PASS(obj) :: getRow4
  PROCEDURE(amField_GetRow5), DEFERRED, PASS(obj) :: getRow5
  PROCEDURE(amField_GetRow6), DEFERRED, PASS(obj) :: getRow6
  PROCEDURE(amField_GetRow7), DEFERRED, PASS(obj) :: getRow7
  GENERIC, PUBLIC :: GetRow => getRow1, getRow2, &
    & GetRow3, getRow4, getRow5, getRow6, getRow7

  ! Set:
  ! @SetColumn
  PROCEDURE(amField_SetColumn1), DEFERRED, PASS(obj) :: setColumn1
  PROCEDURE(amField_SetColumn2), DEFERRED, PASS(obj) :: setColumn2
  PROCEDURE(amField_SetColumn3), DEFERRED, PASS(obj) :: setColumn3
  PROCEDURE(amField_SetColumn4), DEFERRED, PASS(obj) :: setColumn4
  PROCEDURE(amField_SetColumn5), DEFERRED, PASS(obj) :: setColumn5
  PROCEDURE(amField_SetColumn6), DEFERRED, PASS(obj) :: setColumn6
  PROCEDURE(amField_SetColumn7), DEFERRED, PASS(obj) :: setColumn7
  GENERIC, PUBLIC :: SetColumn => setColumn1, setColumn2, &
    & SetColumn3, setColumn4, setColumn5, setColumn6, setColumn7
END TYPE AbstractMatrixField_

!----------------------------------------------------------------------------
!                                                                Display
!----------------------------------------------------------------------------

INTERFACE AbstractMatrixFieldDisplay
  MODULE SUBROUTINE amField_Display(obj, msg, unitNo)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE amField_Display
END INTERFACE AbstractMatrixFieldDisplay

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

INTERFACE AbstractMatrixFieldDeallocate
  MODULE SUBROUTINE amField_Deallocate(obj)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
  END SUBROUTINE amField_Deallocate
END INTERFACE AbstractMatrixFieldDeallocate

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
    !! if true then we do not Set y = 0, and perform
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
    !! if true then we do not Set y = 0, and perform
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
!                                                           SetPrecondition
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine Sets the precondition

ABSTRACT INTERFACE
  SUBROUTINE amField_SetPrecondition(obj, param, dbcPtrs)
    IMPORT :: AbstractMatrixField_, ParameterList_, I4B
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), OPTIONAL, INTENT(IN) :: param
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcPtrs(:)
  END SUBROUTINE amField_SetPrecondition
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetPrecondition
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine returns the precondition

ABSTRACT INTERFACE
  SUBROUTINE amField_GetPrecondition(obj, Pmat)
    IMPORT :: AbstractMatrixField_
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: Pmat
  END SUBROUTINE amField_GetPrecondition
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
!                                                              GetDiagonal
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
!                                                              GetDiagonal
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
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcPtrs(:)
  END SUBROUTINE amField_ApplyDBC
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_GetDBCSubMat(obj, submat)
    IMPORT :: AbstractMatrixField_, I4B
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: submat
  END SUBROUTINE amField_GetDBCSubMat
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyDBCtoRHS@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Apply dirichlet boundary condition to a node field

ABSTRACT INTERFACE
  SUBROUTINE amField_ApplyDBCToRHS(obj, x, y, isTranspose,  &
    & scale, addContribution)
    IMPORT :: AbstractMatrixField_, LGT, DFP, AbstractNodeField_
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: x
    CLASS(AbstractNodeField_), INTENT(INOUT) :: y
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_ApplyDBCToRHS
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_Set1(obj, globalNode, VALUE, storageFMT, scale, &
    & addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: storageFMT
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_Set2(obj, globalNode, VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_Set3(obj, iNodeNum, jNodeNum, idof, jdof, VALUE, &
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
  END SUBROUTINE amField_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_Set4(obj, iNodeNum, jNodeNum, ivar, jvar, VALUE, &
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
  END SUBROUTINE amField_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_Set5(obj, iNodeNum, jNodeNum, ivar, jvar, idof,  &
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
  END SUBROUTINE amField_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_Set6(obj, iNodeNum, jNodeNum, ivar, jvar, &
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
  END SUBROUTINE amField_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_Set7(obj, iNodeNum, jNodeNum, ivar, jvar, &
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
  END SUBROUTINE amField_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_Set8(obj, iNodeNum, jNodeNum, ivar, jvar, &
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
  END SUBROUTINE amField_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_Set9(obj, iNodeNum, jNodeNum, ivar, jvar, &
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
  END SUBROUTINE amField_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE amField_Set10(obj, iNodeNum, jNodeNum, ivar, jvar, &
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
  END SUBROUTINE amField_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_SetRow1(obj, globalNode, idof, scalarVal, vecVal, &
    & nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_SetRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_SetRow2(obj, globalNode, ivar, idof, scalarVal, vecVal, &
    & nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_SetRow2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_SetRow3(obj, globalNode, ivar, spacecompo, timecompo, &
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
  END SUBROUTINE amField_SetRow3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_SetRow4(obj, globalNode, ivar, spacecompo, timecompo, &
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
  END SUBROUTINE amField_SetRow4
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_SetRow5(obj, globalNode, ivar, spacecompo, timecompo, &
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
  END SUBROUTINE amField_SetRow5
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_SetRow6(obj, globalNode, ivar, spacecompo, timecompo, &
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
  END SUBROUTINE amField_SetRow6
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_SetRow7(obj, globalNode, ivar, spacecompo, timecompo, &
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
  END SUBROUTINE amField_SetRow7
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
!
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_SetColumn1(obj, globalNode, idof, scalarVal, vecVal, &
    & nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_SetColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
!
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_SetColumn2(obj, globalNode, ivar, idof, &
    & scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE amField_SetColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
!
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_SetColumn3(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE amField_SetColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
!
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_SetColumn4(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE amField_SetColumn4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
!
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_SetColumn5(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE amField_SetColumn5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
!
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_SetColumn6(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE amField_SetColumn6
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
!
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
!
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

ABSTRACT INTERFACE
  SUBROUTINE amField_SetColumn7(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE amField_SetColumn7
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@getMethod
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
  SUBROUTINE amField_GetRow1(obj, globalNode, idof, VALUE, nodeFieldVal, &
    & scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_GetRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@getMethod
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
  SUBROUTINE amField_GetRow2(obj, globalNode, ivar, idof, VALUE, &
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
  END SUBROUTINE amField_GetRow2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@getMethod
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
  SUBROUTINE amField_GetRow3(obj, globalNode, ivar, spacecompo, timecompo, &
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
  END SUBROUTINE amField_GetRow3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@getMethod
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
  SUBROUTINE amField_GetRow4(obj, globalNode, ivar, spacecompo, timecompo, &
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
  END SUBROUTINE amField_GetRow4
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@getMethod
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
  SUBROUTINE amField_GetRow5(obj, globalNode, ivar, spacecompo, timecompo, &
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
  END SUBROUTINE amField_GetRow5
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@getMethod
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
  SUBROUTINE amField_GetRow6(obj, globalNode, ivar, spacecompo, timecompo, &
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
  END SUBROUTINE amField_GetRow6
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@getMethod
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
  SUBROUTINE amField_GetRow7(obj, globalNode, ivar, spacecompo, timecompo, &
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
  END SUBROUTINE amField_GetRow7
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@getMethod
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
  SUBROUTINE amField_GetColumn1(obj, globalNode, idof, VALUE, nodeFieldVal, &
    & scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE amField_GetColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@getMethod
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
  SUBROUTINE amField_GetColumn2(obj, globalNode, ivar, idof, &
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
  END SUBROUTINE amField_GetColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@getMethod
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
  SUBROUTINE amField_GetColumn3(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE amField_GetColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@getMethod
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
  SUBROUTINE amField_GetColumn4(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE amField_GetColumn4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@getMethod
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
  SUBROUTINE amField_GetColumn5(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE amField_GetColumn5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@getMethod
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
  SUBROUTINE amField_GetColumn6(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE amField_GetColumn6
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@getMethod
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
  SUBROUTINE amField_GetColumn7(obj, globalNode, ivar, spacecompo, &
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
  END SUBROUTINE amField_GetColumn7
END INTERFACE

END MODULE AbstractMatrixField_Class
