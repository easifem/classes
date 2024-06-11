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
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
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

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data

  PROCEDURE(obj_Size), DEFERRED, PUBLIC, PASS(obj) :: Size
  !! Get the Size

  PROCEDURE(obj_Shape), DEFERRED, PUBLIC, PASS(obj) :: Shape
  !! Get the Shape

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of Abstract matrix field

  ! GET:
  ! @MatVecMethods

  PROCEDURE(obj_Matvec1), DEFERRED, PASS(obj) :: Matvec1
  !! Matrix vector multiplication, here vector is fortran array

  PROCEDURE(obj_Matvec2), DEFERRED, PASS(obj) :: Matvec2
  !! Matrix vector multiplication, here vector is AbstractNodeField_

  GENERIC, PUBLIC :: Matvec => Matvec1, Matvec2

  ! GET:
  ! @ILUSolveMethods

  PROCEDURE(obj_ILUSOLVE1), DEFERRED, PASS(obj) :: ILUSOLVE1
  !! LU Solver, here x and y are rank-1 fortan vector

  PROCEDURE(obj_ILUSOLVE2), DEFERRED, PASS(obj) :: ILUSOLVE2
  !! LU solver, here x and y are AbstractNodeField_

  GENERIC, PUBLIC :: ILUSOLVE => ILUSOLVE1, ILUSOLVE2
  !! Generic LU Solver

  PROCEDURE, PUBLIC, PASS(obj) :: isPreconditionSet => obj_isPreconditionSet
  !! True if prcondition is Set

PROCEDURE(obj_SetPrecondition), DEFERRED, PUBLIC, PASS(obj) :: SetPrecondition
  !! Build precondition matrix

PROCEDURE(obj_GetPrecondition), DEFERRED, PUBLIC, PASS(obj) :: GetPrecondition
  !! Get the precondition matrix

  PROCEDURE(obj_reversePermutation), DEFERRED, PUBLIC, PASS(obj) :: reversePermutation
  !!

PROCEDURE(obj_DiagonalScaling), DEFERRED, PUBLIC, PASS(obj) :: DiagonalScaling
  !! DiagonalScaling

  PROCEDURE(obj_GetDiagonal), DEFERRED, PUBLIC, PASS(obj) :: GetDiagonal
  !! Get Diagonal

  PROCEDURE, PUBLIC, PASS(obj) :: SymSchurLargestEigenVal => obj_SymSchurLargestEigenVal
  !! SymSchurLargestEigenVal

  PROCEDURE, PUBLIC, PASS(obj) :: SymLargestEigenVal => obj_SymLargestEigenVal
  !! SymLargestEigenVal

  PROCEDURE(obj_ApplyDBC), DEFERRED, PUBLIC, PASS(obj) :: ApplyDBC
  !! ApplyDBC

  PROCEDURE(obj_GetDBCSubMat), DEFERRED, PUBLIC, PASS(obj) :: GetDBCSubMat
  !! ApplyDBC

  PROCEDURE(obj_ApplyDBCToRHS), DEFERRED, PUBLIC, PASS(obj) :: ApplyDBCtoRHS
  !! ApplyDBC

  PROCEDURE, PUBLIC, PASS(obj) :: SPY => obj_SPY
  ! SPY

  ! SET:
  ! @SetMethods

  PROCEDURE(obj_Set1), DEFERRED, PASS(obj) :: Set1
  PROCEDURE(obj_Set2), DEFERRED, PASS(obj) :: Set2
  PROCEDURE(obj_Set3), DEFERRED, PASS(obj) :: Set3
  PROCEDURE(obj_Set4), DEFERRED, PASS(obj) :: Set4
  PROCEDURE(obj_Set5), DEFERRED, PASS(obj) :: Set5
  PROCEDURE(obj_Set6), DEFERRED, PASS(obj) :: Set6
  PROCEDURE(obj_Set7), DEFERRED, PASS(obj) :: Set7
  PROCEDURE(obj_Set8), DEFERRED, PASS(obj) :: Set8
  PROCEDURE(obj_Set9), DEFERRED, PASS(obj) :: Set9
  PROCEDURE(obj_Set10), DEFERRED, PASS(obj) :: Set10
  PROCEDURE(obj_Set11), DEFERRED, PASS(obj) :: Set11

  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, Set5, &
    Set6, Set7, Set8, Set9, Set10, Set11

PROCEDURE(obj_SetFromSTMatrix), DEFERRED, PUBLIC, PASS(obj) :: SetFromSTMatrix

  PROCEDURE(obj_SetToSTMatrix), DEFERRED, PUBLIC, PASS(obj) :: SetToSTMatrix

  ! SET:
  ! @SetRow

  PROCEDURE(obj_SetRow1), DEFERRED, PASS(obj) :: SetRow1
  PROCEDURE(obj_SetRow2), DEFERRED, PASS(obj) :: SetRow2
  PROCEDURE(obj_SetRow3), DEFERRED, PASS(obj) :: SetRow3
  PROCEDURE(obj_SetRow4), DEFERRED, PASS(obj) :: SetRow4
  PROCEDURE(obj_SetRow5), DEFERRED, PASS(obj) :: SetRow5
  PROCEDURE(obj_SetRow6), DEFERRED, PASS(obj) :: SetRow6
  PROCEDURE(obj_SetRow7), DEFERRED, PASS(obj) :: SetRow7
  GENERIC, PUBLIC :: SetRow => SetRow1, SetRow2, SetRow3, &
    SetRow4, SetRow5, SetRow6, SetRow7

  ! SET:
  ! @SetColumn
  PROCEDURE(obj_SetColumn1), DEFERRED, PASS(obj) :: SetColumn1
  PROCEDURE(obj_SetColumn2), DEFERRED, PASS(obj) :: SetColumn2
  PROCEDURE(obj_SetColumn3), DEFERRED, PASS(obj) :: SetColumn3
  PROCEDURE(obj_SetColumn4), DEFERRED, PASS(obj) :: SetColumn4
  PROCEDURE(obj_SetColumn5), DEFERRED, PASS(obj) :: SetColumn5
  PROCEDURE(obj_SetColumn6), DEFERRED, PASS(obj) :: SetColumn6
  PROCEDURE(obj_SetColumn7), DEFERRED, PASS(obj) :: SetColumn7
  GENERIC, PUBLIC :: SetColumn => SetColumn1, SetColumn2, &
    SetColumn3, SetColumn4, SetColumn5, SetColumn6, SetColumn7

  ! GET:
  ! @GetMethods
  PROCEDURE(obj_Get1), DEFERRED, PASS(obj) :: Get1
  PROCEDURE(obj_Get2), DEFERRED, PASS(obj) :: Get2
  PROCEDURE(obj_Get3), DEFERRED, PASS(obj) :: Get3
  PROCEDURE(obj_Get4), DEFERRED, PASS(obj) :: Get4
  PROCEDURE(obj_Get5), DEFERRED, PASS(obj) :: Get5
  PROCEDURE(obj_Get6), DEFERRED, PASS(obj) :: Get6
  PROCEDURE(obj_Get7), DEFERRED, PASS(obj) :: Get7
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, Get5, Get6, Get7

  ! GET:
  ! @GetColumn
  PROCEDURE(obj_GetColumn1), DEFERRED, PASS(obj) :: GetColumn1
  PROCEDURE(obj_GetColumn2), DEFERRED, PASS(obj) :: GetColumn2
  PROCEDURE(obj_GetColumn3), DEFERRED, PASS(obj) :: GetColumn3
  PROCEDURE(obj_GetColumn4), DEFERRED, PASS(obj) :: GetColumn4
  PROCEDURE(obj_GetColumn5), DEFERRED, PASS(obj) :: GetColumn5
  PROCEDURE(obj_GetColumn6), DEFERRED, PASS(obj) :: GetColumn6
  PROCEDURE(obj_GetColumn7), DEFERRED, PASS(obj) :: GetColumn7
  GENERIC, PUBLIC :: GetColumn => GetColumn1, GetColumn2, &
    GetColumn3, GetColumn4, GetColumn5, GetColumn6, GetColumn7

  ! GET:
  ! @GetRow
  PROCEDURE(obj_GetRow1), DEFERRED, PASS(obj) :: GetRow1
  PROCEDURE(obj_GetRow2), DEFERRED, PASS(obj) :: GetRow2
  PROCEDURE(obj_GetRow3), DEFERRED, PASS(obj) :: GetRow3
  PROCEDURE(obj_GetRow4), DEFERRED, PASS(obj) :: GetRow4
  PROCEDURE(obj_GetRow5), DEFERRED, PASS(obj) :: GetRow5
  PROCEDURE(obj_GetRow6), DEFERRED, PASS(obj) :: GetRow6
  PROCEDURE(obj_GetRow7), DEFERRED, PASS(obj) :: GetRow7
  GENERIC, PUBLIC :: GetRow => GetRow1, GetRow2, &
    GetRow3, GetRow4, GetRow5, GetRow6, GetRow7

  ! GET:
  ! @UnaryMethods
  PROCEDURE(obj_Scal), DEFERRED, PUBLIC, PASS(obj) :: Scal

END TYPE AbstractMatrixField_

!----------------------------------------------------------------------------
!                                                                Display
!----------------------------------------------------------------------------

INTERFACE AbstractMatrixFieldDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE AbstractMatrixFieldDisplay

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

INTERFACE AbstractMatrixFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractMatrixFieldDeallocate

!----------------------------------------------------------------------------
!                                                                      SIZE
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  FUNCTION obj_Size(obj, dim) RESULT(ans)
    IMPORT :: AbstractMatrixField_, I4B
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      SHAPE
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  FUNCTION obj_Shape(obj) RESULT(ans)
    IMPORT :: AbstractMatrixField_, I4B
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans(2)
  END FUNCTION obj_Shape
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
! This routine returns the matrix vector multiplication.
! Here, input vector is a native fortran vector.
! The output vector is also a native fortran vector.
! It should be noted that the output vector should be allocated
! outside and it should have same length as the input vector.

ABSTRACT INTERFACE
  SUBROUTINE obj_Matvec1(obj, x, y, isTranspose, addContribution, scale)
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
  END SUBROUTINE obj_Matvec1
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
!
! y = y + Scale*Matvec(obj, x)

ABSTRACT INTERFACE
  SUBROUTINE obj_Matvec2(obj, x, y, isTranspose, addContribution, scale)
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
  END SUBROUTINE obj_Matvec2
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
! If isTranspose is absent or it is false then:
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
  SUBROUTINE obj_ILUSOLVE1(obj, sol, rhs, isTranspose)
    IMPORT :: AbstractMatrixField_, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: sol(:)
    !! Output vector y=Ax
    REAL(DFP), INTENT(IN) :: rhs(:)
    !! Input vector in y=Ax
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
  END SUBROUTINE obj_ILUSOLVE1
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
  SUBROUTINE obj_ILUSOLVE2(obj, sol, rhs, isTranspose)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: sol
    !! Output vector
    CLASS(AbstractNodeField_), INTENT(IN) :: rhs
    !! Input vector, rhs
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
  END SUBROUTINE obj_ILUSOLVE2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SetPrecondition
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine Sets the precondition

ABSTRACT INTERFACE
  SUBROUTINE obj_SetPrecondition(obj, param, dbcPtrs)
    IMPORT :: AbstractMatrixField_, ParameterList_, I4B
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), OPTIONAL, INTENT(IN) :: param
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcPtrs(:)
  END SUBROUTINE obj_SetPrecondition
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetPrecondition
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine returns the precondition

ABSTRACT INTERFACE
  SUBROUTINE obj_GetPrecondition(obj, Pmat)
    IMPORT :: AbstractMatrixField_
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: Pmat
  END SUBROUTINE obj_GetPrecondition
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
  SUBROUTINE obj_reversePermutation(obj, rhs, sol)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: rhs
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: sol
  END SUBROUTINE obj_reversePermutation
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isPreconditionSet@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_isPreconditionSet(obj) RESULT(Ans)
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isPreconditionSet
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetDiagonal
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: REturns the diagnoal

ABSTRACT INTERFACE
  SUBROUTINE obj_GetDiagonal(obj, diag)
    IMPORT :: AbstractMatrixField_, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: diag(:)
  END SUBROUTINE obj_GetDiagonal
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetDiagonal
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: Perform diagonal scaling to the matrix

ABSTRACT INTERFACE
  SUBROUTINE obj_DiagonalScaling(obj, side, diag, OPERATOR)
    IMPORT :: AbstractMatrixField_, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: side
    REAL(DFP), OPTIONAL, INTENT(IN) :: diag(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: OPERATOR
  END SUBROUTINE obj_DiagonalScaling
END INTERFACE

!----------------------------------------------------------------------------
!                                                              SPY@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine creates spy figure

INTERFACE
  MODULE SUBROUTINE obj_SPY(obj, filename, ext)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: filename
    CHARACTER(*), INTENT(IN) :: ext
  END SUBROUTINE obj_SPY
END INTERFACE

!----------------------------------------------------------------------------
!                                  SymSchurLargestEigenVal@SpectralMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-30
! summary: SymSchurLargestEigenVal

INTERFACE
  MODULE FUNCTION obj_SymSchurLargestEigenVal(obj, B, nev, which, NCV, &
                                              maxIter, tol) RESULT(ans)
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
  END FUNCTION obj_SymSchurLargestEigenVal
END INTERFACE

!----------------------------------------------------------------------------
!                                  SymSchurLargestEigenVal@SpectralMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-30
! summary: SymSchurLargestEigenVal

INTERFACE
  MODULE FUNCTION obj_SymLargestEigenVal(obj, nev, which, NCV, &
                                         maxIter, tol) RESULT(ans)
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
  END FUNCTION obj_SymLargestEigenVal
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_ApplyDBC(obj, dbcPtrs)
    IMPORT :: AbstractMatrixField_, I4B
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcPtrs(:)
  END SUBROUTINE obj_ApplyDBC
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_GetDBCSubMat(obj, submat)
    IMPORT :: AbstractMatrixField_, I4B
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: submat
  END SUBROUTINE obj_GetDBCSubMat
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyDBCtoRHS@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Apply dirichlet boundary condition to a node field

ABSTRACT INTERFACE
  SUBROUTINE obj_ApplyDBCToRHS(obj, x, y, isTranspose,  &
    & scale, addContribution)
    IMPORT :: AbstractMatrixField_, LGT, DFP, AbstractNodeField_
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: x
    CLASS(AbstractNodeField_), INTENT(INOUT) :: y
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_ApplyDBCToRHS
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Set1(obj, globalNode, islocal, VALUE, storageFMT, scale, &
                      addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: storageFMT
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Set2(obj, VALUE, globalNode, islocal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Set3(obj, iNodeNum, jNodeNum, islocal, idof, jdof, VALUE, &
                      scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Set4(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, VALUE, &
                      scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Set5(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, idof, &
                      jdof, VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Set6(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      idof, jdof, VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Set7(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      ispacecompo, itimecompo, jspacecompo, jtimecompo, &
                      VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    INTEGER(I4B), INTENT(IN) :: itimecompo
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Set8(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      ispacecompo, itimecompo, jspacecompo, jtimecompo, &
                      VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    INTEGER(I4B), INTENT(IN) :: itimecompo
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Set9(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      ispacecompo, itimecompo, jspacecompo, jtimecompo, &
                      VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    INTEGER(I4B), INTENT(IN) :: itimecompo(:)
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    INTEGER(I4B), INTENT(IN) :: jtimecompo(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Set10(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                       ispacecompo, itimecompo, jspacecompo, jtimecompo, &
                       VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: ispacecompo(:)
    INTEGER(I4B), INTENT(IN) :: itimecompo
    INTEGER(I4B), INTENT(IN) :: jspacecompo(:)
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-16
! summary:  Set matrix

ABSTRACT INTERFACE
  SUBROUTINE obj_Set11(obj, VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetFromSTMatrix@SetMethods
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_SetFromSTMatrix(obj, VALUE, a, b)
    IMPORT :: AbstractMatrixField_, I4B
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: VALUE
    !! Space-time matrix field
    INTEGER(I4B), INTENT(IN) :: a
    !! itimecompo
    INTEGER(I4B), INTENT(IN) :: b
    !! jtimecompo
  END SUBROUTINE obj_SetFromSTMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetToSTMatrix@SetMethods
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_SetToSTMatrix(obj, VALUE, a, b)
    IMPORT :: AbstractMatrixField_, I4B
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! Space-time matrix
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: VALUE
    !! Space matrix field
    INTEGER(I4B), INTENT(IN) :: a
    !! itimecompo
    INTEGER(I4B), INTENT(IN) :: b
    !! jtimecompo
  END SUBROUTINE obj_SetToSTMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
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
  SUBROUTINE obj_SetRow1(obj, globalNode, islocal, idof, scalarVal, vecVal, &
                         nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
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
  SUBROUTINE obj_SetRow2(obj, globalNode, islocal, ivar, idof, scalarVal, vecVal, &
                         nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetRow2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
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
  SUBROUTINE obj_SetRow3(obj, globalNode, islocal, ivar, spacecompo, timecompo, &
                         scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetRow3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
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
  SUBROUTINE obj_SetRow4(obj, globalNode, islocal, ivar, spacecompo, timecompo, &
                         scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetRow4
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
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
  SUBROUTINE obj_SetRow5(obj, globalNode, islocal, ivar, spacecompo, timecompo, &
                         scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetRow5
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
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
  SUBROUTINE obj_SetRow6(obj, globalNode, islocal, ivar, spacecompo, timecompo, &
                         scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetRow6
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
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
  SUBROUTINE obj_SetRow7(obj, globalNode, islocal, ivar, spacecompo, timecompo, &
                         scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetRow7
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
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
SUBROUTINE obj_SetColumn1(obj, globalNode, islocal, idof, scalarVal, vecVal, &
                            nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
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
  SUBROUTINE obj_SetColumn2(obj, globalNode, islocal, ivar, idof, &
                            scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
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
  SUBROUTINE obj_SetColumn3(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
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
  SUBROUTINE obj_SetColumn4(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetColumn4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
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
  SUBROUTINE obj_SetColumn5(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetColumn5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
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
  SUBROUTINE obj_SetColumn6(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetColumn6
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
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
  SUBROUTINE obj_SetColumn7(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE obj_SetColumn7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Get1(obj, globalNode, islocal, VALUE, nrow, ncol, storageFMT)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! Storage format of value
    !! The default value is storage format of obj
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Get2(obj, iNodeNum, jNodeNum, islocal, idof, jdof, VALUE)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(INOUT) :: VALUE
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Get3(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      VALUE, nrow, ncol)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Get4(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, idof, &
                      jdof, VALUE, nrow, ncol)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Get5(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      idof, jdof, VALUE)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(INOUT) :: VALUE
  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Get6(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      ispacecompo, itimecompo, jspacecompo, jtimecompo, &
                      VALUE)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    INTEGER(I4B), INTENT(IN) :: itimecompo
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    REAL(DFP), INTENT(INOUT) :: VALUE
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Get@GetMethods
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE obj_Get7(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      ispacecompo, itimecompo, jspacecompo, jtimecompo, &
                      VALUE, nrow, ncol)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    INTEGER(I4B), INTENT(IN) :: itimecompo
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE obj_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@GetMethod
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
 SUBROUTINE obj_GetRow1(obj, globalNode, islocal, idof, VALUE, nodeFieldVal, &
                         scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@GetMethod
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
  SUBROUTINE obj_GetRow2(obj, globalNode, islocal, ivar, idof, VALUE, &
                         nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetRow2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@GetMethod
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
  SUBROUTINE obj_GetRow3(obj, globalNode, islocal, ivar, spacecompo, timecompo, &
                         VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetRow3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@GetMethod
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
  SUBROUTINE obj_GetRow4(obj, globalNode, islocal, ivar, spacecompo, timecompo, &
                         VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetRow4
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@GetMethod
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
  SUBROUTINE obj_GetRow5(obj, globalNode, islocal, ivar, spacecompo, timecompo, &
                         VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetRow5
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@GetMethod
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
  SUBROUTINE obj_GetRow6(obj, globalNode, islocal, ivar, spacecompo, timecompo, &
                         VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetRow6
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRow@GetMethod
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
  SUBROUTINE obj_GetRow7(obj, globalNode, islocal, ivar, spacecompo, timecompo, &
                         VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetRow7
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@GetMethod
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
  SUBROUTINE obj_GetColumn1(obj, globalNode, islocal, idof, VALUE, nodeFieldVal, &
                            scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@GetMethod
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
  SUBROUTINE obj_GetColumn2(obj, globalNode, islocal, ivar, idof, &
                            VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@GetMethod
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
  SUBROUTINE obj_GetColumn3(obj, globalNode, islocal, ivar, spacecompo, &
                       timecompo, VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@GetMethod
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
  SUBROUTINE obj_GetColumn4(obj, globalNode, islocal, ivar, spacecompo, &
                       timecompo, VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetColumn4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@GetMethod
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
  SUBROUTINE obj_GetColumn5(obj, globalNode, islocal, ivar, spacecompo, &
                       timecompo, VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetColumn5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@GetMethod
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
  SUBROUTINE obj_GetColumn6(obj, globalNode, islocal, ivar, spacecompo, &
                       timecompo, VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetColumn6
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@GetMethod
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
  SUBROUTINE obj_GetColumn7(obj, globalNode, islocal, ivar, spacecompo, &
                       timecompo, VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_GetColumn7
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Scale@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary:  Scale the matrix field

ABSTRACT INTERFACE
  SUBROUTINE obj_Scal(obj, a)
    IMPORT :: AbstractMatrixField_, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: a
  END SUBROUTINE obj_Scal
END INTERFACE

END MODULE AbstractMatrixField_Class
