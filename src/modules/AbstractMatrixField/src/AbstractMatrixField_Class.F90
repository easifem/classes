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

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-15
! summary: Abstract class for finite element matrix fields
!
! This module defines [[AbstractMatrixField_]], the parent of all global
! finite element matrices (tangent, mass, stiffness) in easifemClasses.
! It declares the deferred interface for setting and getting entries,
! rows and columns, matrix vector products, Dirichlet boundary
! conditions, preconditioning and scaling.
!
! The native implementation is [[MatrixField_]]. All examples of this
! module use it.

MODULE AbstractMatrixField_Class
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
IMPLICIT NONE
PRIVATE

PUBLIC :: AbstractMatrixField_
PUBLIC :: AbstractMatrixFieldDisplay
PUBLIC :: AbstractMatrixFieldDeallocate
PUBLIC :: AbstractMatrixFieldSetFEDOF

!----------------------------------------------------------------------------
!                                                     AbstractMatrixField_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Abstract class for a global finite element matrix
!
! [[AbstractMatrixField_]] extends [[AbstractField_]] with the methods of a
! sparse global matrix. The rows and columns of the matrix are the
! degrees of freedom of one or two FEDOF objects. An entry is addressed
! by a node number and a degree of freedom number, or by a node number,
! a physical variable and its space and time components.
!
! The rows of the matrix belong to `rowFedof` (or `rowFedofs`, one per
! physical variable), and the columns to `colFedof` (or `colFedofs`).
! For a square matrix the column pointers point to the same objects as
! the row pointers. The storage of the matrix and of the preconditioner
! is defined by the child class, for example [[MatrixField_]] stores a
! CSR matrix.
!
! Changes:
!   - 2026-11-11: Added the row and column FEDOF pointers, see
!     [[AbstractMatrixFieldSetFEDOF]].
!   - 2026-11-12: The specific bindings behind the Set, SetRow, SetColumn,
!     Get, GetRow, GetColumn, Matvec and ILUSOLVE generics are PUBLIC, so
!     that child types in other modules can override them (a PRIVATE
!     binding is not overridden from another module). Call the generics.
!
! See also: [[MatrixField_]], [[AbstractNodeField_]]

TYPE, ABSTRACT, EXTENDS(AbstractField_) :: AbstractMatrixField_
  LOGICAL(LGT) :: isPmatInitiated = .FALSE.
  !! True if the preconditioning matrix is initiated
  CLASS(FEDOF_), POINTER :: rowFedof => NULL()
  !! FEDOF of the rows
  CLASS(FEDOF_), POINTER :: colFedof => NULL()
  !! FEDOF of the columns, same as rowFedof for a square matrix
  CLASS(FEDOF_), POINTER :: rowGeoFedof => NULL()
  !! Geometric FEDOF of the rows
  CLASS(FEDOF_), POINTER :: colGeoFedof => NULL()
  !! Geometric FEDOF of the columns
  TYPE(FEDOFPointer_), ALLOCATABLE :: rowFedofs(:)
  !! FEDOF of the rows, one for each physical variable
  TYPE(FEDOFPointer_), ALLOCATABLE :: colFedofs(:)
  !! FEDOF of the columns, one for each physical variable
  TYPE(FEDOFPointer_), ALLOCATABLE :: rowGeoFedofs(:)
  !! Geometric FEDOF of the rows, one for each physical variable
  TYPE(FEDOFPointer_), ALLOCATABLE :: colGeoFedofs(:)
  !! Geometric FEDOF of the columns, one for each physical variable
CONTAINS
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data
  PROCEDURE(obj_Size), DEFERRED, PUBLIC, PASS(obj) :: Size
  !! Get the size of the matrix
  PROCEDURE(obj_Shape), DEFERRED, PUBLIC, PASS(obj) :: Shape
  !! Get the shape of the matrix
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate matrix from a row and a column node field
  GENERIC, PUBLIC :: Initiate => Initiate2

  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of the matrix field

  ! @HDFMethods
  PROCEDURE(obj_Import), DEFERRED, PUBLIC, PASS(obj) :: IMPORT
  !! Import the matrix field from HDF5 file

  ! @GetMethods
  PROCEDURE, NON_OVERRIDABLE, PRIVATE, PASS(obj) :: GetRowFEDOFPointer1 => &
    obj_GetRowFEDOFPointer1
  !! Get the row FEDOF pointer of a physical variable
  PROCEDURE, NON_OVERRIDABLE, PRIVATE, PASS(obj) :: GetRowFEDOFPointer2 => &
    obj_GetRowFEDOFPointer2
  !! Get the row FEDOF pointers of all physical variables
  GENERIC, PUBLIC :: GetRowFEDOFPointer => GetRowFEDOFPointer1, &
    GetRowFEDOFPointer2
  !! Generic method to get the row FEDOF pointers
  PROCEDURE, NON_OVERRIDABLE, PRIVATE, PASS(obj) :: GetColFEDOFPointer1 => &
    obj_GetColFEDOFPointer1
  !! Get the column FEDOF pointer of a physical variable
  PROCEDURE, NON_OVERRIDABLE, PRIVATE, PASS(obj) :: GetColFEDOFPointer2 => &
    obj_GetColFEDOFPointer2
  !! Get the column FEDOF pointers of all physical variables
  GENERIC, PUBLIC :: GetColFEDOFPointer => GetColFEDOFPointer1, &
    GetColFEDOFPointer2
  !! Generic method to get the column FEDOF pointers

  ! @MatVecMethods
  PROCEDURE(obj_Matvec1), DEFERRED, PUBLIC, PASS(obj) :: Matvec1
  !! Matrix vector multiplication, x and y are Fortran arrays
  PROCEDURE(obj_Matvec2), DEFERRED, PUBLIC, PASS(obj) :: Matvec2
  !! Matrix vector multiplication, x and y are node fields
  GENERIC, PUBLIC :: Matvec => Matvec1, Matvec2
  !! Generic matrix vector multiplication

  ! @ILUSolveMethods
  PROCEDURE(obj_ILUSOLVE1), PUBLIC, DEFERRED, PASS(obj) :: ILUSOLVE1
  !! Solve (LU) sol = rhs, sol and rhs are Fortran arrays
  PROCEDURE(obj_ILUSOLVE2), PUBLIC, DEFERRED, PASS(obj) :: ILUSOLVE2
  !! Solve (LU) sol = rhs, sol and rhs are node fields
  GENERIC, PUBLIC :: ILUSOLVE => ILUSOLVE1, ILUSOLVE2
  !! Generic solver with the incomplete LU preconditioner
  PROCEDURE, PUBLIC, PASS(obj) :: IsPreconditionSet => &
    obj_IsPreconditionSet
  !! True if the preconditioner is set
  PROCEDURE(obj_GetPrecondition), DEFERRED, PUBLIC, PASS(obj) :: &
    GetPrecondition
  !! Get the preconditioning matrix
  PROCEDURE(obj_reversePermutation), DEFERRED, PUBLIC, PASS(obj) :: &
    reversePermutation
  !! Undo the permutation of the preconditioner on rhs and sol
  PROCEDURE(obj_DiagonalScaling), DEFERRED, PUBLIC, PASS(obj) :: &
    DiagonalScaling
  !! Scale the matrix by a diagonal
  PROCEDURE(obj_GetDiagonal), DEFERRED, PUBLIC, PASS(obj) :: GetDiagonal
  !! Get the diagonal of the matrix
  PROCEDURE, PUBLIC, PASS(obj) :: SymSchurLargestEigenVal => &
    obj_SymSchurLargestEigenVal
  !! Largest eigenvalues of a symmetric Schur complement
  PROCEDURE, PUBLIC, PASS(obj) :: SymLargestEigenVal => &
    obj_SymLargestEigenVal
  !! Largest eigenvalues of a symmetric matrix
  PROCEDURE(obj_GetDirichletBCSubMat), DEFERRED, PUBLIC, PASS(obj) :: &
    GetDirichletBCSubMat
  !! Get the submatrix of the Dirichlet columns
  PROCEDURE(obj_ApplyDirichletBCToRHS), DEFERRED, PUBLIC, PASS(obj) :: &
    ApplyDirichletBCtoRHS
  !! Add the Dirichlet boundary condition correction to a rhs
  PROCEDURE, PUBLIC, PASS(obj) :: SPY => obj_SPY
  !! Plot the sparsity pattern

  ! @SetMethods
  PROCEDURE(obj_Set1), PUBLIC, DEFERRED, PASS(obj) :: Set1
  !! Set an element matrix
  PROCEDURE(obj_Set2), PUBLIC, DEFERRED, PASS(obj) :: Set2
  !! Set all entries, or a block of nodes, to a scalar
  PROCEDURE(obj_Set3), PUBLIC, DEFERRED, PASS(obj) :: Set3
  !! Set a single entry by node and degree of freedom
  PROCEDURE(obj_Set4), PUBLIC, DEFERRED, PASS(obj) :: Set4
  !! Set the block of two physical variables
  PROCEDURE(obj_Set5), PUBLIC, DEFERRED, PASS(obj) :: Set5
  !! Set the block of two degrees of freedom
  PROCEDURE(obj_Set6), PUBLIC, DEFERRED, PASS(obj) :: Set6
  !! Set a single entry by physical variable and dof
  PROCEDURE(obj_Set7), PUBLIC, DEFERRED, PASS(obj) :: Set7
  !! Set a single entry by space and time components
  PROCEDURE(obj_Set8), PUBLIC, DEFERRED, PASS(obj) :: Set8
  !! Set a block of nodes and components to a scalar
  PROCEDURE(obj_Set9), PUBLIC, DEFERRED, PASS(obj) :: Set9
  !! Set a block of nodes and time components to a scalar
  PROCEDURE(obj_Set10), PUBLIC, DEFERRED, PASS(obj) :: Set10
  !! Set a block of nodes and space components to a scalar
  PROCEDURE(obj_Set11), PUBLIC, DEFERRED, PASS(obj) :: Set11
  !! Set or add another matrix field
  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, Set5, &
    Set6, Set7, Set8, Set9, Set10, Set11
  !! Generic method to set entries
  PROCEDURE, PUBLIC, PASS(obj) :: SetBrokenLagrangeMatrixForVTK => &
    obj_SetBrokenLagrangeMatrixForVTK
  !! Set the matrix that maps the field dofs to the broken Lagrange dofs

  ! @SetRow
  PROCEDURE(obj_SetRow1), PUBLIC, DEFERRED, PASS(obj) :: SetRow1
  !! Set a row given by node and dof
  PROCEDURE(obj_SetRow2), PUBLIC, DEFERRED, PASS(obj) :: SetRow2
  !! Set a row given by node, physical variable and dof
  PROCEDURE(obj_SetRow3), PUBLIC, DEFERRED, PASS(obj) :: SetRow3
  !! Set a row given by node and components
  PROCEDURE(obj_SetRow4), PUBLIC, DEFERRED, PASS(obj) :: SetRow4
  !! Set rows of a node for several time components
  PROCEDURE(obj_SetRow5), PUBLIC, DEFERRED, PASS(obj) :: SetRow5
  !! Set rows of a node for several space components
  PROCEDURE(obj_SetRow6), PUBLIC, DEFERRED, PASS(obj) :: SetRow6
  !! Set rows of several nodes and time components
  PROCEDURE(obj_SetRow7), PUBLIC, DEFERRED, PASS(obj) :: SetRow7
  !! Set rows of several nodes and space components
  GENERIC, PUBLIC :: SetRow => SetRow1, SetRow2, SetRow3, &
    SetRow4, SetRow5, SetRow6, SetRow7
  !! Generic method to set rows

  ! @SetColumn
  PROCEDURE(obj_SetColumn1), PUBLIC, DEFERRED, PASS(obj) :: SetColumn1
  !! Set a column given by node and dof
  PROCEDURE(obj_SetColumn2), PUBLIC, DEFERRED, PASS(obj) :: SetColumn2
  !! Set a column given by node, physical variable and dof
  PROCEDURE(obj_SetColumn3), PUBLIC, DEFERRED, PASS(obj) :: SetColumn3
  !! Set a column given by node and components
  PROCEDURE(obj_SetColumn4), PUBLIC, DEFERRED, PASS(obj) :: SetColumn4
  !! Set columns of a node for several time components
  PROCEDURE(obj_SetColumn5), PUBLIC, DEFERRED, PASS(obj) :: SetColumn5
  !! Set columns of a node for several space components
  PROCEDURE(obj_SetColumn6), PUBLIC, DEFERRED, PASS(obj) :: SetColumn6
  !! Set columns of several nodes and time components
  PROCEDURE(obj_SetColumn7), PUBLIC, DEFERRED, PASS(obj) :: SetColumn7
  !! Set columns of several nodes and space components
  GENERIC, PUBLIC :: SetColumn => SetColumn1, SetColumn2, &
    SetColumn3, SetColumn4, SetColumn5, SetColumn6, SetColumn7
  !! Generic method to set columns

  ! @GetMethods
  PROCEDURE(obj_Get1), PUBLIC, DEFERRED, PASS(obj) :: Get1
  !! Get an element matrix
  PROCEDURE(obj_Get2), PUBLIC, DEFERRED, PASS(obj) :: Get2
  !! Get a single entry by node and dof
  PROCEDURE(obj_Get3), PUBLIC, DEFERRED, PASS(obj) :: Get3
  !! Get the block of two physical variables
  PROCEDURE(obj_Get4), PUBLIC, DEFERRED, PASS(obj) :: Get4
  !! Get the block of two degrees of freedom
  PROCEDURE(obj_Get5), PUBLIC, DEFERRED, PASS(obj) :: Get5
  !! Get a single entry by physical variable and dof
  PROCEDURE(obj_Get6), PUBLIC, DEFERRED, PASS(obj) :: Get6
  !! Get a single entry by space and time components
  PROCEDURE(obj_Get7), PUBLIC, DEFERRED, PASS(obj) :: Get7
  !! Get a block of nodes and components
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, Get5, Get6, Get7
  !! Generic method to get entries

  ! @GetColumn
  PROCEDURE(obj_GetColumn1), PUBLIC, DEFERRED, PASS(obj) :: GetColumn1
  !! Get a column given by node and dof
  PROCEDURE(obj_GetColumn2), PUBLIC, DEFERRED, PASS(obj) :: GetColumn2
  !! Get a column given by node, physical variable and dof
  PROCEDURE(obj_GetColumn3), PUBLIC, DEFERRED, PASS(obj) :: GetColumn3
  !! Get a column given by node and components
  PROCEDURE(obj_GetColumn4), PUBLIC, DEFERRED, PASS(obj) :: GetColumn4
  !! Get columns of a node for several time components
  PROCEDURE(obj_GetColumn5), PUBLIC, DEFERRED, PASS(obj) :: GetColumn5
  !! Get columns of a node for several space components
  PROCEDURE(obj_GetColumn6), PUBLIC, DEFERRED, PASS(obj) :: GetColumn6
  !! Get columns of several nodes and time components
  PROCEDURE(obj_GetColumn7), PUBLIC, DEFERRED, PASS(obj) :: GetColumn7
  !! Get columns of several nodes and space components
  GENERIC, PUBLIC :: GetColumn => GetColumn1, GetColumn2, &
    GetColumn3, GetColumn4, GetColumn5, GetColumn6, GetColumn7
  !! Generic method to get columns

  ! @GetRow
  PROCEDURE(obj_GetRow1), PUBLIC, DEFERRED, PASS(obj) :: GetRow1
  !! Get a row given by node and dof
  PROCEDURE(obj_GetRow2), PUBLIC, DEFERRED, PASS(obj) :: GetRow2
  !! Get a row given by node, physical variable and dof
  PROCEDURE(obj_GetRow3), PUBLIC, DEFERRED, PASS(obj) :: GetRow3
  !! Get a row given by node and components
  PROCEDURE(obj_GetRow4), PUBLIC, DEFERRED, PASS(obj) :: GetRow4
  !! Get rows of a node for several time components
  PROCEDURE(obj_GetRow5), PUBLIC, DEFERRED, PASS(obj) :: GetRow5
  !! Get rows of a node for several space components
  PROCEDURE(obj_GetRow6), PUBLIC, DEFERRED, PASS(obj) :: GetRow6
  !! Get rows of several nodes and time components
  PROCEDURE(obj_GetRow7), PUBLIC, DEFERRED, PASS(obj) :: GetRow7
  !! Get rows of several nodes and space components
  GENERIC, PUBLIC :: GetRow => GetRow1, GetRow2, &
    GetRow3, GetRow4, GetRow5, GetRow6, GetRow7
  !! Generic method to get rows

  ! @UnaryMethods
  PROCEDURE(obj_Scal), DEFERRED, PUBLIC, PASS(obj) :: Scal
  !! Scale the matrix, obj = a * obj
END TYPE AbstractMatrixField_

!----------------------------------------------------------------------------
!                                                                Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Display the content of an abstract matrix field
!
! This routine displays the content of [[AbstractField_]] and then
! `isPmatInitiated`. Child classes call it before they display their
! own components.
!
! Examples
!
!```fortran
! CALL obj%Display(msg="MatrixField: ")
!```
!
! This example still needs a matching test.
!
! See also: [[MatrixField_:obj_Display]]

INTERFACE AbstractMatrixFieldDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed before the content
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
    !! unit number, default is stdout
  END SUBROUTINE obj_Display
END INTERFACE AbstractMatrixFieldDisplay

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Deallocate the data of an abstract matrix field
!
! This routine deallocates the data of [[AbstractField_]] and sets
! `isPmatInitiated` to false. It nullifies the row and column FEDOF
! pointers, but it does not deallocate the FEDOF objects. Child classes
! call it from their own `DEALLOCATE` method.
!
! Changes:
!   - 2026-11-11: Nullify the row and column FEDOF pointers.
!
! Examples
!
!```fortran
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%IsInitiated(), "DEALLOCATE")
!```
!
! Full example: src/modules/MatrixField/examples/Initiate_test_1.F90
!
! See also: [[MatrixField_:obj_Deallocate]]

INTERFACE AbstractMatrixFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractMatrixFieldDeallocate

!----------------------------------------------------------------------------
!                                                                    IMPORT
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Import a matrix field from HDF5 file
!
! The child class reads the matrix from the group and sets the row and
! column FEDOF pointers with [[AbstractMatrixFieldSetFEDOF]]. A column
! argument that is absent takes the row value (square matrix).
!
! Examples
!
!```fortran
! CALL obj%Import(hdf5=h5, group="/K", rowFedof=fedof, &
!                 rowGeoFedof=geofedof)
!```
!
! Full example: src/modules/MatrixField/examples/Export_test_1.F90
!
! See also: [[MatrixField_:obj_Import]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Import(obj, hdf5, group, rowFedof, rowFedofs, &
                        rowGeoFedof, rowGeoFedofs, colFedof, colFedofs, &
                        colGeoFedof, colGeoFedofs)
    IMPORT :: AbstractMatrixField_, HDF5File_, FEDOF_, FEDOFPointer_
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field, not initiated
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file open in read mode
    CHARACTER(*), INTENT(IN) :: group
    !! group of the matrix field in the HDF5 file
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: rowFedof
    !! FEDOF of the rows
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: rowFedofs(:)
    !! FEDOF of the rows, one for each physical variable
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: rowGeoFedof
    !! geometric FEDOF of the rows
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: rowGeoFedofs(:)
    !! geometric FEDOF of the rows, one for each physical variable
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: colFedof
    !! FEDOF of the columns, default is rowFedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: colFedofs(:)
    !! FEDOF of the columns, default is rowFedofs
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: colGeoFedof
    !! geometric FEDOF of the columns, default is rowGeoFedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: colGeoFedofs(:)
    !! geometric FEDOF of the columns, default is rowGeoFedofs
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    SetFEDOF
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Set the row and column FEDOF pointers of a matrix field
!
! This routine first nullifies the row and column FEDOF pointers of obj.
! Then it points the row pointers to the given row FEDOFs. A column
! argument that is absent takes the row value, so for a square matrix
! only the row arguments are needed. The FEDOF objects are not copied.
!
! An unallocated array or a disassociated pointer passed as an actual
! argument counts as absent.
!
! Examples
!
!```fortran
! CALL AbstractMatrixFieldSetFEDOF(obj=obj, rowFedof=fedof, &
!                                  rowGeoFedof=geofedof)
! CALL OK(ASSOCIATED(obj%colFedof, fedof), "square: colFedof")
!```
!
! Full example: src/modules/MatrixField/examples/GetRowFEDOFPointer_test_1.F90
!
! See also: [[AbstractMatrixField_:GetRowFEDOFPointer]],
! [[AbstractMatrixField_:GetColFEDOFPointer]]

INTERFACE AbstractMatrixFieldSetFEDOF
  MODULE SUBROUTINE obj_SetFEDOF(obj, rowFedof, rowFedofs, rowGeoFedof, &
                                 rowGeoFedofs, colFedof, colFedofs, &
                                 colGeoFedof, colGeoFedofs)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: rowFedof
    !! FEDOF of the rows
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: rowFedofs(:)
    !! FEDOF of the rows, one for each physical variable
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: rowGeoFedof
    !! geometric FEDOF of the rows
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: rowGeoFedofs(:)
    !! geometric FEDOF of the rows, one for each physical variable
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: colFedof
    !! FEDOF of the columns, default is rowFedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: colFedofs(:)
    !! FEDOF of the columns, default is rowFedofs
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: colGeoFedof
    !! geometric FEDOF of the columns, default is rowGeoFedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: colGeoFedofs(:)
    !! geometric FEDOF of the columns, default is rowGeoFedofs
  END SUBROUTINE obj_SetFEDOF
END INTERFACE AbstractMatrixFieldSetFEDOF

!----------------------------------------------------------------------------
!                                                          GetRowFEDOFPointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Get the row FEDOF pointer of a matrix field
!
! If indx is present and rowFedofs is allocated, this function returns
! rowFedofs(indx)%ptr. Otherwise it returns rowFedof. The caller should
! check that the result is associated.
!
! Returns:
!   ans : CLASS(FEDOF_), POINTER - row FEDOF pointer
!
! Examples
!
!```fortran
! fedofPtr => obj%GetRowFEDOFPointer()
! CALL OK(ASSOCIATED(fedofPtr, fedof), "GetRowFEDOFPointer")
!```
!
! Full example: src/modules/MatrixField/examples/GetRowFEDOFPointer_test_1.F90
!
! See also: [[AbstractMatrixField_:GetColFEDOFPointer]]

INTERFACE
  MODULE FUNCTION obj_GetRowFEDOFPointer1(obj, indx) RESULT(ans)
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: indx
    !! index of the physical variable
    CLASS(FEDOF_), POINTER :: ans
    !! row FEDOF pointer
  END FUNCTION obj_GetRowFEDOFPointer1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetRowFEDOFPointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Get the row FEDOF pointers of all physical variables
!
! This function returns a copy of rowFedofs. The result has size zero
! when rowFedofs is not allocated.
!
! Returns:
!   ans : TYPE(FEDOFPointer_), ALLOCATABLE - row FEDOF pointers
!
! Examples
!
!```fortran
! fedofPtrs = obj%GetRowFEDOFPointer(indx=[1])
! CALL OK(SIZE(fedofPtrs) .EQ. 1, "GetRowFEDOFPointer: size")
!```
!
! Full example: src/modules/MatrixField/examples/GetRowFEDOFPointer_test_1.F90

INTERFACE
  MODULE FUNCTION obj_GetRowFEDOFPointer2(obj, indx) RESULT(ans)
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: indx(1)
    !! dummy argument, it only selects this procedure
    TYPE(FEDOFPointer_), ALLOCATABLE :: ans(:)
    !! row FEDOF pointers
  END FUNCTION obj_GetRowFEDOFPointer2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetColFEDOFPointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Get the column FEDOF pointer of a matrix field
!
! If indx is present and colFedofs is allocated, this function returns
! colFedofs(indx)%ptr. Otherwise it returns colFedof. For a square
! matrix the result is the same as
! [[AbstractMatrixField_:GetRowFEDOFPointer]].
!
! Returns:
!   ans : CLASS(FEDOF_), POINTER - column FEDOF pointer
!
! Examples
!
!```fortran
! fedofPtr => obj%GetColFEDOFPointer()
! CALL OK(ASSOCIATED(fedofPtr, fedof), "GetColFEDOFPointer")
!```
!
! Full example: src/modules/MatrixField/examples/GetRowFEDOFPointer_test_1.F90
!
! See also: [[AbstractMatrixField_:GetRowFEDOFPointer]]

INTERFACE
  MODULE FUNCTION obj_GetColFEDOFPointer1(obj, indx) RESULT(ans)
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: indx
    !! index of the physical variable
    CLASS(FEDOF_), POINTER :: ans
    !! column FEDOF pointer
  END FUNCTION obj_GetColFEDOFPointer1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetColFEDOFPointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Get the column FEDOF pointers of all physical variables
!
! This function returns a copy of colFedofs. The result has size zero
! when colFedofs is not allocated.
!
! Returns:
!   ans : TYPE(FEDOFPointer_), ALLOCATABLE - column FEDOF pointers
!
! Examples
!
!```fortran
! fedofPtrs = obj%GetColFEDOFPointer(indx=[1])
! CALL OK(SIZE(fedofPtrs) .EQ. 1, "GetColFEDOFPointer: size")
!```
!
! Full example: src/modules/MatrixField/examples/GetRowFEDOFPointer_test_1.F90

INTERFACE
  MODULE FUNCTION obj_GetColFEDOFPointer2(obj, indx) RESULT(ans)
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: indx(1)
    !! dummy argument, it only selects this procedure
    TYPE(FEDOFPointer_), ALLOCATABLE :: ans(:)
    !! column FEDOF pointers
  END FUNCTION obj_GetColFEDOFPointer2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      SIZE
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Size of the matrix
!
! This function returns the number of rows (dim = 1), the number of
! columns (dim = 2), or the number of rows times the number of columns
! when dim is absent.
!
! Returns:
!   ans : INTEGER(I4B) - size of the matrix
!
! Examples
!
!```fortran
! CALL OK(obj%SIZE(1) .EQ. 6, "SIZE(1)")
! CALL OK(obj%SIZE() .EQ. 36, "SIZE()")
!```
!
! Full example: src/modules/MatrixField/examples/Initiate_test_1.F90
!
! See also: [[MatrixField_:obj_Size]]

ABSTRACT INTERFACE
  FUNCTION obj_Size(obj, dim) RESULT(ans)
    IMPORT :: AbstractMatrixField_, I4B
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! 1 for rows, 2 for columns; absent for the total size
    INTEGER(I4B) :: ans
    !! size of the matrix
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      SHAPE
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Shape of the matrix
!
! This function returns the number of rows and the number of columns of
! the matrix.
!
! Returns:
!   ans : INTEGER(I4B)(2) - [number of rows, number of columns]
!
! Examples
!
!```fortran
! CALL OK(ALL(obj%SHAPE() .EQ. [6, 6]), "SHAPE")
!```
!
! Full example: src/modules/MatrixField/examples/Initiate_test_1.F90
!
! See also: [[MatrixField_:obj_Shape]]

ABSTRACT INTERFACE
  FUNCTION obj_Shape(obj) RESULT(ans)
    IMPORT :: AbstractMatrixField_, I4B
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B) :: ans(2)
    !! number of rows and columns
  END FUNCTION obj_Shape
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Initiate a matrix field from a row and a column node field
!
! Make the matrix A of y = A * x, where y = rowObj and x = colObj are
! node fields. Child classes implement this method; this version only
! raises an error (in debug mode).
!
! See also: [[MatrixField_]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, rowObj, colObj)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field A of y = A * x
    CLASS(AbstractNodeField_), INTENT(INOUT) :: rowObj
    !! node field y; gives the rows of obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: colObj
    !! node field x; gives the columns of obj
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Matvec
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-18
! summary: Matrix vector multiplication with Fortran arrays
!
! This routine computes y = scale * A x, or y = scale * A^T x when
! isTranspose is true. When addContribution is true the result is added
! to y: y = y + scale * A x. The vectors x and y are REAL(DFP) arrays;
! y must be allocated by the caller.
!
! Formula:
!   $$ y = \alpha y + s \, \mathrm{op}(A) x $$
!   where alpha is 1 when addContribution is true and 0 otherwise.
!
! Examples
!
!```fortran
! CALL obj%Matvec(x=x, y=y)
! CALL obj%Matvec(x=x, y=y, isTranspose=.TRUE.)
! CALL obj%Matvec(x=x, y=y, addContribution=.TRUE., scale=2.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/Matvec_test_1.F90
!
! See also: [[MatrixField_:obj_Matvec1]], [[obj_Matvec2]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Matvec1(obj, x, y, isTranspose, addContribution, scale)
    IMPORT :: AbstractMatrixField_, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field A
    REAL(DFP), INTENT(INOUT) :: y(:)
    !! output vector, SIZE(y) = number of rows of op(A)
    REAL(DFP), INTENT(IN) :: x(:)
    !! input vector, SIZE(x) = number of columns of op(A)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! if true, use the transpose of A; default .FALSE.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, y = y + scale * op(A) x; default .FALSE.
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of op(A) x; default 1
  END SUBROUTINE obj_Matvec1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Matvec
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-18
! summary: Matrix vector multiplication with node fields
!
! This routine computes y = scale * op(A) x, like [[obj_Matvec1]], but x
! and y are node fields, for example [[ScalarField_]] or
! [[VectorField_]]. Both fields must use the degrees of freedom of the
! matrix field.
!
! Examples
!
!```fortran
! CALL u%Set(VALUE=1.0_DFP)
! CALL obj%Matvec(x=u, y=w)
!```
!
! Full example: src/modules/MatrixField/examples/Matvec_test_1.F90
!
! See also: [[MatrixField_:obj_Matvec2]], [[obj_Matvec1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Matvec2(obj, x, y, isTranspose, addContribution, scale)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, LGT, DFP
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field A
    CLASS(AbstractNodeField_), INTENT(INOUT) :: y
    !! output node field
    CLASS(AbstractNodeField_), INTENT(IN) :: x
    !! input node field
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! if true, use the transpose of A; default .FALSE.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, y = y + scale * op(A) x; default .FALSE.
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of op(A) x; default 1
  END SUBROUTINE obj_Matvec2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ILUSOLVE
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-18
! summary: Solve (LU) sol = rhs with Fortran arrays
!
! This routine solves (LU) sol = rhs, or (LU)^T sol = rhs when
! isTranspose is true. The LU factors are the incomplete LU
! preconditioner stored inside the matrix field, so the preconditioner
! must be set first (see [[obj_isPreconditionSet]]). The array sol must
! be allocated by the caller with SIZE(sol) = SIZE(rhs).
!
! Examples
!
!```fortran
! CALL obj%ILUSOLVE(sol=sol, rhs=rhs)
!```
!
! This example still needs a matching test.
!
! See also: [[MatrixField_:obj_ILUSOLVE1]], [[obj_ILUSOLVE2]]

ABSTRACT INTERFACE
  SUBROUTINE obj_ILUSOLVE1(obj, sol, rhs, isTranspose)
    IMPORT :: AbstractMatrixField_, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field with the LU preconditioner
    REAL(DFP), INTENT(INOUT) :: sol(:)
    !! solution, SIZE(sol) = SIZE(rhs)
    REAL(DFP), INTENT(IN) :: rhs(:)
    !! right hand side
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! if true, solve (LU)^T sol = rhs; default .FALSE.
  END SUBROUTINE obj_ILUSOLVE1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ILUSOLVE
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-18
! summary: Solve (LU) sol = rhs with node fields
!
! This routine is [[obj_ILUSOLVE1]] with node fields for sol and rhs.
!
! Examples
!
!```fortran
! CALL obj%ILUSOLVE(sol=u, rhs=f)
!```
!
! This example still needs a matching test.
!
! See also: [[MatrixField_:obj_ILUSOLVE2]], [[obj_ILUSOLVE1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_ILUSOLVE2(obj, sol, rhs, isTranspose)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field with the LU preconditioner
    CLASS(AbstractNodeField_), INTENT(INOUT) :: sol
    !! solution node field
    CLASS(AbstractNodeField_), INTENT(IN) :: rhs
    !! right hand side node field
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! if true, solve (LU)^T sol = rhs; default .FALSE.
  END SUBROUTINE obj_ILUSOLVE2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetPrecondition
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-18
! summary: Get the preconditioning matrix
!
! This routine copies the preconditioning matrix of obj into Pmat.
!
! Examples
!
!```fortran
! CALL obj%GetPrecondition(Pmat=pmat)
!```
!
! This example still needs a matching test.
!
! See also: [[MatrixField_:obj_GetPrecondition]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetPrecondition(obj, Pmat)
    IMPORT :: AbstractMatrixField_
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: Pmat
    !! preconditioning matrix
  END SUBROUTINE obj_GetPrecondition
END INTERFACE

!----------------------------------------------------------------------------
!                                                        reversePermutation
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-18
! summary: Undo the permutation of the preconditioner
!
! Incomplete LU preconditioners can permute rows or columns to improve
! the sparsity of the factors. After a column permutation the solution
! must be permuted back, and after a row permutation the rhs must be
! permuted back. This routine does it for rhs and sol.
!
! Examples
!
!```fortran
! CALL obj%reversePermutation(rhs=f, sol=u)
!```
!
! This example still needs a matching test.
!
! See also: [[MatrixField_:obj_ReversePermutation]]

ABSTRACT INTERFACE
  SUBROUTINE obj_reversePermutation(obj, rhs, sol)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field with the preconditioner
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: rhs
    !! right hand side node field
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: sol
    !! solution node field
  END SUBROUTINE obj_reversePermutation
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isPreconditionSet
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-18
! summary: True if the preconditioner is set
!
! This function returns `isPmatInitiated`.
!
! Returns:
!   ans : LOGICAL(LGT) - true if the preconditioner is set
!
! Examples
!
!```fortran
! CALL OK(.NOT. obj%isPreconditionSet(), "no preconditioner")
!```
!
! This example still needs a matching test.

INTERFACE
  MODULE PURE FUNCTION obj_IsPreconditionSet(obj) RESULT(Ans)
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    LOGICAL(LGT) :: ans
    !! true if the preconditioner is set
  END FUNCTION obj_IsPreconditionSet
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetDiagonal
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get the diagonal of the matrix
!
! This routine returns the diagonal entries of the matrix in diag. The
! array diag is reallocated to the number of rows.
!
! Examples
!
!```fortran
! REAL(DFP), ALLOCATABLE :: diag(:)
! CALL obj%GetDiagonal(diag=diag)
!```
!
! Full example: src/modules/MatrixField/examples/GetDiagonal_test_1.F90
!
! See also: [[MatrixField_:obj_GetDiagonal]], [[obj_DiagonalScaling]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetDiagonal(obj, diag)
    IMPORT :: AbstractMatrixField_, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: diag(:)
    !! diagonal entries, SIZE(diag) = number of rows
  END SUBROUTINE obj_GetDiagonal
END INTERFACE

!----------------------------------------------------------------------------
!                                                           DiagonalScaling
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Scale the matrix by a diagonal
!
! This routine scales the matrix by a diagonal d from the left, from the
! right, or from both sides. If diag is absent, d is the diagonal of the
! matrix. The operator is applied to d before scaling: "SQRT" uses
! SQRT(|d|) and "NONE" uses d.
!
! Formula:
!   $$ A_{ij} \leftarrow A_{ij} / d_i $$ (LEFT),
!   $$ A_{ij} \leftarrow A_{ij} / d_j $$ (RIGHT),
!   $$ A_{ij} \leftarrow A_{ij} / (d_i d_j) $$ (BOTH)
!
! Examples
!
!```fortran
! CALL obj%DiagonalScaling(side="BOTH")
! CALL obj%DiagonalScaling(side="LEFT", diag=diag, OPERATOR="NONE")
!```
!
! Full example: src/modules/MatrixField/examples/GetDiagonal_test_1.F90
!
! See also: [[MatrixField_:obj_DiagonalScaling]], [[obj_GetDiagonal]]

ABSTRACT INTERFACE
  SUBROUTINE obj_DiagonalScaling(obj, side, diag, OPERATOR)
    IMPORT :: AbstractMatrixField_, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    CHARACTER(*), INTENT(IN) :: side
    !! "LEFT", "RIGHT" or "BOTH"
    REAL(DFP), OPTIONAL, INTENT(IN) :: diag(:)
    !! diagonal d, SIZE(diag) = number of rows; default is the
    !! diagonal of the matrix
    CHARACTER(*), OPTIONAL, INTENT(IN) :: OPERATOR
    !! "SQRT" or "NONE"; default "SQRT"
  END SUBROUTINE obj_DiagonalScaling
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       SPY
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Plot the sparsity pattern of the matrix
!
! This routine writes the sparsity pattern of the matrix to the file
! filename.ext. The abstract class raises an error; child classes
! override it.
!
! Examples
!
!```fortran
! CALL obj%SPY(filename="K", ext="svg")
!```
!
! This example still needs a matching test.
!
! See also: [[MatrixField_:obj_SPY]]

INTERFACE
  MODULE SUBROUTINE obj_SPY(obj, filename, ext)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    CHARACTER(*), INTENT(IN) :: filename
    !! file name without extension
    CHARACTER(*), INTENT(IN) :: ext
    !! file extension, it selects the file format
  END SUBROUTINE obj_SPY
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SymSchurLargestEigenVal
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-30
! summary: Largest eigenvalues of a symmetric Schur complement
!
! This function returns the nev largest eigenvalues of the symmetric
! Schur complement formed from obj and B. It uses the Lanczos method
! (ARPACK). The abstract class raises an error; child classes override
! it.
!
! Returns:
!   ans : REAL(DFP)(nev) - largest eigenvalues
!
! Examples
!
!```fortran
! REAL(DFP) :: ev(1)
! ev = obj%SymSchurLargestEigenVal(B=B, nev=1)
!```
!
! This example still needs a matching test.
!
! See also: [[obj_SymLargestEigenVal]]

INTERFACE
  MODULE FUNCTION obj_SymSchurLargestEigenVal(obj, B, nev, which, NCV, &
                                              maxIter, tol) RESULT(ans)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! symmetric matrix field
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: B
    !! B matrix, possibly rectangle
    INTEGER(I4B), INTENT(IN) :: nev
    !! number of eigenvalues requested
    CHARACTER(*), OPTIONAL, INTENT(IN) :: which
    !! "LM" for the largest absolute value, "LA" for the
    !! largest algebraic value; default "LA"
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: NCV
    !! number of Lanczos vectors, 1 < NCV < n;
    !! default MIN(n, MAX(2*nev+1, 20))
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    !! maximum number of iterations; default n*10
    REAL(DFP), OPTIONAL, INTENT(IN) :: tol
    !! tolerance; default 0
    REAL(DFP) :: ans(nev)
    !! largest eigenvalues
  END FUNCTION obj_SymSchurLargestEigenVal
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SymLargestEigenVal
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-30
! summary: Largest eigenvalues of a symmetric matrix
!
! This function returns the nev largest eigenvalues of a symmetric
! matrix field. It uses the Lanczos method (ARPACK). The abstract class
! raises an error; child classes override it.
!
! Returns:
!   ans : REAL(DFP)(nev) - largest eigenvalues
!
! Examples
!
!```fortran
! REAL(DFP) :: ev(2)
! ev = obj%SymLargestEigenVal(nev=2, which="LM")
!```
!
! This example still needs a matching test.
!
! See also: [[obj_SymSchurLargestEigenVal]]

INTERFACE
  MODULE FUNCTION obj_SymLargestEigenVal(obj, nev, which, NCV, &
                                         maxIter, tol) RESULT(ans)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! symmetric matrix field
    INTEGER(I4B), INTENT(IN) :: nev
    !! number of eigenvalues requested
    CHARACTER(*), OPTIONAL, INTENT(IN) :: which
    !! "LM" for the largest absolute value, "LA" for the
    !! largest algebraic value; default "LA"
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: NCV
    !! number of Lanczos vectors, 1 < NCV < n;
    !! default MIN(n, MAX(2*nev+1, 20))
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: maxIter
    !! maximum number of iterations; default n*10
    REAL(DFP), OPTIONAL, INTENT(IN) :: tol
    !! tolerance; default 0
    REAL(DFP) :: ans(nev)
    !! largest eigenvalues
  END FUNCTION obj_SymLargestEigenVal
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetDirichletBCSubMat
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Get the submatrix of the Dirichlet columns
!
! This routine returns in submat the columns of the matrix that belong
! to the Dirichlet degrees of freedom.
!
! Examples
!
!```fortran
! CALL obj%GetDirichletBCSubMat(submat=submat)
!```
!
! This example still needs a matching test.
!
! See also: [[MatrixField_:obj_GetDirichletBCSubMat]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetDirichletBCSubMat(obj, submat)
    IMPORT :: AbstractMatrixField_, I4B
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: submat
    !! submatrix of the Dirichlet columns
  END SUBROUTINE obj_GetDirichletBCSubMat
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ApplyDirichletBCToRHS
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Apply the Dirichlet boundary condition to a rhs
!
! This routine computes y = scale * S x, or y = y + scale * S x when
! addContribution is true. S is the submatrix of the Dirichlet columns
! of the matrix. With x the field of prescribed values and scale = -1,
! this is the rhs correction -A(:, dbc) u_dbc.
!
! Examples
!
!```fortran
! CALL obj%ApplyDirichletBCToRHS(x=x, y=y, scale=-1.0_DFP, &
!                                addContribution=.TRUE.)
!```
!
! Full example:
! src/modules/MatrixField/examples/ApplyDirichletBC_test_1.F90
!
! See also: [[MatrixField_:obj_ApplyDirichletBCToRHS]]

ABSTRACT INTERFACE
  SUBROUTINE obj_ApplyDirichletBCToRHS(obj, x, y, isTranspose, &
                                       scale, addContribution)
    IMPORT :: AbstractMatrixField_, LGT, DFP, AbstractNodeField_
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field with the Dirichlet submatrix
    CLASS(AbstractNodeField_), INTENT(IN) :: x
    !! node field of prescribed values
    CLASS(AbstractNodeField_), INTENT(INOUT) :: y
    !! rhs node field
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! if true, use the transpose of the submatrix; default .FALSE.
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of S x; default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, y = y + scale * S x; default .FALSE.
  END SUBROUTINE obj_ApplyDirichletBCToRHS
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set an element matrix
!
! This routine sets the block obj(globalNode, globalNode) to VALUE, or
! adds scale * VALUE when addContribution is true. VALUE is an element
! matrix of size (tdof * SIZE(globalNode)) by (tdof * SIZE(globalNode)),
! where tdof is the number of degrees of freedom per node. storageFMT
! is the storage format of VALUE (DOF_FMT or NODES_FMT); it is
! converted to the storage format of obj.
!
! Examples
!
!```fortran
! CALL obj%Set(globalNode=[1, 5, 6, 4], islocal=.TRUE., VALUE=ke, &
!              storageFMT=TypeFieldOpt%storageFormatDOF, &
!              addContribution=.TRUE.)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_2.F90
!
! See also: [[MatrixField_:obj_Set1]], [[obj_Get1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Set1(obj, globalNode, islocal, VALUE, storageFMT, scale, &
                      addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers of the element
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode are local node numbers
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! element matrix, tdof * SIZE(globalNode) square
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format of VALUE, DOF_FMT or NODES_FMT
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of VALUE, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE; default .FALSE.
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set all entries, or a block of nodes, to a scalar
!
! If globalNode is absent, this routine sets all nonzero entries of the
! matrix to VALUE: obj = VALUE. If globalNode is present, it sets only
! the block obj(globalNode, globalNode). When addContribution is true
! the routine adds scale * VALUE instead.
!
! Examples
!
!```fortran
! CALL obj%Set(VALUE=1.0_DFP)
! CALL obj%Set(VALUE=0.0_DFP, globalNode=[1, 5], islocal=.TRUE.)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_1.F90
!
! See also: [[MatrixField_:obj_Set2]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Set2(obj, VALUE, globalNode, islocal, scale, &
                      addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode(:)
    !! global or local node numbers; default all nodes
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true, globalNode are local node numbers
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of VALUE, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE; default .FALSE.
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set a single entry by node and degree of freedom
!
! This routine sets the entry in the row of (iNodeNum, idof) and the
! column of (jNodeNum, jdof) to VALUE, or adds scale * VALUE when
! addContribution is true.
!
! Examples
!
!```fortran
! CALL obj%Set(iNodeNum=5, jNodeNum=6, islocal=.TRUE., idof=1, jdof=1, &
!              VALUE=7.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_1.F90
!
! See also: [[MatrixField_:obj_Set3]], [[obj_Get2]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Set3(obj, iNodeNum, jNodeNum, islocal, idof, jdof, VALUE, &
                      scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    !! global or local node number of the row
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of the row
    INTEGER(I4B), INTENT(IN) :: jdof
    !! degree of freedom of the column
    REAL(DFP), INTENT(IN) :: VALUE
    !! value of the entry
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of VALUE, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE; default .FALSE.
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set the block of two physical variables
!
! This routine sets the block of the rows of physical variable ivar at
! nodes iNodeNum and the columns of physical variable jvar at nodes
! jNodeNum. VALUE is in DOF_FMT: the rows run over the nodes of the
! first degree of freedom, then the nodes of the second one, and so on.
!
! Examples
!
!```fortran
! CALL obj%Set(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=.TRUE., &
!              ivar=1, jvar=1, VALUE=a)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[MatrixField_:obj_Set4]], [[obj_Get3]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Set4(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, VALUE, &
                      scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! global or local node numbers of the rows
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the columns
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! block in DOF_FMT, (idofs * SIZE(iNodeNum)) by
    !! (jdofs * SIZE(jNodeNum))
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of VALUE, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE; default .FALSE.
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set the block of two degrees of freedom
!
! This routine sets the block of the rows of degree of freedom idof of
! physical variable ivar at nodes iNodeNum and the columns of degree of
! freedom jdof of physical variable jvar at nodes jNodeNum.
!
! Examples
!
!```fortran
! CALL obj%Set(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=.TRUE., &
!              ivar=1, jvar=1, idof=2, jdof=2, VALUE=b)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[MatrixField_:obj_Set5]], [[obj_Get4]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Set5(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, idof, &
                      jdof, VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! global or local node numbers of the rows
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of ivar for the rows
    INTEGER(I4B), INTENT(IN) :: jdof
    !! degree of freedom of jvar for the columns
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! block, SIZE(iNodeNum) by SIZE(jNodeNum)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of VALUE, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE; default .FALSE.
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set a single entry by physical variable and dof
!
! This routine is [[obj_Set3]] with the degrees of freedom idof and
! jdof counted inside the physical variables ivar and jvar.
!
! Examples
!
!```fortran
! CALL obj%Set(iNodeNum=5, jNodeNum=6, islocal=.TRUE., ivar=1, jvar=1, &
!              idof=2, jdof=1, VALUE=4.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[MatrixField_:obj_Set6]], [[obj_Get5]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Set6(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      idof, jdof, VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    !! global or local node number of the row
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the row
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the column
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of ivar for the row
    INTEGER(I4B), INTENT(IN) :: jdof
    !! degree of freedom of jvar for the column
    REAL(DFP), INTENT(IN) :: VALUE
    !! value of the entry
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of VALUE, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE; default .FALSE.
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set a single entry by space and time components
!
! This routine is [[obj_Set6]] with the degrees of freedom given by
! their space and time components.
!
! Examples
!
!```fortran
! CALL obj%Set(iNodeNum=5, jNodeNum=6, islocal=.TRUE., ivar=1, jvar=1, &
!              ispacecompo=2, itimecompo=1, jspacecompo=1, &
!              jtimecompo=1, VALUE=5.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[MatrixField_:obj_Set7]], [[obj_Get6]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Set7(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      ispacecompo, itimecompo, jspacecompo, jtimecompo, &
                      VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    !! global or local node number of the row
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the row
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the column
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    !! space component of the row
    INTEGER(I4B), INTENT(IN) :: itimecompo
    !! time component of the row
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    !! space component of the column
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    !! time component of the column
    REAL(DFP), INTENT(IN) :: VALUE
    !! value of the entry
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of VALUE, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE; default .FALSE.
  END SUBROUTINE obj_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set a block of nodes and components to a scalar
!
! This routine sets all entries in the rows of (iNodeNum, ispacecompo,
! itimecompo) and the columns of (jNodeNum, jspacecompo, jtimecompo) to
! the scalar VALUE.
!
! Examples
!
!```fortran
! CALL obj%Set(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=.TRUE., &
!              ivar=1, jvar=1, ispacecompo=1, itimecompo=1, &
!              jspacecompo=1, jtimecompo=1, VALUE=9.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[MatrixField_:obj_Set8]], [[obj_Get7]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Set8(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      ispacecompo, itimecompo, jspacecompo, jtimecompo, &
                      VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! global or local node numbers of the rows
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    !! space component of the rows
    INTEGER(I4B), INTENT(IN) :: itimecompo
    !! time component of the rows
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    !! space component of the columns
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    !! time component of the columns
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of VALUE, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE; default .FALSE.
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set a block of nodes and time components to a scalar
!
! This routine is [[obj_Set8]] with several time components for the
! rows (itimecompo) and the columns (jtimecompo).
!
! Examples
!
!```fortran
! CALL obj%Set(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=.TRUE., &
!              ivar=1, jvar=1, ispacecompo=1, itimecompo=[1], &
!              jspacecompo=1, jtimecompo=[1], VALUE=6.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[MatrixField_:obj_Set9]], [[obj_Set8]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Set9(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      ispacecompo, itimecompo, jspacecompo, jtimecompo, &
                      VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! global or local node numbers of the rows
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    !! space component of the rows
    INTEGER(I4B), INTENT(IN) :: itimecompo(:)
    !! time components of the rows
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    !! space component of the columns
    INTEGER(I4B), INTENT(IN) :: jtimecompo(:)
    !! time components of the columns
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of VALUE, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE; default .FALSE.
  END SUBROUTINE obj_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set a block of nodes and space components to a scalar
!
! This routine is [[obj_Set8]] with several space components for the
! rows (ispacecompo) and the columns (jspacecompo).
!
! Examples
!
!```fortran
! CALL obj%Set(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=.TRUE., &
!              ivar=1, jvar=1, ispacecompo=[1, 2], itimecompo=1, &
!              jspacecompo=[1, 2], jtimecompo=1, VALUE=8.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[MatrixField_:obj_Set10]], [[obj_Set8]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Set10(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                       ispacecompo, itimecompo, jspacecompo, jtimecompo, &
                       VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! global or local node numbers of the rows
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: ispacecompo(:)
    !! space components of the rows
    INTEGER(I4B), INTENT(IN) :: itimecompo
    !! time component of the rows
    INTEGER(I4B), INTENT(IN) :: jspacecompo(:)
    !! space components of the columns
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    !! time component of the columns
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of VALUE, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE; default .FALSE.
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-16
! summary: Set or add another matrix field
!
! This routine sets obj = scale * VALUE, or obj = obj + scale * VALUE
! when addContribution is true. Both matrix fields must have the same
! sparsity.
!
! Examples
!
!```fortran
! CALL obj%Set(VALUE=obj2, scale=3.0_DFP, addContribution=.TRUE.)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_4.F90
!
! See also: [[MatrixField_:obj_Set11]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Set11(obj, VALUE, scale, addContribution)
    IMPORT :: AbstractMatrixField_, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: VALUE
    !! matrix field with the same sparsity as obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of VALUE; default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, obj = obj + scale * VALUE; default .FALSE.
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Set a row given by node and dof
!
! This routine sets the row of (globalNode, idof). Give exactly one of
! the following values.
!
!- scalarVal sets every nonzero entry of the row to scalarVal.
!- vecVal is a dense row of length equal to the number of columns. Only
!  the entries in the sparsity pattern are written.
!- nodeFieldVal is a node field, used like vecVal.
!
! Examples
!
!```fortran
! CALL obj%SetRow(globalNode=1, islocal=.TRUE., idof=1, &
!                 scalarVal=2.0_DFP)
! CALL obj%SetRow(globalNode=5, islocal=.TRUE., idof=1, vecVal=vec)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_1.F90
!
! See also: [[MatrixField_:obj_SetRow1]], [[obj_GetRow1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_SetRow1(obj, globalNode, islocal, idof, scalarVal, &
                         vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the row
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of the row
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    !! scalar value for all nonzero entries of the row
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    !! dense row, SIZE(vecVal) = number of columns
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
    !! node field with the values of the row
  END SUBROUTINE obj_SetRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Set a row given by node, physical variable and dof
!
! This routine is [[obj_SetRow1]] with idof counted inside the physical
! variable ivar.
!
! Examples
!
!```fortran
! CALL obj%SetRow(globalNode=5, islocal=.TRUE., ivar=1, idof=2, &
!                 scalarVal=3.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[MatrixField_:obj_SetRow2]], [[obj_SetRow1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_SetRow2(obj, globalNode, islocal, ivar, idof, &
                         scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the row
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the row
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of ivar
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    !! scalar value for all nonzero entries of the row
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    !! dense row, SIZE(vecVal) = number of columns
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
    !! node field with the values of the row
  END SUBROUTINE obj_SetRow2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Set a row given by node and components
!
! This routine is [[obj_SetRow1]] with the degree of freedom given by
! the physical variable ivar and its space and time components.
!
! Examples
!
!```fortran
! CALL obj%SetRow(globalNode=1, islocal=.TRUE., ivar=1, spacecompo=1, &
!                 timecompo=1, scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[MatrixField_:obj_SetRow3]], [[obj_SetRow1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_SetRow3(obj, globalNode, islocal, ivar, spacecompo, &
                         timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the row
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the row
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of the row
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of the row
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    !! scalar value for all nonzero entries of the row
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    !! dense row, SIZE(vecVal) = number of columns
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
    !! node field with the values of the row
  END SUBROUTINE obj_SetRow3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Set rows of a node for several time components
!
! This routine sets the rows of (globalNode, spacecompo, timecompo(:))
! of the physical variable ivar. Every row gets the same value.
!
! Examples
!
!```fortran
! CALL obj%SetRow(globalNode=1, islocal=.TRUE., ivar=1, spacecompo=1, &
!                 timecompo=[1], scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[MatrixField_:obj_SetRow4]], [[obj_SetRow3]]

ABSTRACT INTERFACE
  SUBROUTINE obj_SetRow4(obj, globalNode, islocal, ivar, spacecompo, &
                         timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the rows
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of the rows
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time components of the rows
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    !! scalar value for all nonzero entries of the rows
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    !! dense row, SIZE(vecVal) = number of columns
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
    !! node field with the values of the rows
  END SUBROUTINE obj_SetRow4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Set rows of a node for several space components
!
! This routine sets the rows of (globalNode, spacecompo(:), timecompo)
! of the physical variable ivar. Every row gets the same value.
!
! Examples
!
!```fortran
! CALL obj%SetRow(globalNode=5, islocal=.TRUE., ivar=1, &
!                 spacecompo=[1, 2], timecompo=1, scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[MatrixField_:obj_SetRow5]], [[obj_SetRow3]]

ABSTRACT INTERFACE
  SUBROUTINE obj_SetRow5(obj, globalNode, islocal, ivar, spacecompo, &
                         timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the rows
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! space components of the rows
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of the rows
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    !! scalar value for all nonzero entries of the rows
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    !! dense row, SIZE(vecVal) = number of columns
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
    !! node field with the values of the rows
  END SUBROUTINE obj_SetRow5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Set rows of several nodes and time components
!
! This routine sets the rows of (globalNode(:), spacecompo,
! timecompo(:)) of the physical variable ivar. Every row gets the same
! value.
!
! Examples
!
!```fortran
! CALL obj%SetRow(globalNode=[5, 6], islocal=.TRUE., ivar=1, &
!                 spacecompo=1, timecompo=[1], vecVal=vec)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[MatrixField_:obj_SetRow6]], [[obj_SetRow4]]

ABSTRACT INTERFACE
  SUBROUTINE obj_SetRow6(obj, globalNode, islocal, ivar, spacecompo, &
                         timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers of the rows
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode are local node numbers
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of the rows
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time components of the rows
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    !! scalar value for all nonzero entries of the rows
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    !! dense row, SIZE(vecVal) = number of columns
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
    !! node field with the values of the rows
  END SUBROUTINE obj_SetRow6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    SetRow
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Set rows of several nodes and space components
!
! This routine sets the rows of (globalNode(:), spacecompo(:),
! timecompo) of the physical variable ivar. Every row gets the same
! value.
!
! Examples
!
!```fortran
! CALL obj%SetRow(globalNode=[5, 6], islocal=.TRUE., ivar=1, &
!                 spacecompo=[1, 2], timecompo=1, scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[MatrixField_:obj_SetRow7]], [[obj_SetRow5]]

ABSTRACT INTERFACE
  SUBROUTINE obj_SetRow7(obj, globalNode, islocal, ivar, spacecompo, &
                         timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers of the rows
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode are local node numbers
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! space components of the rows
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of the rows
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    !! scalar value for all nonzero entries of the rows
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    !! dense row, SIZE(vecVal) = number of columns
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
    !! node field with the values of the rows
  END SUBROUTINE obj_SetRow7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SetColumn
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Set a column given by node and dof
!
! This routine sets the column of (globalNode, idof). Give exactly one
! of the following values.
!
!- scalarVal sets every nonzero entry of the column to scalarVal.
!- vecVal is a dense column of length equal to the number of rows. Only
!  the entries in the sparsity pattern are written.
!- nodeFieldVal is a node field, used like vecVal.
!
! Examples
!
!```fortran
! CALL obj%SetColumn(globalNode=1, islocal=.TRUE., idof=1, &
!                    scalarVal=2.0_DFP)
! CALL obj%SetColumn(globalNode=5, islocal=.TRUE., idof=1, vecVal=vec)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_1.F90
!
! See also: [[MatrixField_:obj_SetColumn1]], [[obj_GetColumn1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_SetColumn1(obj, globalNode, islocal, idof, scalarVal, &
                            vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of the column
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    !! scalar value for all nonzero entries of the column
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    !! dense column, SIZE(vecVal) = number of rows
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
    !! node field with the values of the column
  END SUBROUTINE obj_SetColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SetColumn
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Set a column given by node, physical variable and dof
!
! This routine is [[obj_SetColumn1]] with idof counted inside the
! physical variable ivar.
!
! Examples
!
!```fortran
! CALL obj%SetColumn(globalNode=5, islocal=.TRUE., ivar=1, idof=2, &
!                    scalarVal=3.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[MatrixField_:obj_SetColumn2]], [[obj_SetColumn1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_SetColumn2(obj, globalNode, islocal, ivar, idof, &
                            scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the column
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of ivar
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    !! scalar value for all nonzero entries of the column
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    !! dense column, SIZE(vecVal) = number of rows
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
    !! node field with the values of the column
  END SUBROUTINE obj_SetColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SetColumn
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Set a column given by node and components
!
! This routine is [[obj_SetColumn1]] with the degree of freedom given by
! the physical variable ivar and its space and time components.
!
! Examples
!
!```fortran
! CALL obj%SetColumn(globalNode=1, islocal=.TRUE., ivar=1, &
!                    spacecompo=1, timecompo=1, scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[MatrixField_:obj_SetColumn3]], [[obj_SetColumn1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_SetColumn3(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the column
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of the column
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of the column
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    !! scalar value for all nonzero entries of the column
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    !! dense column, SIZE(vecVal) = number of rows
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
    !! node field with the values of the column
  END SUBROUTINE obj_SetColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SetColumn
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Set columns of a node for several time components
!
! This routine sets the columns of (globalNode, spacecompo,
! timecompo(:)) of the physical variable ivar. Every column gets the
! same value.
!
! Examples
!
!```fortran
! CALL obj%SetColumn(globalNode=1, islocal=.TRUE., ivar=1, &
!                    spacecompo=1, timecompo=[1], scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[MatrixField_:obj_SetColumn4]], [[obj_SetColumn3]]

ABSTRACT INTERFACE
  SUBROUTINE obj_SetColumn4(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of the columns
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time components of the columns
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    !! scalar value for all nonzero entries of the columns
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    !! dense column, SIZE(vecVal) = number of rows
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
    !! node field with the values of the columns
  END SUBROUTINE obj_SetColumn4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SetColumn
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Set columns of a node for several space components
!
! This routine sets the columns of (globalNode, spacecompo(:),
! timecompo) of the physical variable ivar. Every column gets the same
! value.
!
! Examples
!
!```fortran
! CALL obj%SetColumn(globalNode=5, islocal=.TRUE., ivar=1, &
!                    spacecompo=[1, 2], timecompo=1, scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[MatrixField_:obj_SetColumn5]], [[obj_SetColumn3]]

ABSTRACT INTERFACE
  SUBROUTINE obj_SetColumn5(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! space components of the columns
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of the columns
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    !! scalar value for all nonzero entries of the columns
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    !! dense column, SIZE(vecVal) = number of rows
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
    !! node field with the values of the columns
  END SUBROUTINE obj_SetColumn5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SetColumn
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Set columns of several nodes and time components
!
! This routine sets the columns of (globalNode(:), spacecompo,
! timecompo(:)) of the physical variable ivar. Every column gets the
! same value.
!
! Examples
!
!```fortran
! CALL obj%SetColumn(globalNode=[5, 6], islocal=.TRUE., ivar=1, &
!                    spacecompo=1, timecompo=[1], vecVal=vec)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[MatrixField_:obj_SetColumn6]], [[obj_SetColumn4]]

ABSTRACT INTERFACE
  SUBROUTINE obj_SetColumn6(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode are local node numbers
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of the columns
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time components of the columns
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    !! scalar value for all nonzero entries of the columns
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    !! dense column, SIZE(vecVal) = number of rows
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
    !! node field with the values of the columns
  END SUBROUTINE obj_SetColumn6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SetColumn
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Set columns of several nodes and space components
!
! This routine sets the columns of (globalNode(:), spacecompo(:),
! timecompo) of the physical variable ivar. Every column gets the same
! value.
!
! Examples
!
!```fortran
! CALL obj%SetColumn(globalNode=[5, 6], islocal=.TRUE., ivar=1, &
!                    spacecompo=[1, 2], timecompo=1, scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[MatrixField_:obj_SetColumn7]], [[obj_SetColumn5]]

ABSTRACT INTERFACE
  SUBROUTINE obj_SetColumn7(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, scalarVal, vecVal, nodeFieldVal)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode are local node numbers
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! space components of the columns
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of the columns
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    !! scalar value for all nonzero entries of the columns
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    !! dense column, SIZE(vecVal) = number of rows
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
    !! node field with the values of the columns
  END SUBROUTINE obj_SetColumn7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Get an element matrix
!
! This routine returns the block obj(globalNode, globalNode) in VALUE,
! in the storage format storageFMT. nrow and ncol are the numbers of
! rows and columns written in VALUE.
!
! Examples
!
!```fortran
! CALL obj%Get(globalNode=[1, 5, 6, 4], islocal=.TRUE., VALUE=found, &
!              nrow=nrow, ncol=ncol, &
!              storageFMT=TypeFieldOpt%storageFormatDOF)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_2.F90
!
! See also: [[MatrixField_:obj_Get1]], [[obj_Set1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Get1(obj, globalNode, islocal, VALUE, nrow, ncol, &
                      storageFMT)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers of the element
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode are local node numbers
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! element matrix, at least nrow by ncol
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in VALUE
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format of VALUE, DOF_FMT or NODES_FMT
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Get a single entry by node and dof
!
! This routine returns the entry in the row of (iNodeNum, idof) and the
! column of (jNodeNum, jdof). An entry outside the sparsity pattern is
! zero.
!
! Examples
!
!```fortran
! CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=.TRUE., idof=1, jdof=1, &
!              VALUE=val)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_1.F90
!
! See also: [[MatrixField_:obj_Get2]], [[obj_Set3]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Get2(obj, iNodeNum, jNodeNum, islocal, idof, jdof, VALUE)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    !! global or local node number of the row
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of the row
    INTEGER(I4B), INTENT(IN) :: jdof
    !! degree of freedom of the column
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! value of the entry
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Get the block of two physical variables
!
! This routine returns the block of [[obj_Set4]] in VALUE, in DOF_FMT.
!
! Examples
!
!```fortran
! CALL obj%Get(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=.TRUE., &
!              ivar=1, jvar=1, VALUE=found, nrow=nrow, ncol=ncol)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[MatrixField_:obj_Get3]], [[obj_Set4]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Get3(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      VALUE, nrow, ncol)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! global or local node numbers of the rows
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the columns
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! block in DOF_FMT, at least nrow by ncol
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in VALUE
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Get the block of two degrees of freedom
!
! This routine returns the block of [[obj_Set5]] in VALUE.
!
! Examples
!
!```fortran
! CALL obj%Get(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=.TRUE., &
!              ivar=1, jvar=1, idof=2, jdof=2, VALUE=found, nrow=nrow, &
!              ncol=ncol)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[MatrixField_:obj_Get4]], [[obj_Set5]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Get4(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, idof, &
                      jdof, VALUE, nrow, ncol)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! global or local node numbers of the rows
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of ivar for the rows
    INTEGER(I4B), INTENT(IN) :: jdof
    !! degree of freedom of jvar for the columns
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! block, at least SIZE(iNodeNum) by SIZE(jNodeNum)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in VALUE
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Get a single entry by physical variable and dof
!
! This routine returns the entry of [[obj_Set6]].
!
! Examples
!
!```fortran
! CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=.TRUE., ivar=1, jvar=1, &
!              idof=2, jdof=1, VALUE=val)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[MatrixField_:obj_Get5]], [[obj_Set6]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Get5(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      idof, jdof, VALUE)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    !! global or local node number of the row
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the row
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the column
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of ivar for the row
    INTEGER(I4B), INTENT(IN) :: jdof
    !! degree of freedom of jvar for the column
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! value of the entry
  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Get a single entry by space and time components
!
! This routine returns the entry of [[obj_Set7]].
!
! Examples
!
!```fortran
! CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=.TRUE., ivar=1, jvar=1, &
!              ispacecompo=2, itimecompo=1, jspacecompo=1, &
!              jtimecompo=1, VALUE=val)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[MatrixField_:obj_Get6]], [[obj_Set7]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Get6(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      ispacecompo, itimecompo, jspacecompo, jtimecompo, &
                      VALUE)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    !! global or local node number of the row
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the row
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the column
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    !! space component of the row
    INTEGER(I4B), INTENT(IN) :: itimecompo
    !! time component of the row
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    !! space component of the column
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    !! time component of the column
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! value of the entry
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Get a block of nodes and components
!
! This routine returns the block of the rows of (iNodeNum, ispacecompo,
! itimecompo) and the columns of (jNodeNum, jspacecompo, jtimecompo).
!
! Examples
!
!```fortran
! CALL obj%Get(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=.TRUE., &
!              ivar=1, jvar=1, ispacecompo=1, itimecompo=1, &
!              jspacecompo=1, jtimecompo=1, VALUE=found, nrow=nrow, &
!              ncol=ncol)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[MatrixField_:obj_Get7]], [[obj_Set8]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Get7(obj, iNodeNum, jNodeNum, islocal, ivar, jvar, &
                      ispacecompo, itimecompo, jspacecompo, jtimecompo, &
                      VALUE, nrow, ncol)
    IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! global or local node numbers of the rows
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    !! space component of the rows
    INTEGER(I4B), INTENT(IN) :: itimecompo
    !! time component of the rows
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    !! space component of the columns
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    !! time component of the columns
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! block, at least SIZE(iNodeNum) by SIZE(jNodeNum)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in VALUE
  END SUBROUTINE obj_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    GetRow
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get a row given by node and dof
!
! This routine returns the row of (globalNode, idof) as a dense vector
! of length equal to the number of columns, in VALUE or in
! nodeFieldVal. Entries outside the sparsity pattern are zero.
!
! Without addContribution the output is overwritten. With
! addContribution = .TRUE. the routine adds scale times the row to the
! output.
!
! Examples
!
!```fortran
! CALL obj%GetRow(globalNode=1, islocal=.TRUE., idof=1, VALUE=row)
! CALL obj%GetRow(globalNode=1, islocal=.TRUE., idof=1, &
!                 nodeFieldVal=u)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_1.F90
!
! See also: [[MatrixField_:obj_GetRow1]], [[obj_SetRow1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetRow1(obj, globalNode, islocal, idof, VALUE, &
                         nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the row
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of the row
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    !! dense row, SIZE(VALUE) = number of columns
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    !! node field for the row
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of the row, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale times the row to the output
  END SUBROUTINE obj_GetRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    GetRow
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get a row given by node, physical variable and dof
!
! This routine is [[obj_GetRow1]] with idof counted inside the physical
! variable ivar.
!
! Examples
!
!```fortran
! CALL obj%GetRow(globalNode=5, islocal=.TRUE., ivar=1, idof=2, &
!                 VALUE=row)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[MatrixField_:obj_GetRow2]], [[obj_GetRow1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetRow2(obj, globalNode, islocal, ivar, idof, VALUE, &
                         nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the row
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the row
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of ivar
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    !! dense row, SIZE(VALUE) = number of columns
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    !! node field for the row
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of the row, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale times the row to the output
  END SUBROUTINE obj_GetRow2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    GetRow
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get a row given by node and components
!
! This routine is [[obj_GetRow1]] with the degree of freedom given by
! the physical variable ivar and its space and time components.
!
! Examples
!
!```fortran
! CALL obj%GetRow(globalNode=1, islocal=.TRUE., ivar=1, spacecompo=1, &
!                 timecompo=1, VALUE=row)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[MatrixField_:obj_GetRow3]], [[obj_GetRow1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetRow3(obj, globalNode, islocal, ivar, spacecompo, &
                         timecompo, VALUE, nodeFieldVal, scale, &
                         addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the row
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the row
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of the row
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of the row
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    !! dense row, SIZE(VALUE) = number of columns
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    !! node field for the row
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of the row, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale times the row to the output
  END SUBROUTINE obj_GetRow3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    GetRow
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get rows of a node for several time components
!
! This routine gets the rows of (globalNode, spacecompo, timecompo(:))
! of the physical variable ivar. With addContribution = .TRUE. the rows
! are summed into the output; without it only the last row is kept.
!
! Examples
!
!```fortran
! row = 0.0_DFP
! CALL obj%GetRow(globalNode=1, islocal=.TRUE., ivar=1, spacecompo=1, &
!                 timecompo=[1], VALUE=row, addContribution=.TRUE.)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[MatrixField_:obj_GetRow4]], [[obj_GetRow3]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetRow4(obj, globalNode, islocal, ivar, spacecompo, &
                         timecompo, VALUE, nodeFieldVal, scale, &
                         addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the rows
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of the rows
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time components of the rows
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    !! dense row, SIZE(VALUE) = number of columns
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    !! node field for the rows
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of the rows, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale times the rows to the output
  END SUBROUTINE obj_GetRow4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    GetRow
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get rows of a node for several space components
!
! This routine gets the rows of (globalNode, spacecompo(:), timecompo)
! of the physical variable ivar. With addContribution = .TRUE. the rows
! are summed into the output; without it only the last row is kept.
!
! Examples
!
!```fortran
! row = 0.0_DFP
! CALL obj%GetRow(globalNode=5, islocal=.TRUE., ivar=1, &
!                 spacecompo=[1, 2], timecompo=1, VALUE=row, &
!                 addContribution=.TRUE.)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[MatrixField_:obj_GetRow5]], [[obj_GetRow3]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetRow5(obj, globalNode, islocal, ivar, spacecompo, &
                         timecompo, VALUE, nodeFieldVal, scale, &
                         addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the rows
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! space components of the rows
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of the rows
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    !! dense row, SIZE(VALUE) = number of columns
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    !! node field for the rows
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of the rows, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale times the rows to the output
  END SUBROUTINE obj_GetRow5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    GetRow
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get rows of several nodes and time components
!
! This routine gets the rows of (globalNode(:), spacecompo,
! timecompo(:)) of the physical variable ivar. With addContribution =
! .TRUE. the rows are summed into the output; without it only the last
! row is kept.
!
! Examples
!
!```fortran
! row = 0.0_DFP
! CALL obj%GetRow(globalNode=[5, 6], islocal=.TRUE., ivar=1, &
!                 spacecompo=1, timecompo=[1], VALUE=row, &
!                 addContribution=.TRUE.)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[MatrixField_:obj_GetRow6]], [[obj_GetRow4]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetRow6(obj, globalNode, islocal, ivar, spacecompo, &
                         timecompo, VALUE, nodeFieldVal, scale, &
                         addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers of the rows
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode are local node numbers
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of the rows
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time components of the rows
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    !! dense row, SIZE(VALUE) = number of columns
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    !! node field for the rows
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of the rows, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale times the rows to the output
  END SUBROUTINE obj_GetRow6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    GetRow
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get rows of several nodes and space components
!
! This routine gets the rows of (globalNode(:), spacecompo(:),
! timecompo) of the physical variable ivar. With addContribution =
! .TRUE. the rows are summed into the output; without it only the last
! row is kept.
!
! Examples
!
!```fortran
! row = 0.0_DFP
! CALL obj%GetRow(globalNode=[5, 6], islocal=.TRUE., ivar=1, &
!                 spacecompo=[1, 2], timecompo=1, VALUE=row, &
!                 addContribution=.TRUE.)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[MatrixField_:obj_GetRow7]], [[obj_GetRow5]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetRow7(obj, globalNode, islocal, ivar, spacecompo, &
                         timecompo, VALUE, nodeFieldVal, scale, &
                         addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers of the rows
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode are local node numbers
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! space components of the rows
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of the rows
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    !! dense row, SIZE(VALUE) = number of columns
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    !! node field for the rows
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of the rows, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale times the rows to the output
  END SUBROUTINE obj_GetRow7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 GetColumn
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get a column given by node and dof
!
! This routine returns the column of (globalNode, idof) as a dense
! vector of length equal to the number of rows, in VALUE or in
! nodeFieldVal. Entries outside the sparsity pattern are zero.
!
! Without addContribution the output is overwritten. With
! addContribution = .TRUE. the routine adds scale times the column to
! the output.
!
! Examples
!
!```fortran
! CALL obj%GetColumn(globalNode=1, islocal=.TRUE., idof=1, VALUE=col)
! CALL obj%GetColumn(globalNode=1, islocal=.TRUE., idof=1, &
!                    nodeFieldVal=u)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_1.F90
!
! See also: [[MatrixField_:obj_GetColumn1]], [[obj_SetColumn1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetColumn1(obj, globalNode, islocal, idof, VALUE, &
                            nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of the column
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    !! dense column, SIZE(VALUE) = number of rows
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    !! node field for the column
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of the column, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale times the column to the output
  END SUBROUTINE obj_GetColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 GetColumn
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get a column given by node, physical variable and dof
!
! This routine is [[obj_GetColumn1]] with idof counted inside the
! physical variable ivar.
!
! Examples
!
!```fortran
! CALL obj%GetColumn(globalNode=5, islocal=.TRUE., ivar=1, idof=2, &
!                    VALUE=col)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[MatrixField_:obj_GetColumn2]], [[obj_GetColumn1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetColumn2(obj, globalNode, islocal, ivar, idof, &
                            VALUE, nodeFieldVal, scale, addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the column
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of ivar
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    !! dense column, SIZE(VALUE) = number of rows
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    !! node field for the column
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of the column, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale times the column to the output
  END SUBROUTINE obj_GetColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 GetColumn
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get a column given by node and components
!
! This routine is [[obj_GetColumn1]] with the degree of freedom given by
! the physical variable ivar and its space and time components.
!
! Examples
!
!```fortran
! CALL obj%GetColumn(globalNode=1, islocal=.TRUE., ivar=1, &
!                    spacecompo=1, timecompo=1, VALUE=col)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[MatrixField_:obj_GetColumn3]], [[obj_GetColumn1]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetColumn3(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, VALUE, nodeFieldVal, scale, &
                            addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the column
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of the column
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of the column
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    !! dense column, SIZE(VALUE) = number of rows
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    !! node field for the column
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of the column, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale times the column to the output
  END SUBROUTINE obj_GetColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 GetColumn
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get columns of a node for several time components
!
! This routine gets the columns of (globalNode, spacecompo,
! timecompo(:)) of the physical variable ivar. With addContribution =
! .TRUE. the columns are summed into the output; without it only the
! last column is kept.
!
! Examples
!
!```fortran
! col = 0.0_DFP
! CALL obj%GetColumn(globalNode=1, islocal=.TRUE., ivar=1, &
!                    spacecompo=1, timecompo=[1], VALUE=col, &
!                    addContribution=.TRUE.)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[MatrixField_:obj_GetColumn4]], [[obj_GetColumn3]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetColumn4(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, VALUE, nodeFieldVal, scale, &
                            addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of the columns
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time components of the columns
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    !! dense column, SIZE(VALUE) = number of rows
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    !! node field for the columns
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of the columns, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale times the columns to the output
  END SUBROUTINE obj_GetColumn4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 GetColumn
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get columns of a node for several space components
!
! This routine gets the columns of (globalNode, spacecompo(:),
! timecompo) of the physical variable ivar. With addContribution =
! .TRUE. the columns are summed into the output; without it only the
! last column is kept.
!
! Examples
!
!```fortran
! col = 0.0_DFP
! CALL obj%GetColumn(globalNode=5, islocal=.TRUE., ivar=1, &
!                    spacecompo=[1, 2], timecompo=1, VALUE=col, &
!                    addContribution=.TRUE.)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[MatrixField_:obj_GetColumn5]], [[obj_GetColumn3]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetColumn5(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, VALUE, nodeFieldVal, scale, &
                            addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! space components of the columns
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of the columns
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    !! dense column, SIZE(VALUE) = number of rows
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    !! node field for the columns
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of the columns, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale times the columns to the output
  END SUBROUTINE obj_GetColumn5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 GetColumn
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get columns of several nodes and time components
!
! This routine gets the columns of (globalNode(:), spacecompo,
! timecompo(:)) of the physical variable ivar. With addContribution =
! .TRUE. the columns are summed into the output; without it only the
! last column is kept.
!
! Examples
!
!```fortran
! col = 0.0_DFP
! CALL obj%GetColumn(globalNode=[5, 6], islocal=.TRUE., ivar=1, &
!                    spacecompo=1, timecompo=[1], VALUE=col, &
!                    addContribution=.TRUE.)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[MatrixField_:obj_GetColumn6]], [[obj_GetColumn4]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetColumn6(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, VALUE, nodeFieldVal, scale, &
                            addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode are local node numbers
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of the columns
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time components of the columns
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    !! dense column, SIZE(VALUE) = number of rows
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    !! node field for the columns
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of the columns, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale times the columns to the output
  END SUBROUTINE obj_GetColumn6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 GetColumn
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get columns of several nodes and space components
!
! This routine gets the columns of (globalNode(:), spacecompo(:),
! timecompo) of the physical variable ivar. With addContribution =
! .TRUE. the columns are summed into the output; without it only the
! last column is kept.
!
! Examples
!
!```fortran
! col = 0.0_DFP
! CALL obj%GetColumn(globalNode=[5, 6], islocal=.TRUE., ivar=1, &
!                    spacecompo=[1, 2], timecompo=1, VALUE=col, &
!                    addContribution=.TRUE.)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[MatrixField_:obj_GetColumn7]], [[obj_GetColumn5]]

ABSTRACT INTERFACE
  SUBROUTINE obj_GetColumn7(obj, globalNode, islocal, ivar, spacecompo, &
                            timecompo, VALUE, nodeFieldVal, scale, &
                            addContribution)
    IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
    CLASS(AbstractMatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode are local node numbers
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! space components of the columns
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of the columns
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    !! dense column, SIZE(VALUE) = number of rows
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    !! node field for the columns
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of the columns, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale times the columns to the output
  END SUBROUTINE obj_GetColumn7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Scal
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary: Scale the matrix field
!
! This routine multiplies every nonzero entry of the matrix by a:
! obj = a * obj.
!
! Examples
!
!```fortran
! CALL obj%Scal(a=3.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/Scal_test_1.F90
!
! See also: [[MatrixField_:obj_Scal]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Scal(obj, a)
    IMPORT :: AbstractMatrixField_, DFP
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    REAL(DFP), INTENT(IN) :: a
    !! scale factor
  END SUBROUTINE obj_Scal
END INTERFACE

!----------------------------------------------------------------------------
!                                      SetBrokenLagrangeMatrixForVTK@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-13
! summary: Set the matrix that maps field dofs to broken Lagrange dofs
!   for VTK output.
!
! This routine fills the block (ivar, jvar) of the matrix. The rows belong
! to a broken Lagrange FEDOF (see
! [[FEDOF_:InitiateBrokenLagrangeFedofForVTK]]), whose dofs are the points
! of the VTK cells, one set per element. The columns belong to the FEDOF of
! the field, for example a hierarchical basis. If u holds the dofs of the
! field, then M u gives the values of the field at the VTK points.
!
! For each active element the routine:
!
! - gets the order of the element from the column FEDOF,
! - computes the equidistant interpolation points of that order in the
!   reference element,
! - evaluates the column shape functions at these points,
! - orients the column connectivity with `FixOrientOfNptrs`,
! - sets the element block in the matrix for each of the nCopy
!   diagonal copies (idof = jdof = 1, ..., nCopy).
!
! A row dof belongs to exactly one element, so the entries are set, not
! added.
!
! The routine needs the following.
!
! - The matrix is initiated, and its sparsity pattern contains the
!   element blocks.
! - The row and column FEDOFs are set with [[AbstractMatrixFieldSetFEDOF]].
!   Pass colFedof explicitly, because by default the columns use the row
!   FEDOF.
! - The row and column FEDOFs use the same mesh.
! - The order of an element is the same in all directions.
! - `FixOrientOfNptrs` is implemented for the element type of the column
!   FE.
!
! Formula:
!
! $$ M_{ij} = N_j(x_i) $$
!
! where x_i is the i-th VTK point of an element and N_j is the j-th
! shape function of the column FE.
!
! See also: [[AbstractMatrixFieldSetFEDOF]],
! [[FEDOF_:InitiateBrokenLagrangeFedofForVTK]]

INTERFACE
  MODULE SUBROUTINE obj_SetBrokenLagrangeMatrixForVTK(obj, ivar, jvar, &
                                                     nCopy)
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: obj
    !! Matrix field, initiated with the sparsity of the element blocks
    INTEGER(I4B), INTENT(IN) :: ivar
    !! Physical variable of the rows. If ivar > 1, rowFedofs must be
    !! allocated, and rowFedofs(ivar) is the broken Lagrange FEDOF.
    INTEGER(I4B), INTENT(IN) :: jvar
    !! Physical variable of the columns. If jvar > 1, colFedofs must be
    !! allocated, and colFedofs(jvar) is the FEDOF of the field.
    INTEGER(I4B), INTENT(IN) :: nCopy
    !! Number of diagonal copies of the element block. Use 1 for a scalar
    !! field and spaceCompo for a vector field.
  END SUBROUTINE obj_SetBrokenLagrangeMatrixForVTK
END INTERFACE

END MODULE AbstractMatrixField_Class
