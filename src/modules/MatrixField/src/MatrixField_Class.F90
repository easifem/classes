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

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-15
! summary: Native serial finite element matrix field
!
! This module defines [[MatrixField_]], the native implementation of
! [[AbstractMatrixField_]]. It stores a global finite element matrix
! (tangent, stiffness, mass) in a CSR matrix and uses the
! `NATIVE_SERIAL` engine.
!
! A [[MatrixField_]] is created from a node field, for example a
! [[ScalarField_]] or a [[VectorField_]]. The degrees of freedom and the
! sparsity pattern come from the FEDOF of that node field.

MODULE MatrixField_Class
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractMatrixField_Class, ONLY: AbstractMatrixField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE BaseType, ONLY: CSRMatrix_
USE BaseType, ONLY: math => TypeMathOpt
USE DirichletBC_Class, ONLY: DirichletBC_
USE DirichletBC_Class, ONLY: DirichletBCPointer_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DOF_FMT
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE TimeFEDOF_Class, ONLY: TimeFEDOFPointer_
IMPLICIT NONE
PRIVATE

PUBLIC :: MatrixField_
PUBLIC :: MatrixFieldPointer_
PUBLIC :: MatrixFieldAllocate

INTEGER(I4B), PARAMETER :: MYSTORAGEFORMAT = TypeFieldOpt%storageFormatDOF
INTEGER(I4B), PRIVATE, PARAMETER :: IPAR_LENGTH = 14
INTEGER(I4B), PRIVATE, PARAMETER :: FPAR_LENGTH = 14

!----------------------------------------------------------------------------
!                                                   MatrixFieldPrecondition_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-13
! summary: Preconditioning matrix of a matrix field
!
! This type stores the preconditioning matrix of [[MatrixField_]]. It is
! meant for internal use. The storage depends on the preconditioner and
! on the linear solver library. Incomplete LU preconditioners (ILUT and
! its variants) are stored in the Modified Sparse Row (MSR) format of
! the Sparsekit library.
!
! In the MSR format, with n rows and nnz = number of nonzeros + 1:
!
!- A(1:n) contains the diagonal of the matrix.
!- A(n+2:nnz) contains the off-diagonal entries, stored row by row.
!- JA(n+2:nnz) contains their column indices.
!- JA(1:n+1) points to the beginning of each row in A and JA: for
!  i <= n + 1, JA(i) is the start of row i.
!
! See also: [[MatrixField_]]

TYPE :: MatrixFieldPrecondition_
  LOGICAL(LGT) :: isInitiated = math%no
  !! True if the preconditioner is initiated
  INTEGER(I4B) :: pmatName = math%zero_i
  !! Name (kind) of the preconditioner
  INTEGER(I4B) :: nnz = math%zero_i
  !! Number of entries in A and JA
  INTEGER(I4B) :: ncol = math%zero_i
  !! Number of columns
  INTEGER(I4B) :: nrow = math%zero_i
  !! Number of rows
  INTEGER(I4B) :: lfil = math%zero_i
  !! Fill-in parameter of ILUT and ILUK
  INTEGER(I4B) :: mbloc = math%zero_i
  !! Block size for the permutation of ILUTP and ILUDP
  REAL(DFP) :: alpha = math%zero
  !! Diagonal compensation parameter of ILUD and ILUDP
  REAL(DFP) :: droptol = math%zero
  !! Drop tolerance
  REAL(DFP) :: permtol = math%zero
  !! Permutation tolerance
  REAL(DFP), ALLOCATABLE :: A(:)
  !! Values in MSR format
  INTEGER(I4B), ALLOCATABLE :: JA(:)
  !! Column indices and row pointers in MSR format
  INTEGER(I4B), ALLOCATABLE :: IA(:)
  !! Row pointers
  INTEGER(I4B), ALLOCATABLE :: JU(:)
  !! Pointers to the diagonal of U
  INTEGER(I4B), ALLOCATABLE :: IPERM(:)
  !! Permutation of the columns
  INTEGER(I4B), ALLOCATABLE :: LEVS(:)
  !! Fill levels of ILUK
END TYPE MatrixFieldPrecondition_

!----------------------------------------------------------------------------
!                                               MatrixFieldPreconditionCopy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Copy a preconditioner
!
! This routine copies all components of obj2 into obj.
!
! Examples
!
!```fortran
! CALL MatrixFieldPreconditionCopy(obj=obj%pmat, obj2=obj2%pmat)
!```
!
! This example still needs a matching test.

INTERFACE
  MODULE SUBROUTINE MatrixFieldPreconditionCopy(obj, obj2)
    TYPE(MatrixFieldPrecondition_), INTENT(INOUT) :: obj
    !! preconditioner to copy into
    TYPE(MatrixFieldPrecondition_), INTENT(IN) :: obj2
    !! preconditioner to copy from
  END SUBROUTINE MatrixFieldPreconditionCopy
END INTERFACE

!----------------------------------------------------------------------------
!                                                              MatrixField_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-15
! summary: Native serial finite element matrix
!
! [[MatrixField_]] stores a global finite element matrix in the CSR
! matrix `mat`. Its rows and columns are the degrees of freedom of a
! node field, stored in DOF_FMT: the index of node `i` and degree of
! freedom `j` is `(j - 1) * tNodes + i`.
!
! An instance is created by [[obj_Initiate1]] from a [[ScalarField_]],
! a [[VectorField_]] or another [[MatrixField_]].
!
! Dirichlet boundary conditions are applied by [[obj_ApplyDirichletBC1]].
! It stores the Dirichlet degrees of freedom in `dbcPtrs` and the
! columns of `mat` for these degrees of freedom in `submat`. Then
! [[obj_ApplyDirichletBCToRHS]] uses `submat` to correct the rhs.
!
! See also: [[AbstractMatrixField_]], [[CSRMatrix_]]

TYPE, EXTENDS(AbstractMatrixField_) :: MatrixField_
  LOGICAL(LGT) :: isRectangle = math%no
  !! True if the matrix is a rectangle matrix
  LOGICAL(LGT) :: isSubmatInit = math%no
  !! True if the submatrix is initiated
  INTEGER(I4B) :: tdbcPtrs = math%zero_i
  !! Number of Dirichlet degrees of freedom in dbcPtrs
  INTEGER(I4B) :: tsubIndices = math%zero_i
  !! Number of entries in subIndices
  TYPE(CSRMatrix_) :: mat
  !! Main csr sparse matrix which is formed internally
  TYPE(MatrixFieldPrecondition_) :: pmat
  !! Preconditioner, formed internallly
  TYPE(CSRMatrix_) :: submat
  !! Columns of mat for the Dirichlet degrees of freedom, used to
  !! apply the Dirichlet boundary condition to the rhs
  INTEGER(I4B), ALLOCATABLE :: dbcPtrs(:)
  !! Dirichlet degrees of freedom (columns of mat)
  INTEGER(I4B), ALLOCATABLE :: subIndices(:)
  !! Indices of the entries of submat in mat

CONTAINS
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate matrix from a node field or another matrix field
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate matrix from row and col node field
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the field
  FINAL :: obj_Final
  !! Deallocate the field

  !@IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the field
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import from an HDF5 file
  PROCEDURE, PUBLIC, PASS(obj) :: ImportPmat => obj_ImportPmat
  !! Import the preconditioner from an HDF5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export to an HDF5 file
  PROCEDURE, PUBLIC, PASS(obj) :: ExportPmat => obj_ExportPmat
  !! Export the preconditioner to an HDF5 file
  PROCEDURE, PUBLIC, PASS(obj) :: SPY => obj_SPY
  !! Plot the sparsity pattern in various file formats

  ! Deferred methods from AbstractMatrixField and AbstractField
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalPhysicalVars => &
    obj_GetTotalPhysicalVars
  !! Get the number of physical variables
  PROCEDURE, PUBLIC, PASS(obj) :: GetPhysicalNames => &
    obj_GetPhysicalNames
  !! Get the names of the physical variables
  PROCEDURE, PUBLIC, PASS(obj) :: GetSpaceCompo => obj_GetSpaceCompo
  !! Get the space components of the physical variables
  PROCEDURE, PUBLIC, PASS(obj) :: GetTimeCompo => obj_GetTimeCompo
  !! Get the time components of the physical variables
  PROCEDURE, PUBLIC, PASS(obj) :: GetStorageFMT => obj_GetStorageFMT
  !! Get the storage format of the matrix field

  !@GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IsSubmatInitiated => &
    obj_IsSubmatInitiated
  !! True if the Dirichlet submatrix is initiated
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  !! Get the size of the matrix
  PROCEDURE, PUBLIC, PASS(obj) :: Shape => obj_Shape
  !! Get the shape of the matrix
  PROCEDURE, PRIVATE, PASS(obj) :: Get1 => obj_Get1
  !! Get an element matrix
  PROCEDURE, PRIVATE, PASS(obj) :: Get2 => obj_Get2
  !! Get a single entry by node and dof
  PROCEDURE, PRIVATE, PASS(obj) :: Get3 => obj_Get3
  !! Get the block of two physical variables
  PROCEDURE, PRIVATE, PASS(obj) :: Get4 => obj_Get4
  !! Get the block of two degrees of freedom
  PROCEDURE, PRIVATE, PASS(obj) :: Get5 => obj_Get5
  !! Get a single entry by physical variable and dof
  PROCEDURE, PRIVATE, PASS(obj) :: Get6 => obj_Get6
  !! Get a single entry by space and time components
  PROCEDURE, PRIVATE, PASS(obj) :: Get7 => obj_Get7
  !! Get a block of nodes and components

  ! @GetRowMethods
  PROCEDURE, PRIVATE, PASS(obj) :: GetRow1 => obj_GetRow1
  !! Get a row given by node and dof
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: GetRow2 => obj_GetRow2
  !! Get a row given by node, physical variable and dof
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: GetRow3 => obj_GetRow3
  !! Get a row given by node and components
  PROCEDURE, PRIVATE, PASS(obj) :: GetRow4 => obj_GetRow4
  !! Get rows of a node for several time components
  PROCEDURE, PRIVATE, PASS(obj) :: GetRow5 => obj_GetRow5
  !! Get rows of a node for several space components
  PROCEDURE, PRIVATE, PASS(obj) :: GetRow6 => obj_GetRow6
  !! Get rows of several nodes and time components
  PROCEDURE, PRIVATE, PASS(obj) :: GetRow7 => obj_GetRow7
  !! Get rows of several nodes and space components

  ! @GetColMethods
  PROCEDURE, PRIVATE, PASS(obj) :: GetColumn1 => obj_GetColumn1
  !! Get a column given by node and dof
  PROCEDURE, PRIVATE, PASS(obj) :: GetColumn2 => obj_GetColumn2
  !! Get a column given by node, physical variable and dof
  PROCEDURE, PRIVATE, PASS(obj) :: GetColumn3 => obj_GetColumn3
  !! Get a column given by node and components
  PROCEDURE, PRIVATE, PASS(obj) :: GetColumn4 => obj_GetColumn4
  !! Get columns of a node for several time components
  PROCEDURE, PRIVATE, PASS(obj) :: GetColumn5 => obj_GetColumn5
  !! Get columns of a node for several space components
  PROCEDURE, PRIVATE, PASS(obj) :: GetColumn6 => obj_GetColumn6
  !! Get columns of several nodes and time components
  PROCEDURE, PRIVATE, PASS(obj) :: GetColumn7 => obj_GetColumn7
  !! Get columns of several nodes and space components

  ! @SetMethods
  PROCEDURE, PRIVATE, PASS(obj) :: Set1 => obj_Set1
  !! Set an element matrix
  PROCEDURE, PRIVATE, PASS(obj) :: Set2 => obj_Set2
  !! Set all entries, or a block of nodes, to a scalar
  PROCEDURE, PRIVATE, PASS(obj) :: Set3 => obj_Set3
  !! Set a single entry by node and dof
  PROCEDURE, PRIVATE, PASS(obj) :: Set4 => obj_Set4
  !! Set the block of two physical variables
  PROCEDURE, PRIVATE, PASS(obj) :: Set5 => obj_Set5
  !! Set the block of two degrees of freedom
  PROCEDURE, PRIVATE, PASS(obj) :: Set6 => obj_Set6
  !! Set a single entry by physical variable and dof
  PROCEDURE, PRIVATE, PASS(obj) :: Set7 => obj_Set7
  !! Set a single entry by space and time components
  PROCEDURE, PRIVATE, PASS(obj) :: Set8 => obj_Set8
  !! Set a block of nodes and components to a scalar
  PROCEDURE, PRIVATE, PASS(obj) :: Set9 => obj_Set9
  !! Set a block of nodes and time components to a scalar
  PROCEDURE, PRIVATE, PASS(obj) :: Set10 => obj_Set10
  !! Set a block of nodes and space components to a scalar
  PROCEDURE, PRIVATE, PASS(obj) :: Set11 => obj_Set11
  !! Set or add another matrix field
  PROCEDURE, PUBLIC, PASS(obj) :: SetFromSTMatrix => obj_SetFromSTMatrix
  !! Set a time block of a space-time matrix into obj
  PROCEDURE, PUBLIC, PASS(obj) :: SetToSTMatrix => obj_SetToSTMatrix
  !! Set a space matrix into a time block of obj

  ! SET:
  ! @SetRowMethods
  PROCEDURE, PRIVATE, PASS(obj) :: SetRow1 => obj_SetRow1
  !! Set a row given by node and dof
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: SetRow2 => obj_SetRow2
  !! Set a row given by node, physical variable and dof
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: SetRow3 => obj_SetRow3
  !! Set a row given by node and components
  PROCEDURE, PRIVATE, PASS(obj) :: SetRow4 => obj_SetRow4
  !! Set rows of a node for several time components
  PROCEDURE, PRIVATE, PASS(obj) :: SetRow5 => obj_SetRow5
  !! Set rows of a node for several space components
  PROCEDURE, PRIVATE, PASS(obj) :: SetRow6 => obj_SetRow6
  !! Set rows of several nodes and time components
  PROCEDURE, PRIVATE, PASS(obj) :: SetRow7 => obj_SetRow7
  !! Set rows of several nodes and space components

  ! @SetColMethods
  PROCEDURE, PRIVATE, PASS(obj) :: SetColumn1 => obj_SetColumn1
  !! Set a column given by node and dof
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: SetColumn2 => &
    obj_SetColumn2
  !! Set a column given by node, physical variable and dof
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: SetColumn3 => &
    obj_SetColumn3
  !! Set a column given by node and components
  PROCEDURE, PRIVATE, PASS(obj) :: SetColumn4 => obj_SetColumn4
  !! Set columns of a node for several time components
  PROCEDURE, PRIVATE, PASS(obj) :: SetColumn5 => obj_SetColumn5
  !! Set columns of a node for several space components
  PROCEDURE, PRIVATE, PASS(obj) :: SetColumn6 => obj_SetColumn6
  !! Set columns of several nodes and time components
  PROCEDURE, PRIVATE, PASS(obj) :: SetColumn7 => obj_SetColumn7
  !! Set columns of several nodes and space components

  ! @MatvecMethods
  PROCEDURE, PRIVATE, PASS(obj) :: Matvec1 => obj_Matvec1
  !! Matrix vector multiplication with Fortran arrays
  PROCEDURE, PRIVATE, PASS(obj) :: Matvec2 => obj_Matvec2
  !! Matrix vector multiplication with node fields

  ! @LUSolveMethods
  PROCEDURE, PRIVATE, PASS(obj) :: ILUSOLVE1 => obj_ILUSOLVE1
  !! Solve (LU) sol = rhs with Fortran arrays
  PROCEDURE, PRIVATE, PASS(obj) :: ILUSOLVE2 => obj_ILUSOLVE2
  !! Solve (LU) sol = rhs with node fields

  ! @PreconditionMethods
  ! PROCEDURE, PUBLIC, PASS(obj) :: SetPrecondition => obj_SetPrecondition
  !! Building precondition matrix
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrecondition => obj_GetPrecondition
  !! Get the preconditioning matrix
  PROCEDURE, PUBLIC, PASS(obj) :: ReversePermutation => &
    obj_ReversePermutation
  !! Undo the permutation of the preconditioner

  ! @DiagonalMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetDiagonal => obj_GetDiagonal
  !! Get the diagonal of the matrix

  ! @DiagonalScalingMethods
  PROCEDURE, PUBLIC, PASS(obj) :: DiagonalScaling => obj_DiagonalScaling
  !! Scale the matrix by a diagonal

  ! @SpectralMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SymSchurLargestEigenVal => &
    obj_SymSchurLargestEigenVal
  !! Largest eigenvalues of a symmetric Schur complement
  PROCEDURE, PUBLIC, PASS(obj) :: SymLargestEigenVal => &
    obj_SymLargestEigenVal
  !! Largest eigenvalues of a symmetric matrix

  ! @DBCMethods
  PROCEDURE, PRIVATE, PASS(obj) :: ApplyDirichletBC1 => obj_ApplyDirichletBC1
  !! Apply the Dirichlet boundary condition to the matrix
  GENERIC, PUBLIC :: ApplyDirichletBC => ApplyDirichletBC1
  !! Generic method to apply the Dirichlet boundary condition
  PROCEDURE, PUBLIC, PASS(obj) :: ApplyDirichletBCtoRHS => &
    obj_ApplyDirichletBCToRHS
  !! Apply the Dirichlet boundary condition to a rhs
  PROCEDURE, PUBLIC, PASS(obj) :: GetDirichletBCSubMat => &
    obj_GetDirichletBCSubMat
  !! Get the submatrix of the Dirichlet columns

  ! @UnaryMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Scal => obj_Scal
  !! Scale the matrix, obj = a * obj

END TYPE MatrixField_

!----------------------------------------------------------------------------
!                                                     MatrixFieldPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-15
! summary: Pointer to a matrix field
!
! This type holds a pointer to a [[MatrixField_]], so that arrays of
! matrix fields of different child classes can be built.

TYPE :: MatrixFieldPointer_
  CLASS(MatrixField_), POINTER :: ptr => NULL()
  !! pointer to a matrix field
END TYPE MatrixFieldPointer_

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-09
! summary: Deallocate a preconditioner
!
! This routine deallocates the arrays of [[MatrixFieldPrecondition_]]
! and resets its scalar components to their default values.
!
! Examples
!
!```fortran
! CALL MatrixFieldDeallocate(obj%pmat)
!```
!
! This example still needs a matching test.
!
! See also: [[obj_Deallocate]]

INTERFACE MatrixFieldDeallocate
  MODULE SUBROUTINE Pmat_Deallocate(obj)
    TYPE(MatrixFieldPrecondition_), INTENT(INOUT) :: obj
    !! preconditioner
  END SUBROUTINE Pmat_Deallocate
END INTERFACE MatrixFieldDeallocate

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Finalizer of a matrix field
!
! This finalizer calls [[obj_Deallocate]].
!
! See also: [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Initiate a matrix field from a node field or a matrix field
!
! This routine deallocates obj and initiates it from obj2. First it
! copies the data of [[AbstractField_]] (name, engine, FEDOF pointers)
! from obj2. Then:
!
!- If obj2 is a node field ([[AbstractNodeField_]], for example a
!  [[ScalarField_]] or a [[VectorField_]]), it creates a square CSR
!  matrix with one row per degree of freedom of obj2, in DOF_FMT. The
!  matrix property is "UNSYM" and the sparsity pattern comes from the
!  FEDOF of obj2. All entries are zero.
!- If obj2 is a [[MatrixField_]], it copies mat, submat, pmat, dbcPtrs
!  and subIndices from obj2.
!
! obj2 must be initiated.
!
! Changes:
!   - 2026-11-09: The CSR matrix is created with matrixProp "UNSYM".
!   - 2026-11-11: Set rowFedof and colFedof (square matrix) from obj2.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE FieldOpt_Class, ONLY: TypeFieldOpt
! USE MatrixField_Class, ONLY: MatrixField_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MatrixField_) :: obj
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! CALL obj%Initiate(obj2=u)
!
! CALL OK(obj%IsInitiated(), "IsInitiated")
! CALL OK(obj%SIZE(1) .EQ. 6, "Size(1) = 6")
! CALL OK(obj%SIZE(2) .EQ. 6, "Size(2) = 6")
! CALL OK(obj%SIZE() .EQ. 36, "Size() = 36")
! CALL OK(ALL(obj%SHAPE() .EQ. [6, 6]), "Shape = [6, 6]")
! CALL OK(obj%mat%csr%nnz .EQ. 28, "nnz = 28")
! CALL OK(TRIM(obj%mat%matrixProp) .EQ. "UNSYM", "matrixProp = UNSYM")
! CALL OK(obj%GetStorageFMT() .EQ. TypeFieldOpt%storageFormatDOF, &
!         "GetStorageFMT")
! CALL OK(.NOT. obj%isRectangle, "not rectangle")
! CALL OK(.NOT. obj%IsSubmatInitiated(), "IsSubmatInitiated is false")
!
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%IsInitiated(), "DEALLOCATE")
!
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! Initiate from a vector field and copy a matrix field:
! src/modules/MatrixField/examples/Initiate_test_2.F90
!
! See also: [[obj_Deallocate]], [[AbstractNodeField_]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate1( &
    obj, obj2, copyFull, copyStructure, usePointer)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field to initiate
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! node field or matrix field, it must be initiated
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    !! passed to AbstractFieldInitiate
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    !! passed to AbstractFieldInitiate
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
    !! passed to AbstractFieldInitiate
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Construct a matrix for y = A * x
!
! This method create a sparse matrix field from given row and col
! fields, rowField and colField.
!
! We can make matrix for following operations:
!
! 1. y = A * x, where both x=colObj and y=rowObj are children of
! [[AbstractNodeField_]]
! 2. y = A * B * x, where A = rowObj and B = colObj are both
! children of [[AbstractMatrixField]]
INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, rowObj, colObj)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! Matrix field to map rowObj with colObj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: rowObj
    !! Abstract field for row dimension
    CLASS(AbstractNodeField_), INTENT(INOUT) :: colObj
    !! Abstract field for col dimension
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Deallocate a matrix field
!
! This routine deallocates mat, submat, pmat, dbcPtrs and subIndices,
! and resets the other components to their default values.
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
! See also: [[obj_Initiate1]]

INTERFACE MatrixFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
  END SUBROUTINE obj_Deallocate
END INTERFACE MatrixFieldDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Deallocate a vector of matrix field pointers
!
! This routine deallocates the matrix field of every pointer and then
! the vector obj.
!
! Examples
!
!```fortran
! TYPE(MatrixFieldPointer_), ALLOCATABLE :: mats(:)
! CALL MatrixFieldDeallocate(mats)
!```
!
! This example still needs a matching test.
!
! See also: [[obj_MatrixFieldAllocate1]]

INTERFACE MatrixFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate_ptr_vector(obj)
    TYPE(MatrixFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of matrix field pointers
  END SUBROUTINE obj_Deallocate_ptr_vector
END INTERFACE MatrixFieldDeallocate

!----------------------------------------------------------------------------
!                                MatrixFieldSafeAllocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-09-25
! summary: Safely allocate a vector of matrix field pointers
!
! This routine allocates obj with newsize entries if it is not
! allocated. If it is allocated with fewer than newsize entries, it is
! deallocated and allocated again. Otherwise it is left unchanged.
!
! Examples
!
!```fortran
! TYPE(MatrixFieldPointer_), ALLOCATABLE :: mats(:)
! CALL MatrixFieldAllocate(mats, newsize=2)
!```
!
! This example still needs a matching test.
!
! See also: [[obj_Deallocate_ptr_vector]]

INTERFACE MatrixFieldAllocate
  MODULE SUBROUTINE obj_MatrixFieldAllocate1(obj, newsize)
    TYPE(MatrixFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of matrix field pointers
    INTEGER(I4B), INTENT(IN) :: newsize
    !! minimum size of obj
  END SUBROUTINE obj_MatrixFieldAllocate1
END INTERFACE MatrixFieldAllocate

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Display the content of a matrix field
!
! This routine displays the content of [[AbstractMatrixField_]], the
! Dirichlet data (dbcPtrs, subIndices), the matrix mat and, if it is
! initiated, the submatrix submat.
!
! Examples
!
!```fortran
! CALL obj%Display(msg="MatrixField: ")
!```
!
! This example still needs a matching test.

INTERFACE MatrixFieldDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed before the content
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
    !! unit number, default is stdout
  END SUBROUTINE obj_Display
END INTERFACE MatrixFieldDisplay

!----------------------------------------------------------------------------
!                                                          Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Import a matrix field from an HDF5 file
!
! This routine reads a matrix field written by [[obj_Export]] from the
! group `group` of an HDF5 file open in read mode. obj must not be
! initiated.
!
!- The data of [[AbstractField_]] (name, engine, fieldType, comm and
!  sizes) is read by AbstractFieldImport.
!- The row and column FEDOF pointers are set by
!  [[AbstractMatrixFieldSetFEDOF]]. An absent column argument takes the
!  row value (square matrix).
!- matrixProp and isRectangle are read from `group`.
!- The CSR matrix is read from `group/mat`.
!- The preconditioner is read from `group/pmat` if it is present.
!
! Changes:
!   - 2026-11-10: Read the header with AbstractFieldImport, the layout
!     written by obj_Export. Restore dof. Do not raise an error when
!     `group/pmat` is absent.
!   - 2026-11-11: The arguments are rowFedof(s), rowGeoFedof(s),
!     colFedof(s) and colGeoFedof(s) instead of fedof(s), geofedof(s)
!     and timefedof(s). The DOF of the matrix is read with `group/mat`;
!     an old `group/DofR0/dof` entry is ignored.
!
! Examples
!
!```fortran
! CALL h5%Initiate(filename=filename, mode="READ")
! CALL h5%OPEN()
! CALL obj2%Import(hdf5=h5, group="/K", rowFedof=fedof, &
!                   rowGeoFedof=geofedof)
! CALL h5%DEALLOCATE()
!```
!
! Full example: src/modules/MatrixField/examples/Export_test_1.F90
!
! See also: [[obj_Export]], [[obj_ImportPmat]]

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, rowFedof, rowFedofs, &
                               rowGeoFedof, rowGeoFedofs, colFedof, &
                               colFedofs, colGeoFedof, colGeoFedofs)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Import the preconditioner from an HDF5 file
!
! This routine reads the preconditioner pmat from the group `group`
! (usually `<field group>/pmat`) and sets isPmatInitiated to true.
! [[obj_Import]] calls it when `<field group>/pmat` is present.
!
! Examples
!
!```fortran
! CALL obj%ImportPmat(hdf5=h5, group="/K/pmat", fedof=fedof)
!```
!
! This example still needs a matching test.
!
! See also: [[obj_ExportPmat]], [[obj_Import]]

INTERFACE
  MODULE SUBROUTINE obj_ImportPmat( &
    obj, hdf5, group, fedof, fedofs, timefedof, timefedofs)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file open in read mode
    CHARACTER(*), INTENT(IN) :: group
    !! group of the preconditioner in the HDF5 file
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof
    !! FEDOF of a square matrix field
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:)
    !! FEDOFs of a rectangle matrix field
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
    !! time FEDOF
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedofs(:)
    !! time FEDOFs
  END SUBROUTINE obj_ImportPmat
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Export a matrix field to an HDF5 file
!
! This routine writes the matrix field to the group `group` of an HDF5
! file open in write mode.
!
!- AbstractFieldExport writes the data of [[AbstractField_]], for
!  example `group/StringR0/name`.
!- `group/isPmatInitiated`, `group/isRectangle` and `group/matrixProp`
!  are written next.
!- The CSR matrix is written to `group/mat`.
!- The preconditioner is written to `group/pmat` if it is initiated.
!
! Changes:
!   - 2026-11-11: `group/DofR0/dof` is not written; the DOF of the
!     matrix is written with `group/mat`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE MatrixField_Class, ONLY: MatrixField_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
! CHARACTER(*), PARAMETER :: filename = "./Export_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MatrixField_) :: obj, obj2
! TYPE(HDF5File_) :: h5
! REAL(DFP) :: val1, val2
! INTEGER(I4B) :: ii
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL obj%Initiate(obj2=u)
! DO ii = 1, SIZE(obj%mat%A)
!   obj%mat%A(ii) = REAL(ii, DFP)
! END DO
!
! CALL h5%Initiate(filename=filename, mode="NEW")
! CALL h5%OPEN()
! CALL obj%Export(hdf5=h5, group="/K")
! CALL h5%DEALLOCATE()
!
! CALL h5%Initiate(filename=filename, mode="READ")
! CALL h5%OPEN()
! CALL obj2%Import(hdf5=h5, group="/K", rowFedof=fedof, &
!                   rowGeoFedof=geofedof)
! CALL h5%DEALLOCATE()
!
! CALL OK(obj2%IsInitiated(), "Import: IsInitiated")
! CALL OK(ALL(obj2%SHAPE() .EQ. [6, 6]), "Import: Shape")
! CALL OK(obj2%mat%csr%nnz .EQ. 28, "Import: nnz")
! CALL OK(ALL(obj2%mat%csr%JA .EQ. obj%mat%csr%JA), "Import: sparsity")
! CALL OK(ALL(ABS(obj2%mat%A - obj%mat%A) .LT. tol), "Import: values")
! CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, jdof=1, &
!              VALUE=val1)
! CALL obj2%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, jdof=1, &
!               VALUE=val2)
! CALL OK(ABS(val1 - val2) .LT. tol, "Import: dof, Get(5, 6)")
!
! CALL obj2%DEALLOCATE()
! CALL obj%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Import]], [[obj_ExportPmat]]

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file open in write mode
    CHARACTER(*), INTENT(IN) :: group
    !! group of the matrix field in the HDF5 file
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ExportPmat@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Export the preconditioner to an HDF5 file
!
! This routine writes the preconditioner pmat to `group/pmat`. It
! writes nothing if isPmatInitiated is false. [[obj_Export]] calls it.
!
! Examples
!
!```fortran
! CALL obj%ExportPmat(hdf5=h5, group="/K")
!```
!
! This example still needs a matching test.
!
! See also: [[obj_ImportPmat]], [[obj_Export]]

INTERFACE
  MODULE SUBROUTINE obj_ExportPmat(obj, hdf5, group)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file open in write mode
    CHARACTER(*), INTENT(IN) :: group
    !! group of the matrix field in the HDF5 file
  END SUBROUTINE obj_ExportPmat
END INTERFACE

!----------------------------------------------------------------------------
!                                                              SPY@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Plot the sparsity pattern of the matrix
!
! This routine writes the sparsity pattern of mat to the file
! filename.ext by calling SPY of [[CSRMatrix_]].
!
! Examples
!
!```fortran
! CALL obj%SPY(filename="K", ext="svg")
!```
!
! This example still needs a matching test.

INTERFACE
  MODULE SUBROUTINE obj_SPY(obj, filename, ext)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    CHARACTER(*), INTENT(IN) :: filename
    !! file name without extension
    CHARACTER(*), INTENT(IN) :: ext
    !! file extension, it selects the file format
  END SUBROUTINE obj_SPY
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetTotalPhysicalVars@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-07
! summary: Get the number of physical variables
!
! This function implements the deferred method of [[AbstractField_]].
! It is under development and raises a "[WIP ERROR]" in debug mode.
!
! Returns:
!   ans : INTEGER(I4B) - number of physical variables
!
! Examples
!
!```fortran
! n = obj%GetTotalPhysicalVars()
!```
!
! This example still needs a matching test.

INTERFACE
  MODULE FUNCTION obj_GetTotalPhysicalVars(obj) RESULT(ans)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B) :: ans
    !! number of physical variables
  END FUNCTION obj_GetTotalPhysicalVars
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetPhysicalNames@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-07
! summary: Get the names of the physical variables
!
! This routine implements the deferred method of [[AbstractField_]].
! It is under development and raises a "[WIP ERROR]" in debug mode.
!
! Examples
!
!```fortran
! CHARACTER(1) :: names(1)
! CALL obj%GetPhysicalNames(ans=names)
!```
!
! This example still needs a matching test.

INTERFACE
  MODULE SUBROUTINE obj_GetPhysicalNames(obj, ans)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field
    CHARACTER(*), INTENT(INOUT) :: ans(:)
    !! names of the physical variables
  END SUBROUTINE obj_GetPhysicalNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetSpaceCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-07
! summary: Get the space components of the physical variables
!
! This function implements the deferred method of [[AbstractField_]].
! It is under development and raises a "[WIP ERROR]" in debug mode.
!
! Returns:
!   ans : INTEGER(I4B)(tPhysicalVars) - number of space components
!
! Examples
!
!```fortran
! INTEGER(I4B) :: sc(1)
! sc = obj%GetSpaceCompo(tPhysicalVars=1)
!```
!
! This example still needs a matching test.

INTERFACE
  MODULE FUNCTION obj_GetSpaceCompo(obj, tPhysicalVars) RESULT(ans)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    !! number of physical variables, see GetTotalPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
    !! number of space components of each physical variable
  END FUNCTION obj_GetSpaceCompo
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetTimeCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-07
! summary: Get the time components of the physical variables
!
! This function implements the deferred method of [[AbstractField_]].
! It is under development and raises a "[WIP ERROR]" in debug mode.
!
! Returns:
!   ans : INTEGER(I4B)(tPhysicalVars) - number of time components
!
! Examples
!
!```fortran
! INTEGER(I4B) :: tc(1)
! tc = obj%GetTimeCompo(tPhysicalVars=1)
!```
!
! This example still needs a matching test.

INTERFACE
  MODULE FUNCTION obj_GetTimeCompo(obj, tPhysicalVars) RESULT(ans)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    !! number of physical variables, see GetTotalPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
    !! number of time components of each physical variable
  END FUNCTION obj_GetTimeCompo
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetStorageFMT@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-07
! summary: Get the storage format of the matrix field
!
! This function implements the deferred method of [[AbstractField_]].
! [[MatrixField_]] always uses DOF_FMT
! (TypeFieldOpt%storageFormatDOF).
!
! Returns:
!   ans : INTEGER(I4B) - storage format, DOF_FMT
!
! Examples
!
!```fortran
! CALL OK(obj%GetStorageFMT() .EQ. TypeFieldOpt%storageFormatDOF, &
!         "GetStorageFMT")
!```
!
! Full example: src/modules/MatrixField/examples/Initiate_test_1.F90

INTERFACE
  MODULE FUNCTION obj_GetStorageFMT(obj) RESULT(ans)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B) :: ans
    !! storage format
  END FUNCTION obj_GetStorageFMT
END INTERFACE

!----------------------------------------------------------------------------
!                                               IsSubmatInitiated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-06
! summary: True if the Dirichlet submatrix is initiated
!
! This function returns isSubmatInit. The submatrix is initiated by
! [[obj_ApplyDirichletBC1]] when dbcPtrs is given.
!
! Returns:
!   ans : LOGICAL(LGT) - true if submat is initiated
!
! Examples
!
!```fortran
! CALL obj%ApplyDirichletBC(dbcPtrs=[1, 2])
! CALL OK(obj%IsSubmatInitiated(), "IsSubmatInitiated")
!```
!
! Full example:
! src/modules/MatrixField/examples/ApplyDirichletBC_test_1.F90

INTERFACE
  MODULE FUNCTION obj_IsSubmatInitiated(obj) RESULT(ans)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field
    LOGICAL(LGT) :: ans
    !! true if submat is initiated
  END FUNCTION obj_IsSubmatInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Size@GetMethods
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
! See also: [[obj_Shape]]

INTERFACE
  MODULE FUNCTION obj_Size(obj, dim) RESULT(ans)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! 1 for rows, 2 for columns; absent for the total size
    INTEGER(I4B) :: ans
    !! size of the matrix
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Shape@GetMethods
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
! See also: [[obj_Size]]

INTERFACE
  MODULE FUNCTION obj_Shape(obj) RESULT(ans)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B) :: ans(2)
    !! number of rows and columns
  END FUNCTION obj_Shape
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Matvec@MatVecMethods
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
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE FieldOpt_Class, ONLY: TypeFieldOpt
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatrixField_Class, ONLY: MatrixField_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! REAL(DFP), PARAMETER :: ax(6) = [14.0_DFP, 4.0_DFP, 4.0_DFP, 4.0_DFP, &
!                                  8.0_DFP, 8.0_DFP]
! REAL(DFP), PARAMETER :: atx(6) = [4.0_DFP, 4.0_DFP, 4.0_DFP, 4.0_DFP, &
!                                   18.0_DFP, 8.0_DFP]
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u, w
! TYPE(MatrixField_) :: obj
! REAL(DFP) :: x(6), y(6), ke(4, 4), found(6)
! INTEGER(I4B) :: ii
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL w%Initiate(NAME="w", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
! CALL obj%Initiate(obj2=u)
!
! CALL obj%Set(VALUE=0.0_DFP)
! ke = 1.0_DFP
! CALL obj%Set(globalNode=[1, 5, 6, 4], islocal=math%yes, VALUE=ke, &
!              storageFMT=TypeFieldOpt%storageFormatDOF, &
!              addContribution=math%yes)
! CALL obj%Set(globalNode=[5, 2, 3, 6], islocal=math%yes, VALUE=ke, &
!              storageFMT=TypeFieldOpt%storageFormatDOF, &
!              addContribution=math%yes)
! CALL obj%Set(iNodeNum=1, jNodeNum=5, islocal=math%yes, idof=1, jdof=1, &
!              VALUE=10.0_DFP, addContribution=math%yes)
! x = 1.0_DFP
!
! ! Test 1: y = A x
! CALL obj%Matvec(x=x, y=y)
! CALL OK(ALL(ABS(y - ax) .LT. tol), "y = A x")
!
! ! Test 2: y = A^T x
! CALL obj%Matvec(x=x, y=y, isTranspose=math%yes)
! CALL OK(ALL(ABS(y - atx) .LT. tol), "y = A^T x")
!
! ! Test 3: y = y + 2 A x
! y = 1.0_DFP
! CALL obj%Matvec(x=x, y=y, addContribution=math%yes, scale=2.0_DFP)
! CALL OK(ALL(ABS(y - (1.0_DFP + 2.0_DFP * ax)) .LT. tol), "y = y + 2 A x")
!
! ! Test 4: node fields, w = A u
! CALL u%Set(VALUE=1.0_DFP)
! CALL obj%Matvec(x=u, y=w)
! DO ii = 1, 6
!   CALL w%Get(VALUE=found(ii), globalNode=ii, islocal=math%yes)
! END DO
! CALL OK(ALL(ABS(found - ax) .LT. tol), "node fields: w = A u")
!
! CALL obj%DEALLOCATE()
! CALL w%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Matvec2]]

INTERFACE
  MODULE SUBROUTINE obj_Matvec1( &
    obj, x, y, isTranspose, addContribution, scale)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field A
    REAL(DFP), INTENT(IN) :: x(:)
    !! input vector, SIZE(x) = number of columns of op(A)
    REAL(DFP), INTENT(INOUT) :: y(:)
    !! output vector, SIZE(y) = number of rows of op(A)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! if true, use the transpose of A; default .FALSE.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, y = y + scale * op(A) x; default .FALSE.
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of op(A) x; default 1
  END SUBROUTINE obj_Matvec1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Matvec@MatVecMethods
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
! See also: [[obj_Matvec1]]

INTERFACE
  MODULE SUBROUTINE obj_Matvec2( &
    obj, x, y, isTranspose, addContribution, scale)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field A
    CLASS(AbstractNodeField_), INTENT(IN) :: x
    !! input node field
    CLASS(AbstractNodeField_), INTENT(INOUT) :: y
    !! output node field
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! if true, use the transpose of A; default .FALSE.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, y = y + scale * op(A) x; default .FALSE.
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of op(A) x; default 1
  END SUBROUTINE obj_Matvec2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     LUSOLVE@MatVecMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-18
! summary: Solve (LU) sol = rhs with Fortran arrays
!
! This routine solves (LU) sol = rhs, or (LU)^T sol = rhs when
! isTranspose is true. The LU factors are the incomplete LU
! preconditioner pmat, stored in Modified Sparse Row format (see
! [[MatrixFieldPrecondition_]]). The preconditioner must be set first.
! The array sol must be allocated by the caller with
! SIZE(sol) = SIZE(rhs) = number of rows. This routine is available for
! the NATIVE_SERIAL engine only.
!
! Examples
!
!```fortran
! CALL obj%ILUSOLVE(sol=sol, rhs=rhs)
!```
!
! This example still needs a matching test.
!
! See also: [[obj_ILUSOLVE2]]

INTERFACE
  MODULE SUBROUTINE obj_ILUSOLVE1(obj, sol, rhs, isTranspose)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                      LUSOLVE@MatVecMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-18
! summary: Solve (LU) sol = rhs with node fields
!
! This routine is [[obj_ILUSOLVE1]] with node fields for sol and rhs.
! It is available for the NATIVE_SERIAL engine only.
!
! Examples
!
!```fortran
! CALL obj%ILUSOLVE(sol=u, rhs=f)
!```
!
! This example still needs a matching test.
!
! See also: [[obj_ILUSOLVE1]]

INTERFACE
  MODULE SUBROUTINE obj_ILUSOLVE2(obj, sol, rhs, isTranspose)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                       GetPrecondition@PreconditionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-18
! summary: Get the preconditioning matrix
!
! This routine is under development and raises a "[WIP ERROR]" in debug
! mode.
!
! Examples
!
!```fortran
! CALL obj%GetPrecondition(Pmat=pmat)
!```
!
! This example still needs a matching test.

INTERFACE
  MODULE SUBROUTINE obj_GetPrecondition(obj, pmat)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: pmat
    !! preconditioning matrix
  END SUBROUTINE obj_GetPrecondition
END INTERFACE

!----------------------------------------------------------------------------
!                                    ReversePermutation@PreconditionMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-20
! summary: Undo the permutation of the preconditioner
!
! Incomplete LU preconditioners can permute rows or columns to improve
! the sparsity of the factors. After a column permutation the solution
! must be permuted back, and after a row permutation the rhs must be
! permuted back.
!
! This routine is under development and raises a "[WIP ERROR]" in debug
! mode.
!
! Examples
!
!```fortran
! CALL obj%ReversePermutation(rhs=f, sol=u)
!```
!
! This example still needs a matching test.

INTERFACE
  MODULE SUBROUTINE obj_ReversePermutation(obj, rhs, sol)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field with the preconditioner
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: rhs
    !! right hand side node field
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: sol
    !! solution node field
  END SUBROUTINE obj_ReversePermutation
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetDiagonal@DiagonalMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get the diagonal of the matrix
!
! This routine returns the diagonal entries of mat in diag. The array
! diag is reallocated to the number of rows.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE FieldOpt_Class, ONLY: TypeFieldOpt
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatrixField_Class, ONLY: MatrixField_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MatrixField_) :: obj
! REAL(DFP), ALLOCATABLE :: diag(:)
! REAL(DFP) :: val
! INTEGER(I4B) :: ii
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL obj%Initiate(obj2=u)
!
! ! Test 1: GetDiagonal
! CALL Assemble(obj)
! CALL obj%GetDiagonal(diag=diag)
! CALL OK(SIZE(diag) .EQ. 6, "GetDiagonal: size")
! CALL OK(ALL(ABS(diag - [1.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP, 2.0_DFP, &
!                         2.0_DFP]) .LT. tol), "GetDiagonal: values")
!
! ! Test 2: symmetric scaling by the own diagonal
! CALL obj%DiagonalScaling(side="BOTH")
! CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 1.0_DFP) .LT. tol, "BOTH: (5,6) = 2 / 2")
! CALL obj%Get(iNodeNum=1, jNodeNum=5, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 1.0_DFP / SQRT(2.0_DFP)) .LT. tol, &
!         "BOTH: (1,5) = 1 / sqrt(2)")
!
! ! Test 3: left scaling by a given diagonal
! CALL Assemble(obj)
! diag = [(REAL(ii, DFP), ii=1, 6)]
! CALL obj%DiagonalScaling(side="LEFT", diag=diag, OPERATOR="NONE")
! CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 2.0_DFP / 5.0_DFP) .LT. tol, "LEFT: (5,6) = 2 / 5")
! CALL obj%Get(iNodeNum=6, jNodeNum=5, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 2.0_DFP / 6.0_DFP) .LT. tol, "LEFT: (6,5) = 2 / 6")
!
! CALL obj%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
!
! CONTAINS
!
! SUBROUTINE Assemble(mat)
!   TYPE(MatrixField_), INTENT(INOUT) :: mat
!   REAL(DFP) :: ke(4, 4)
!   ke = 1.0_DFP
!   CALL mat%Set(VALUE=0.0_DFP)
!   CALL mat%Set(globalNode=[1, 5, 6, 4], islocal=math%yes, VALUE=ke, &
!                storageFMT=TypeFieldOpt%storageFormatDOF, &
!                addContribution=math%yes)
!   CALL mat%Set(globalNode=[5, 2, 3, 6], islocal=math%yes, VALUE=ke, &
!                storageFMT=TypeFieldOpt%storageFormatDOF, &
!                addContribution=math%yes)
! END SUBROUTINE Assemble
!
! END PROGRAM main
!```
!
! See also: [[obj_DiagonalScaling]]

INTERFACE
  MODULE SUBROUTINE obj_GetDiagonal(obj, diag)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: diag(:)
    !! diagonal entries, SIZE(diag) = number of rows
  END SUBROUTINE obj_GetDiagonal
END INTERFACE

!----------------------------------------------------------------------------
!                                       DiagonalScaling@DiagonalScalingMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Scale the matrix by a diagonal
!
! This routine scales mat by a diagonal d from the left, from the right,
! or from both sides. If diag is absent, d is the diagonal of mat. The
! operator is applied to d before scaling: "SQRT" uses SQRT(|d|) and
! "NONE" uses d.
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
! See also: [[obj_GetDiagonal]]

INTERFACE
  MODULE SUBROUTINE obj_DiagonalScaling(obj, side, diag, OPERATOR)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    CHARACTER(*), INTENT(IN) :: side
    !! "LEFT", "RIGHT" or "BOTH"
    REAL(DFP), OPTIONAL, INTENT(IN) :: diag(:)
    !! diagonal d, SIZE(diag) = number of rows; default is the
    !! diagonal of mat
    CHARACTER(*), OPTIONAL, INTENT(IN) :: OPERATOR
    !! "SQRT" or "NONE"; default "SQRT"
  END SUBROUTINE obj_DiagonalScaling
END INTERFACE

!----------------------------------------------------------------------------
!                                  SymSchurLargestEigenVal@SpectralMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-30
! summary: Largest eigenvalues of a symmetric Schur complement
!
! This function returns the nev largest eigenvalues of the symmetric
! Schur complement formed from obj%mat and B%mat. It calls
! SymSchurLargestEigenVal of [[CSRMatrix_]], which uses the Lanczos
! method (ARPACK). B must be a [[MatrixField_]].
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
  MODULE FUNCTION obj_SymSchurLargestEigenVal( &
    obj, B, nev, which, NCV, maxIter, tol) RESULT(ans)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! symmetric matrix field
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: B
    !! B matrix, a MatrixField_, possibly rectangle
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
!                                       SymLargestEigenVal@SpectralMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-30
! summary: Largest eigenvalues of a symmetric matrix
!
! This function returns the nev largest eigenvalues of the symmetric
! matrix mat. It calls SymLargestEigenVal of [[CSRMatrix_]], which uses
! the Lanczos method (ARPACK).
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
  MODULE FUNCTION obj_SymLargestEigenVal( &
    obj, nev, which, NCV, maxIter, tol) RESULT(ans)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                 ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Apply the Dirichlet boundary condition to the matrix
!
! If dbcPtrs is present, this routine stores dbcPtrs in obj, copies the
! columns dbcPtrs of mat into submat, and then sets the rows and the
! columns dbcPtrs of mat to zero and their diagonal entries to one.
!
! If dbcPtrs is absent, the routine uses the stored dbcPtrs. It copies
! the columns into submat again (for example after mat is assembled
! again) and then applies the same row and column operation.
!
! dbcPtrs are row (column) numbers of mat, that is degree of freedom
! indices in DOF_FMT.
!
! Examples
!
!```fortran
! CALL obj%ApplyDirichletBC(dbcPtrs=[1, 2])
! CALL obj%ApplyDirichletBCToRHS(x=x, y=y, scale=-1.0_DFP, &
!                                addContribution=math%yes)
! ! after a new assembly of obj
! CALL obj%ApplyDirichletBC()
!```
!
! Full example:
! src/modules/MatrixField/examples/ApplyDirichletBC_test_1.F90
!
! See also: [[obj_ApplyDirichletBCToRHS]], [[obj_IsSubmatInitiated]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbcPtrs)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcPtrs(:)
    !! Dirichlet degrees of freedom (columns of mat);
    !! default the stored dbcPtrs
  END SUBROUTINE obj_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                            ApplyDirichletBCtoRHS@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Apply the Dirichlet boundary condition to a rhs
!
! This routine computes y = scale * S x, or y = y + scale * S x when
! addContribution is true. S is submat, the Dirichlet columns of mat
! stored by [[obj_ApplyDirichletBC1]]. With x the field of prescribed
! values and scale = -1, this is the rhs correction -A(:, dbc) u_dbc.
!
! Examples
!
!```fortran
! CALL obj%ApplyDirichletBCToRHS(x=x, y=y, scale=-1.0_DFP, &
!                                addContribution=math%yes)
!```
!
! Full example:
! src/modules/MatrixField/examples/ApplyDirichletBC_test_1.F90
!
! See also: [[obj_ApplyDirichletBC1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBCToRHS( &
    obj, x, y, isTranspose, scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field with the Dirichlet submatrix
    CLASS(AbstractNodeField_), INTENT(IN) :: x
    !! node field of prescribed values
    CLASS(AbstractNodeField_), INTENT(INOUT) :: y
    !! rhs node field
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! if true, use the transpose of submat; default .FALSE.
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of S x; default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, y = y + scale * S x; default .FALSE.
  END SUBROUTINE obj_ApplyDirichletBCToRHS
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetDirichletBCSubMat@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Get the submatrix of the Dirichlet columns
!
! This routine is under development and raises a "[WIP ERROR]" in debug
! mode.
!
! Examples
!
!```fortran
! CALL obj%GetDirichletBCSubMat(submat=submat)
!```
!
! This example still needs a matching test.

INTERFACE
  MODULE SUBROUTINE obj_GetDirichletBCSubMat(obj, submat)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: submat
    !! submatrix of the Dirichlet columns
  END SUBROUTINE obj_GetDirichletBCSubMat
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set an element matrix
!
! This routine sets the block obj(globalNode, globalNode) to VALUE, or
! adds scale * VALUE when addContribution is true. This is how element
! matrices are assembled.
!
! VALUE is an element matrix of size (tdof * SIZE(globalNode)) by
! (tdof * SIZE(globalNode)), where tdof is the number of degrees of
! freedom per node. storageFMT is the storage format of VALUE (DOF_FMT
! or NODES_FMT); it is converted to the storage format of obj. This
! routine is not for a rectangle matrix.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE FieldOpt_Class, ONLY: TypeFieldOpt
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatrixField_Class, ONLY: MatrixField_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! INTEGER(I4B), PARAMETER :: elem1(4) = [1, 5, 6, 4]
! INTEGER(I4B), PARAMETER :: elem2(4) = [5, 2, 3, 6]
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MatrixField_) :: obj
! REAL(DFP) :: ke(4, 4), found(4, 4), val
! INTEGER(I4B) :: ii, jj, nrow, ncol, fmt
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL obj%Initiate(obj2=u)
! fmt = TypeFieldOpt%storageFormatDOF
!
! ! Test 1: set an element matrix and read it back
! DO jj = 1, 4
!   DO ii = 1, 4
!     ke(ii, jj) = REAL(10 * ii + jj, DFP)
!   END DO
! END DO
! CALL obj%Set(VALUE=0.0_DFP)
! CALL obj%Set(globalNode=elem1, islocal=math%yes, VALUE=ke, &
!              storageFMT=fmt)
! CALL obj%Get(globalNode=elem1, islocal=math%yes, VALUE=found, &
!              nrow=nrow, ncol=ncol, storageFMT=fmt)
! CALL OK(nrow .EQ. 4 .AND. ncol .EQ. 4, "Get: nrow, ncol")
! CALL OK(ALL(ABS(found - ke) .LT. tol), "Set element matrix")
! CALL obj%Get(iNodeNum=6, jNodeNum=4, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 34.0_DFP) .LT. tol, "Set element matrix: (6,4)")
!
! ! Test 2: assemble ke1 = 1 and 2 * ke2 with ke2 = 1
! CALL obj%Set(VALUE=0.0_DFP)
! ke = 1.0_DFP
! CALL obj%Set(globalNode=elem1, islocal=math%yes, VALUE=ke, &
!              storageFMT=fmt, addContribution=math%yes)
! CALL obj%Set(globalNode=elem2, islocal=math%yes, VALUE=ke, &
!              storageFMT=fmt, scale=2.0_DFP, addContribution=math%yes)
! CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 3.0_DFP) .LT. tol, "assemble: shared (5,6) = 3")
! CALL obj%Get(iNodeNum=1, jNodeNum=4, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 1.0_DFP) .LT. tol, "assemble: (1,4) = 1")
! CALL obj%Get(iNodeNum=2, jNodeNum=3, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 2.0_DFP) .LT. tol, "assemble: (2,3) = 2")
!
! CALL obj%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Get1]], [[obj_Set2]]

INTERFACE
  MODULE SUBROUTINE obj_Set1( &
    obj, globalNode, islocal, VALUE, storageFMT, scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set all entries, or a block of nodes, to a scalar
!
! If globalNode is absent, this routine sets all nonzero entries of mat
! to VALUE: obj = VALUE. If globalNode is present, it sets only the
! block obj(globalNode, globalNode). When addContribution is true the
! routine adds scale * VALUE instead. A block of nodes cannot be used
! for a rectangle matrix.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE MatrixField_Class, ONLY: MatrixField_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MatrixField_) :: obj
! REAL(DFP) :: val
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL obj%Initiate(obj2=u)
!
! ! Test 1: set all entries
! CALL obj%Set(VALUE=1.0_DFP)
! CALL obj%Get(iNodeNum=1, jNodeNum=5, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 1.0_DFP) .LT. tol, "Set all")
!
! ! Test 2: add to all entries
! CALL obj%Set(VALUE=1.0_DFP, scale=2.0_DFP, addContribution=math%yes)
! CALL obj%Get(iNodeNum=6, jNodeNum=6, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 3.0_DFP) .LT. tol, "Set all: add 2 * 1")
!
! ! Test 3: set a single entry
! CALL obj%Set(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=7.0_DFP)
! CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 7.0_DFP) .LT. tol, "Set single")
! CALL obj%Get(iNodeNum=6, jNodeNum=5, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 3.0_DFP) .LT. tol, "Set single: (6,5) unchanged")
!
! ! Test 4: add to a single entry
! CALL obj%Set(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=1.0_DFP, scale=3.0_DFP, &
!              addContribution=math%yes)
! CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 10.0_DFP) .LT. tol, "Set single: add 3 * 1")
!
! ! Test 5: set the block obj([1,5], [1,5])
! CALL obj%Set(VALUE=0.0_DFP, globalNode=[1, 5], islocal=math%yes)
! CALL obj%Get(iNodeNum=1, jNodeNum=5, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val) .LT. tol, "Set block: (1,5) = 0")
! CALL obj%Get(iNodeNum=1, jNodeNum=4, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 3.0_DFP) .LT. tol, "Set block: (1,4) unchanged")
!
! ! Test 6: add to the block obj([1,5], [1,5])
! CALL obj%Set(VALUE=2.0_DFP, globalNode=[1, 5], islocal=math%yes, &
!              scale=0.5_DFP, addContribution=math%yes)
! CALL obj%Get(iNodeNum=5, jNodeNum=5, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 1.0_DFP) .LT. tol, "Set block: add 0.5 * 2")
!
! CALL obj%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Set3]], [[obj_Set1]]

INTERFACE
  MODULE SUBROUTINE obj_Set2( &
    obj, VALUE, globalNode, islocal, scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set a single entry by node and dof
!
! This routine sets the entry in the row of (iNodeNum, idof) and the
! column of (jNodeNum, jdof) to VALUE, or adds scale * VALUE when
! addContribution is true. The entry must be in the sparsity pattern.
!
! Examples
!
!```fortran
! CALL obj%Set(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=7.0_DFP)
! CALL obj%Set(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=1.0_DFP, scale=3.0_DFP, &
!              addContribution=math%yes)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_1.F90
!
! See also: [[obj_Get2]], [[obj_Set2]]

INTERFACE
  MODULE SUBROUTINE obj_Set3( &
    obj, iNodeNum, jNodeNum, islocal, idof, jdof, VALUE, scale, &
    addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set the block of two physical variables
!
! This routine sets the block of the rows of physical variable ivar at
! nodes iNodeNum and the columns of physical variable jvar at nodes
! jNodeNum. VALUE is in DOF_FMT: the rows run over the nodes of the
! first degree of freedom, then the nodes of the second one, and so on.
! For iNodeNum = [1, 5] and two components the row order is (1, c1),
! (5, c1), (1, c2), (5, c2).
!
! Examples
!
!```fortran
! REAL(DFP) :: a(4, 4)
! CALL obj%Set(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=math%yes, &
!              ivar=1, jvar=1, VALUE=a)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[obj_Get3]], [[obj_Set5]]

INTERFACE
  MODULE SUBROUTINE obj_Set4( &
    obj, iNodeNum, jNodeNum, islocal, ivar, jvar, VALUE, scale, &
    addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! global or local node numbers of the rows
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(in) :: islocal
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
!                                                           Set@SetMethods
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
! REAL(DFP) :: b(2, 2)
! CALL obj%Set(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=math%yes, &
!              ivar=1, jvar=1, idof=2, jdof=2, VALUE=b)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[obj_Get4]], [[obj_Set4]]

INTERFACE
  MODULE SUBROUTINE obj_Set5( &
    obj, iNodeNum, jNodeNum, islocal, ivar, jvar, idof, jdof, VALUE, &
    scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! global or local node numbers of the rows
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(in) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of ivar for the rows, see
    !! GetIDOF(ispacecompo, itimecompo, tspacecompo)
    INTEGER(I4B), INTENT(IN) :: jdof
    !! degree of freedom of jvar for the columns, see
    !! GetIDOF(jspacecompo, jtimecompo, tspacecompo)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! block, SIZE(iNodeNum) by SIZE(jNodeNum)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of VALUE, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE; default .FALSE.
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
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
! CALL obj%Set(iNodeNum=5, jNodeNum=6, islocal=math%yes, ivar=1, &
!              jvar=1, idof=2, jdof=1, VALUE=4.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[obj_Get5]], [[obj_Set3]]

INTERFACE
  MODULE SUBROUTINE obj_Set6( &
    obj, iNodeNum, jNodeNum, islocal, ivar, jvar, idof, jdof, VALUE, scale, &
    addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    !! global or local node number of the row
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(in) :: islocal
    !! if true, node numbers are local
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the row
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the column
    INTEGER(I4B), INTENT(IN) :: idof
    !! degree of freedom of ivar for the row, see
    !! GetIDOF(ispacecompo, itimecompo, tspacecompo)
    INTEGER(I4B), INTENT(IN) :: jdof
    !! degree of freedom of jvar for the column, see
    !! GetIDOF(jspacecompo, jtimecompo, tspacecompo)
    REAL(DFP), INTENT(IN) :: VALUE
    !! value of the entry
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of VALUE, used only when addContribution is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE; default .FALSE.
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
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
! CALL obj%Set(iNodeNum=5, jNodeNum=6, islocal=math%yes, ivar=1, &
!              jvar=1, ispacecompo=2, itimecompo=1, jspacecompo=1, &
!              jtimecompo=1, VALUE=5.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[obj_Get6]], [[obj_Set6]]

INTERFACE
  MODULE SUBROUTINE obj_Set7( &
    obj, iNodeNum, jNodeNum, islocal, ivar, jvar, ispacecompo, itimecompo, &
    jspacecompo, jtimecompo, VALUE, scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    !! global or local node number of the row
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(in) :: islocal
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
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-25
! summary: Set a block of nodes and components to a scalar
!
! This routine sets all entries in the rows of (iNodeNum, ispacecompo,
! itimecompo) and the columns of (jNodeNum, jspacecompo, jtimecompo) to
! the scalar VALUE, or adds scale * VALUE when addContribution is true.
!
! Examples
!
!```fortran
! CALL obj%Set(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=math%yes, &
!              ivar=1, jvar=1, ispacecompo=1, itimecompo=1, &
!              jspacecompo=1, jtimecompo=1, VALUE=9.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[obj_Get7]], [[obj_Set9]], [[obj_Set10]]

INTERFACE
  MODULE SUBROUTINE obj_Set8( &
    obj, iNodeNum, jNodeNum, islocal, ivar, jvar, ispacecompo, itimecompo, &
    jspacecompo, jtimecompo, VALUE, scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! global or local node numbers of the rows
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(in) :: islocal
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
!                                                           Set@SetMethods
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
! CALL obj%Set(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=math%yes, &
!              ivar=1, jvar=1, ispacecompo=1, itimecompo=[1], &
!              jspacecompo=1, jtimecompo=[1], VALUE=6.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[obj_Set8]]

INTERFACE
  MODULE SUBROUTINE obj_Set9( &
    obj, iNodeNum, jNodeNum, islocal, ivar, jvar, ispacecompo, itimecompo, &
    jspacecompo, jtimecompo, VALUE, scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! global or local node numbers of the rows
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(in) :: islocal
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
!                                                           Set@SetMethods
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
! CALL obj%Set(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=math%yes, &
!              ivar=1, jvar=1, ispacecompo=[1, 2], itimecompo=1, &
!              jspacecompo=[1, 2], jtimecompo=1, VALUE=8.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[obj_Set8]]

INTERFACE
  MODULE SUBROUTINE obj_Set10( &
    obj, iNodeNum, jNodeNum, islocal, ivar, jvar, ispacecompo, itimecompo, &
    jspacecompo, jtimecompo, VALUE, scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! global or local node numbers of the rows
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(in) :: islocal
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
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-16
! summary: Set or add another matrix field
!
! This routine sets obj%mat = scale * VALUE%mat, or obj%mat = obj%mat +
! scale * VALUE%mat when addContribution is true. VALUE must be a
! [[MatrixField_]] with the same sparsity as obj.
!
! Changes:
!   - 2026-11-09: scale is applied in set mode too; default scale is 1.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE MatrixField_Class, ONLY: MatrixField_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MatrixField_) :: obj, obj2
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL obj%Initiate(obj2=u)
! CALL obj2%Initiate(obj2=obj)
!
! CALL obj%Set(VALUE=1.0_DFP)
! CALL obj2%Set(VALUE=2.0_DFP)
!
! ! Test 1: obj = obj + 3 * obj2
! CALL obj%Set(VALUE=obj2, scale=3.0_DFP, addContribution=math%yes)
! CALL OK(ALL(ABS(obj%mat%A - 7.0_DFP) .LT. tol), "add 3 * obj2")
!
! ! Test 2: obj = obj2
! CALL obj%Set(VALUE=obj2)
! CALL OK(ALL(ABS(obj%mat%A - 2.0_DFP) .LT. tol), "set obj2")
!
! CALL obj2%DEALLOCATE()
! CALL obj%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Set2]]

INTERFACE
  MODULE SUBROUTINE obj_Set11(obj, VALUE, scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: VALUE
    !! MatrixField_ with the same sparsity as obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale of VALUE; default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, obj = obj + scale * VALUE; default .FALSE.
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-16
! summary: Set a time block of a space-time matrix into obj
!
! obj is a space matrix and VALUE is a space-time matrix with the same
! space components. This routine takes the block (itimecompo,
! jtimecompo) of VALUE and sets it in obj, or adds scale times it when
! addContribution is true. It is not for a rectangle matrix.
!
! This routine is under development and raises a "[WIP ERROR]" in debug
! mode.
!
! Examples
!
!```fortran
! CALL obj%SetFromSTMatrix(VALUE=stmat, itimecompo=1, jtimecompo=2, &
!                          scale=1.0_DFP, addContribution=math%no)
!```
!
! This example still needs a matching test.
!
! See also: [[obj_SetToSTMatrix]]

INTERFACE
  MODULE SUBROUTINE obj_SetFromSTMatrix( &
    obj, VALUE, itimecompo, jtimecompo, scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! space matrix field
    CLASS(MatrixField_), INTENT(INOUT) :: VALUE
    !! space-time matrix field
    INTEGER(I4B), INTENT(IN) :: itimecompo
    !! time component of the rows of the block in VALUE
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    !! time component of the columns of the block in VALUE
    REAL(DFP), INTENT(IN) :: scale
    !! scale of the block
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add scale times the block to obj
  END SUBROUTINE obj_SetFromSTMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-16
! summary: Set a space matrix into a time block of obj
!
! obj is a space-time matrix and VALUE is a space matrix with the same
! space components. This routine sets VALUE in the block (itimecompo,
! jtimecompo) of obj, or adds scale * VALUE when addContribution is
! true. Both matrices must use DOF_FMT. It is not for a rectangle
! matrix.
!
! Examples
!
!```fortran
! CALL stmat%SetToSTMatrix(VALUE=obj, itimecompo=1, jtimecompo=2, &
!                          scale=1.0_DFP, addContribution=math%yes)
!```
!
! This example still needs a matching test.
!
! See also: [[obj_SetFromSTMatrix]]

INTERFACE
  MODULE SUBROUTINE obj_SetToSTMatrix( &
    obj, VALUE, itimecompo, jtimecompo, scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! space-time matrix field
    CLASS(MatrixField_), INTENT(INOUT) :: VALUE
    !! space matrix field
    INTEGER(I4B), INTENT(IN) :: itimecompo
    !! time component of the rows of the block in obj
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    !! time component of the columns of the block in obj
    REAL(DFP), INTENT(IN) :: scale
    !! scale of VALUE
    LOGICAL(LGT), INTENT(IN) :: addContribution
    !! if true, add scale * VALUE to the block
  END SUBROUTINE obj_SetToSTMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
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
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatrixField_Class, ONLY: MatrixField_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MatrixField_) :: obj
! REAL(DFP) :: row(6), vec(6), found(6)
! INTEGER(I4B) :: ii
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL obj%Initiate(obj2=u)
! CALL obj%Set(VALUE=1.0_DFP)
! vec = [(REAL(ii, DFP), ii=1, 6)]
!
! ! Test 1: set a row to a scalar
! CALL obj%SetRow(globalNode=1, islocal=math%yes, idof=1, scalarVal=2.0_DFP)
! CALL obj%GetRow(globalNode=1, islocal=math%yes, idof=1, VALUE=row)
! CALL OK(ALL(ABS(row - [2.0_DFP, 0.0_DFP, 0.0_DFP, 2.0_DFP, 2.0_DFP, &
!                        2.0_DFP]) .LT. tol), "SetRow scalarVal")
!
! ! Test 2: set a row from a vector
! CALL obj%SetRow(globalNode=5, islocal=math%yes, idof=1, vecVal=vec)
! CALL obj%GetRow(globalNode=5, islocal=math%yes, idof=1, VALUE=row)
! CALL OK(ALL(ABS(row - vec) .LT. tol), "SetRow vecVal: full row")
! CALL obj%SetRow(globalNode=2, islocal=math%yes, idof=1, vecVal=vec)
! CALL obj%GetRow(globalNode=2, islocal=math%yes, idof=1, VALUE=row)
! CALL OK(ALL(ABS(row - [0.0_DFP, 2.0_DFP, 3.0_DFP, 0.0_DFP, 5.0_DFP, &
!                        6.0_DFP]) .LT. tol), "SetRow vecVal: sparsity")
!
! ! Test 3: add scale * row to VALUE
! row = 1.0_DFP
! CALL obj%GetRow(globalNode=5, islocal=math%yes, idof=1, VALUE=row, &
!                 scale=2.0_DFP, addContribution=math%yes)
! CALL OK(ALL(ABS(row - (1.0_DFP + 2.0_DFP * vec)) .LT. tol), &
!         "GetRow: add 2 * row")
!
! ! Test 4: set a row from a node field
! DO ii = 1, 6
!   CALL u%Set(globalNode=ii, islocal=math%yes, VALUE=vec(ii))
! END DO
! CALL obj%SetRow(globalNode=6, islocal=math%yes, idof=1, nodeFieldVal=u)
! CALL obj%GetRow(globalNode=6, islocal=math%yes, idof=1, VALUE=row)
! CALL OK(ALL(ABS(row - vec) .LT. tol), "SetRow nodeFieldVal")
!
! ! Test 5: get a row into a node field
! CALL obj%GetRow(globalNode=1, islocal=math%yes, idof=1, nodeFieldVal=u)
! DO ii = 1, 6
!   CALL u%Get(VALUE=found(ii), globalNode=ii, islocal=math%yes)
! END DO
! CALL OK(ALL(ABS(found - [2.0_DFP, 0.0_DFP, 0.0_DFP, 2.0_DFP, 2.0_DFP, &
!                          2.0_DFP]) .LT. tol), "GetRow nodeFieldVal")
!
! CALL obj%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetRow1]], [[obj_SetColumn1]]

INTERFACE
  MODULE SUBROUTINE obj_SetRow1( &
    obj, globalNode, islocal, idof, scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                          SetRow@SetMethod
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
! CALL obj%SetRow(globalNode=5, islocal=math%yes, ivar=1, idof=2, &
!                 scalarVal=3.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[obj_SetRow1]], [[obj_GetRow2]]

INTERFACE
  MODULE SUBROUTINE obj_SetRow2( &
    obj, globalNode, islocal, ivar, idof, scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                          SetRow@SetMethod
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
! CALL obj%SetRow(globalNode=1, islocal=math%yes, ivar=1, &
!                 spacecompo=1, timecompo=1, scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[obj_SetRow1]], [[obj_GetRow3]]

INTERFACE
  MODULE SUBROUTINE obj_SetRow3( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, scalarVal, &
    vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                          SetRow@SetMethod
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
! CALL obj%SetRow(globalNode=1, islocal=math%yes, ivar=1, &
!                 spacecompo=1, timecompo=[1], scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[obj_SetRow3]], [[obj_GetRow4]]

INTERFACE
  MODULE SUBROUTINE obj_SetRow4( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, scalarVal, &
    vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                          SetRow@SetMethod
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
! CALL obj%SetRow(globalNode=5, islocal=math%yes, ivar=1, &
!                 spacecompo=[1, 2], timecompo=1, scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[obj_SetRow3]], [[obj_GetRow5]]

INTERFACE
  MODULE SUBROUTINE obj_SetRow5( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, scalarVal, &
    vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                          SetRow@SetMethod
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
! CALL obj%SetRow(globalNode=[5, 6], islocal=math%yes, ivar=1, &
!                 spacecompo=1, timecompo=[1], vecVal=vec)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[obj_SetRow4]], [[obj_GetRow6]]

INTERFACE
  MODULE SUBROUTINE obj_SetRow6( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, scalarVal, &
    vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                          SetRow@SetMethod
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
! CALL obj%SetRow(globalNode=[5, 6], islocal=math%yes, ivar=1, &
!                 spacecompo=[1, 2], timecompo=1, scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[obj_SetRow5]], [[obj_GetRow7]]

INTERFACE
  MODULE SUBROUTINE obj_SetRow7( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, scalarVal, &
    vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                       SetColumn@SetMethod
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
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MatrixField_Class, ONLY: MatrixField_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MatrixField_) :: obj
! REAL(DFP) :: col(6), vec(6), found(6)
! INTEGER(I4B) :: ii
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL obj%Initiate(obj2=u)
! CALL obj%Set(VALUE=1.0_DFP)
! vec = [(REAL(ii, DFP), ii=1, 6)]
!
! ! Test 1: set a column to a scalar
! CALL obj%SetColumn(globalNode=1, islocal=math%yes, idof=1, &
!                    scalarVal=2.0_DFP)
! CALL obj%GetColumn(globalNode=1, islocal=math%yes, idof=1, VALUE=col)
! CALL OK(ALL(ABS(col - [2.0_DFP, 0.0_DFP, 0.0_DFP, 2.0_DFP, 2.0_DFP, &
!                        2.0_DFP]) .LT. tol), "SetColumn scalarVal")
!
! ! Test 2: set a column from a vector
! CALL obj%SetColumn(globalNode=5, islocal=math%yes, idof=1, vecVal=vec)
! CALL obj%GetColumn(globalNode=5, islocal=math%yes, idof=1, VALUE=col)
! CALL OK(ALL(ABS(col - vec) .LT. tol), "SetColumn vecVal: full column")
! CALL obj%SetColumn(globalNode=2, islocal=math%yes, idof=1, vecVal=vec)
! CALL obj%GetColumn(globalNode=2, islocal=math%yes, idof=1, VALUE=col)
! CALL OK(ALL(ABS(col - [0.0_DFP, 2.0_DFP, 3.0_DFP, 0.0_DFP, 5.0_DFP, &
!                        6.0_DFP]) .LT. tol), "SetColumn vecVal: sparsity")
!
! ! Test 3: add scale * column to VALUE
! col = 1.0_DFP
! CALL obj%GetColumn(globalNode=5, islocal=math%yes, idof=1, VALUE=col, &
!                 scale=2.0_DFP, addContribution=math%yes)
! CALL OK(ALL(ABS(col - (1.0_DFP + 2.0_DFP * vec)) .LT. tol), &
!         "GetColumn: add 2 * column")
!
! ! Test 4: set a column from a node field
! DO ii = 1, 6
!   CALL u%Set(globalNode=ii, islocal=math%yes, VALUE=vec(ii))
! END DO
! CALL obj%SetColumn(globalNode=6, islocal=math%yes, idof=1, nodeFieldVal=u)
! CALL obj%GetColumn(globalNode=6, islocal=math%yes, idof=1, VALUE=col)
! CALL OK(ALL(ABS(col - vec) .LT. tol), "SetColumn nodeFieldVal")
!
! ! Test 5: get a column into a node field
! CALL obj%GetColumn(globalNode=1, islocal=math%yes, idof=1, nodeFieldVal=u)
! DO ii = 1, 6
!   CALL u%Get(VALUE=found(ii), globalNode=ii, islocal=math%yes)
! END DO
! CALL OK(ALL(ABS(found - [2.0_DFP, 0.0_DFP, 0.0_DFP, 2.0_DFP, 2.0_DFP, &
!                          2.0_DFP]) .LT. tol), "GetColumn nodeFieldVal")
!
! CALL obj%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetColumn1]], [[obj_SetRow1]]

INTERFACE
  MODULE SUBROUTINE obj_SetColumn1( &
    obj, globalNode, islocal, idof, scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                       SetColumn@SetMethod
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
! CALL obj%SetColumn(globalNode=5, islocal=math%yes, ivar=1, idof=2, &
!                    scalarVal=3.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[obj_SetColumn1]], [[obj_GetColumn2]]

INTERFACE
  MODULE SUBROUTINE obj_SetColumn2( &
    obj, globalNode, islocal, ivar, idof, scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                       SetColumn@SetMethod
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
! CALL obj%SetColumn(globalNode=1, islocal=math%yes, ivar=1, &
!                    spacecompo=1, timecompo=1, scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[obj_SetColumn1]], [[obj_GetColumn3]]

INTERFACE
  MODULE SUBROUTINE obj_SetColumn3( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, scalarVal, &
    vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                       SetColumn@SetMethod
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
! CALL obj%SetColumn(globalNode=1, islocal=math%yes, ivar=1, &
!                    spacecompo=1, timecompo=[1], scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[obj_SetColumn3]], [[obj_GetColumn4]]

INTERFACE
  MODULE SUBROUTINE obj_SetColumn4( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, scalarVal, &
    vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                       SetColumn@SetMethod
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
! CALL obj%SetColumn(globalNode=5, islocal=math%yes, ivar=1, &
!                    spacecompo=[1, 2], timecompo=1, scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[obj_SetColumn3]], [[obj_GetColumn5]]

INTERFACE
  MODULE SUBROUTINE obj_SetColumn5( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, scalarVal, &
    vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                       SetColumn@SetMethod
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
! CALL obj%SetColumn(globalNode=[5, 6], islocal=math%yes, ivar=1, &
!                    spacecompo=1, timecompo=[1], vecVal=vec)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[obj_SetColumn4]], [[obj_GetColumn6]]

INTERFACE
  MODULE SUBROUTINE obj_SetColumn6( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, scalarVal, &
    vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                       SetColumn@SetMethod
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
! CALL obj%SetColumn(globalNode=[5, 6], islocal=math%yes, ivar=1, &
!                    spacecompo=[1, 2], timecompo=1, scalarVal=1.0_DFP)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[obj_SetColumn5]], [[obj_GetColumn7]]

INTERFACE
  MODULE SUBROUTINE obj_SetColumn7( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, scalarVal, &
    vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
!                                                           Get@GetMethods
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
! REAL(DFP) :: found(4, 4)
! INTEGER(I4B) :: nrow, ncol
! CALL obj%Get(globalNode=[1, 5, 6, 4], islocal=math%yes, VALUE=found, &
!              nrow=nrow, ncol=ncol, &
!              storageFMT=TypeFieldOpt%storageFormatDOF)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_2.F90
!
! See also: [[obj_Set1]]

INTERFACE
  MODULE SUBROUTINE obj_Get1( &
    obj, globalNode, islocal, VALUE, nrow, ncol, storageFMT)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                           Get@GetMethods
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
! CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_1.F90
!
! See also: [[obj_Set3]]

INTERFACE
  MODULE SUBROUTINE obj_Get2( &
    obj, iNodeNum, jNodeNum, islocal, idof, jdof, VALUE)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                           Get@GetMethods
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
! REAL(DFP) :: found(4, 4)
! INTEGER(I4B) :: nrow, ncol
! CALL obj%Get(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=math%yes, &
!              ivar=1, jvar=1, VALUE=found, nrow=nrow, ncol=ncol)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[obj_Set4]]

INTERFACE
  MODULE SUBROUTINE obj_Get3( &
    obj, iNodeNum, jNodeNum, islocal, ivar, jvar, VALUE, nrow, ncol)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                           Get@GetMethods
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
! REAL(DFP) :: found(2, 2)
! INTEGER(I4B) :: nrow, ncol
! CALL obj%Get(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=math%yes, &
!              ivar=1, jvar=1, idof=2, jdof=2, VALUE=found, nrow=nrow, &
!              ncol=ncol)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[obj_Set5]]

INTERFACE
  MODULE SUBROUTINE obj_Get4( &
    obj, iNodeNum, jNodeNum, islocal, ivar, jvar, idof, jdof, VALUE, &
    nrow, ncol)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                           Get@GetMethods
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
! CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, ivar=1, &
!              jvar=1, idof=2, jdof=1, VALUE=val)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[obj_Set6]]

INTERFACE
  MODULE SUBROUTINE obj_Get5( &
    obj, iNodeNum, jNodeNum, islocal, ivar, jvar, idof, jdof, VALUE)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                           Get@GetMethods
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
! CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, ivar=1, &
!              jvar=1, ispacecompo=2, itimecompo=1, jspacecompo=1, &
!              jtimecompo=1, VALUE=val)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[obj_Set7]]

INTERFACE
  MODULE SUBROUTINE obj_Get6( &
    obj, iNodeNum, jNodeNum, islocal, ivar, jvar, ispacecompo, itimecompo, &
    jspacecompo, jtimecompo, VALUE)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                           Get@GetMethods
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
! REAL(DFP) :: found(2, 2)
! INTEGER(I4B) :: nrow, ncol
! CALL obj%Get(iNodeNum=[1, 5], jNodeNum=[1, 5], islocal=math%yes, &
!              ivar=1, jvar=1, ispacecompo=1, itimecompo=1, &
!              jspacecompo=1, jtimecompo=1, VALUE=found, nrow=nrow, &
!              ncol=ncol)
!```
!
! Full example: src/modules/MatrixField/examples/Set_test_3.F90
!
! See also: [[obj_Set8]]

INTERFACE
  MODULE SUBROUTINE obj_Get7( &
    obj, iNodeNum, jNodeNum, islocal, ivar, jvar, ispacecompo, itimecompo, &
    jspacecompo, jtimecompo, VALUE, nrow, ncol)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                          GetRow@getMethods
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
! CALL obj%GetRow(globalNode=1, islocal=math%yes, idof=1, VALUE=row)
! CALL obj%GetRow(globalNode=1, islocal=math%yes, idof=1, VALUE=row, &
!                 scale=2.0_DFP, addContribution=math%yes)
! CALL obj%GetRow(globalNode=1, islocal=math%yes, idof=1, &
!                 nodeFieldVal=u)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_1.F90
!
! See also: [[obj_SetRow1]], [[obj_GetColumn1]]

INTERFACE
  MODULE SUBROUTINE obj_GetRow1( &
    obj, globalNode, islocal, idof, VALUE, nodeFieldVal, scale, &
    addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                          GetRow@getMethods
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
! CALL obj%GetRow(globalNode=5, islocal=math%yes, ivar=1, idof=2, &
!                 VALUE=row)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[obj_GetRow1]], [[obj_SetRow2]]

INTERFACE
  MODULE SUBROUTINE obj_GetRow2( &
    obj, globalNode, islocal, ivar, idof, VALUE, nodeFieldVal, scale, &
    addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                          GetRow@getMethod
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
! CALL obj%GetRow(globalNode=1, islocal=math%yes, ivar=1, &
!                 spacecompo=1, timecompo=1, VALUE=row)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[obj_GetRow1]], [[obj_SetRow3]]

INTERFACE
  MODULE SUBROUTINE obj_GetRow3( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, VALUE, &
    nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                          GetRow@getMethod
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
! CALL obj%GetRow(globalNode=1, islocal=math%yes, ivar=1, &
!                 spacecompo=1, timecompo=[1], VALUE=row, &
!                 addContribution=math%yes)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[obj_GetRow3]], [[obj_SetRow4]]

INTERFACE
  MODULE SUBROUTINE obj_GetRow4( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, VALUE, &
    nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                          GetRow@getMethod
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
! CALL obj%GetRow(globalNode=5, islocal=math%yes, ivar=1, &
!                 spacecompo=[1, 2], timecompo=1, VALUE=row, &
!                 addContribution=math%yes)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[obj_GetRow3]], [[obj_SetRow5]]

INTERFACE
  MODULE SUBROUTINE obj_GetRow5( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, VALUE, &
    nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                          GetRow@getMethod
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
! CALL obj%GetRow(globalNode=[5, 6], islocal=math%yes, ivar=1, &
!                 spacecompo=1, timecompo=[1], VALUE=row, &
!                 addContribution=math%yes)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[obj_GetRow4]], [[obj_SetRow6]]

INTERFACE
  MODULE SUBROUTINE obj_GetRow6( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, VALUE, &
    nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                          GetRow@getMethod
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
! CALL obj%GetRow(globalNode=[5, 6], islocal=math%yes, ivar=1, &
!                 spacecompo=[1, 2], timecompo=1, VALUE=row, &
!                 addContribution=math%yes)
!```
!
! Full example: src/modules/MatrixField/examples/SetRow_test_2.F90
!
! See also: [[obj_GetRow5]], [[obj_SetRow7]]

INTERFACE
  MODULE SUBROUTINE obj_GetRow7( &
    obj, globalNode, islocal, ivar, spacecompo, timecompo, VALUE, &
    nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                       GetColumn@getMethod
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
! CALL obj%GetColumn(globalNode=1, islocal=math%yes, idof=1, VALUE=col)
! CALL obj%GetColumn(globalNode=1, islocal=math%yes, idof=1, &
!                    VALUE=col, scale=2.0_DFP, addContribution=math%yes)
! CALL obj%GetColumn(globalNode=1, islocal=math%yes, idof=1, &
!                    nodeFieldVal=u)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_1.F90
!
! See also: [[obj_SetColumn1]], [[obj_GetRow1]]

INTERFACE
  MODULE SUBROUTINE obj_GetColumn1( &
    obj, globalNode, islocal, idof, VALUE, nodeFieldVal, scale, &
    addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                       GetColumn@getMethod
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
! CALL obj%GetColumn(globalNode=5, islocal=math%yes, ivar=1, idof=2, &
!                    VALUE=col)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[obj_GetColumn1]], [[obj_SetColumn2]]

INTERFACE
  MODULE SUBROUTINE obj_GetColumn2( &
    obj, globalNode, islocal, ivar, idof, VALUE, nodeFieldVal, scale, &
    addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
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
!                                                       GetColumn@getMethod
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
! CALL obj%GetColumn(globalNode=1, islocal=math%yes, ivar=1, &
!                    spaceCompo=1, timeCompo=1, VALUE=col)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[obj_GetColumn1]], [[obj_SetColumn3]]

INTERFACE
  MODULE SUBROUTINE obj_GetColumn3( &
    obj, globalNode, islocal, ivar, spaceCompo, timeCompo, VALUE, &
    nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the column
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the column
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component of the column
    INTEGER(I4B), INTENT(IN) :: timeCompo
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
!                                                       GetColumn@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get columns of a node for several time components
!
! This routine gets the columns of (globalNode, spaceCompo,
! timeCompo(:)) of the physical variable ivar. With addContribution =
! .TRUE. the columns are summed into the output; without it only the
! last column is kept.
!
! Examples
!
!```fortran
! col = 0.0_DFP
! CALL obj%GetColumn(globalNode=1, islocal=math%yes, ivar=1, &
!                    spaceCompo=1, timeCompo=[1], VALUE=col, &
!                    addContribution=math%yes)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[obj_GetColumn3]], [[obj_SetColumn4]]

INTERFACE
  MODULE SUBROUTINE obj_GetColumn4( &
    obj, globalNode, islocal, ivar, spaceCompo, timeCompo, VALUE, &
    nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component of the columns
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
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
!                                                       GetColumn@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get columns of a node for several space components
!
! This routine gets the columns of (globalNode, spaceCompo(:),
! timeCompo) of the physical variable ivar. With addContribution =
! .TRUE. the columns are summed into the output; without it only the
! last column is kept.
!
! Examples
!
!```fortran
! col = 0.0_DFP
! CALL obj%GetColumn(globalNode=5, islocal=math%yes, ivar=1, &
!                    spaceCompo=[1, 2], timeCompo=1, VALUE=col, &
!                    addContribution=math%yes)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[obj_GetColumn3]], [[obj_SetColumn5]]

INTERFACE
  MODULE SUBROUTINE obj_GetColumn5( &
    obj, globalNode, islocal, ivar, spaceCompo, timeCompo, VALUE, &
    nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! space components of the columns
    INTEGER(I4B), INTENT(IN) :: timeCompo
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
!                                                       GetColumn@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get columns of several nodes and time components
!
! This routine gets the columns of (globalNode(:), spaceCompo,
! timeCompo(:)) of the physical variable ivar. With addContribution =
! .TRUE. the columns are summed into the output; without it only the
! last column is kept.
!
! Examples
!
!```fortran
! col = 0.0_DFP
! CALL obj%GetColumn(globalNode=[5, 6], islocal=math%yes, ivar=1, &
!                    spaceCompo=1, timeCompo=[1], VALUE=col, &
!                    addContribution=math%yes)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[obj_GetColumn4]], [[obj_SetColumn6]]

INTERFACE
  MODULE SUBROUTINE obj_GetColumn6( &
    obj, globalNode, islocal, ivar, spaceCompo, timeCompo, VALUE, &
    nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode are local node numbers
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component of the columns
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
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
!                                                       GetColumn@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-24
! summary: Get columns of several nodes and space components
!
! This routine gets the columns of (globalNode(:), spaceCompo(:),
! timeCompo) of the physical variable ivar. With addContribution =
! .TRUE. the columns are summed into the output; without it only the
! last column is kept.
!
! Examples
!
!```fortran
! col = 0.0_DFP
! CALL obj%GetColumn(globalNode=[5, 6], islocal=math%yes, ivar=1, &
!                    spaceCompo=[1, 2], timeCompo=1, VALUE=col, &
!                    addContribution=math%yes)
!```
!
! Full example: src/modules/MatrixField/examples/SetColumn_test_2.F90
!
! See also: [[obj_GetColumn5]], [[obj_SetColumn7]]

INTERFACE
  MODULE SUBROUTINE obj_GetColumn7( &
    obj, globalNode, islocal, ivar, spaceCompo, timeCompo, VALUE, &
    nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    !! matrix field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers of the columns
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalNode are local node numbers
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the columns
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! space components of the columns
    INTEGER(I4B), INTENT(IN) :: timeCompo
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
!                                                         Scal@UnaryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary: Scale the matrix field
!
! This routine multiplies every nonzero entry of mat by a:
! obj = a * obj.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE MatrixField_Class, ONLY: MatrixField_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MatrixField_) :: obj
! REAL(DFP) :: val
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL obj%Initiate(obj2=u)
!
! CALL obj%Set(VALUE=2.0_DFP)
! CALL obj%Scal(a=3.0_DFP)
! CALL OK(ALL(ABS(obj%mat%A - 6.0_DFP) .LT. tol), "Scal: all entries")
! CALL obj%Get(iNodeNum=5, jNodeNum=6, islocal=math%yes, idof=1, &
!              jdof=1, VALUE=val)
! CALL OK(ABS(val - 6.0_DFP) .LT. tol, "Scal: (5,6) = 3 * 2")
!
! CALL obj%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_Scal(obj, a)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    !! matrix field
    REAL(DFP), INTENT(IN) :: a
    !! scale factor
  END SUBROUTINE obj_Scal
END INTERFACE

END MODULE MatrixField_Class
