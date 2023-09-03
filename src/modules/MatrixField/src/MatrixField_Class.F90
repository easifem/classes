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

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: This module defines [[MatrixField_]] class
!
!# Introduction
!
! - This module defines [[MatrixField_]] class
! - It is designed for handling the tangent matrix in FEM
!
!@note
! [[MatrixField_]] uses `NATIVE_SERIAL` engine for handling the
! global tangent matrices.
!@endnote

MODULE MatrixField_Class
USE GlobalData
USE BaseType
USE String_Class, ONLY: String
USE FPL, ONLY: ParameterList_
USE FPL_Method
USE HDF5File_Class
USE ExceptionHandler_Class, ONLY: e
USE AbstractField_Class
USE AbstractNodeField_Class
USE AbstractMatrixField_Class
USE Domain_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PRIVATE, PARAMETER :: modName = "MatrixField_Class"
CHARACTER(*), PRIVATE, PARAMETER :: myPrefix = "MatrixField"
INTEGER(I4B), PRIVATE, PARAMETER :: IPAR_LENGTH = 14
INTEGER(I4B), PRIVATE, PARAMETER :: FPAR_LENGTH = 14

!----------------------------------------------------------------------------
!                                                               MSRSparsity_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 June 2021
! summary: User data type for handling the preconditioning of [[MatrixField_]]
!
!# Introduction
!
! This is a data type for storing the the precondition matrix.
! The storage pattern of the precondition matrix depends upon the type of
! preconditioning. For example, ILU type preconditioners are stored in
! modified sparse row by sparseKit library. In this way, storage also depends
! upon the linear solver library. That is why it is better to hide the
! preconditioner from user. More details about the MSR format is given as
! follows.
!
! We have used Modified Sparse Row, which is used by Sparsekit lib to store
! the precondition matrix, this data type is meant to be used internally only.
! The precondition matrix that will be stored inside it is mainly ILUT.
! User should not worry about this data type.
!
! ```fortran
! INTEGER :: JA( : )
! REAL :: A( : )
! ```
!
! - `A(1:n)` contains the diagonal of the matrix.
! - `A(n+2:nnz)` contains the nondiagonal elements of the matrix, stored
! ROWWISE.
! - `JA(n+2:nnz)`  contains their column indices
! - `JA(1:n+1)` Contains the pointer array for the nondiagonal, elements in
! A(n+1:nnz) and JA(n+2:nnz), i.e., for `i .LE. n+1` `JA(i)` points to
! beginning of row i in arrays A, JA.
! - Here, nnz = number of nonzero elements+1

TYPE :: MatrixFieldPrecondition_
  LOGICAL(LGT) :: isInitiated = .FALSE.
  INTEGER(I4B) :: PmatName = 0
  INTEGER(I4B) :: nnz = 0
  INTEGER(I4B) :: ncol = 0
  INTEGER(I4B) :: nrow = 0
  INTEGER(I4B) :: lfil = 0
  INTEGER(I4B) :: mbloc = 0
  REAL(DFP) :: alpha = 0.0_DFP
  REAL(DFP) :: droptol = 0.0_DFP
  REAL(DFP) :: permtol = 0.0_DFP
  REAL(DFP), ALLOCATABLE :: A(:)
  INTEGER(I4B), ALLOCATABLE :: JA(:)
  INTEGER(I4B), ALLOCATABLE :: IA(:)
  INTEGER(I4B), ALLOCATABLE :: JU(:)
  INTEGER(I4B), ALLOCATABLE :: IPERM(:)
  INTEGER(I4B), ALLOCATABLE :: LEVS(:)
END TYPE MatrixFieldPrecondition_

!----------------------------------------------------------------------------
!                                                              MatrixField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: This is native implementation of finite element tangent matrices.
!
!{!pages/MatrixField_.md!}

TYPE, EXTENDS(AbstractMatrixField_) :: MatrixField_
  LOGICAL(LGT) :: isRectangle = .FALSE.
  TYPE(CSRMatrix_) :: mat
  TYPE(MatrixFieldPrecondition_) :: Pmat
CONTAINS
  PRIVATE
  !
  ! @ConstructorMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    & MatrixFieldCheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => mField_Initiate1
  !! Initiate from the parameter list
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => mField_Initiate2
  !! Initiate by copying other object
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => mField_Initiate3
  !! Initiate for block matrices
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => mField_Deallocate
  !! Deallocate the field
  FINAL :: mField_Final
  !! Deallocate the field
  !
  ! @IOMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: Display => mField_Display
  !! Display the field
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => mField_Import
  !! Import from hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: ImportPmat => mField_ImportPmat
  !! Import from hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => mField_Export
  !! export matrix field in hdf5file_
  PROCEDURE, PUBLIC, PASS(obj) :: ExportPmat => mField_ExportPmat
  !! export PMat
  PROCEDURE, PUBLIC, PASS(obj) :: SPY => mField_SPY
  !! Get the sparsity pattern in various file formats
  !
  ! @GetMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: Size => mField_Size
  !! Returns the size of the matrix
  PROCEDURE, PUBLIC, PASS(obj) :: Shape => mField_Shape
  !! Returns the shape of the matrix
  !
  ! @MatvecMethods
  !
  PROCEDURE, PASS(obj) :: Matvec1 => mField_Matvec1
  !! Matrix vector multiplication
  PROCEDURE, PASS(obj) :: Matvec2 => mField_Matvec2
  !! Matrix vector multiplication
  !
  ! @LUSolveMethods
  !
  PROCEDURE, PASS(obj) :: ILUSOLVE1 => mField_ILUSOLVE1
  !! Solve (LU) sol = rhs
  PROCEDURE, PASS(obj) :: ILUSOLVE2 => mField_ILUSOLVE2
  !! Solve (LU) sol = rhs
  !
  ! @PreconditionMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: SetPrecondition => &
    & mField_SetPrecondition
  !! Building precondition matrix
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrecondition => &
    & mField_GetPrecondition
  !! Get the precondition matrix
  PROCEDURE, PUBLIC, PASS(obj) :: ReversePermutation => &
    & mField_ReversePermutation
  !
  ! @SetMethods
  !
  PROCEDURE, PASS(obj) :: Set1 => mField_Set1
  PROCEDURE, PASS(obj) :: Set2 => mField_Set2
  PROCEDURE, PASS(obj) :: Set3 => mField_Set3
  PROCEDURE, PASS(obj) :: Set4 => mField_Set4
  PROCEDURE, PASS(obj) :: Set5 => mField_Set5
  PROCEDURE, PASS(obj) :: Set6 => mField_Set6
  PROCEDURE, PASS(obj) :: Set7 => mField_Set7
  PROCEDURE, PASS(obj) :: Set8 => mField_Set8
  PROCEDURE, PASS(obj) :: Set9 => mField_Set9
  PROCEDURE, PASS(obj) :: Set10 => mField_Set10
  !
  ! @SetColMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: SetColumn1 => mField_SetColumn1
  PROCEDURE, PUBLIC, PASS(obj) :: SetColumn2 => mField_SetColumn2
  PROCEDURE, PUBLIC, PASS(obj) :: SetColumn3 => mField_SetColumn3
  PROCEDURE, PUBLIC, PASS(obj) :: SetColumn4 => mField_SetColumn4
  PROCEDURE, PUBLIC, PASS(obj) :: SetColumn5 => mField_SetColumn5
  PROCEDURE, PUBLIC, PASS(obj) :: SetColumn6 => mField_SetColumn6
  PROCEDURE, PUBLIC, PASS(obj) :: SetColumn7 => mField_SetColumn7
  !
  ! @SetRowMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: SetRow1 => mField_SetRow1
  PROCEDURE, PUBLIC, PASS(obj) :: SetRow2 => mField_SetRow2
  PROCEDURE, PUBLIC, PASS(obj) :: SetRow3 => mField_SetRow3
  PROCEDURE, PUBLIC, PASS(obj) :: SetRow4 => mField_SetRow4
  PROCEDURE, PUBLIC, PASS(obj) :: SetRow5 => mField_SetRow5
  PROCEDURE, PUBLIC, PASS(obj) :: SetRow6 => mField_SetRow6
  PROCEDURE, PUBLIC, PASS(obj) :: SetRow7 => mField_SetRow7
  !
  ! @GetColMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn1 => mField_GetColumn1
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn2 => mField_GetColumn2
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn3 => mField_GetColumn3
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn4 => mField_GetColumn4
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn5 => mField_GetColumn5
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn6 => mField_GetColumn6
  PROCEDURE, PUBLIC, PASS(obj) :: GetColumn7 => mField_GetColumn7
  !
  ! @GetRowMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: GetRow1 => mField_GetRow1
  PROCEDURE, PUBLIC, PASS(obj) :: GetRow2 => mField_GetRow2
  PROCEDURE, PUBLIC, PASS(obj) :: GetRow3 => mField_GetRow3
  PROCEDURE, PUBLIC, PASS(obj) :: GetRow4 => mField_GetRow4
  PROCEDURE, PUBLIC, PASS(obj) :: GetRow5 => mField_GetRow5
  PROCEDURE, PUBLIC, PASS(obj) :: GetRow6 => mField_GetRow6
  PROCEDURE, PUBLIC, PASS(obj) :: GetRow7 => mField_GetRow7
  !
  ! @DiagonalMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: GetDiagonal => &
    & mField_GetDiagonal
  !
  ! @DiagonalScalingMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: DiagonalScaling => &
    & mField_DiagonalScaling
  !
  ! @SpectralMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: &
    & SymSchurLargestEigenVal => mField_SymSchurLargestEigenVal
  PROCEDURE, PUBLIC, PASS(obj) :: &
    & SymLargestEigenVal => mField_SymLargestEigenVal

  PROCEDURE, PUBLIC, PASS(obj) :: ApplyDBC => mField_ApplyDBC
END TYPE MatrixField_

PUBLIC :: MatrixField_

TYPE(MatrixField_), PARAMETER, PUBLIC :: TypeMatrixField = &
  & MatrixField_(domains=NULL())

!----------------------------------------------------------------------------
!                                    SetMatrixFieldParam@sConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2021
! summary: This routine Sets the parameter for creating MatrixField_

INTERFACE
  MODULE SUBROUTINE SetMatrixFieldParam( &
    & param, &
    & name, &
    & matrixProp, &
    & engine,  &
    & spaceCompo, &
    & timeCompo, &
    & fieldType, &
    & comm, &
    & local_n, &
    & global_n)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    !! Options to create [[MatrixField_]] will be stored in this
    CHARACTER(*), INTENT(IN) :: name
    !! Name of the matrix field
    CHARACTER(*), INTENT(IN) :: matrixProp
    !! Matrix property, "SYM" or "UNSYM"
    CHARACTER(*), INTENT(IN) :: engine
    !! "NATIVE_SERIE"
    !! "LIS_OMP"
    !! "LIS_MPI"
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    !! Number of space-components, see [[DOF_]]
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
    !! Number of time-components, see [[DOF_]]
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! fieldType can be following
    !! FIELD_TYPE_NORMAL
    !! FIELD_TYPE_CONSTANT
    !! FIELD_TYPE_CONSTANT_SPACE
    !! FIELD_TYPE_CONSTANT_TIME
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
  END SUBROUTINE SetMatrixFieldParam
END INTERFACE

PUBLIC :: SetMatrixFieldParam

!----------------------------------------------------------------------------
!                           SetMatrixFieldPrecondParam@sConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2021
! summary: This routine Sets the parameter for precondition of MatrixField_

INTERFACE
  MODULE SUBROUTINE SetMatrixFieldPrecondParam( &
    & param, &
    & name, &
    & engine, &
    & lfil, mbloc, droptol, permtol, alpha, &
    & comm, local_n, global_n)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    !! Options to create precondition of [[MatrixField_]]
    INTEGER(I4B), INTENT(IN) :: name
    !! Name of precondition
    CHARACTER(*), INTENT(IN) :: engine
    !! "NATIVE_SERIE"
    !! "LIS_OMP"
    !! "LIS_MPI"
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: lfil
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: mbloc
    REAL(DFP), OPTIONAL, INTENT(IN) :: droptol
    !! Droptoleranace
    REAL(DFP), OPTIONAL, INTENT(IN) :: permtol
    !! permutation tolerance
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
  END SUBROUTINE SetMatrixFieldPrecondParam
END INTERFACE

PUBLIC :: SetMatrixFieldPrecondParam

!----------------------------------------------------------------------------
!                            SetRectangleMatrixFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine Sets the parameter for creating
! Rectangle [[MatrixField_]]
!
!# Introduction
!
! - This routine Sets the essential parameter for initiating
! an instance of [[MatrixField_]], which is rectangle.
! - Such matrices are used in Mixed finite element formulations
! - The results will be returned in `param`
! - After getting `param` user can call initiate method
!
!@note
! The size of `physicalVarNames`, `spaceCompo`, and `timeCompo` should be
! the same and equal to 2.
!@endnote

INTERFACE
  MODULE SUBROUTINE SetRectangleMatrixFieldParam( &
    & param, &
    & name, &
    & matrixProp, &
    & physicalVarNames, &
    & spaceCompo, &
    & timeCompo, &
    & engine, &
    & fieldType, &
    & comm, local_n, global_n)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    !! Options to create [[BlockMatrixField_]] will be stored in this
    CHARACTER(*), INTENT(IN) :: name
    !! Name of the matrix field
    CHARACTER(*), INTENT(IN) :: matrixProp
    !! Matrix property, "SYM" or "UNSYM"
    CHARACTER(*), INTENT(IN) :: physicalVarNames(2)
    !! Name of physical variables
    INTEGER(I4B), INTENT(IN) :: spaceCompo(2)
    !! Number of space-components in each physicalVarNames, see [[DOF_]]
    INTEGER(I4B), INTENT(IN) :: timeCompo(2)
    !! Number of time-components in each physicalVarNames, see [[DOF_]]
    CHARACTER(*), OPTIONAL, INTENT(IN) :: engine
    !! "NATIVE_SERIE"
    !! "LIS_OMP"
    !! "LIS_MPI"
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! fieldType can be following
    !! FIELD_TYPE_NORMAL <-- DEFAULT
    !! FIELD_TYPE_CONSTANT
    !! FIELD_TYPE_CONSTANT_SPACE
    !! FIELD_TYPE_CONSTANT_TIME
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
  END SUBROUTINE SetRectangleMatrixFieldParam
END INTERFACE

PUBLIC :: SetRectangleMatrixFieldParam

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 9 Oct 2021
! summary: Deallocates the data stored inside [[MatrixFieldPrecondition_]]

INTERFACE
  MODULE SUBROUTINE Pmat_Deallocate(obj)
    TYPE(MatrixFieldPrecondition_), INTENT(INOUT) :: obj
  END SUBROUTINE Pmat_Deallocate
END INTERFACE

INTERFACE DEALLOCATE
  MODULE PROCEDURE Pmat_Deallocate
END INTERFACE DEALLOCATE

PUBLIC :: DEALLOCATE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE mField_Final(obj)
    TYPE(MatrixField_), INTENT(INOUT) :: obj
  END SUBROUTINE mField_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                    CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.
!
!# Introduction
!
! This routine check the essential parameters required to the initiate the
! [[MatrixField_]] data type.

INTERFACE
  MODULE SUBROUTINE MatrixFieldCheckEssentialParam(obj, param)
    CLASS(MatrixField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE MatrixFieldCheckEssentialParam
END INTERFACE

PUBLIC :: MatrixFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.
!
!# Introduction
!
! This routine check the essential parameters required to the initiate the
! [[MatrixField_]] data type.

INTERFACE
  MODULE SUBROUTINE RectangleMatrixFieldCheckEssentialParam(obj, param)
    CLASS(MatrixField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE RectangleMatrixFieldCheckEssentialParam
END INTERFACE

PUBLIC :: RectangleMatrixFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field
!
!# Introduction
!
! This routine initiates an instance of [[MatrixField_]].
! The options/arguments to initiate the matrix field are
! contained inside param, which is an instance of [[ParameterList_]].
! In addition, [[Domain_]] `dom` is target to the pointer
! [[AbstractField_:domain]]
!
! - Param contains both essential and optional parameters which are used in
! constructing the matrix field
! - dom is a pointer to a domain, where we are interested in constructing the
! matrix
!
! ESSENTIAL PARAMETERS are
!
! - `name` This is name of field (char)
! - `matrixProp`, UNSYM, SYM (char)
!
! OPTIONAL PARAMETERS
!
! - `spaceCompo`, INT, default is 1
! - `timeCompo`, INT, default is 1
! - `fieldType`, INT, default is FIELD_TYPE_NORMAL

INTERFACE
  MODULE SUBROUTINE mField_Initiate1(obj, param, dom)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE mField_Initiate1
END INTERFACE

INTERFACE MatrixFieldInitiate1
  MODULE PROCEDURE mField_Initiate1
END INTERFACE MatrixFieldInitiate1

PUBLIC :: MatrixFieldInitiate1

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field
!
!# Introduction
!
! This routine initiates the `obj` [[MatrixField_]] by copying contents
! from `obj2`, an instance of chid class of [[AbstractField_]].
! In this way we try to minimize the computation effort.
!
!@note
! If `copyFull, copyStructure, usePointer` are absent then this subroutine,
! copies the value of the matrix from obj2 to obj.
!@endnote
!
!@note
! However, in [[MatrixField_:mat]], it will not allocate space for
! [[CSRSparsity_]] field of
! [[CSRMatrix_]], that is [[CSRMatrix_:CSR]] field of [[MatrixField_:mat]].
! Instead, it will use the obj2%mat%csr as the target for the pointer
! obj%mat%csr.
! In this way, there is no need to create multiple sparsity patterns
! for the same domain.
!@endnote
!
!@todo
! At present, the routine works for `copyFull=.TRUE., copyStructure=.TRUE.,
! usePointer=.TRUE.`, which equivalent to the default behavior.
! Add functionality for other options too.
!@endtodo

INTERFACE
  MODULE SUBROUTINE mField_Initiate2(obj, obj2, copyFull, copyStructure, &
    & usePointer)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be an instance of MatrixField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE mField_Initiate2
END INTERFACE

INTERFACE MatrixFieldInitiate2
  MODULE PROCEDURE mField_Initiate2
END INTERFACE MatrixFieldInitiate2

PUBLIC :: MatrixFieldInitiate2

!----------------------------------------------------------------------------
!                                               Initiate@sConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field

INTERFACE
  MODULE SUBROUTINE mField_Initiate3(obj, param, dom)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
  END SUBROUTINE mField_Initiate3
END INTERFACE

INTERFACE MatrixFieldInitiate3
  MODULE PROCEDURE mField_Initiate3
END INTERFACE MatrixFieldInitiate3

PUBLIC :: MatrixFieldInitiate3

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine deallocates the data stored inside the matrix

INTERFACE
  MODULE SUBROUTINE mField_Deallocate(obj)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
  END SUBROUTINE mField_Deallocate
END INTERFACE

INTERFACE MatrixFieldDeallocate
  MODULE PROCEDURE mField_Deallocate
END INTERFACE MatrixFieldDeallocate

PUBLIC :: MatrixFieldDeallocate

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine displays the content

INTERFACE
  MODULE SUBROUTINE mField_Display(obj, msg, unitNo)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE mField_Display
END INTERFACE

INTERFACE MatrixFieldDisplay
  MODULE PROCEDURE mField_Display
END INTERFACE MatrixFieldDisplay

PUBLIC :: MatrixFieldDisplay

!----------------------------------------------------------------------------
!                                                          Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content of matrix field from hdf5file

INTERFACE
  MODULE SUBROUTINE mField_Import(obj, hdf5, group, dom, domains)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE mField_Import
END INTERFACE

INTERFACE MatrixFieldImport
  MODULE PROCEDURE mField_Import
END INTERFACE MatrixFieldImport

PUBLIC :: MatrixFieldImport

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content of matrix field from hdf5file

INTERFACE
  MODULE SUBROUTINE mField_ImportPmat(obj, hdf5, group, dom, domains)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE mField_ImportPmat
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content of matrixfield_ to hdf5 file

INTERFACE
  MODULE SUBROUTINE mField_Export(obj, hdf5, group)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE mField_Export
END INTERFACE

INTERFACE MatrixFieldExport
  MODULE PROCEDURE mField_Export
END INTERFACE MatrixFieldExport

PUBLIC :: MatrixFieldExport

!----------------------------------------------------------------------------
!                                                       ExportPmat@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine ExportPmats the content of matrixfield_ to hdf5 file

INTERFACE
  MODULE SUBROUTINE mField_ExportPmat(obj, hdf5, group)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE mField_ExportPmat
END INTERFACE

!----------------------------------------------------------------------------
!                                                              SPY@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content of matrixfield_ to hdf5 file

INTERFACE
  MODULE SUBROUTINE mField_SPY(obj, filename, ext)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: filename
    CHARACTER(*), INTENT(IN) :: ext
  END SUBROUTINE mField_SPY
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Size@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine returns the SIZE of the matrix

INTERFACE
  MODULE FUNCTION mField_Size(obj, dim) RESULT(ans)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B) :: ans
  END FUNCTION mField_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Shape@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine returns the shape of the matrix

INTERFACE
  MODULE FUNCTION mField_Shape(obj) RESULT(ans)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans(2)
  END FUNCTION mField_Shape
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Matvec@MatVecMethods
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

INTERFACE
  MODULE SUBROUTINE mField_Matvec1(obj, x, y, isTranspose, &
    & addContribution, scale)
    CLASS(MatrixField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    !! Input vector in y=Ax
    REAL(DFP), INTENT(INOUT) :: y(:)
    !! Output vector y=Ax
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
  END SUBROUTINE mField_Matvec1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Matvec@MatVecMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine returns the maxtrix vector multiplication
!
!# Introduction
!
! This routine returns the matrix vector multiplication. Here, input vector
! is an instance of AbstractNodeField.
! The output vector is also an instance of AbstractNodeField.
! It should be noted that the output vector should be allocated
! outside and it should have same length as the input vector.

INTERFACE
  MODULE SUBROUTINE mField_Matvec2(obj, x, y, isTranspose, &
    & addContribution, scale)
    CLASS(MatrixField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: x
    !! Input vector in y=Ax
    CLASS(AbstractNodeField_), INTENT(INOUT) :: y
    !! Output vector y=Ax
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
  END SUBROUTINE mField_Matvec2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     LUSOLVE@MatVecMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine solves (LU) sol = rhs
!
!# Introduction
!
! This routine solves (LU) sol = rhs
! sol and rhs are fortran real vector
! The LU decomposition is stored inside the AbstractMatrixField_.
! Note that sol should be allocated by the user, and size of sol should be
! same as the size of rhs
!
!@note
! LU matrix is stored inside the object in Modified Sparse Row format
! This form of matrix is obtained by incomplete LU decomposition type
! precodnitioners
!@endnote
!
!@note
! This routine is avaiable for NATIVE_SERIAL
!@endnote

INTERFACE
  MODULE SUBROUTINE mField_ILUSOLVE1(obj, sol, rhs, isTranspose)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(INOUT) :: sol(:)
    !! Output vector y=Ax
    REAL(DFP), INTENT(IN) :: rhs(:)
    !! Input vector in y=Ax
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
  END SUBROUTINE mField_ILUSOLVE1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      LUSOLVE@MatVecMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine solves (LU) sol = rhs
!
!# Introduction
!
! - This routine solves (LU) sol = rhs
! - sol and rhs are [[AbstractNodeField_]]
! - The LU decomposition is stored inside the AbstractMatrixField_.
!
!@note
! LU matrix is stored inside the object in Modified Sparse Row format
! This form of matrix is obtained by incomplete LU decomposition type
! precodnitioners
!@endnote
!
!@note
! This routine is avaiable for NATIVE_SERIAL
!@endnote

INTERFACE
  MODULE SUBROUTINE mField_ILUSOLVE2(obj, sol, rhs, isTranspose)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: sol
    !! Output vector y=Ax
    CLASS(AbstractNodeField_), INTENT(IN) :: rhs
    !! Input vector in y=Ax
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
  END SUBROUTINE mField_ILUSOLVE2
END INTERFACE

!----------------------------------------------------------------------------
!                                       SetPrecondition@PreconditionMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine Sets the precondition
!
!# Introduction
! This routine Sets the precondition matrix
!
! - The information about the precondition is given in `param`
! - The number of parameters defined in `param` actually  depends upon the
! type of precondition matrix
!
! Following preconditions has been added.
!
! ILUT : preconditionName, droptol, lfil
! ILUTP : preconditionName, droptol, permtol, lfil, mbloc
! ILUD : preconditionName, droptol, alpha
! ILUDP : preconditionName, droptol, permtol, alpha, mbloc
! ILUK : preconditionName, lfil
!
! This routine is avaiable for NATIVE_SERIAL only.

INTERFACE
  MODULE SUBROUTINE mField_SetPrecondition(obj, param, dbcPtrs)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), OPTIONAL, INTENT(IN) :: param
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcPtrs(:)
  END SUBROUTINE mField_SetPrecondition
END INTERFACE

!----------------------------------------------------------------------------
!                                       GetPrecondition@PreconditionMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine returns the precondition matrix

INTERFACE
  MODULE SUBROUTINE mField_GetPrecondition(obj, Pmat)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractMatrixField_), INTENT(INOUT) :: Pmat
  END SUBROUTINE mField_GetPrecondition
END INTERFACE

!----------------------------------------------------------------------------
!                                    ReversePermutation@PreconditionMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2021
! summary: This routine fix the solution
!
!# Introduction
! In sparse solver, it is common to use row or column permutations. This is
! done to improve the sparsity of ILU decomposition.
! In case of column permutation, the solution needs to be permutated
! In case of row permulation, the rhs needs to be permutated

INTERFACE
  MODULE SUBROUTINE mField_ReversePermutation(obj, rhs, sol)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: rhs
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: sol
  END SUBROUTINE mField_ReversePermutation
END INTERFACE

!----------------------------------------------------------------------------
!                                               getDiagonal@DiagonalMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: Returns the diagnoal

INTERFACE
  MODULE SUBROUTINE mField_GetDiagonal(obj, diag)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: diag(:)
  END SUBROUTINE mField_GetDiagonal
END INTERFACE

!----------------------------------------------------------------------------
!                                         getDiagonal@DiagonalScalingMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: REturns the diagnoal

INTERFACE
  MODULE SUBROUTINE mField_DiagonalScaling(obj, side, diag, OPERATOR)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: side
    REAL(DFP), OPTIONAL, INTENT(IN) :: diag(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: OPERATOR
  END SUBROUTINE mField_DiagonalScaling
END INTERFACE

!----------------------------------------------------------------------------
!                                  SymSchurLargestEigenVal@SpectralMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-30
! summary: SymSchurLargestEigenVal

INTERFACE
  MODULE FUNCTION mField_SymSchurLargestEigenVal(obj, B, nev, which, NCV, &
      & maxIter, tol) RESULT(ans)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
  END FUNCTION mField_SymSchurLargestEigenVal
END INTERFACE

!----------------------------------------------------------------------------
!                                  SymSchurLargestEigenVal@SpectralMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-30
! summary: SymSchurLargestEigenVal

INTERFACE
  MODULE FUNCTION mField_SymLargestEigenVal(obj, nev, which, NCV, &
      & maxIter, tol) RESULT(ans)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
  END FUNCTION mField_SymLargestEigenVal
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE mField_ApplyDBC(obj, dbcPtrs)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dbcPtrs(:)
  END SUBROUTINE mField_ApplyDBC
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
!
! IF `addContribution` and `scale` is absent then:
!
! - This subroutine Sets a block of data to matrix.
! - This block data is contained in `value(:,:)`
! - The size of value should be tdof * size( globalNode ), where
! `tdof` is the total degrees of freedom
! - `globalNode` contains the node number
! - storageFMT is the storage format of value(:,:), it can be DOF_FMT, or
! `FMT_NODES`.
!
! If `addContribution` and `scale` are present  then:
! This subroutine adds a block of data to matrix.
! This block data is contained in `value(:,:)`
! The sized of value should be tdof * size( globalNode )
! `globalNode` contains the node number
! storageFMT is the storage format of value(:,:), it can be DOF_FMT, or
! `FMT_NODES`.
! scale is scaling used for value.
!
!@note
! This matrix should not be called for rectangle matrix.
!@endnote

INTERFACE
  MODULE SUBROUTINE mField_Set1(obj, globalNode, VALUE, storageFMT, scale, &
    & addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: storageFMT
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
!
! IF `addContribution` and `scale` is absent then:
!
!- This subroutine Sets a scalar value `value` to all or selected the
! entries of the matrix.
!- If `globalNode` is present then this routine Sets a scalar value `value` to
! selected the entries of the matrix. These entries are specified by the
! `globalNode(:)` vector, which denotes the global node numbers. symbolically
! it does the following: `obj(glocalNode)=value`
!- If `globalNode` is absent then all entries are Set to the scalar values.
! Symbolically it does the following: `obj=value`
!
! IF `addContribution` and `scale` not present:
!
! IF globalNode is not present then:
! This subroutine adds a scalar value `value` to all the entries of the matrix
! symbolically it does the following: `obj=obj+scale*value`
! If globalNode is present then:
! This subroutine adds a scalar value `value` to selected the entries of the
! matrix. These entries are spacified by the `globalNode(:)` vector, which
! denotes the global node numbers.
! symbolically it does the following:
! `obj(glocalNode)=obj(globalNode)+scale*value`
!
!@note
! This method cannot be called for Rectangle matrix field
!@endnote

INTERFACE
  MODULE SUBROUTINE mField_Set2(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
!
! If addContribution and scale not present then:
!
! This subroutine Sets a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.
!
! If addContribution and scale present then:
!
! This subroutine adds a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.

INTERFACE
  MODULE SUBROUTINE mField_Set3(obj, iNodeNum, jNodeNum, idof, &
    & jdof, VALUE, scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
!
! See [[CSRMatrix_Method:Set6]]
! See [[CSRMatrix_Method:Add6]]

INTERFACE
  MODULE SUBROUTINE mField_Set4(obj, iNodeNum, jNodeNum, ivar, &
    & jvar, VALUE, scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! Storage format should be FMT_NODES
    !! This is because MatrixField stores data in FMT_NODES
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
!
! See [[CSRMatrix_Method:Set6]]
! See [[CSRMatrix_Method:Add6]]

INTERFACE
  MODULE SUBROUTINE mField_Set5(obj, iNodeNum, jNodeNum, ivar, &
    & jvar, idof, jdof, VALUE, scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine Sets a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.
!
! If addContribution and scale present then:
!
! This subroutine adds a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.

INTERFACE
  MODULE SUBROUTINE mField_Set6(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & idof, jdof, VALUE, scale, addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine Sets a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.
!
! If addContribution and scale present then:
!
! This subroutine adds a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.

INTERFACE
  MODULE SUBROUTINE mField_Set7(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE, scale, &
    & addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
  END SUBROUTINE mField_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine Sets a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.
!
! If addContribution and scale present then:
!
! This subroutine adds a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.

INTERFACE
  MODULE SUBROUTINE mField_Set8(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE, scale, &
    & addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
  END SUBROUTINE mField_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine Sets a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.
!
! If addContribution and scale present then:
!
! This subroutine adds a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.

INTERFACE
  MODULE SUBROUTINE mField_Set9(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE, scale, &
    & addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
  END SUBROUTINE mField_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine Sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine Sets a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.
!
! If addContribution and scale present then:
!
! This subroutine adds a scalar value `value` to a single entry of the matrix.
! This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`,
! `colNodeNum` and `colDOF`.

INTERFACE
  MODULE SUBROUTINE mField_Set10(obj, iNodeNum, jNodeNum, ivar, jvar, &
    & ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE, scale, &
    & addContribution)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
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
  END SUBROUTINE mField_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_SetRow1(obj, globalNode, idof, scalarVal, vecVal, &
    & nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_SetRow1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_SetRow2(obj, globalNode, ivar, idof, &
    & scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_SetRow2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_SetRow3(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_SetRow3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_SetRow4(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_SetRow4
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_SetRow5(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_SetRow5
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_SetRow6(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_SetRow6
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetRow@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the row of a sparse matrix
!
!# Introduction
! This routine Sets the row of a sparse matrix. The row index is calculated /
! using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_SetRow7(obj, globalNode, ivar, spacecompo, &
    & timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_SetRow7
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_SetColumn1(obj, globalNode, idof, scalarVal, &
    & vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_SetColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_SetColumn2(obj, globalNode, ivar, idof, &
    & scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_SetColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_SetColumn3(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_SetColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set
! to this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_SetColumn4(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_SetColumn4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_SetColumn5(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_SetColumn5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_SetColumn6(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_SetColumn6
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetColumn@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine Set the column of a sparse matrix
!
!# Introduction
! This routine Sets the column of a sparse matrix. The column index is
! calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be Set to
! this scalar value
! - `vectorVal` is the vector value, if present then the row will be Set to
! this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
  MODULE SUBROUTINE mField_SetColumn7(obj, globalNode, ivar, &
    & spacecompo, timecompo, scalarVal, vecVal, nodeFieldVal)
    CLASS(MatrixField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scalarVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: vecVal(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(IN) :: nodeFieldVal
  END SUBROUTINE mField_SetColumn7
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
!
! - `globalNode` is the global node number of mesh
! - `idof` is the degree of freedom number
!
! - If `value` is present then the vector is returned inside the rank 1 vector
! - If `nodeFieldVal` is present then the row is returned inside the
! node field

INTERFACE
  MODULE SUBROUTINE mField_GetRow1(obj, globalNode, idof, VALUE, &
    & nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_GetRow1
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
!
! - `globalNode` is the global node number of mesh
! - `idof` is the degree of freedom number
!
! - If `value` is present then the vector is returned inside the rank 1 vector
! - If `nodeFieldVal` is present then the row is returned inside the
! node field

INTERFACE
  MODULE SUBROUTINE mField_GetRow2(obj, globalNode, ivar, idof, VALUE, &
    & nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_GetRow2
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
!
! - `globalNode` is the global node number of mesh
! - `idof` is the degree of freedom number
!
! - If `value` is present then the vector is returned inside the rank 1 vector
! - If `nodeFieldVal` is present then the row is returned inside the
! node field

INTERFACE
  MODULE SUBROUTINE mField_GetRow3(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_GetRow3
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
!
! - `globalNode` is the global node number of mesh
! - `idof` is the degree of freedom number
!
! - If `value` is present then the vector is returned inside the rank 1 vector
! - If `nodeFieldVal` is present then the row is returned inside the
! node field

INTERFACE
  MODULE SUBROUTINE mField_GetRow4(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_GetRow4
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
!
! - `globalNode` is the global node number of mesh
! - `idof` is the degree of freedom number
!
! - If `value` is present then the vector is returned inside the rank 1 vector
! - If `nodeFieldVal` is present then the row is returned inside the
! node field

INTERFACE
  MODULE SUBROUTINE mField_GetRow5(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_GetRow5
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
!
! - `globalNode` is the global node number of mesh
! - `idof` is the degree of freedom number
!
! - If `value` is present then the vector is returned inside the rank 1 vector
! - If `nodeFieldVal` is present then the row is returned inside the
! node field

INTERFACE
  MODULE SUBROUTINE mField_GetRow6(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_GetRow6
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
!
! - `globalNode` is the global node number of mesh
! - `idof` is the degree of freedom number
!
! - If `value` is present then the vector is returned inside the rank 1 vector
! - If `nodeFieldVal` is present then the row is returned inside the
! node field

INTERFACE
  MODULE SUBROUTINE mField_GetRow7(obj, globalNode, ivar, spacecompo, &
    & timecompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_GetRow7
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
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

INTERFACE
  MODULE SUBROUTINE mField_GetColumn1(obj, globalNode, idof, VALUE, &
    & nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_GetColumn1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
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

INTERFACE
  MODULE SUBROUTINE mField_GetColumn2(obj, globalNode, ivar, idof, VALUE, &
    & nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_GetColumn2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
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

INTERFACE
  MODULE SUBROUTINE mField_GetColumn3(obj, globalNode, ivar, spaceCompo, &
    & timeCompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_GetColumn3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
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

INTERFACE
  MODULE SUBROUTINE mField_GetColumn4(obj, globalNode, ivar, spaceCompo, &
    & timeCompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_GetColumn4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
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

INTERFACE
  MODULE SUBROUTINE mField_GetColumn5(obj, globalNode, ivar, spaceCompo, &
    & timeCompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_GetColumn5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
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

INTERFACE
  MODULE SUBROUTINE mField_GetColumn6(obj, globalNode, ivar, spaceCompo, &
    & timeCompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_GetColumn6
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetColumn@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine returns the column of a sparse matrix
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

INTERFACE
  MODULE SUBROUTINE mField_GetColumn7(obj, globalNode, ivar, spaceCompo, &
    & timeCompo, VALUE, nodeFieldVal, scale, addContribution)
    CLASS(MatrixField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: VALUE(:)
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: nodeFieldVal
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE mField_GetColumn7
END INTERFACE

END MODULE MatrixField_Class
