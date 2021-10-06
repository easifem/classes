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
! summary: This module defines a concretre class called MatrixField_, which can handle the finite element matrix field. This is a native implementation of finite element tangent matrices.

MODULE MatrixField_Class
USE GlobalData
USE BaseType
USE FPL, ONLY: ParameterList_
USE FPL_Method
USE HDF5File_Class
USE ExceptionHandler_Class
USE AbstractField_Class
USE AbstractNodeField_Class
USE AbstractMatrixField_Class
USE Domain_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "MATRIXFIELD_CLASS"
TYPE( ExceptionHandler_ ) :: e
INTEGER( I4B ), PARAMETER :: IPAR_LENGTH = 14
INTEGER( I4B ), PARAMETER :: FPAR_LENGTH = 14

!----------------------------------------------------------------------------
!                                                               MSRSparsity_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 June 2021
! summary: User data type for handling the preconditioning of MatrixField_
!
!# Introduction
!
! This is a data type for storing the the precondition matrix.
! The storage pattern of the precondition matrix depends upon the type of
! preconditioning. For example, ILU type preconditioners are stored in modified sparse row by sparseKit library. In this way, storage also depends upon the linear solver library. That is why it is better to hide the preconditioner from user. More details about the MSR format is given as follows.
!
! We have used Modified Sparse Row, which is used by Sparsekit lib to store the precondition matrix, this data type is meant to be used internally only.
! The precondition matrix that will be stored inside it is mainly ILUT.
! User should not worry about this data type.
!
! ```fortran
! INTEGER :: JA( : )
! REAL :: A( : )
! ```
!
! - `A(1:n)` contains the diagonal of the matrix.
! - `A(n+2:nnz)` contains the nondiagonal elements of the matrix, stored ROWWISE.
! - `JA(n+2:nnz)`  contains their column indices
! - `JA(1:n+1)` Contains the pointer array for the nondiagonal, elements in A(n+1:nnz) and JA(n+2:nnz), i.e., for `i .LE. n+1` `JA(i)` points to beginning of row i in arrays A, JA.
! - Here, nnz = number of nonzero elements+1

TYPE :: MatrixFieldPrecondition_
  LOGICAL( LGT ) :: isInitiated = .FALSE.
  INTEGER( I4B ) :: PmatName = 0
  INTEGER( I4B ) :: nnz = 0
  INTEGER( I4B ) :: ncol = 0
  INTEGER( I4B ) :: nrow = 0
  INTEGER( I4B ) :: lfil = 0
  INTEGER( I4B ) :: mbloc = 0
  REAL( DFP ) :: alpha = 0.0_DFP
  REAL( DFP ) :: droptol = 0.0_DFP
  REAL( DFP ) :: permtol = 0.0_DFP
  REAL( DFP ), ALLOCATABLE :: A( : )
  INTEGER( I4B ), ALLOCATABLE :: JA( : )
  INTEGER( I4B ), ALLOCATABLE :: IA( : )
  INTEGER( I4B ), ALLOCATABLE :: JU( : )
  INTEGER( I4B ), ALLOCATABLE :: IPERM( : )
  INTEGER( I4B ), ALLOCATABLE :: LEVS( : )
END TYPE MatrixFieldPrecondition_

!----------------------------------------------------------------------------
!                                                              MatrixField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: This datatype can handle the finite element matrix field. This is a native implementation of finite element tangent matrices.
!
!# Introduction
!
! This datatype is native implementation of finite element tangent matrix. This data type is sequential, and it uses Sparsekit library for sparse matrix manipulations.
!

TYPE, EXTENDS( AbstractMatrixField_ ) :: MatrixField_
  TYPE( CSRMatrix_ ) :: mat
  TYPE( MatrixFieldPrecondition_ ) :: Pmat
  CONTAINS
  PRIVATE
    PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => mField_addSurrogate
    PROCEDURE, PUBLIC, PASS( obj ) :: checkEssentialParam => mField_checkEssentialParam
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate1 => mField_Initiate1
      !! Initiate from the parameter list
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate2 => mField_Initiate2
      !! Initiate by copying other object
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate3 => mField_Initiate3
      !! Initiate for block matrices
    PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => mField_DeallocateData
      !! Deallocate the field
    PROCEDURE, PUBLIC, PASS( obj ) :: Display => mField_Display
      !! Display the field
    PROCEDURE, PUBLIC, PASS( obj ) :: Import => mField_Import
      !! Import from hdf5 file
    PROCEDURE, PUBLIC, PASS( obj ) :: Export => mField_Export
      !! export matrix field in hdf5file_
    PROCEDURE, PUBLIC, PASS( obj ) :: SPY => mField_SPY
      !! SPY
    PROCEDURE, PUBLIC, PASS( obj ) :: Size => mField_Size
      !! Returns the size of the matrix
    PROCEDURE, PUBLIC, PASS( obj ) :: Shape => mField_Shape
      !! Returns the shape of the matrix
    PROCEDURE, PASS( obj ) :: Matvec1 => mField_Matvec1
      !! Matrix vector multiplication, here vector is fortran array
    PROCEDURE, PASS( obj ) :: Matvec2 => mField_Matvec2
      !! Matrix vector multiplication, here vector is AbstractNodeField_
    PROCEDURE, PASS( obj ) :: LUSOLVE1 => mField_LUSOLVE1
      !! Solve (LU) sol = rhs
    PROCEDURE, PASS( obj ) :: LUSOLVE2 => mField_LUSOLVE2
      !! Solve (LU) sol = rhs
    PROCEDURE, PUBLIC, PASS( obj ) :: setPrecondition => mField_setPrecondition
      !! Building precondition matrix
    PROCEDURE, PUBLIC, PASS( obj ) :: getPrecondition => mField_getPrecondition
      !! Get the precondition matrix
    PROCEDURE, PUBLIC, PASS( obj ) :: reversePermutation => mField_reversePermutation
    PROCEDURE, PASS( obj ) :: set1 => mField_set1
    PROCEDURE, PASS( obj ) :: set2 => mField_set2
    PROCEDURE, PASS( obj ) :: set3 => mField_set3
    PROCEDURE, PUBLIC, PASS( obj ) :: setRow => mField_setRow
    PROCEDURE, PUBLIC, PASS( obj ) :: setColumn => mField_setColumn
    PROCEDURE, PUBLIC, PASS( obj ) :: getRow => mField_getRow
    PROCEDURE, PUBLIC, PASS( obj ) :: getColumn => mField_getColumn
END TYPE MatrixField_

PUBLIC :: MatrixField_


!----------------------------------------------------------------------------
!                                                 addSurrogate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.

INTERFACE
MODULE SUBROUTINE mField_addSurrogate( obj, UserObj )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: UserObj
END SUBROUTINE mField_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                            setMatrixFieldParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2021
! summary: This routine sets the parameter for creating MatrixField_

INTERFACE
MODULE SUBROUTINE setMatrixFieldParam( param, name, matrixProp, spaceCompo, &
  & timeCompo, fieldType )
  TYPE( ParameterList_ ), INTENT( INOUT ) :: param
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  CHARACTER( LEN = * ), INTENT( IN ) :: matrixProp
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: spaceCompo
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: timeCompo
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: fieldType
    !! fieldType can be following
    !! FIELD_TYPE_NORMAL
    !! FIELD_TYPE_CONSTANT
    !! FIELD_TYPE_CONSTANT_SPACE
    !! FIELD_TYPE_CONSTANT_TIME
END SUBROUTINE setMatrixFieldParam
END INTERFACE

PUBLIC :: setMatrixFieldParam

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE Pmat_DeallocateData( obj )
  TYPE( MatrixFieldPrecondition_ ), INTENT( INOUT ) :: obj
END SUBROUTINE Pmat_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE Pmat_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                           checkEssentialParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.
!
!# Introduction
! This routine check the essential parameters required to the initiate the
! [[MatrixField_]] data type.

INTERFACE
MODULE SUBROUTINE mField_checkEssentialParam( obj, param )
  CLASS( MatrixField_ ), INTENT( IN ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE mField_checkEssentialParam
END INTERFACE

PUBLIC :: mField_checkEssentialParam

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field
!
!# Introduction
! This routine initiates the the matrix field. It uses parameter list, and dom.
! - Param contains both essential and optional parameters which are used in constructing the matrix field
! - dom is a pointer to a domain, where we are interested in constructing the matrix
!
! ESSENTIAL PARAMETERS
!
! - `name` This is name of field (char)
! - `matrixProp`, UNSYM, SYM (char)
!
! OPTIONAL PARAMETERS
!
! - `spaceCompo`, INT, default is 1
! - `timeCompo`, INT, default is 1
! - `fieldType`, INT, default is FIELD_TYPE_NORMAL
!
!### Usage
!
!```fortran
  ! type( domain_ ) :: dom
  ! type( MatrixField_ ) :: obj
  ! type( HDF5File_ ) :: meshfile, hdf5
  ! type( ParameterList_ ) :: param
  ! integer( i4b ) :: ierr, tnodes
  ! call display( "TESTING INITIATE AND DEALLOCATEDATA" )
  ! CALL FPL_INIT()
  ! call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  ! call meshfile%open()
  ! call dom%initiate( meshfile )
  ! call meshfile%close()
  ! call meshfile%deallocateData()
  ! tnodes = dom%getTotalNodes()
  ! call param%initiate()
  ! call setMatrixFieldParam( param, "K", "UNSYM", 3, 2, FIELD_TYPE_NORMAL )
  ! call obj%initiate( param, dom )
  ! CALL hdf5%initiate(filename="./matrixField.h5", mode="NEW" )
  ! CALL hdf5%open()
  ! CALL obj%export(hdf5=hdf5,group='')
  ! CALL hdf5%close()
  ! CALL hdf5%deallocateData()
  ! call obj%deallocateData()
  ! call dom%deallocateData()
  ! call param%deallocateData()
  ! call FPL_FINALIZE()
!```

INTERFACE
MODULE SUBROUTINE mField_Initiate1( obj, param, dom )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE mField_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field
!
!# Introduction
! This routine initiates the `obj` by copying contents from `obj2`. In this way we try to minimize the computation effort.
!
! Default behavior:
!
! - If `copyFull, copyStructure, usePointer` are absent then this subroutine, copy the value of the matrix, however, it will be not allocate the [[CSRSparsity_]] field of [[CSRMatrix_]]. Instead, it will use pointer reference to the obj2%mat%csr. In this way, we do not have to create multiple sparsity patterns on the same domain.
!
! - `copyFull=.TRUE., copyStructure=.TRUE., usePointer=.TRUE.`, then default behavior
!
! Other type of behaviors can be included in the future
!
!

INTERFACE
MODULE SUBROUTINE mField_Initiate2( obj, obj2, copyFull, copyStructure, usePointer )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  CLASS( AbstractField_ ), INTENT( INOUT ) :: obj2
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: copyFull
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: copyStructure
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: usePointer
END SUBROUTINE mField_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field
!
!### Usage
!

INTERFACE
MODULE SUBROUTINE mField_Initiate3( obj, param, dom )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( DomainPointer_ ), TARGET, INTENT( IN ) :: dom( : )
END SUBROUTINE mField_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine deallocates the data stored inside the matrix

INTERFACE
MODULE SUBROUTINE mField_DeallocateData( obj )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
END SUBROUTINE mField_DeallocateData
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine displays the content

INTERFACE
MODULE SUBROUTINE mField_Display( obj, msg, unitNo )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE mField_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Import@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content of matrix field from hdf5file

INTERFACE
MODULE SUBROUTINE mField_Import( obj, hdf5, group, dom )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE mField_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Export@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content of matrixfield_ to hdf5 file

INTERFACE
MODULE SUBROUTINE mField_Export( obj, hdf5, group )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END SUBROUTINE mField_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   SPY@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content of matrixfield_ to hdf5 file

INTERFACE
MODULE SUBROUTINE mField_SPY( obj, filename )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: filename
END SUBROUTINE mField_SPY
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Size@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine returns the SIZE of the matrix

INTERFACE
MODULE FUNCTION mField_Size( obj, dim ) RESULT( ans )
  CLASS( MatrixField_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dim
  INTEGER( I4B ) :: ans
END FUNCTION mField_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Shape@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine returns the shape of the matrix

INTERFACE
MODULE FUNCTION mField_Shape( obj ) RESULT( ans )
  CLASS( MatrixField_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans( 2 )
END FUNCTION mField_Shape
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
! This routine returns the row of a sparse matrix. The row index is calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `val` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the row is returned inside the node field

INTERFACE
MODULE SUBROUTINE mField_getRow( obj, globalNode, idof, val, nodeFieldVal, &
  & scale, addContribution )
  CLASS( MatrixField_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), OPTIONAL, INTENT( INOUT ) :: val( : )
  CLASS( AbstractNodeField_ ), OPTIONAL, INTENT( INOUT ) :: nodeFieldVal
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE mField_getRow
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
! This routine returns the column of a sparse matrix. The column index is calculated using the `globalNode` and `idof`.
! `globalNode` is the global node number
! `idof` is the degree of freedom number
!
! If `val` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node field

INTERFACE
MODULE SUBROUTINE mField_getColumn( obj, globalNode, idof, val, nodeFieldVal,&
  & scale, addContribution )
  CLASS( MatrixField_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), OPTIONAL, INTENT( INOUT ) :: val( : )
  CLASS( AbstractNodeField_ ), OPTIONAL, INTENT( INOUT ) :: nodeFieldVal
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE mField_getColumn
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
! is a native fortran vector. The output vector is also a native fortran vector. It should be noted that the output vector should be allocated outside and it should have same length as the input vector.
!

INTERFACE
MODULE SUBROUTINE mField_Matvec1( obj, x, y, transp )
  CLASS( MatrixField_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x( : )
    !! Input vector in y=Ax
  REAL( DFP ), INTENT( INOUT ) :: y( : )
    !! Output vector y=Ax
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: transp
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
! is a native fortran vector. The output vector is also a native fortran vector. It should be noted that the output vector should be allocated outside and it should have same length as the input vector.

INTERFACE
MODULE SUBROUTINE mField_Matvec2( obj, x, y, transp )
  CLASS( MatrixField_ ), INTENT( IN ) :: obj
  CLASS( AbstractNodeField_ ), INTENT( IN ) :: x
    !! Input vector in y=Ax
  CLASS( AbstractNodeField_ ), INTENT( INOUT ) :: y
    !! Output vector y=Ax
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: transp
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
! Note that sol should be allocated by the user, and size of sol should be same as the size of rhs
!
!@note
! LU matrix is stored inside the object in Modified Sparse Row format
! This form of matrix is obtained by incomplete LU decomposition type precodnitioners
!@endnote

INTERFACE
MODULE SUBROUTINE mField_LUSOLVE1( obj, sol, rhs, transp )
  CLASS( MatrixField_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( INOUT ) :: sol( : )
    !! Output vector y=Ax
  REAL( DFP ), INTENT( IN ) :: rhs( : )
    !! Input vector in y=Ax
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: transp
END SUBROUTINE mField_LUSOLVE1
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
! This form of matrix is obtained by incomplete LU decomposition type precodnitioners
!@endnote

INTERFACE
MODULE SUBROUTINE mField_LUSOLVE2( obj, sol, rhs, transp )
  CLASS( MatrixField_ ), INTENT( IN ) :: obj
  CLASS( AbstractNodeField_ ), INTENT( INOUT ) :: sol
    !! Output vector y=Ax
  CLASS( AbstractNodeField_ ), INTENT( IN ) :: rhs
    !! Input vector in y=Ax
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: transp
END SUBROUTINE mField_LUSOLVE2
END INTERFACE

!----------------------------------------------------------------------------
!                                       setPrecondition@PreconditionMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine sets the precondition
!
!# Introduction
! This routine sets the preconditioning
!
! The parameters inside the param depends upon the type of preconditioners
! Following preconditions has been added.
!
! ILUT : preconditionName, droptol, lfil
! ILUTP : preconditionName, droptol, permtol, lfil, mbloc
! ILUD : preconditionName, droptol, alpha
! ILUDP : preconditionName, droptol, permtol, alpha, mbloc
! ILUK : preconditionName, lfil

INTERFACE
MODULE SUBROUTINE mField_setPrecondition( obj, param )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), OPTIONAL, INTENT( IN ) :: param
END SUBROUTINE mField_setPrecondition
END INTERFACE

!----------------------------------------------------------------------------
!                                       getPrecondition@PreconditionMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine returns the precondition matrix

INTERFACE
MODULE SUBROUTINE mField_getPrecondition( obj, Pmat )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  CLASS( AbstractMatrixField_ ), INTENT( INOUT ) :: Pmat
END SUBROUTINE mField_getPrecondition
END INTERFACE

!----------------------------------------------------------------------------
!                                    reversePermutation@PreconditionMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2021
! summary: This routine fix the solution
!
!# Introduction
! In sparse solver, it is common to use row or column permutations. This is done to improve the sparsity of ILU decomposition.
! In case of column permutation, the solution needs to be permutated
! In case of row permulation, the rhs needs to be permutated

INTERFACE
MODULE SUBROUTINE mField_reversePermutation( obj, rhs, sol )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  CLASS( AbstractNodeField_ ), TARGET, INTENT( INOUT ) :: rhs
  CLASS( AbstractNodeField_ ), TARGET, INTENT( INOUT ) :: sol
END SUBROUTINE mField_reversePermutation
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine sets data to matrix field
!
!# Introduction
! IF `addContribution` and `scale` is absent then:
!
! - This subroutine sets a block of data to matrix.
! - This block data is contained in `val(:,:)`
! - The sized of val should be tdof * size( globalNode )
! - `globalNode` contains the node number
! - storageFMT is the storage format of val(:,:), it can be DOF_FMT, or NODES_FMT.
!
! If `addContribution` and `scale` are present  then:
! This subroutine adds a block of data to matrix.
! This block data is contained in `val(:,:)`
! The sized of val should be tdof * size( globalNode )
! `globalNode` contains the node number
! storageFMT is the storage format of val(:,:), it can be DOF_FMT, or NODES_FMT.
! scale is scaling used for val.

INTERFACE
MODULE SUBROUTINE mField_set1( obj, globalNode, val, storageFMT, scale, addContribution )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode(:)
  REAL( DFP ), INTENT( IN ) :: val(:,:)
  INTEGER( I4B ), INTENT( IN ) :: storageFMT
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE mField_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine sets data to matrix field
!
!# Introduction
! IF `addContribution` and `scale` is absent then:
! This subroutine sets a scalar value `val` to all or selected the entries of the matrix.
! If `globalNode` is present then this routine sets a scalar value `val` to selected the entries of the matrix. These entries are spacified by the `globalNode(:)` vector, which denotes the global node numbers. symbolically it does the following: `obj(glocalNode)=val`
! If `globalNode` is absent then all entries are set to the scalar values. Symbolically it does the following: `obj=val`
!
! IF `addContribution` and `scale` not present:
! IF globalNode is not present then:
! This subroutine adds a scalar value `val` to all the entries of the matrix
! symbolically it does the following: `obj=obj+scale*val`
! If globalNode is present then:
! This subroutine adds a scalar value `val` to selected the entries of the matrix. These entries are spacified by the `globalNode(:)` vector, which denotes the global node numbers.
! symbolically it does the following:
! `obj(glocalNode)=obj(globalNode)+scale*val`

INTERFACE
MODULE SUBROUTINE mField_set2( obj, globalNode, val, scale, addContribution )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: globalNode(:)
  REAL( DFP ), INTENT( IN ) :: val
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE mField_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 July 2021
! summary: This routine sets data to matrix field
!
!# Introduction
! If addContribution and scale not present then:
! This subroutine sets a scalar value `val` to a single entry of the matrix. This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`, `colNodeNum` and `colDOF`.
!
! If addContribution and scale present then:
!
! This subroutine adds a scalar value `val` to a single entry of the matrix. This entry is specified by the `rowNodeNum` and `colNodeNum`.
! The exact location of the entry is computed using `rowNodeNum`, `rowDOF`, `colNodeNum` and `colDOF`.

INTERFACE
MODULE SUBROUTINE mField_set3( obj, rowNodeNum, colNodeNum, rowDOF, colDOF, &
  & val, scale, addContribution )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: rowNodeNum
  INTEGER( I4B ), INTENT( IN ) :: colNodeNum
  INTEGER( I4B ), INTENT( IN ) :: rowDOF
  INTEGER( I4B ), INTENT( IN ) :: colDOF
  REAL( DFP ), INTENT( IN ) :: val
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE mField_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRow@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the row of a sparse matrix
!
!# Introduction
! This routine sets the row of a sparse matrix. The row index is calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
MODULE SUBROUTINE mField_setRow( obj, globalNode, idof, scalarVal, vecVal, &
  & nodeFieldVal )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scalarVal
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: vecVal( : )
  CLASS( AbstractNodeField_ ), OPTIONAL, INTENT( IN ) :: nodeFieldVal
END SUBROUTINE mField_setRow
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setColumn@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 July 2021
! summary: This routine set the column of a sparse matrix
!
!# Introduction
! This routine sets the column of a sparse matrix. The column index is calculated using the `globalNode` and `idof`.
! - `globalNode` is global node number.
! - `idof` is the degree of freedom number
! - `scalarVal` is the scalar value, if present then the row will be set to this scalar value
! - `vectorVal` is the vector value, if present then the row will be set to this vector value
! - `nodeFieldVal` is the field of nodal values

INTERFACE
MODULE SUBROUTINE mField_setColumn( obj, globalNode, idof, scalarVal, &
  & vecVal, nodeFieldVal )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scalarVal
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: vecVal( : )
  CLASS( AbstractNodeField_ ), OPTIONAL, INTENT( IN ) :: nodeFieldVal
END SUBROUTINE mField_setColumn
END INTERFACE

END MODULE MatrixField_Class