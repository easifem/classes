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
! summary: This module defines [[BlockMatrixField_]] class
!
!# Introduction
!
! - This module defines [[BlockMatrixField_]] class
! - It is designed for handling the block tangent matrix in FEM
!
!@note
! [[BlockMatrixField_]] uses `NATIVE_SERIAL` engine for handling the
! global tangent matrices.
!@endnote
!
!@todo
! Add getting-started manual
!@endtodo

MODULE BlockMatrixField_Class
USE GlobalData
USE BaseType
USE FPL, ONLY: ParameterList_
USE FPL_Method
USE HDF5File_Class
USE ExceptionHandler_Class
USE AbstractField_Class
USE AbstractNodeField_Class
USE AbstractMatrixField_Class
USE MatrixField_Class
USE Domain_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "BlockMatrixField_Class"
TYPE( ExceptionHandler_ ) :: e

!----------------------------------------------------------------------------
!                                                          BlockMatrixField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: This is native implementation of finite element tangent matrices.
!
!{!pages/BlockMatrixField_.md!}

TYPE, EXTENDS( MatrixField_ ) :: BlockMatrixField_
  CONTAINS
  PRIVATE
    PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => mField_addSurrogate
    PROCEDURE, PUBLIC, PASS( obj ) :: checkEssentialParam => &
      & bmField_checkEssentialParam
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate1 => mField_Initiate1
      !! Initiate from the parameter list
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate2 => mField_Initiate2
      !! Initiate by copying other object
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate3 => mField_Initiate3
      !! Initiate for block matrices
    PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => mField_DeallocateData
      !! Deallocate the field
    PROCEDURE, PUBLIC, PASS( obj ) :: Import => mField_Import
      !! Import from hdf5 file
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
    PROCEDURE, PUBLIC, PASS( obj ) :: setPrecondition => &
      & mField_setPrecondition
      !! Building precondition matrix
    PROCEDURE, PUBLIC, PASS( obj ) :: getPrecondition => &
      & mField_getPrecondition
      !! Get the precondition matrix
    PROCEDURE, PUBLIC, PASS( obj ) :: reversePermutation => &
      & mField_reversePermutation
    PROCEDURE, PASS( obj ) :: set1 => mField_set1
    PROCEDURE, PASS( obj ) :: set2 => mField_set2
    PROCEDURE, PASS( obj ) :: set3 => mField_set3
    PROCEDURE, PUBLIC, PASS( obj ) :: setRow => mField_setRow
    PROCEDURE, PUBLIC, PASS( obj ) :: setColumn => mField_setColumn
    PROCEDURE, PUBLIC, PASS( obj ) :: getRow => mField_getRow
    PROCEDURE, PUBLIC, PASS( obj ) :: getColumn => mField_getColumn
      !! SPY
END TYPE BlockMatrixField_

PUBLIC :: BlockMatrixField_

TYPE( BlockMatrixField_ ), PARAMETER, PUBLIC :: TypeBlockMatrixField = &
  & BlockMatrixField_(domains=NULL())

!----------------------------------------------------------------------------
!                                            addSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine adds surrogate to module's [[ExceptionHandler_]]

INTERFACE
MODULE SUBROUTINE mField_addSurrogate( obj, UserObj )
  CLASS( BlockMatrixField_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: UserObj
END SUBROUTINE mField_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                setBlockMatrixFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2021
! summary: This routine sets the parameter for creating [[BlockMatrixField_]]
!
!# Introduction
!
! - This routine sets the essential parameter for initiating
! an instance of [[BlockMatrixField_]]
! - The results will be returned in `param`
! - After getting `param` user can call [[BlockMatrixField_:Initiate]] method
!
!@note
! The size of `physicalVarNames`, `spaceCompo`, and `timeCompo` should be
! the same.
!@endnote
!
!
!## Usage
!
!```fortran
! PROGRAM main
!   USE easifemBase
!   USE easifemClasses
!   IMPLICIT NONE
!   !
!   ! [[ParameterList_]], [[BlockMatrixField_]]
!   !
!   TYPE( BlockMatrixField_ ) :: obj
!   TYPE( ParameterList_ ) :: param
!   !> main
!   CALL FPL_INIT(); CALL param%Initiate()
!   ! #BlockMatrixField/SetBlockMatrixFieldParam
!   CALL SetBlockMatrixFieldParam(param=param, name="K",  &
!     & physicalVarNames=["V", "P"], spaceCompo=[2, 1], &
!     & timeCompo=[1,1], fieldType=FIELD_TYPE_NORMAL,  &
!     & matrixProp="UNSYM" )
!   ! #BlockMatrixField/CheckEssentialParam
!   CALL obj%CheckEssentialParam( param )
!   CALL param%Print()
!   CALL param%DeallocateData(); CALL FPL_FINALIZE()
! END PROGRAM main
!```

INTERFACE
MODULE SUBROUTINE setBlockMatrixFieldParam( param, name, matrixProp, &
  & physicalVarNames, spaceCompo, timeCompo, fieldType )
  TYPE( ParameterList_ ), INTENT( INOUT ) :: param
    !! Options to create [[BlockMatrixField_]] will be stored in this
  CHARACTER( LEN = * ), INTENT( IN ) :: name
    !! Name of the matrix field
  CHARACTER( LEN = * ), INTENT( IN ) :: matrixProp
    !! Matrix property, "SYM" or "UNSYM"
  CHARACTER( LEN = * ), INTENT( IN ) :: physicalVarNames( : )
    !! Name of physical variables
  INTEGER( I4B ), INTENT( IN ) :: spaceCompo( : )
    !! Number of space-components in each physicalVarNames, see [[DOF_]]
  INTEGER( I4B ), INTENT( IN ) :: timeCompo( : )
    !! Number of time-components in each physicalVarNames, see [[DOF_]]
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: fieldType
    !! fieldType can be following
    !! FIELD_TYPE_NORMAL <-- DEFAULT
    !! FIELD_TYPE_CONSTANT
    !! FIELD_TYPE_CONSTANT_SPACE
    !! FIELD_TYPE_CONSTANT_TIME
END SUBROUTINE setBlockMatrixFieldParam
END INTERFACE

PUBLIC :: setBlockMatrixFieldParam

!----------------------------------------------------------------------------
!                                     checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.
!
!# Introduction
!
! This routine check the essential parameters required to the initiate the
! [[BlockMatrixField_]] data type.

INTERFACE
MODULE SUBROUTINE bmField_checkEssentialParam( obj, param )
  CLASS( BlockMatrixField_ ), INTENT( IN ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE bmField_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                          DeallocateData@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine deallocates the data stored inside the matrix

INTERFACE
MODULE SUBROUTINE mField_DeallocateData( obj )
  CLASS( BlockMatrixField_ ), INTENT( INOUT ) :: obj
END SUBROUTINE mField_DeallocateData
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field
!
!# Introduction
!
! This routine initiates an instance of [[BlockMatrixField_]].
! The options/arguments to initiate the matrix field are
! contained inside param, which is an instance of [[ParameterList_]].
! In addition, [[Domain_]] `dom` is target to the pointer
! [[AbstractField_:domain]] and [[AbstractField_::domains]]
!
! - `param` contains both essential and optional parameters which are used in
! constructing the matrix field
! - `dom` is a pointer to a domain
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
!
!### Usage
!
!```fortran
! type( domain_ ) :: dom
! type( BlockMatrixField_ ) :: obj
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
! call setBlockMatrixFieldParam( param, "K", "UNSYM", 3, 2,
! FIELD_TYPE_NORMAL )
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
  CLASS( BlockMatrixField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE mField_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field
!
!# Introduction
!
! This routine initiates the `obj` [[BlockMatrixField_]] by copying contents
! from `obj2`, an instance of chid class of [[AbstractField_]].
! In this way we try to minimize the computation effort.
!
!@note
! If `copyFull, copyStructure, usePointer` are absent then this subroutine,
! copies the value of the matrix from obj2 to obj.
!@endnote
!
!@note
! However, in [[BlockMatrixField_:mat]], it will not allocate space for
! [[CSRSparsity_]] field of
! [[CSRMatrix_]], that is [[CSRMatrix_:CSR]] field of
! [[BlockMatrixField_:mat]].
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
MODULE SUBROUTINE mField_Initiate2( obj, obj2, copyFull, copyStructure, &
  & usePointer )
  CLASS( BlockMatrixField_ ), INTENT( INOUT ) :: obj
  CLASS( AbstractField_ ), INTENT( INOUT ) :: obj2
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: copyFull
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: copyStructure
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: usePointer
END SUBROUTINE mField_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field
!
!### Usage
!

INTERFACE
MODULE SUBROUTINE mField_Initiate3( obj, param, dom )
  CLASS( BlockMatrixField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( DomainPointer_ ), TARGET, INTENT( IN ) :: dom( : )
END SUBROUTINE mField_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content of matrix field from hdf5file

INTERFACE
MODULE SUBROUTINE mField_Import( obj, hdf5, group, dom, domains )
  CLASS( BlockMatrixField_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  TYPE( Domain_ ), TARGET, OPTIONAL, INTENT( IN ) :: dom
  TYPE( DomainPointer_ ), TARGET, OPTIONAL, INTENT( IN ) :: domains( : )
END SUBROUTINE mField_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Size@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine returns the SIZE of the matrix

INTERFACE
MODULE FUNCTION mField_Size( obj, dim ) RESULT( ans )
  CLASS( BlockMatrixField_ ), INTENT( IN ) :: obj
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
  CLASS( BlockMatrixField_ ), INTENT( IN ) :: obj
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
  CLASS( BlockMatrixField_ ), INTENT( IN ) :: obj
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
  CLASS( BlockMatrixField_ ), INTENT( IN ) :: obj
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
  CLASS( BlockMatrixField_ ), INTENT( IN ) :: obj
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
  CLASS( BlockMatrixField_ ), INTENT( IN ) :: obj
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
! Note that sol should be allocated by the user, and size of sol should be
! same as the size of rhs
!
!@note
! LU matrix is stored inside the object in Modified Sparse Row format
! This form of matrix is obtained by incomplete LU decomposition type precodnitioners
!@endnote

INTERFACE
MODULE SUBROUTINE mField_LUSOLVE1( obj, sol, rhs, transp )
  CLASS( BlockMatrixField_ ), INTENT( IN ) :: obj
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
  CLASS( BlockMatrixField_ ), INTENT( IN ) :: obj
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
  CLASS( BlockMatrixField_ ), INTENT( INOUT ) :: obj
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
  CLASS( BlockMatrixField_ ), INTENT( INOUT ) :: obj
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
  CLASS( BlockMatrixField_ ), INTENT( INOUT ) :: obj
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
  CLASS( BlockMatrixField_ ), INTENT( INOUT ) :: obj
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
  CLASS( BlockMatrixField_ ), INTENT( INOUT ) :: obj
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
  CLASS( BlockMatrixField_ ), INTENT( INOUT ) :: obj
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
  CLASS( BlockMatrixField_ ), INTENT( INOUT ) :: obj
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
  CLASS( BlockMatrixField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scalarVal
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: vecVal( : )
  CLASS( AbstractNodeField_ ), OPTIONAL, INTENT( IN ) :: nodeFieldVal
END SUBROUTINE mField_setColumn
END INTERFACE

END MODULE BlockMatrixField_Class