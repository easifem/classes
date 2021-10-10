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
USE FPL, ONLY : ParameterList_
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                     AbstractMatrixField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This abstract class is defined to handle the finite element matrix
!
!{!page/AbstractMatrixField_.md!}

TYPE, ABSTRACT, EXTENDS( AbstractField_ ) :: AbstractMatrixField_
  LOGICAL( LGT ) :: isPmatInitiated = .FALSE.
    !! True if precondition matrix is initiated
  CONTAINS
  PRIVATE
  PROCEDURE( amField_Size ), DEFERRED, PUBLIC, PASS( obj ) :: Size
  PROCEDURE( amField_Shape ), DEFERRED, PUBLIC, PASS( obj ) :: Shape
  PROCEDURE( amField_Matvec1 ), DEFERRED, PASS( obj ) :: Matvec1
    !! Matrix vector multiplication, here vector is fortran array
  PROCEDURE( amField_Matvec2 ), DEFERRED, PASS( obj ) :: Matvec2
    !! Matrix vector multiplication, here vector is AbstractNodeField_
  GENERIC, PUBLIC :: Matvec => Matvec1, Matvec2
  PROCEDURE( amField_LUSOLVE1 ), DEFERRED, PASS( obj ) :: LUSOLVE1
    !! Matrix vector multiplication, here vector is fortran array
  PROCEDURE( amField_LUSOLVE2 ), DEFERRED, PASS( obj ) :: LUSOLVE2
    !! Matrix vector multiplication, here vector is AbstractNodeField_
  GENERIC, PUBLIC :: LUSOLVE => LUSOLVE1, LUSOLVE2
    !! Generic LU Solve
  PROCEDURE, PUBLIC, PASS( obj ) :: isPreconditionSet => &
    & amField_isPreconditionSet
    !! True if prcondition is set
  PROCEDURE( amField_setPrecondition ), DEFERRED, PUBLIC, PASS( obj ) :: &
    & setPrecondition
    !! Build precondition matrix
  PROCEDURE( amField_getPrecondition ), DEFERRED, PUBLIC, PASS( obj ) :: &
    & getPrecondition
    !! Get the precondition matrix
  PROCEDURE( amField_reversePermutation ), DEFERRED, PUBLIC, PASS( obj ) :: &
    & reversePermutation
  PROCEDURE( amField_set1 ), DEFERRED, PASS( obj ) :: set1
  PROCEDURE( amField_set2 ), DEFERRED, PASS( obj ) :: set2
  PROCEDURE( amField_set3 ), DEFERRED, PASS( obj ) :: set3
  GENERIC, PUBLIC :: set => set1, set2, set3
  PROCEDURE( amField_setRow ), PUBLIC, DEFERRED, PASS( obj ) :: setRow
  PROCEDURE( amField_setColumn ), PUBLIC, DEFERRED, PASS( obj ) :: setColumn
  PROCEDURE( amField_getRow ), PUBLIC, DEFERRED, PASS( obj ) :: getRow
  PROCEDURE( amField_getColumn ), PUBLIC, DEFERRED, PASS( obj ) :: getColumn
END TYPE AbstractMatrixField_

PUBLIC :: AbstractMatrixField_

!----------------------------------------------------------------------------
!                                                                      SIZE
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
FUNCTION amField_Size( obj, dim ) RESULT( ans )
  IMPORT :: AbstractMatrixField_, I4B
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dim
  INTEGER( I4B ) :: ans
END FUNCTION amField_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      SHAPE
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
FUNCTION amField_Shape( obj ) RESULT( ans )
  IMPORT :: AbstractMatrixField_, I4B
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans( 2 )
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
SUBROUTINE amField_Matvec1( obj, x, y, transp )
  IMPORT :: AbstractMatrixField_, DFP, LGT
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( INOUT ) :: y( : )
    !! Output vector y=Ax
  REAL( DFP ), INTENT( IN ) :: x( : )
    !! Input vector in y=Ax
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: transp
    !! True if we have to use TRANSPOSE of matrix
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
SUBROUTINE amField_Matvec2( obj, x, y, transp )
  IMPORT :: AbstractMatrixField_, AbstractNodeField_, LGT
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: obj
  CLASS( AbstractNodeField_ ), INTENT( INOUT ) :: y
    !! Output vector y=Ax
  CLASS( AbstractNodeField_ ), INTENT( IN ) :: x
    !! Input vector in y=Ax
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: transp
    !! True if we have to use TRANSPOSE of matrix
END SUBROUTINE amField_Matvec2
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
SUBROUTINE amField_LUSOLVE1( obj, sol, rhs, transp )
  IMPORT :: AbstractMatrixField_, DFP, LGT
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( INOUT ) :: sol( : )
    !! Output vector y=Ax
  REAL( DFP ), INTENT( IN ) :: rhs( : )
    !! Input vector in y=Ax
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: transp
END SUBROUTINE amField_LUSOLVE1
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
SUBROUTINE amField_LUSOLVE2( obj, sol, rhs, transp )
  IMPORT :: AbstractMatrixField_, AbstractNodeField_, LGT
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: obj
  CLASS( AbstractNodeField_ ), INTENT( INOUT ) :: sol
    !! Output vector
  CLASS( AbstractNodeField_ ), INTENT( IN ) :: rhs
    !! Input vector, rhs
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: transp
END SUBROUTINE amField_LUSOLVE2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           setPrecondition
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine sets the precondition

ABSTRACT INTERFACE
SUBROUTINE amField_setPrecondition( obj, param )
  IMPORT :: AbstractMatrixField_, ParameterList_
  CLASS( AbstractMatrixField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), OPTIONAL, INTENT( IN ) :: param
END SUBROUTINE amField_setPrecondition
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getPrecondition
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine returns the precondition

ABSTRACT INTERFACE
SUBROUTINE amField_getPrecondition( obj, Pmat )
  IMPORT :: AbstractMatrixField_
  CLASS( AbstractMatrixField_ ), INTENT( INOUT ) :: obj
  CLASS( AbstractMatrixField_ ), INTENT( INOUT ) :: Pmat
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
SUBROUTINE amField_reversePermutation( obj, rhs, sol )
  IMPORT :: AbstractMatrixField_, AbstractNodeField_
  CLASS( AbstractMatrixField_ ), INTENT( INOUT ) :: obj
  CLASS( AbstractNodeField_ ), TARGET, INTENT( INOUT ) :: rhs
  CLASS( AbstractNodeField_ ), TARGET, INTENT( INOUT ) :: sol
END SUBROUTINE amField_reversePermutation
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE amField_set1( obj, globalNode, val, storageFMT, scale, &
  & addContribution )
  IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
  CLASS( AbstractMatrixField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode(:)
  REAL( DFP ), INTENT( IN ) :: val(:,:)
  INTEGER( I4B ), INTENT( IN ) :: storageFMT
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE amField_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE amField_set2( obj, globalNode, val, scale, addContribution )
  IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
  CLASS( AbstractMatrixField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: globalNode(:)
  REAL( DFP ), INTENT( IN ) :: val
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE amField_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           set@setMethod
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE amField_set3( obj, rowNodeNum, colNodeNum, rowDOF, colDOF, val, &
  & scale, addContribution )
  IMPORT :: AbstractMatrixField_, I4B, DFP, LGT
  CLASS( AbstractMatrixField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: rowNodeNum
  INTEGER( I4B ), INTENT( IN ) :: colNodeNum
  INTEGER( I4B ), INTENT( IN ) :: rowDOF
  INTEGER( I4B ), INTENT( IN ) :: colDOF
  REAL( DFP ), INTENT( IN ) :: val
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE amField_set3
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
SUBROUTINE amField_setRow( obj, globalNode, idof, scalarVal, vecVal, &
  & nodeFieldVal )
  IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
  CLASS( AbstractMatrixField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scalarVal
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: vecVal( : )
  CLASS( AbstractNodeField_ ), OPTIONAL, INTENT( IN ) :: nodeFieldVal
END SUBROUTINE amField_setRow
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
SUBROUTINE amField_setColumn( obj, globalNode, idof, scalarVal, vecVal, &
  & nodeFieldVal )
  IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP
  CLASS( AbstractMatrixField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scalarVal
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: vecVal( : )
  CLASS( AbstractNodeField_ ), OPTIONAL, INTENT( IN ) :: nodeFieldVal
END SUBROUTINE amField_setColumn
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
! If `val` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the row is returned inside the node field

ABSTRACT INTERFACE
SUBROUTINE amField_getRow( obj, globalNode, idof, val, nodeFieldVal, &
  & scale, addContribution )
  IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), OPTIONAL, INTENT( INOUT ) :: val( : )
  CLASS( AbstractNodeField_ ), OPTIONAL, INTENT( INOUT ) :: nodeFieldVal
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE amField_getRow
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
! If `val` is present then the vector is returned inside the rank 1 vector
! If `nodeFieldVal` is present then the column is returned inside the node
! field

ABSTRACT INTERFACE
SUBROUTINE amField_getColumn( obj, globalNode, idof, val, nodeFieldVal, &
  & scale, addContribution )
  IMPORT :: AbstractMatrixField_, AbstractNodeField_, I4B, DFP, LGT
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), OPTIONAL, INTENT( INOUT ) :: val( : )
  CLASS( AbstractNodeField_ ), OPTIONAL, INTENT( INOUT ) :: nodeFieldVal
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: scale
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: addContribution
END SUBROUTINE amField_getColumn
END INTERFACE


!----------------------------------------------------------------------------
!                                                 isPreconditionSet@Methods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION amField_isPreconditionSet( obj ) RESULT( Ans )
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION amField_isPreconditionSet
END INTERFACE


END MODULE AbstractMatrixField_Class