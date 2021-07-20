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
! summary: This module defines an abstract class for finite element matrix field.

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
!### Introduction
! This abstract class is designed to handle the global tangent matrices
! defined for a computation domain. This abstract class will be extended to
!
! - [[MatrixField_]]
! - [[MPIMatrixField_]]
! - [[PetscMatrixField_]]
! - [[LISMatrixField_]]
! - [[OMPMatrixField_]]
! - [[ACCMatrixField_]]

TYPE, ABSTRACT, EXTENDS( AbstractField_ ) :: AbstractMatrixField_
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
  PROCEDURE( amField_LUTSOLVE1 ), DEFERRED, PASS( obj ) :: LUTSOLVE1
    !! Matrix vector multiplication, here vector is fortran array
  PROCEDURE( amField_LUTSOLVE2 ), DEFERRED, PASS( obj ) :: LUTSOLVE2
    !! Matrix vector multiplication, here vector is AbstractNodeField_
  GENERIC, PUBLIC :: LUTSOLVE => LUTSOLVE1, LUTSOLVE2
  PROCEDURE( amField_setPrecondition ), DEFERRED, PUBLIC, PASS( obj ) :: setPrecondition
    !! Build precondition matrix
  PROCEDURE( amField_getPrecondition ), DEFERRED, PUBLIC, PASS( obj ) :: getPrecondition
    !! Get the precondition matrix
  PROCEDURE( amField_reversePermutation ), DEFERRED, PUBLIC, PASS( obj ) :: reversePermutation
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
!### Introduction
!
! This routine returns the matrix vector multiplication. Here, input vector
! is a native fortran vector. The output vector is also a native fortran vector. It should be noted that the output vector should be allocated outside and it should have same length as the input vector.

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
!### Introduction
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
!### Introduction
!
! This routine solves (LU) sol = rhs
! sol and rhs are fortran real vector
! The LU decomposition is stored inside the AbstractMatrixField_.
! Note that sol should be allocated by the user, and size of sol should be same as the size of rhs

ABSTRACT INTERFACE
SUBROUTINE amField_LUSOLVE1( obj, sol, rhs )
  IMPORT :: AbstractMatrixField_, DFP
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( INOUT ) :: sol( : )
    !! Output vector y=Ax
  REAL( DFP ), INTENT( IN ) :: rhs( : )
    !! Input vector in y=Ax
END SUBROUTINE amField_LUSOLVE1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   LUSOLVE
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine solves (LU) sol = rhs
!
!### Introduction
!
! This routine solves (LU) sol = rhs
! sol and rhs are [[AbstractNodeField_]]
! The LU decomposition is stored inside the AbstractMatrixField_.

ABSTRACT INTERFACE
SUBROUTINE amField_LUSOLVE2( obj, sol, rhs )
  IMPORT :: AbstractMatrixField_, AbstractNodeField_
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: obj
  CLASS( AbstractNodeField_ ), INTENT( INOUT ) :: sol
    !! Output vector
  CLASS( AbstractNodeField_ ), INTENT( IN ) :: rhs
    !! Input vector, rhs
END SUBROUTINE amField_LUSOLVE2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  LUTSOLVE
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine solves (LU) sol = rhs
!
!### Introduction
!
! This routine solves (LU) sol = rhs
! sol and rhs are fortran real vector
! The LU decomposition is stored inside the AbstractMatrixField_.
! Note that sol should be allocated by the user, and size of sol should be same as the size of rhs

ABSTRACT INTERFACE
SUBROUTINE amField_LUTSOLVE1( obj, sol, rhs )
  IMPORT :: AbstractMatrixField_, DFP
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( INOUT ) :: sol( : )
    !! Output vector y=Ax
  REAL( DFP ), INTENT( IN ) :: rhs( : )
    !! Input vector in y=Ax
END SUBROUTINE amField_LUTSOLVE1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  LUTSOLVE
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: This routine solves (LU) sol = rhs
!
!### Introduction
!
! This routine solves (LU) sol = rhs
! sol and rhs are [[AbstractNodeField_]]
! The LU decomposition is stored inside the AbstractMatrixField_.

ABSTRACT INTERFACE
SUBROUTINE amField_LUTSOLVE2( obj, sol, rhs )
  IMPORT :: AbstractMatrixField_, AbstractNodeField_
  CLASS( AbstractMatrixField_ ), INTENT( IN ) :: obj
  CLASS( AbstractNodeField_ ), INTENT( INOUT ) :: sol
    !! Output vector
  CLASS( AbstractNodeField_ ), INTENT( IN ) :: rhs
    !! Input vector, rhs
END SUBROUTINE amField_LUTSOLVE2
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
  TYPE( ParameterList_ ), INTENT( IN ) :: param
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
!### Introduction
! In sparse solver, it is common to use row or column permutations. This is done to improve the sparsity of ILU decomposition.
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

END MODULE AbstractMatrixField_Class