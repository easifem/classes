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
! summary: This module defines a concretre class called MatrixField_, which can handle the finite element matrix field. This is a native implementation of finite element tangent matrices.

MODULE MatrixField_Class
USE GlobalData
USE BaseType
USE FPL, ONLY: ParameterList_
USE FPL_Method
USE ExceptionHandler_Class
USE AbstractField_Class
USE AbstractMatrixField_Class
USE Domain_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "MATRIXFIELD_CLASS"
TYPE( ExceptionHandler_ ) :: e
INTEGER( I4B ), PARAMETER :: eUnitNo = 1010
CHARACTER( LEN = * ), PARAMETER :: eLogFile = "MATRIXFIELD_CLASS_EXCEPTION.txt"

!----------------------------------------------------------------------------
!                                                              MatrixField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: This datatype can handle the finite element matrix field. This is a native implementation of finite element tangent matrices.
!
!### Introduction
!
! This datatype is native implementation of finite element tangent matrix. This data type is sequential, and it uses Sparsekit library for sparse matrix manipulations.
!

TYPE, EXTENDS( AbstractMatrixField_ ) :: MatrixField_
  CONTAINS
  PRIVATE
    PROCEDURE, PUBLIC, PASS( obj ) :: checkEssentialParam => mField_checkEssentialParam
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate1 => mField_Initiate1
      !! Initiate from the parameter list
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate2 => mField_Initiate2
      !! Initiate by copying other object
    PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => mField_DeallocateData
      !! Deallocate the field
    PROCEDURE, PUBLIC, PASS( obj ) :: Display => mField_Display
      !! Display the field
END TYPE MatrixField_

PUBLIC :: MatrixField_

!----------------------------------------------------------------------------
!                                           checkEssentialParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.
!
!### Introduction
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
!                                                                 Initiate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field
!
!### Introduction
! This routine initiates the the matrix field. It uses parameter list, and dom.
! - Param contains both essential and optional parameters which are used in constructing the matrix field
! - dom is a pointer to a domain, where we are interested in constructing the matrix
!
! Details of essential parameters are given below
! Details of optional parameters are given below

INTERFACE
MODULE SUBROUTINE mField_Initiate1( obj, param, dom )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE mField_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine initiates the Matrix Field
!
!### Introduction
! This routine initiates the the matrix field by copying other field. In this way we try to minimize the computation effort.
!
! Default behavior:
!
! - If `copyFull, copyStructure, usePointer` are absent then this subroutine, copy the value of the matrix, however, it will be not allocate the [[CSRSparsity_]] object field of [[CSRMatrix_]]. It will simply point to the obj2%mat%csr. In this way, we do not have to create multiple sparsity on the same domain.
!
! - `copyFull=.TRUE., copyStructure=.TRUE., usePointer=.TRUE.`, then default behavior
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
!                                                                 Display
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mField_Display( obj, msg, unitNo )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE mField_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mField_DeallocateData( obj )
  CLASS( MatrixField_ ), INTENT( INOUT ) :: obj
END SUBROUTINE mField_DeallocateData
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MatrixField_Class