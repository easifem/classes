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

MODULE MatrixField_Class
USE GlobalData
USE BaseType
USE FPL, ONLY: ParameterList_
USE AbstractField_Class
USE Domain_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                     AbstractMatrixField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This abstract class is defined to handle finite element matrix

TYPE, ABSTRACT, EXTENDS( AbstractField_ ) :: AbstractMatrixField_
  TYPE( CSRMatrix_ ) :: mat
  TYPE( CSRSparsity_ ), POINTER :: csr => NULL()
  CONTAINS
  PRIVATE
    PROCEDURE(aField_Initiate), DEFERRED, PUBLIC, PASS( obj ) :: Initiate
      !! Initiate the field
    PROCEDURE(aField_DeallocateData), DEFERRED, PUBLIC, PASS( obj ) :: DeallocateData
      !! Deallocate the field
    PROCEDURE(aField_Display), DEFERRED, PUBLIC, PASS( obj ) :: Display
      !! Display the field
END TYPE AbstractMatrixField_

PUBLIC :: AbstractMatrixField_

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aField_Initiate( obj, param, dom )
  IMPORT :: AbstractMatrixField_, ParameterList_, Domain_
  CLASS( AbstractMatrixField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE aField_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aField_Display( obj, msg, unitNo )
  IMPORT :: AbstractMatrixField_, I4B
  CLASS( AbstractMatrixField_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE aField_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aField_DeallocateData( obj )
  IMPORT :: AbstractMatrixField_
  CLASS( AbstractMatrixField_ ), INTENT( INOUT ) :: obj
END SUBROUTINE aField_DeallocateData
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractMatrixField_Class