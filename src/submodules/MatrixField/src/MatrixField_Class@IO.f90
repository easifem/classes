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
! date: 16 July 2021
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE( MatrixField_Class ) IO
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Display
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Display
  INTEGER( I4B ) :: I
  I = INPUT( option=unitNo, default=stdout )
  IF( .NOT. obj%isInitiated ) THEN
    CALL Display( "Matrix Field is not initiated", unitNo=I )
    RETURN
  END IF
  CALL Display( msg, unitNo = I )
  CALL Display( obj%name//'',  msg="# Field Name : ", unitNo=I )
  CALL Display( obj%fieldType, msg='# Field Type : ', unitNo=I )
  IF( ASSOCIATED( obj%domain ) ) THEN
    CALL Display( "# Domain is associated in Matrix field", unitNo=I )
  ELSE
    CALL Display( "# Domain is NOT associated in Matrix field", unitNo=I )
  END IF
  CALL Display( obj%fieldType, msg='# Field Type : ', unitNo=I )
  CALL Display( obj%mat, msg="SparseMatrix in Matrix Field : ", unitNo=I )
END PROCEDURE mField_Display

END SUBMODULE IO