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

MODULE AbstractField_Class
USE GlobalData
USE BaseType
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE FPL, ONLY: ParameterList_
USE Domain_Class
IMPLICIT NONE
PRIVATE

INTEGER( I4B ), PARAMETER, PUBLIC :: FIELD_TYPE_NORMAL = 0
INTEGER( I4B ), PARAMETER, PUBLIC :: FIELD_TYPE_CONSTANT = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: FIELD_TYPE_CONSTANT_SPACE = 2
INTEGER( I4B ), PARAMETER, PUBLIC :: FIELD_TYPE_CONSTANT_TIME = 3

!----------------------------------------------------------------------------
!                                                           AbstractField_
!----------------------------------------------------------------------------

TYPE, ABSTRACT :: AbstractField_
  LOGICAL( LGT ) :: isInitiated = .FALSE.
  INTEGER( I4B ) :: tSize = 0
  INTEGER( I4B ) :: fieldType = FIELD_TYPE_NORMAL
  TYPE( RealVector_ ) :: realVec
  TYPE( DOF_ ) :: dof
  TYPE( Domain_ ), POINTER :: domain => NULL()
  CONTAINS
  PRIVATE
    PROCEDURE(aField_Initiate), DEFERRED, PUBLIC, PASS( obj ) :: initiate
      !! Initiate the field
    PROCEDURE(aField_DeallocateData), DEFERRED, PUBLIC, PASS( obj ) :: DeallocateData
      !! Deallocate the field
    PROCEDURE(aField_Display), DEFERRED, PUBLIC, PASS( obj ) :: Display
      !! Display the field
END TYPE AbstractField_

PUBLIC :: AbstractField_

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aField_Initiate( obj, param, dom )
  IMPORT :: AbstractField_, ParameterList_, Domain_
  CLASS( AbstractField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE aField_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aField_Display( obj, msg, unitNo )
  IMPORT :: AbstractField_, I4B
  CLASS( AbstractField_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE aField_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE aField_DeallocateData( obj )
  IMPORT :: AbstractField_
  CLASS( AbstractField_ ), INTENT( INOUT ) :: obj
END SUBROUTINE aField_DeallocateData
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractField_Class