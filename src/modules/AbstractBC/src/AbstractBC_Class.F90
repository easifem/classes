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

MODULE AbstractBC_Class
USE GlobalData
USE BaseType
USE String_Class, ONLY:String
USE ExceptionHandler_Class
USE MeshSelection_Class
USE Domain_Class
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                                AbstractBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This is an abstract data type for boundary conditions

TYPE, ABSTRACT :: AbstractBC_
  LOGICAL( LGT ) :: isInitiated = .FALSE.
  TYPE( String ) :: name
  TYPE( MeshSelection_ ) :: boundary
  CLASS( Domain_ ), POINTER :: dom => NULL()
  CONTAINS
  PRIVATE
  PROCEDURE( abc_checkEssentialParam ), DEFERRED, PUBLIC, PASS( obj ) ::  &
    & checkEssentialParam
  PROCEDURE( abc_addSurrogate ), DEFERRED, PUBLIC, PASS( obj ) :: addSurrogate
  PROCEDURE( abc_Initiate ), DEFERRED, PUBLIC, PASS( obj ) :: Initiate
  PROCEDURE( abc_DeallocateData ), DEFERRED, PUBLIC, PASS( obj ) :: &
    & DeallocateData
  PROCEDURE( abc_Import ), DEFERRED, PUBLIC, PASS( obj ) :: Import
  PROCEDURE( abc_Export ), DEFERRED, PUBLIC, PASS( obj ) :: Export
  PROCEDURE( abc_Display ), DEFERRED, PUBLIC, PASS( obj ) :: Display
END TYPE AbstractBC_

PUBLIC :: AbstractBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractBCPointer_
  CLASS( AbstractBC_ ), POINTER :: ptr => NULL()
END TYPE AbstractBCPointer_

PUBLIC :: AbstractBCPointer_
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE abc_checkEssentialParam( obj, param )
  IMPORT
  CLASS( AbstractBC_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE abc_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE abc_addSurrogate( obj, userObj )
  IMPORT
  CLASS( AbstractBC_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: userObj
END SUBROUTINE abc_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE abc_Initiate( obj, param, boundary, dom )
  IMPORT
  CLASS( AbstractBC_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( MeshSelection_ ), INTENT( IN ) :: boundary
  CLASS( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE abc_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE abc_DeallocateData( obj )
  IMPORT
  CLASS( AbstractBC_ ), INTENT( INOUT ) :: obj
END SUBROUTINE abc_DeallocateData
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE abc_Import( obj, hdf5, group, dom )
  IMPORT
  CLASS( AbstractBC_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  CLASS( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE abc_Import
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE abc_Export( obj, hdf5, group )
  IMPORT
  CLASS( AbstractBC_ ), INTENT( IN ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END SUBROUTINE abc_Export
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
SUBROUTINE abc_Display( obj, msg, unitNo )
  IMPORT
  CLASS( AbstractBC_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE abc_Display
END INTERFACE

END MODULE AbstractBC_Class