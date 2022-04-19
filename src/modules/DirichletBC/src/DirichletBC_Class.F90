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

MODULE DirichletBC_Class
USE GlobalData
USE BaseType
USE String_Class
USE ExceptionHandler_Class
USE MeshSelection_Class
USE Domain_Class
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
USE AbstractBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName="DirichletBC_Class"
TYPE( ExceptionHandler_ ) ::  e

!----------------------------------------------------------------------------
!                                                               DirichletBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This is an abstract data type for boundary conditions

TYPE, EXTENDS( AbstractBC_ ) :: DirichletBC_
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => bc_addSurrogate
  PROCEDURE, PUBLIC, PASS( obj ) :: checkEssentialParam => &
    & bc_checkEssentialParam
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => bc_Initiate
  PROCEDURE, PUBLIC, PASS( obj ) :: Deallocate => bc_Deallocate
  FINAL :: bc_Final
  PROCEDURE, PUBLIC, PASS( obj ) :: Import => bc_Import
  PROCEDURE, PUBLIC, PASS( obj ) :: Export => bc_Export
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => bc_Display
  PROCEDURE, PUBLIC, PASS( obj ) :: Set => bc_Set
END TYPE DirichletBC_

PUBLIC :: DirichletBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: DirichletBCPointer_
  CLASS( DirichletBC_ ), POINTER :: ptr => NULL()
END TYPE DirichletBCPointer_

PUBLIC :: DirichletBCPointer_

!----------------------------------------------------------------------------
!                                            addSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bc_addSurrogate( obj, userObj )
  CLASS( DirichletBC_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: userObj
END SUBROUTINE bc_addSurrogate
END INTERFACE

INTERFACE DirichletBCAddSurrogate
  MODULE PROCEDURE bc_addSurrogate
END INTERFACE DirichletBCAddSurrogate

PUBLIC :: DirichletBCAddSurrogate

!----------------------------------------------------------------------------
!                                    checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bc_checkEssentialParam( obj, param )
  CLASS( DirichletBC_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE bc_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                       setDirichletParam@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE setDirichletBCParam( param, name, idof, nodalValueType, &
  & useFunction )
  TYPE( ParameterList_ ), INTENT( INOUT ) :: param
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ), INTENT( IN ) :: idof
  INTEGER( I4B ), INTENT( IN ) :: nodalValueType
    !! Space
    !! Time
    !! SpaceTime
    !! Constant
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: useFunction
END SUBROUTINE setDirichletBCParam
END INTERFACE

PUBLIC :: setDirichletBCParam

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bc_Initiate( obj, param, boundary, dom )
  CLASS( DirichletBC_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( MeshSelection_ ), INTENT( IN ) :: boundary
  CLASS( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE bc_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bc_Deallocate( obj )
  CLASS( DirichletBC_ ), INTENT( INOUT ) :: obj
END SUBROUTINE bc_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bc_Final( obj )
  TYPE( DirichletBC_ ), INTENT( INOUT ) :: obj
END SUBROUTINE bc_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bc_Import( obj, hdf5, group, dom )
  CLASS( DirichletBC_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  CLASS( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE bc_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bc_Export( obj, hdf5, group )
  CLASS( DirichletBC_ ), INTENT( IN ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END SUBROUTINE bc_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bc_Display( obj, msg, unitNo )
  CLASS( DirichletBC_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE bc_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bc_Set( obj, ConstantNodalValue, SpaceNodalValue, &
  & TimeNodalValue, SpaceTimeNodalValue, SpaceFunction, TimeFunction, &
  & SpaceTimeFunction )
  CLASS( DirichletBC_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: ConstantNodalValue
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: SpaceNodalValue( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: TimeNodalValue( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: SpaceTimeNodalValue( :, : )
  PROCEDURE( iface_SpaceTimeFunction ), POINTER, OPTIONAL, INTENT( IN ) :: &
    & SpaceTimeFunction
  PROCEDURE( iface_SpaceFunction ), POINTER, OPTIONAL, INTENT( IN ) :: &
    & SpaceFunction
  PROCEDURE( iface_TimeFunction ), POINTER, OPTIONAL, INTENT( IN ) :: &
    & TimeFunction
END SUBROUTINE bc_Set
END INTERFACE

END MODULE DirichletBC_Class