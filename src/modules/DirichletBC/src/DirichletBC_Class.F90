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
CHARACTER( LEN = * ), PARAMETER :: modName="DIRICHLETBC_CLASS"
TYPE( ExceptionHandler_ ) ::  e

!----------------------------------------------------------------------------
!                                                               DirichletBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This is an abstract data type for boundary conditions

TYPE, EXTENDS( AbstractBC_ ) :: DirichletBC_
  INTEGER( I4B ) :: idof = 0
  REAL( DFP ), ALLOCATABLE :: nodalValue( :, : )
  INTEGER( I4B ) :: nodalValueType = -1
    !! Constant, Space, Time, SpaceTime
  LOGICAL( LGT ) :: useFunction = .FALSE.
  PROCEDURE( iface_SpaceTimeFunction ), POINTER, NOPASS :: &
    & SpaceTimeFunction => NULL()
  PROCEDURE( iface_SpaceFunction ), POINTER, NOPASS :: &
    & SpaceFunction => NULL()
  PROCEDURE( iface_TimeFunction ), POINTER, NOPASS :: &
    & TimeFunction => NULL()
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => dbc_addSurrogate
  PROCEDURE, PUBLIC, PASS( obj ) :: checkEssentialParam => &
    & dbc_checkEssentialParam
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => dbc_Initiate
  PROCEDURE, PUBLIC, PASS( obj ) :: Deallocate => dbc_Deallocate
  FINAL :: dbc_Final
  PROCEDURE, PUBLIC, PASS( obj ) :: Import => dbc_Import
  PROCEDURE, PUBLIC, PASS( obj ) :: Export => dbc_Export
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => dbc_Display
  PROCEDURE, PUBLIC, PASS( obj ) :: Set => dbc_Set
  PROCEDURE, PUBLIC, PASS( obj ) :: Get => dbc_Get
  PROCEDURE, PUBLIC, PASS( obj ) :: getDOFNo => dbc_getDOFNo
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
!                                          addSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE dbc_addSurrogate( obj, userObj )
  CLASS( DirichletBC_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: userObj
END SUBROUTINE dbc_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                      checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE dbc_checkEssentialParam( obj, param )
  CLASS( DirichletBC_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE dbc_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                      setDirichletParam@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE setDirichletBCParam( param, name, idof, nodalValueType, &
  & useFunction )
  TYPE( ParameterList_ ), INTENT( INOUT ) :: param
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ), INTENT( IN ) :: idof
  INTEGER( I4B ), INTENT( IN ) :: nodalValueType
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: useFunction
END SUBROUTINE setDirichletBCParam
END INTERFACE

PUBLIC :: setDirichletBCParam

!----------------------------------------------------------------------------
!                                             addSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE addSurrogate_DirichletBC( userObj )
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: userObj
END SUBROUTINE addSurrogate_DirichletBC
END INTERFACE

PUBLIC :: addSurrogate_DirichletBC

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE dbc_Initiate( obj, param, boundary, dom )
  CLASS( DirichletBC_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( MeshSelection_ ), INTENT( IN ) :: boundary
  CLASS( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE dbc_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE dbc_Deallocate( obj )
  CLASS( DirichletBC_ ), INTENT( INOUT ) :: obj
END SUBROUTINE dbc_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE dbc_Final( obj )
  TYPE( DirichletBC_ ), INTENT( INOUT ) :: obj
END SUBROUTINE dbc_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE dbc_Import( obj, hdf5, group, dom )
  CLASS( DirichletBC_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  CLASS( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE dbc_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE dbc_Export( obj, hdf5, group )
  CLASS( DirichletBC_ ), INTENT( IN ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END SUBROUTINE dbc_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE dbc_Display( obj, msg, unitNo )
  CLASS( DirichletBC_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE dbc_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE dbc_Set( obj, ConstantNodalValue, SpaceNodalValue, &
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
END SUBROUTINE dbc_Set
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE dbc_Get( obj, NodeNum, NodalValue, times )
  CLASS( DirichletBC_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: NodeNum( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: NodalValue( :, : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: times( : )
END SUBROUTINE dbc_Get
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getDOFNo@getMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION dbc_getDOFNo( obj ) RESULT( Ans )
  CLASS( DirichletBC_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION dbc_getDOFNo
END INTERFACE


END MODULE DirichletBC_Class