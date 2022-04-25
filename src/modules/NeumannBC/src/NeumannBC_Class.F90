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

MODULE NeumannBC_Class
USE GlobalData
USE BaseType
USE String_Class
USE ExceptionHandler_Class
USE MeshSelection_Class
USE Domain_Class
USE HDF5File_Class
USE FPL, ONLY: ParameterList_
USE AbstractBC_Class
USE DirichletBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName="NeumannBC_CLASS"
TYPE( ExceptionHandler_ ) ::  e

!----------------------------------------------------------------------------
!                                                               NeumannBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This is an abstract data type for boundary conditions

TYPE, EXTENDS( DirichletBC_ ) :: NeumannBC_
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
END TYPE NeumannBC_

PUBLIC :: NeumannBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: NeumannBCPointer_
  CLASS( NeumannBC_ ), POINTER :: ptr => NULL()
END TYPE NeumannBCPointer_

PUBLIC :: NeumannBCPointer_

!----------------------------------------------------------------------------
!                                          addSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Feb 2022
! summary: Add surrogate to NeumannBC_Class module

INTERFACE
MODULE SUBROUTINE bc_addSurrogate( obj, userObj )
  CLASS( NeumannBC_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: userObj
END SUBROUTINE bc_addSurrogate
END INTERFACE

INTERFACE NeumannBCAddSurrogate
  MODULE PROCEDURE bc_addSurrogate
END INTERFACE NeumannBCAddSurrogate

PUBLIC :: NeumannBCAddSurrogate

!----------------------------------------------------------------------------
!                                      checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Feb 2022
! summary: Check essential parameters

INTERFACE
MODULE SUBROUTINE bc_checkEssentialParam( obj, param )
  CLASS( NeumannBC_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE bc_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                      setDirichletParam@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE setNeumannBCParam( param, name, idof, nodalValueType, &
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
END SUBROUTINE setNeumannBCParam
END INTERFACE

PUBLIC :: setNeumannBCParam

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bc_Initiate( obj, param, boundary, dom )
  CLASS( NeumannBC_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( MeshSelection_ ), INTENT( IN ) :: boundary
  CLASS( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE bc_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bc_Deallocate( obj )
  CLASS( NeumannBC_ ), INTENT( INOUT ) :: obj
END SUBROUTINE bc_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bc_Final( obj )
  TYPE( NeumannBC_ ), INTENT( INOUT ) :: obj
END SUBROUTINE bc_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bc_Import( obj, hdf5, group, dom )
  CLASS( NeumannBC_ ), INTENT( INOUT ) :: obj
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
  CLASS( NeumannBC_ ), INTENT( IN ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END SUBROUTINE bc_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE bc_Display( obj, msg, unitNo )
  CLASS( NeumannBC_ ), INTENT( IN ) :: obj
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
  CLASS( NeumannBC_ ), INTENT( INOUT ) :: obj
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

END MODULE NeumannBC_Class