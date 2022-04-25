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

!> authors: Vikas Sharma, Ph. D.
! date: 28 June 2021
! summary: Scalar field data type is defined

MODULE ScalarField_Class
USE GlobalData
USE String_Class
USE BaseType
USE AbstractField_Class
USE AbstractNodeField_Class
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
USE DirichletBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "ScalarField_Class"
TYPE( ExceptionHandler_ ) :: e

!----------------------------------------------------------------------------
!                                                              ScalarField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Native vector type
!
!{!pages/ScalarField.md}

TYPE, EXTENDS( AbstractNodeField_ ) :: ScalarField_
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => sField_addSurrogate
  PROCEDURE, PUBLIC, PASS( obj ) :: checkEssentialParam => &
    & sField_checkEssentialParam
  PROCEDURE, PUBLIC, PASS( obj ) :: initiate1 => sField_initiate1
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => sField_Display
  PROCEDURE, PUBLIC, PASS( obj ) :: Deallocate => sField_Deallocate
  FINAL :: sField_Final
  PROCEDURE, PASS( obj ) :: set1 => sField_set1
    !! set single entry
  PROCEDURE, PASS( obj ) :: set2 => sField_set2
    !! set all values to a scalar values
  PROCEDURE, PASS( obj ) :: set3 => sField_set3
    !! set all values to a given vector
  PROCEDURE, PASS( obj ) :: set4 => sField_set4
    !! set selected values to given scalar
  PROCEDURE, PASS( obj ) :: set5 => sField_set5
    !! set selected values to given vector
  PROCEDURE, PASS( obj ) :: set6 => sField_set6
    !! set values to a scalar by using triplet
  PROCEDURE, PASS( obj ) :: set7 => sField_set7
    !! set values to a vector by using triplet
  PROCEDURE, PASS( obj ) :: set8 => sField_set8
    !! This method is used for assignment operator
  PROCEDURE, PASS( obj ) :: set9 => sField_set9
    !! Set selected values using FEVariable
  GENERIC, PUBLIC :: set => set1, set2, set3, set4, &
    & set5, set6, set7, set8, set9
  GENERIC, PUBLIC :: ASSIGNMENT(=) => set8
    !! set values to a vector
  PROCEDURE, PASS( obj ) :: get1 => sField_get1
    !! get single entry
  PROCEDURE, PASS( obj ) :: get2 => sField_get2
    !! get all values in Real vector
  PROCEDURE, PASS( obj ) :: get3 => sField_get3
    !! get selected values
  PROCEDURE, PASS( obj ) :: get4 => sField_get4
    !! get values from triplet
  PROCEDURE, PASS( obj ) :: get5 => sField_get5
    !! get selected values in FEVariable
  GENERIC, PUBLIC :: get => get1, get2, get3, get4, get5
  !! get the entries of scalar field
  PROCEDURE, PASS( obj ) :: sField_applyDirichletBC1
  PROCEDURE, PASS( obj ) :: sField_applyDirichletBC2
  GENERIC, PUBLIC :: applyDirichletBC => &
    & sField_applyDirichletBC1, &
    & sField_applyDirichletBC2
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: Import => sField_Import
  PROCEDURE, PUBLIC, PASS( obj ) :: Export => sField_Export
END TYPE ScalarField_

PUBLIC :: ScalarField_
TYPE( ScalarField_ ), PARAMETER, PUBLIC :: Type = ScalarField_(domains=NULL())

!----------------------------------------------------------------------------
!                                                       ScalarFieldPointer_
!----------------------------------------------------------------------------

TYPE :: ScalarFieldPointer_
  CLASS( ScalarField_ ), POINTER :: ptr => NULL()
END TYPE ScalarFieldPointer_

PUBLIC :: ScalarFieldPointer_

!----------------------------------------------------------------------------
!                                           setScalarFieldParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Sept 2021
! summary: Set the essential parameters

INTERFACE
MODULE SUBROUTINE setScalarFieldParam( param, name, fieldType )
  TYPE( ParameterList_ ), INTENT( INOUT ) :: param
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: fieldType

END SUBROUTINE setScalarFieldParam
END INTERFACE

PUBLIC :: setScalarFieldParam

!----------------------------------------------------------------------------
!                                                 addSurrogate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.

INTERFACE
MODULE SUBROUTINE sField_addSurrogate( obj, UserObj )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: UserObj
END SUBROUTINE sField_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                           checkEssentialParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.

INTERFACE
MODULE SUBROUTINE sField_checkEssentialParam( obj, param )
  CLASS( ScalarField_ ), INTENT( IN ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE sField_checkEssentialParam
END INTERFACE

PUBLIC :: sField_checkEssentialParam

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the ScalarField_ object
!
!# Introduction
!
! This routine initiate the [[ScalarField_]] object.
! `param` contains the information of parameters required to initiate the
! scalar field. There are essential and optional information.
! Essential information are described below.

INTERFACE
MODULE SUBROUTINE sField_Initiate1( obj, param, dom )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE sField_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine deallocates the data stored inside the
! `ScalarField_` obj

INTERFACE
MODULE SUBROUTINE sField_Deallocate( obj )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
END SUBROUTINE sField_Deallocate
END INTERFACE

INTERFACE Deallocate
  MODULE PROCEDURE sField_Deallocate
END INTERFACE Deallocate

PUBLIC :: Deallocate

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE sField_Final( obj )
  TYPE( ScalarField_ ), INTENT( INOUT ) :: obj
END SUBROUTINE sField_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Vector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: 	This function returns an instance of [[ScalarField_]]

INTERFACE
MODULE FUNCTION sField_Constructor1( param, dom ) RESULT( Ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
  TYPE( ScalarField_ ) :: ans
END FUNCTION sField_Constructor1
END INTERFACE

INTERFACE ScalarField
  MODULE PROCEDURE sField_Constructor1
END INTERFACE ScalarField

PUBLIC :: ScalarField

!----------------------------------------------------------------------------
!                                                 ScalarField_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: 	This function returns an instance of [[ScalarField_]]

INTERFACE
MODULE FUNCTION sField_Constructor_1( param, dom ) RESULT( Ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
  CLASS( ScalarField_ ), POINTER :: ans
END FUNCTION sField_Constructor_1
END INTERFACE

INTERFACE ScalarField_Pointer
  MODULE PROCEDURE sField_Constructor_1
END INTERFACE ScalarField_Pointer

PUBLIC :: ScalarField_Pointer

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[ScalarField_]]

INTERFACE
MODULE SUBROUTINE sField_Display( obj, msg, unitNo )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE sField_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Import@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
MODULE SUBROUTINE sField_Import( obj, hdf5, group, dom, domains )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  TYPE( Domain_ ), TARGET, OPTIONAL, INTENT( IN ) :: dom
  TYPE( DomainPointer_ ), TARGET, OPTIONAL, INTENT( IN ) :: domains(:)
END SUBROUTINE sField_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Export@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE
MODULE SUBROUTINE sField_Export( obj, hdf5, group )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END SUBROUTINE sField_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the single entry of the scalar field

INTERFACE
MODULE SUBROUTINE sField_set1( obj, globalNode, value )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE sField_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets all the entries of a scalar field

INTERFACE
MODULE SUBROUTINE sField_set2( obj, value )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE sField_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all the entries by using given scalar field

INTERFACE
MODULE SUBROUTINE sField_set3( obj, value )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE sField_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
MODULE SUBROUTINE sField_set4(obj, globalNode, value)
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE sField_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
MODULE SUBROUTINE sField_set5(obj, globalNode, value)
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE sField_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
MODULE SUBROUTINE sField_set6( obj, istart, iend, stride, value )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE sField_set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: set the vector vals using triplet

INTERFACE
MODULE SUBROUTINE sField_set7( obj, istart, iend, stride, value )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE sField_set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: used for assignment operator

INTERFACE
MODULE SUBROUTINE sField_set8( obj, obj2 )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  CLASS( ScalarField_ ), INTENT( IN ) :: obj2
END SUBROUTINE sField_set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries using [[FEVariable_]]

INTERFACE
MODULE SUBROUTINE sField_set9(obj, globalNode, value)
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
  TYPE(FEVariable_), INTENT( IN ) :: value
  !! Scalar, Nodal, FEVariable (Space or Constant)
END SUBROUTINE sField_set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the single entry of the scalar field

INTERFACE
MODULE SUBROUTINE sField_get1( obj, value, globalNode )
  CLASS( ScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( INOUT ) :: value
  INTEGER( I4B ), INTENT( IN ) :: globalNode
END SUBROUTINE sField_get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns all the entries by using given scalar field

INTERFACE
MODULE SUBROUTINE sField_get2( obj, value )
  CLASS( ScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value( : )
END SUBROUTINE sField_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
MODULE SUBROUTINE sField_get3(obj, value, globalNode )
  CLASS( ScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
END SUBROUTINE sField_get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: returns the value using triplet

INTERFACE
MODULE SUBROUTINE sField_get4( obj, value, istart, iend, stride )
  CLASS( ScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
END SUBROUTINE sField_get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: returns the selected values in FEVariable

INTERFACE
MODULE SUBROUTINE sField_get5( obj, value, globalNode )
  CLASS( ScalarField_ ), INTENT( IN ) :: obj
  TYPE(FEVariable_), INTENT( INOUT ) :: value
  !! Scalar Nodal FEVariable
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
END SUBROUTINE sField_get5
END INTERFACE

!----------------------------------------------------------------------------
!                                               applyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
MODULE SUBROUTINE sField_applyDirichletBC1( obj, dbc )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  CLASS( DirichletBC_ ), INTENT( IN ) :: dbc
END SUBROUTINE sField_applyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               applyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
MODULE SUBROUTINE sField_applyDirichletBC2( obj, dbc )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  CLASS( DirichletBCPointer_ ), INTENT( IN ) :: dbc(:)
END SUBROUTINE sField_applyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ScalarField_Class
