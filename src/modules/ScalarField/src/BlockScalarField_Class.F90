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

MODULE BlockScalarField_Class
USE GlobalData
USE BaseType
USE AbstractField_Class
USE AbstractNodeField_Class
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "BLOCKSCALARFIELD_CLASS"
TYPE( ExceptionHandler_ ) :: e
INTEGER( I4B ), PARAMETER :: eUnitNo = 1004
CHARACTER( LEN = * ), PARAMETER :: eLogFile = "BLOCKSCALARFIELD_CLASS_EXCEPTION.txt"

!----------------------------------------------------------------------------
!                                                              ScalarField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Native vector type
!
!{!pages/ScalarField.md}

TYPE, EXTENDS( AbstractNodeField_ ) :: BlockScalarField_
  CONTAINS
  PRIVATE
  PROCEDURE, PASS( obj ) :: checkEssentialParam => sField_checkEssentialParam
  PROCEDURE, PUBLIC, PASS( obj ) :: initiate1 => sField_initiate1
  PROCEDURE, PUBLIC, PASS( obj ) :: initiate2 => sField_initiate2
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => sField_Display
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => sField_DeallocateData
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
  GENERIC, PUBLIC :: set => set1, set2, set3, set4, set5, set6, set7
    !! set values to a vector
  PROCEDURE, PASS( obj ) :: get1 => sField_get1
    !! get single entry
  PROCEDURE, PASS( obj ) :: get2 => sField_get2
    !! get all values to a scalar values
  PROCEDURE, PASS( obj ) :: get3 => sField_get3
    !! get all values to a given vector
  PROCEDURE, PASS( obj ) :: get4 => sField_get4
    !! get selected values to given scalar
  GENERIC, PUBLIC :: get => get1, get2, get3, get4
    !! get the entries of scalar field
  PROCEDURE, PUBLIC, PASS( obj ) :: Import => sField_Import
  PROCEDURE, PUBLIC, PASS( obj ) :: Export => sField_Export
END TYPE BlockScalarField_

PUBLIC :: ScalarField_
TYPE( ScalarField_ ), PARAMETER, PUBLIC :: Type = ScalarField_()

!----------------------------------------------------------------------------
!                                                             ScalarFieldPointer_
!----------------------------------------------------------------------------

TYPE :: ScalarFieldPointer_
  CLASS( ScalarField_ ), POINTER :: ptr => NULL()
END TYPE ScalarFieldPointer_

PUBLIC :: ScalarFieldPointer_

!----------------------------------------------------------------------------
!                                           setScalarFieldParam@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE setScalarFieldParam( param, name, fieldType )
  TYPE( ParameterList_ ), INTENT( INOUT ) :: param
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: fieldType
END SUBROUTINE setScalarFieldParam
END INTERFACE

PUBLIC :: setScalarFieldParam

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
!### Introduction
! This routine initiate the [[ScalarField_]] object.
! `param` contains the information of parameters required to initiate the
! scalar field. There are essential and optional information.
! Essential information are described below.
!
! `CHARACTER(LEN=*) :: name` name of field
! `INTEGER(I4B) :: tdof` total degrees of freedom

INTERFACE
MODULE SUBROUTINE sField_Initiate1( obj, param, dom )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE sField_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the ScalarField_ object by copying
!
!### Introduction
! This routine initiate the [[ScalarField_]] object by copying

INTERFACE
MODULE SUBROUTINE sField_Initiate2( obj, obj2, copyFull, copyStructure, usePointer )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  CLASS( AbstractField_ ), INTENT( INOUT ) :: obj2
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: copyFull
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: copyStructure
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: usePointer
END SUBROUTINE sField_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine deallocates the data stored inside the ScalarField_ obj

INTERFACE
MODULE SUBROUTINE sField_DeallocateData( obj )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
END SUBROUTINE sField_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE sField_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

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
MODULE SUBROUTINE sField_Import( obj, hdf5, group )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
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
MODULE SUBROUTINE sField_set1( obj, globalNode, val )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  REAL( DFP ), INTENT( IN ) :: val
END SUBROUTINE sField_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets all the entries of a scalar field

INTERFACE
MODULE SUBROUTINE sField_set2( obj, val )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: val
END SUBROUTINE sField_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all the entries by using given scalar field

INTERFACE
MODULE SUBROUTINE sField_set3( obj, val )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: val( : )
END SUBROUTINE sField_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
MODULE SUBROUTINE sField_set4(obj, globalNode, val)
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
  REAL( DFP ), INTENT( IN ) :: val
END SUBROUTINE sField_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
MODULE SUBROUTINE sField_set5(obj, globalNode, val)
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
  REAL( DFP ), INTENT( IN ) :: val( : )
END SUBROUTINE sField_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
MODULE SUBROUTINE sField_set6( obj, istart, iend, stride, val )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
  REAL( DFP ), INTENT( IN ) :: val
END SUBROUTINE sField_set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: set the vector vals using triplet

INTERFACE
MODULE SUBROUTINE sField_set7( obj, istart, iend, stride, val )
  CLASS( ScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
  REAL( DFP ), INTENT( IN ) :: val( : )
END SUBROUTINE sField_set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the single entry of the scalar field

INTERFACE
MODULE SUBROUTINE sField_get1( obj, val, globalNode )
  CLASS( ScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ) :: val
  INTEGER( I4B ), INTENT( IN ) :: globalNode
END SUBROUTINE sField_get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all the entries by using given scalar field

INTERFACE
MODULE SUBROUTINE sField_get2( obj, val )
  CLASS( ScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: val( : )
END SUBROUTINE sField_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
MODULE SUBROUTINE sField_get3(obj, val, globalNode )
  CLASS( ScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: val( : )
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
END SUBROUTINE sField_get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: set the vector vals using triplet

INTERFACE
MODULE SUBROUTINE sField_get4( obj, val, istart, iend, stride )
  CLASS( ScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: val( : )
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
END SUBROUTINE sField_get4
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BlockScalarField_Class