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

MODULE Vector_Class
USE GlobalData
USE BaseType
USE AbstractVector_Class
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE FPL, ONLY: ParameterList_
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "VECTOR_CLASS"
TYPE( ExceptionHandler_ ), PUBLIC :: eVector
INTEGER( I4B ), PARAMETER :: eUnitNo = 1004
CHARACTER( LEN = * ), PARAMETER :: eLogFile = "VECTOR_CLASS.txt"

!----------------------------------------------------------------------------
!                                                                 Vector_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Native vector type
!
!{!pages/Vector.md}

TYPE, EXTENDS( AbstractVector_ ) :: Vector_
  PRIVATE
  TYPE( RealVector_ ) :: RealVec
  TYPE( DOF_ ) :: dof
  CONTAINS
  PRIVATE
  PROCEDURE, PASS( obj ) :: checkEssentialParam => vec_checkEssentialParam
  PROCEDURE, PUBLIC, PASS( obj ) :: initiate => vec_initiate
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => vec_Display
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => vec_DeallocateData
  FINAL :: vec_Final
  PROCEDURE, PASS( obj ) :: set1 => vec_set1
  PROCEDURE, PASS( obj ) :: set2 => vec_set2
  PROCEDURE, PASS( obj ) :: set3 => vec_set3
  PROCEDURE, PASS( obj ) :: set4 => vec_set4
  PROCEDURE, PASS( obj ) :: set5 => vec_set5
  PROCEDURE, PASS( obj ) :: set6 => vec_set6
  PROCEDURE, PASS( obj ) :: set7 => vec_set7

  PROCEDURE, PASS( obj ) :: get1 => vec_get1
  PROCEDURE, PASS( obj ) :: get2 => vec_get2
  PROCEDURE, PASS( obj ) :: get3 => vec_get3
  PROCEDURE, PASS( obj ) :: get4 => vec_get4
  PROCEDURE, PASS( obj ) :: get5 => vec_get5

END TYPE Vector_

PUBLIC :: Vector_
TYPE( Vector_ ), PARAMETER, PUBLIC :: TypeVector = Vector_()

!----------------------------------------------------------------------------
!                                                             VectorPointer_
!----------------------------------------------------------------------------

TYPE :: VectorPointer_
  CLASS( Vector_ ), POINTER :: ptr => NULL()
END TYPE VectorPointer_

PUBLIC :: VectorPointer_

!----------------------------------------------------------------------------
!                                           checkEssentialParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.

INTERFACE
MODULE SUBROUTINE vec_checkEssentialParam( obj, param )
  CLASS( Vector_ ), INTENT( IN ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE vec_checkEssentialParam
END INTERFACE

PUBLIC :: vec_checkEssentialParam

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the vector_ object
!
!# Introduction
! This routine initiate the vector object.
! `param` contains the information of parameters required to initiate the
! vector. There are essential and optional information.
! Essential information are described below.
!
!#### `tNodes`
! this variable can either be a scalar or a vector of integer.
! If it is a scalar then it means there is only one physical variable (let say velocity).
! If it is a vector then it means there are several physical variable such as velocity, displacement, pressure. The size of tNodes then represent the total number of physical variables. Also, `tNodes(i)` denotes the total nodes for physical variable 1.
!
!#### `names`
! This is second essential variable. It denotes the name of physical variable such as `V` for velocity and `U` for displacement.
! Only a single character can be used to define the name of physical variable
! To define name of several variables use comman separater values, for example `U, V, W` denotes the `U` and `V` and `W`.
! Note that the total number of names should be same as the total number of physical variables present inside the vector.

INTERFACE
MODULE SUBROUTINE vec_Initiate( obj, param )
  CLASS( Vector_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE vec_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine deallocates the data stored inside the Vector_ obj

INTERFACE
MODULE SUBROUTINE vec_DeallocateData( obj )
  CLASS( Vector_ ), INTENT( INOUT ) :: obj
END SUBROUTINE vec_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE vec_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE vec_Final( obj )
  TYPE( Vector_ ), INTENT( INOUT ) :: obj
END SUBROUTINE vec_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Vector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: 	This function returns an instance of [[Vector_]]

INTERFACE
MODULE FUNCTION vec_Constructor1( param ) RESULT( Ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Vector_ ) :: ans
END FUNCTION vec_Constructor1
END INTERFACE

INTERFACE RealVector
  MODULE PROCEDURE vec_Constructor1
END INTERFACE RealVector

PUBLIC :: RealVector

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: 	This function returns an instance of [[Vector_]]

INTERFACE
MODULE FUNCTION vec_Constructor_1( param ) RESULT( Ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  CLASS( Vector_ ), POINTER :: ans
END FUNCTION vec_Constructor_1
END INTERFACE

INTERFACE RealVector_Pointer
  MODULE PROCEDURE vec_Constructor_1
END INTERFACE RealVector_Pointer

PUBLIC :: RealVector_Pointer

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[Vector_]]

INTERFACE
MODULE SUBROUTINE vec_Display( obj, msg, unitNo )
  CLASS( Vector_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE vec_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the single entry of the vector

INTERFACE
MODULE SUBROUTINE vec_set1( obj, indx, value )
  CLASS( Vector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE vec_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets all the entries of a vector

INTERFACE
MODULE SUBROUTINE vec_set2( obj, value )
  CLASS( Vector_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE vec_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all the entries by using given vector

INTERFACE
MODULE SUBROUTINE vec_set3( obj, value )
  CLASS( Vector_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE vec_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
MODULE SUBROUTINE vec_set4(obj, indx, value)
  CLASS( Vector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE vec_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
MODULE SUBROUTINE vec_set5( obj, iStart, iEnd, stride, value )
  CLASS( Vector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE vec_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: set the vector values using triplet

INTERFACE
MODULE SUBROUTINE vec_set6( obj, istart, iend, stride, value )
  CLASS( Vector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE vec_set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: set the selected values of vector

INTERFACE
MODULE SUBROUTINE vec_set7( obj, nodeNum, value, storageFMT, dofs )
  CLASS( Vector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: storageFMT
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dofs(:)
END SUBROUTINE vec_set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                            get@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Returns the single entry of the vector

INTERFACE
MODULE FUNCTION vec_get1( obj, indx ) RESULT( ans )
  CLASS( Vector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
  REAL( DFP ) :: ans
END FUNCTION vec_get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            get@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Returns all the entries of the vector

INTERFACE
MODULE MODULE FUNCTION vec_get2( obj ) RESULT( ans )
  CLASS( Vector_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE:: ans( : )
END FUNCTION vec_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            get@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Returns selected values of vector

INTERFACE
MODULE FUNCTION vec_get3( obj, indx ) RESULT( ans )
  CLASS( Vector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx( : )
  REAL( DFP ) :: ans( SIZE( indx ) )
END FUNCTION vec_get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                            get@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Get values using triplets

INTERFACE
MODULE FUNCTION vec_get4( obj, istart, iend, stride ) RESULT( ans )
  CLASS( Vector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION vec_get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                            get@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Get values using triplets

INTERFACE
MODULE FUNCTION vec_get5( obj, nodeNum, storageFMT, dofs ) RESULT( ans )
  CLASS( Vector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum( : )
  INTEGER( I4B ), INTENT( IN ) :: storageFMT
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dofs(:)
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION vec_get5
END INTERFACE

END MODULE Vector_Class