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
! date: 27 June 2021
! summary: This data type is designed for distributed parallel computing

MODULE DistributedVector_Class
USE GlobalData
USE BaseType
USE Vector_Class
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE FPL, ONLY: ParameterList_
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "DISTRIBUTEDVECTOR_CLASS"

!----------------------------------------------------------------------------
!                                                        DistributedVector_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Data type to handle distributed parallel computing using MPI
!
!{!pages/DistributedVector.md}

TYPE, EXTENDS( Vector_ ) :: DistributedVector_
  PRIVATE
  LOGICAL( LGT ) :: isAssembled = .TRUE.
  INTEGER( I4B ) :: commID = -1
  INTEGER( I4B ) :: myID = -1
  INTEGER( I4B ) :: globalSize = 0
  INTEGER( I4B ) :: chunkSize = 0
  CONTAINS
  PRIVATE
  PROCEDURE, PASS( obj ) :: checkEssentialParam => distVec_checkEssentialParam
  PROCEDURE, PUBLIC, PASS( obj ) :: initiate => distVec_initiate
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => distVec_initiate
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => distVec_DeallocateData
  FINAL :: distVec_Final
  PROCEDURE, PASS( obj ) :: set1 => distVec_set1
  PROCEDURE, PASS( obj ) :: set2 => distVec_set2
  PROCEDURE, PASS( obj ) :: set3 => distVec_set3
  PROCEDURE, PASS( obj ) :: set4 => distVec_set4
  PROCEDURE, PASS( obj ) :: set5 => distVec_set5
  PROCEDURE, PASS( obj ) :: set6 => distVec_set6
  PROCEDURE, PASS( obj ) :: set7 => distVec_set7

  PROCEDURE, PASS( obj ) :: get1 => distVec_get1
  PROCEDURE, PASS( obj ) :: get2 => distVec_get2
  PROCEDURE, PASS( obj ) :: get3 => distVec_get3
  PROCEDURE, PASS( obj ) :: get4 => distVec_get4
  PROCEDURE, PASS( obj ) :: get5 => distVec_get5
END TYPE DistributedVector_

PUBLIC :: DistributedVector_
TYPE( DistributedVector_ ), PARAMETER, PUBLIC :: TypeDistributedVector = &
  & DistributedVector_()

!----------------------------------------------------------------------------
!                                                 DistributedVectorPointer_
!----------------------------------------------------------------------------

TYPE :: DistributedVectorPointer_
  CLASS( DistributedVector_ ), POINTER :: ptr => NULL()
END TYPE DistributedVectorPointer_

PUBLIC :: DistributedVectorPointer_


!----------------------------------------------------------------------------
!                                           checkEssentialParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.

INTERFACE
MODULE SUBROUTINE distVec_checkEssentialParam( obj, param )
  CLASS( DistributedVector_ ), INTENT( IN ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE distVec_checkEssentialParam
END INTERFACE

PUBLIC :: distVec_checkEssentialParam

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the vector_ object
!
!### Introduction
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
MODULE SUBROUTINE distVec_Initiate( obj, param )
  CLASS( DistributedVector_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE distVec_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine deallocates the data stored inside the Vector_ obj

INTERFACE
MODULE SUBROUTINE distVec_DeallocateData( obj )
  CLASS( DistributedVector_ ), INTENT( INOUT ) :: obj
END SUBROUTINE distVec_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE distVec_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE distVec_Final( obj )
  TYPE( Vector_ ), INTENT( INOUT ) :: obj
END SUBROUTINE distVec_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Vector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: 	This function returns an instance of [[Vector_]]

INTERFACE
MODULE FUNCTION distVec_Constructor1( param ) RESULT( Ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Vector_ ) :: ans
END FUNCTION distVec_Constructor1
END INTERFACE

INTERFACE RealVector
  MODULE PROCEDURE distVec_Constructor1
END INTERFACE RealVector

PUBLIC :: RealVector

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: 	This function returns an instance of [[Vector_]]

INTERFACE
MODULE FUNCTION distVec_Constructor_1( param ) RESULT( Ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  CLASS( DistributedVector_ ), POINTER :: ans
END FUNCTION distVec_Constructor_1
END INTERFACE

INTERFACE RealVector_Pointer
  MODULE PROCEDURE distVec_Constructor_1
END INTERFACE RealVector_Pointer

PUBLIC :: RealVector_Pointer

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[Vector_]]

INTERFACE
MODULE SUBROUTINE distVec_Display( obj, msg, unitNo )
  CLASS( DistributedVector_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE distVec_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the single entry of the vector

INTERFACE
MODULE SUBROUTINE distVec_set1( obj, indx, value )
  CLASS( DistributedVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE distVec_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets all the entries of a vector

INTERFACE
MODULE SUBROUTINE distVec_set2( obj, value )
  CLASS( DistributedVector_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE distVec_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all the entries by using given vector

INTERFACE
MODULE SUBROUTINE distVec_set3( obj, value )
  CLASS( DistributedVector_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE distVec_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
MODULE SUBROUTINE distVec_set4(obj, indx, value)
  CLASS( DistributedVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE distVec_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
MODULE SUBROUTINE distVec_set5( obj, iStart, iEnd, stride, value )
  CLASS( DistributedVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE distVec_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: set the vector values using triplet

INTERFACE
MODULE SUBROUTINE distVec_set6( obj, istart, iend, stride, value )
  CLASS( DistributedVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE distVec_set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: set the selected values of vector

INTERFACE
MODULE SUBROUTINE distVec_set7( obj, nodeNum, value, storageFMT, dofs )
  CLASS( DistributedVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: storageFMT
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dofs(:)
END SUBROUTINE distVec_set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                            get@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Returns the single entry of the vector

INTERFACE
MODULE FUNCTION distVec_get1( obj, indx ) RESULT( ans )
  CLASS( DistributedVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
  REAL( DFP ) :: ans
END FUNCTION distVec_get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            get@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Returns all the entries of the vector

INTERFACE
MODULE MODULE FUNCTION distVec_get2( obj ) RESULT( ans )
  CLASS( DistributedVector_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE:: ans( : )
END FUNCTION distVec_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            get@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Returns selected values of vector

INTERFACE
MODULE FUNCTION distVec_get3( obj, indx ) RESULT( ans )
  CLASS( DistributedVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx( : )
  REAL( DFP ) :: ans( SIZE( indx ) )
END FUNCTION distVec_get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                            get@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Get values using triplets

INTERFACE
MODULE FUNCTION distVec_get4( obj, istart, iend, stride ) RESULT( ans )
  CLASS( DistributedVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION distVec_get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                            get@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Get values using triplets

INTERFACE
MODULE FUNCTION distVec_get5( obj, nodeNum, storageFMT, dofs ) RESULT( ans )
  CLASS( DistributedVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodeNum( : )
  INTEGER( I4B ), INTENT( IN ) :: storageFMT
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dofs(:)
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION distVec_get5
END INTERFACE

END MODULE DistributedVector_Class
