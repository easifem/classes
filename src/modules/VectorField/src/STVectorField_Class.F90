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
! summary: STVector field data type is defined

MODULE STVectorField_Class
USE GlobalData
USE BaseType
USE AbstractField_Class
USE AbstractNodeField_Class
USE ScalarField_Class, ONLY: ScalarField_
USE VectorField_Class, ONLY: VectorField_
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "STVECTORFIELD_CLASS"
TYPE( ExceptionHandler_ ) :: e

!----------------------------------------------------------------------------
!                                                              STVectorField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: STVector field
!
!{!pages/STVectorField.md}

TYPE, EXTENDS( AbstractNodeField_ ) :: STVectorField_
  INTEGER( I4B ) :: spaceCompo = 0_I4B
  INTEGER( I4B ) :: timeCompo = 0_I4B
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => stvField_addSurrogate
  PROCEDURE, PUBLIC, PASS( obj ) :: checkEssentialParam => stvField_checkEssentialParam
  PROCEDURE, PUBLIC, PASS( obj ) :: initiate1 => stvField_initiate1
  PROCEDURE, PUBLIC, PASS( obj ) :: initiate2 => stvField_initiate2
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => stvField_Display
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => stvField_DeallocateData
  FINAL :: stvField_Final
  PROCEDURE, PASS( obj ) :: set1 => stvField_set1
    !! set single entry
  PROCEDURE, PASS( obj ) :: set2 => stvField_set2
    !! set all values to a STVector values
  PROCEDURE, PASS( obj ) :: set3 => stvField_set3
    !! set all values to a given STvector
  PROCEDURE, PASS( obj ) :: set4 => stvField_set4
    !! set selected values to given STVector
  PROCEDURE, PASS( obj ) :: set5 => stvField_set5
    !! set selected values to given STvector
  PROCEDURE, PASS( obj ) :: set6 => stvField_set6
    !! set values to a STVector by using triplet
  PROCEDURE, PASS( obj ) :: set7 => stvField_set7
    !! set values to a STvector by using triplet
  PROCEDURE, PASS( obj ) :: set8 => stvField_set8
    !! set values to a STvector by using triplet
  PROCEDURE, PASS( obj ) :: set9 => stvField_set9
    !! set values to a STvector by using triplet
  PROCEDURE, PASS( obj ) :: set10 => stvField_set10
    !! set values to a STvector by using triplet
  PROCEDURE, PASS( obj ) :: set11 => stvField_set11
    !! set values to a STvector by using triplet
  PROCEDURE, PASS( obj ) :: set12 => stvField_set12
    !! set values to a STvector by using triplet
  GENERIC, PUBLIC :: set => set1, set2, set3, set4, set5, set6, &
    & set7, set8, set9, set10, set11, set12

  PROCEDURE, PASS( obj ) :: get1 => stvField_get1
  PROCEDURE, PASS( obj ) :: get2 => stvField_get2
  PROCEDURE, PASS( obj ) :: get3 => stvField_get3
  PROCEDURE, PASS( obj ) :: get4 => stvField_get4
  PROCEDURE, PASS( obj ) :: get5 => stvField_get5
  PROCEDURE, PASS( obj ) :: get6 => stvField_get6
  PROCEDURE, PASS( obj ) :: get7 => stvField_get7
  PROCEDURE, PASS( obj ) :: get8 => stvField_get8
  GENERIC, PUBLIC :: get => get1, get2, get3, get4, get5, get6, get7, get8
    !! get the entries of STVector field
  PROCEDURE, PASS( obj ) :: getPointerOfComponent => stvField_getPointerOfComponent
  PROCEDURE, PUBLIC, PASS( obj ) :: Import => stvField_Import
  PROCEDURE, PUBLIC, PASS( obj ) :: Export => stvField_Export
END TYPE STVectorField_

PUBLIC :: STVectorField_
TYPE( STVectorField_ ), PARAMETER, PUBLIC :: Type = STVectorField_()

!---------------------------------------------------------------------------
!                                                     STVectorFieldPointer_
!----------------------------------------------------------------------------

TYPE :: STVectorFieldPointer_
  CLASS( STVectorField_ ), POINTER :: ptr => NULL()
END TYPE STVectorFieldPointer_

PUBLIC :: STVectorFieldPointer_

!----------------------------------------------------------------------------
!                                         setSTVectorFieldParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: set essential parameter

INTERFACE
MODULE SUBROUTINE setSTVectorFieldParam( param, name, spaceCompo, &
  & timeCompo, fieldType )
  TYPE( ParameterList_ ), INTENT( INOUT ) :: param
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ), INTENT( IN ) :: spaceCompo
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: fieldType
END SUBROUTINE setSTVectorFieldParam
END INTERFACE

PUBLIC :: setSTVectorFieldParam

!----------------------------------------------------------------------------
!                                                 addSurrogate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.

INTERFACE
MODULE SUBROUTINE stvField_addSurrogate( obj, UserObj )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: UserObj
END SUBROUTINE stvField_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                           checkEssentialParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.

INTERFACE
MODULE SUBROUTINE stvField_checkEssentialParam( obj, param )
  CLASS( STVectorField_ ), INTENT( IN ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE stvField_checkEssentialParam
END INTERFACE

PUBLIC :: stvField_checkEssentialParam

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the space-time vector field
!
!# Introduction
!
! This routine initiate the space-time vector field object.
! `param` contains the information of parameters required to initiate the
! this field.
! There are essential and optional information.
! Essential information are described below.
! - `name`  character defining the name of STvector field
! - `spaceCompo` is the total degree of freedom or components
! - `timeCompo` is the total degree of freedom or components
! - `fieldType` type of field type; FIELD_TYPE_CONSTANT, FIELD_TYPE_NORMAL
!
!
!### Usage
!
!```fortran
  ! type( domain_ ) :: dom
  ! type( STVectorField_ ) :: obj
  ! type( HDF5File_ ) :: meshfile
  ! type( ParameterList_ ) :: param
  ! integer( i4b ) :: ierr
  ! call display( "Testing Initiate and DeallocateData for normal data" )
  ! call FPL_INIT()
  ! call param%initiate()
  ! ierr = param%set(key="name", value="U" )
  ! ierr = param%set(key="fieldType", value=FIELD_TYPE_NORMAL)
  ! ierr = param%set(key="spaceCompo", value=3)
  ! ierr = param%set(key="timeCompo", value=3)
  ! call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  ! call meshfile%open()
  ! call dom%initiate( meshfile )
  ! call obj%initiate( param, dom )
  ! call obj%display( "space-time vector field = ")
  ! call obj%deallocateData()
  ! call dom%deallocateData()
  ! call meshfile%close()
  ! call meshfile%deallocateData()
  ! call param%deallocateData()
  ! call FPL_FINALIZE()
!```
INTERFACE
MODULE SUBROUTINE stvField_Initiate1( obj, param, dom )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE stvField_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the object by copying
!
!# Introduction
! This routine initiate the object by copying

INTERFACE
MODULE SUBROUTINE stvField_Initiate2( obj, obj2, copyFull, copyStructure, usePointer )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  CLASS( AbstractField_ ), INTENT( INOUT ) :: obj2
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: copyFull
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: copyStructure
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: usePointer
END SUBROUTINE stvField_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine deallocates the data stored inside the STVectorField_ obj

INTERFACE
MODULE SUBROUTINE stvField_DeallocateData( obj )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
END SUBROUTINE stvField_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE stvField_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE stvField_Final( obj )
  TYPE( STVectorField_ ), INTENT( INOUT ) :: obj
END SUBROUTINE stvField_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                         STVector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: 	This function returns an instance of [[STVectorField_]]

INTERFACE
MODULE FUNCTION stvField_Constructor1( param, dom ) RESULT( Ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
  TYPE( STVectorField_ ) :: ans
END FUNCTION stvField_Constructor1
END INTERFACE

INTERFACE STVectorField
  MODULE PROCEDURE stvField_Constructor1
END INTERFACE STVectorField

PUBLIC :: STVectorField

!----------------------------------------------------------------------------
!                                          STVectorField_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: 	This function returns an instance of [[STVectorField_]]

INTERFACE
MODULE FUNCTION stvField_Constructor_1( param, dom ) RESULT( Ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
  CLASS( STVectorField_ ), POINTER :: ans
END FUNCTION stvField_Constructor_1
END INTERFACE

INTERFACE STVectorField_Pointer
  MODULE PROCEDURE stvField_Constructor_1
END INTERFACE STVectorField_Pointer

PUBLIC :: STVectorField_Pointer

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[STVectorField_]]

INTERFACE
MODULE SUBROUTINE stvField_Display( obj, msg, unitNo )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE stvField_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Import@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
MODULE SUBROUTINE stvField_Import( obj, hdf5, group, dom )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE stvField_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Export@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE
MODULE SUBROUTINE stvField_Export( obj, hdf5, group )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END SUBROUTINE stvField_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the single entry of the STVector field
!
!# Introduction
! This routine sets the single entry of the STvector field. Here,
! `value` is a two dimensional array of real numbers, denoting the space-time components of the vector. The first index denotes the space components, second index denotes the time-components. As a result, total number of rows and columns in `value` are equal to the total number of spaceCompo and timeCompo.
!
! STvector( :, :, globalNode ) = value( :, : )

INTERFACE
MODULE SUBROUTINE stvField_set1( obj, globalNode, value )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  REAL( DFP ), INTENT( IN ) :: value( :, : )
END SUBROUTINE stvField_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets all the entries of a STVector field
!
!# Introduction
! This routine sets all entries of the STvector field. Here,
! `value` is a two dimensional array of real numbers, denoting the space-time components of the vector. The first index denotes the space components, second index denotes the time-components. As a result, total number of rows and columns in `value` are equal to the total number of spaceCompo and timeCompo.
!
! STvector( :, :, i ) = value( :, : ), for i = 1, tNodes

INTERFACE
MODULE SUBROUTINE stvField_set2( obj, value )
  CLASS( STVectorField_ ), TARGET, INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( :, : )
END SUBROUTINE stvField_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets all the entries of a STVector field
!
!# Introduction
! This routine sets all entries of the space-time vector field. Here `spaceCompo` and `timeCompo` are the spatial temporal components, which we want to replace by a scalar value `value`.
!
! STvector( spaceCompo, timeCompo, i ) = value, for i = 1, tNodes

INTERFACE
MODULE SUBROUTINE stvField_set3( obj, value, spaceCompo, timeCompo )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: spaceCompo
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stvField_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all the entries by using given STVector field
!
!# Introduction
! This routine set all entries of the space-time vector.
! The first index of `value` denotes the spatial components
! The second index of `value` denotes the temporal components
! The thrid index of `value` denotes the node number
!
! STvector( :, :, : ) = value( :, :, : )

INTERFACE
MODULE SUBROUTINE stvField_set4( obj, value )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( :, :, : )
END SUBROUTINE stvField_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all nodal values of a given space-time component
!
!# Introduction
!
! This routine sets all entries of the space-time vector field. Here `spaceCompo` and `timeCompo` are the spatial temporal components, which we want to replace by a vector `value`. Note that the size of `value` should be equal to the total number of nodes in the mesh.
!
! STvector( spaceCompo, timeCompo, : ) = value( : )

INTERFACE
MODULE SUBROUTINE stvField_set5( obj, value, spaceCompo, timeCompo )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: spaceCompo
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stvField_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all nodal values of a given space-time component
!
!# Introduction
!
! This routine sets all entries of the space-time vector field. Here `spaceCompo` and `timeCompo` are the spatial temporal components, which we want to replace by a vector of scalars. These vectors of scalar are stored inside a scalar field called `value`. Note that the size of `value` should be equal to the total number of nodes in the mesh.
!
! STvector( spaceCompo, : ) = value

INTERFACE
MODULE SUBROUTINE stvField_set6( obj, value, spaceCompo, timeCompo )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  TYPE( ScalarField_ ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: spaceCompo
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stvField_set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! This soubroutine sets the selected enties in space-time vector to a constant space-time nodal values. Here globalNode is the list of global node number. `value` is a rank2 array of real numbers. Its first index denotes the space component and second component denotes the time component.
!
!Effectively it does the following:
!
! STvector( :, :, globalNode ) = value( :, : ), for entries in global nodes

INTERFACE
MODULE SUBROUTINE stvField_set7(obj, value, globalNode )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
  REAL( DFP ), INTENT( IN ) :: value( :, : )
END SUBROUTINE stvField_set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! This routine sets selected entries of space-time vector field. Here globalNode contains the list of global nodes where values will be changed. `value` is a rank-3 array. Its first index denotes the space component, second index denotes the time components, and third component denotes the node number. The size of dimension should be equal to the size of globalNode.
!
! STvector( :, :, globalNode ) = value( :, :, : )

INTERFACE
MODULE SUBROUTINE stvField_set8(obj, globalNode, value)
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
  REAL( DFP ), INTENT( IN ) :: value( :, :, : )
END SUBROUTINE stvField_set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! This routine sets the selected components of selected nodes to a given value
!
! STvector( spaceCompo, globalNode ) = value( : )

INTERFACE
MODULE SUBROUTINE stvField_set9(obj, value, globalNode, spaceCompo, timeCompo)
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
  INTEGER( I4B ), INTENT( IN ) :: spaceCompo
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stvField_set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! selected components, selected nodes
!
! STvector( spaceCompo, globalNode ) = value

INTERFACE
MODULE SUBROUTINE stvField_set10(obj, value, globalNode, spaceCompo, &
  & timeCompo)
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  INTEGER( I4B ), INTENT( IN ) :: spaceCompo
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stvField_set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
MODULE SUBROUTINE stvField_set11( obj, value, istart, iend, stride )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
  REAL( DFP ), INTENT( IN ) :: value( :, : )
END SUBROUTINE stvField_set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: set the STvector values using triplet

INTERFACE
MODULE SUBROUTINE stvField_set12( obj, value, istart, iend, stride )
  CLASS( STVectorField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
END SUBROUTINE stvField_set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns values from space-time vector field
!
!# Introduction
!
! This subroutine returns the values from the space-time vector field
! The values are returned in a vector of real numbers.
!
! Here GlobalNode denotes the node number, spaceCompo is the spatial component, and timeCompo is the time component.
! - Either globalNode should be present or

INTERFACE
MODULE SUBROUTINE stvField_get1( obj, value, globalNode, spaceCompo, &
  & timeCompo )
  CLASS( STVectorField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: globalNode
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: spaceCompo
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: timeCompo
END SUBROUTINE stvField_get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine get all the entries of the space-time vector field
!
!# Introduction
! This routine returns all the nodal values of a space-time nodal vector field. Here value is a rank3 array of reals. Its first index denotes the spatial component, second index denotes the temporal component, and third index denotes the node number.

INTERFACE
MODULE SUBROUTINE stvField_get2( obj, value )
  CLASS( STVectorField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value( :, :, : )
END SUBROUTINE stvField_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the space-time nodal values of selected nodes
!
!# Introduction
! 	This routine returns the space-time nodal values of selected nodes.
! The values will be returned in rank3 array value. The first index corresponds to the spatial components, the second index corresponds to the temporal components, and the third index corresponds to the node number.
! Note that the size of third dimension of value should be equal to the size of globalNode.

INTERFACE
MODULE SUBROUTINE stvField_get3(obj, value, globalNode)
  CLASS( STVectorField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value(:,:,:)
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
END SUBROUTINE stvField_get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the nodal value of a space-time vector field
!
!# Introduction
! This routine returns the nodal values of a space-time nodal vector. In this routine we can specify the spatial and temporal component using spaceCompo and timeCompo. globalNode contains the list of global node number. Also, the values are returned in the a vector scalar `values`. Note that the length of value should be equal to the size of globalNode vector.

INTERFACE
MODULE SUBROUTINE stvField_get4(obj, value, globalNode, spaceCompo, &
  & timeCompo)
  CLASS( STVectorField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
  INTEGER( I4B ), INTENT( IN ) :: spaceCompo
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stvField_get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
MODULE SUBROUTINE stvField_get5(obj, value, globalNode, spaceCompo, &
  & timeCompo)
  CLASS( STVectorField_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( INOUT ) :: value
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  INTEGER( I4B ), INTENT( IN ) :: spaceCompo
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stvField_get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
MODULE SUBROUTINE stvField_get6( obj, value, istart, iend, stride )
  CLASS( STVectorField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value( :, :, : )
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
END SUBROUTINE stvField_get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
MODULE SUBROUTINE stvField_get7( obj, value, istart, iend, stride, &
  & spaceCompo, timeCompo )
  CLASS( STVectorField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
  INTEGER( I4B ), INTENT( IN ) :: spaceCompo
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stvField_get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the space-time value at given node number

INTERFACE
MODULE SUBROUTINE stvField_get8( obj, value, globalNode )
  CLASS( STVectorField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value( :, : )
  INTEGER( I4B ), INTENT( IN ) :: globalNode
END SUBROUTINE stvField_get8
END INTERFACE

!----------------------------------------------------------------------------
!                                         getPointerOfComponent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns pointer to a specific component

INTERFACE
MODULE FUNCTION stvField_getPointerOfComponent( obj, spaceCompo, timeCompo ) &
  & RESULT( ans )
  CLASS( STVectorField_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: spaceCompo
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
  REAL( DFP ), POINTER :: ans ( : )
END FUNCTION stvField_getPointerOfComponent
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STVectorField_Class