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
! summary: STScalar field data type is defined

MODULE STScalarField_Class
USE GlobalData
USE BaseType
USE AbstractField_Class
USE AbstractNodeField_Class
USE ScalarField_Class, ONLY: ScalarField_
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "STSCALARFIELD_CLASS"
TYPE( ExceptionHandler_ ) :: e

!----------------------------------------------------------------------------
!                                                              STScalarField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: STScalar field
!
!{!pages/STScalarField.md}

TYPE, EXTENDS( AbstractNodeField_ ) :: STScalarField_
  INTEGER( I4B ) :: timeCompo = 0_I4B
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => stsField_addSurrogate
  PROCEDURE, PUBLIC, PASS( obj ) :: checkEssentialParam => stsField_checkEssentialParam
  PROCEDURE, PUBLIC, PASS( obj ) :: initiate1 => stsField_initiate1
  PROCEDURE, PUBLIC, PASS( obj ) :: initiate2 => stsField_initiate2
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => stsField_Display
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => stsField_DeallocateData
  FINAL :: stsField_Final
  PROCEDURE, PASS( obj ) :: set1 => stsField_set1
    !! set single entry
  PROCEDURE, PASS( obj ) :: set2 => stsField_set2
    !! set all values to a STScalar values
  PROCEDURE, PASS( obj ) :: set3 => stsField_set3
    !! set all values to a given STScalar
  PROCEDURE, PASS( obj ) :: set4 => stsField_set4
    !! set selected values to given STScalar
  PROCEDURE, PASS( obj ) :: set5 => stsField_set5
    !! set selected values to given STScalar
  PROCEDURE, PASS( obj ) :: set6 => stsField_set6
    !! set values to a STScalar by using triplet
  PROCEDURE, PASS( obj ) :: set7 => stsField_set7
    !! set values to a STScalar by using triplet
  PROCEDURE, PASS( obj ) :: set8 => stsField_set8
    !! set values to a STScalar by using triplet
  PROCEDURE, PASS( obj ) :: set9 => stsField_set9
    !! set values to a STScalar by using triplet
  PROCEDURE, PASS( obj ) :: set10 => stsField_set10
    !! set values to a STScalar by using triplet
  PROCEDURE, PASS( obj ) :: set11 => stsField_set11
    !! set values to a STScalar by using triplet
  PROCEDURE, PASS( obj ) :: set12 => stsField_set12
    !! set values to a STScalar by using triplet
  GENERIC, PUBLIC :: set => set1, set2, set3, set4, set5, set6, &
    & set7, set8, set9, set10, set11, set12

  PROCEDURE, PASS( obj ) :: get1 => stsField_get1
  PROCEDURE, PASS( obj ) :: get2 => stsField_get2
  PROCEDURE, PASS( obj ) :: get3 => stsField_get3
  PROCEDURE, PASS( obj ) :: get4 => stsField_get4
  PROCEDURE, PASS( obj ) :: get5 => stsField_get5
  PROCEDURE, PASS( obj ) :: get6 => stsField_get6
  PROCEDURE, PASS( obj ) :: get7 => stsField_get7
  GENERIC, PUBLIC :: get => get1, get2, get3, get4, get5, get6, get7
    !! get the entries of STScalar field
  PROCEDURE, PASS( obj ) :: getPointerOfComponent => stsField_getPointerOfComponent
  PROCEDURE, PUBLIC, PASS( obj ) :: Import => stsField_Import
  PROCEDURE, PUBLIC, PASS( obj ) :: Export => stsField_Export
END TYPE STScalarField_

PUBLIC :: STScalarField_
TYPE( STScalarField_ ), PARAMETER, PUBLIC :: Type = STScalarField_()

!----------------------------------------------------------------------------
!                                                             STScalarFieldPointer_
!----------------------------------------------------------------------------

TYPE :: STScalarFieldPointer_
  CLASS( STScalarField_ ), POINTER :: ptr => NULL()
END TYPE STScalarFieldPointer_

PUBLIC :: STScalarFieldPointer_

!----------------------------------------------------------------------------
!                                                 addSurrogate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.

INTERFACE
MODULE SUBROUTINE stsField_addSurrogate( obj, UserObj )
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: UserObj
END SUBROUTINE stsField_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                         setSTScalarFieldParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This routine is used to for setting space time scalar field

INTERFACE
MODULE SUBROUTINE setSTScalarFieldParam( param, name, timeCompo, &
  & fieldType )
  TYPE( ParameterList_ ), INTENT( INOUT ) :: param
  CHARACTER( LEN = * ), INTENT( IN ) :: name
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: fieldType
END SUBROUTINE setSTScalarFieldParam
END INTERFACE

PUBLIC :: setSTScalarFieldParam

!----------------------------------------------------------------------------
!                                           checkEssentialParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.
!
!# Introduction
! This routine check the essential parameters required to the initiate the
! [[STScalarField_]] data type. We need following parameters
!
! - CHARACTER( LEN= * ) :: name
! - INTEGER( I4B ) :: tdof

INTERFACE
MODULE SUBROUTINE stsField_checkEssentialParam( obj, param )
  CLASS( STScalarField_ ), INTENT( IN ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE stsField_checkEssentialParam
END INTERFACE

PUBLIC :: stsField_checkEssentialParam

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the STScalarField_ object
!
!# Introduction
! This routine initiate the STScalar field object.
! `param` contains the information of parameters required to initiate the
! STScalar. There are essential and optional information.
! Essential information are described below.
! - `name`  character defining the name of STScalar field
! - `timeCompo` is the total degree of freedom or components
! - `fieldType` type of field type; FIELD_TYPE_CONSTANT, FIELD_TYPE_NORMAL
!
!
!### Usage
!
!```fortran
  ! type( domain_ ) :: dom
  ! type( STScalarField_ ) :: obj
  ! type( ScalarField_ ) :: scalarObj
  ! type( HDF5File_ ) :: meshfile
  ! type( ParameterList_ ) :: param
  ! integer( i4b ) :: ierr
  ! real( DFP ), ALLOCATABLE :: real1( : ), real2( :, : )
  ! call display( "Testing set methods for normal data" )
  ! CALL FPL_INIT()
  ! CALL param%initiate()
  ! ierr = param%set(key="name", value="U" )
  ! ierr = param%set(key="fieldType", value=FIELD_TYPE_NORMAL)
  ! ierr = param%set(key="timeCompo", value=3)
  ! call meshfile%initiate( filename="./mesh.h5", mode="READ" )
  ! call meshfile%open()
  ! call dom%initiate( meshfile )
  ! call obj%initiate( param, dom )
!```
INTERFACE
MODULE SUBROUTINE stsField_Initiate1( obj, param, dom )
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE stsField_Initiate1
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
MODULE SUBROUTINE stsField_Initiate2( obj, obj2, copyFull, copyStructure, usePointer )
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  CLASS( AbstractField_ ), INTENT( INOUT ) :: obj2
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: copyFull
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: copyStructure
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: usePointer
END SUBROUTINE stsField_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine deallocates the data stored inside the STScalarField_ obj

INTERFACE
MODULE SUBROUTINE stsField_DeallocateData( obj )
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
END SUBROUTINE stsField_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE stsField_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE stsField_Final( obj )
  TYPE( STScalarField_ ), INTENT( INOUT ) :: obj
END SUBROUTINE stsField_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                       STScalar@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: 	This function returns an instance of [[STScalarField_]]

INTERFACE
MODULE FUNCTION stsField_Constructor1( param, dom ) RESULT( Ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
  TYPE( STScalarField_ ) :: ans
END FUNCTION stsField_Constructor1
END INTERFACE

INTERFACE STScalarField
  MODULE PROCEDURE stsField_Constructor1
END INTERFACE STScalarField

PUBLIC :: STScalarField

!----------------------------------------------------------------------------
!                                         STScalarField_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: 	This function returns an instance of [[STScalarField_]]

INTERFACE
MODULE FUNCTION stsField_Constructor_1( param, dom ) RESULT( Ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
  CLASS( STScalarField_ ), POINTER :: ans
END FUNCTION stsField_Constructor_1
END INTERFACE

INTERFACE STScalarField_Pointer
  MODULE PROCEDURE stsField_Constructor_1
END INTERFACE STScalarField_Pointer

PUBLIC :: STScalarField_Pointer

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[STScalarField_]]

INTERFACE
MODULE SUBROUTINE stsField_Display( obj, msg, unitNo )
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE stsField_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Import@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
MODULE SUBROUTINE stsField_Import( obj, hdf5, group, dom )
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE stsField_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Export@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE
MODULE SUBROUTINE stsField_Export( obj, hdf5, group )
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END SUBROUTINE stsField_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the single entry of the STScalar field
!
!# Introduction
! This routine sets the single entry of the STScalar field. Here, val should
! be a STScalar representing the components of a STScalar. The size of `value`
! should be same as `obj%timeCompo`. In simple words it does following.
!
! STScalar( :, globalNode ) = value( : )
!
!
!### Usage
!
!```fortran
  ! call obj%set( globalNode = 10, value= 100.0_DFP*[1,1,1] )
  ! call obj%display( "test-1: STScalar field = ")
!```

INTERFACE
MODULE SUBROUTINE stsField_set1( obj, globalNode, value )
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE stsField_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets all the entries of a STScalar field
!
!# Introduction
! This routine work as follows. The size of value should be same as obj%timeCompo, then this value is set for all the nodal values
!
! STScalar( :, i ) = value( : ), for i = 1, tNodes
!
!
!### Usage
!
!```fortran
  ! call obj%set( value= 10.0_DFP*[1,1,1] )
  ! call obj%display( "test-2: STScalar field = ")
!```

INTERFACE
MODULE SUBROUTINE stsField_set2( obj, value )
  CLASS( STScalarField_ ), TARGET, INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE stsField_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets all the entries of a STScalar field
!
!# Introduction
! This routine sets all values of `timeCompo` component of the STScalar field
! to given scalar value `value`
!
! STScalar( timeCompo, i ) = value, for i = 1, tNodes
!
!
!### Usage
!
!```fortran
  ! call obj%set( value= -10.0_DFP, timeCompo=1 )
  ! call obj%set( value= -20.0_DFP, timeCompo=2 )
  ! call obj%set( value= -30.0_DFP, timeCompo=3 )
  ! call obj%display( "test-3: STScalar field = ")
!```

INTERFACE
MODULE SUBROUTINE stsField_set3( obj, value, timeCompo )
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stsField_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all the entries by using given STScalar field
!
!# Introduction
! This routine set all entries of STScalar field to given STScalar
! Here shape of should be value(1:timeCompo, tNodes).
!
! STScalar( :, : ) = value( :, : )
!
!
!### Usage
!
!```fortran
  ! call reallocate( real2, 3, dom%getTotalNodes() )
  ! real2 = 1.0_DFP
  ! call obj%set( value=real2 )
  ! call obj%display( "test-4: STScalar field = " )
!```

INTERFACE
MODULE SUBROUTINE stsField_set4( obj, value )
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( :, : )
END SUBROUTINE stsField_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all the entries by using given STScalar field
!
!# Introduction
! This routine set all entries of the component `timeCompo` STScalar
! field  to given fortran STScalar `value`
!
! STScalar( timeCompo, : ) = value( : )
!
!
!### Usage
!
!```fortran
  ! call reallocate( real1, dom%getTotalNodes() )
  ! real1 = 3.0_DFP
  ! call obj%set( value=real1, timeCompo=3 )
  ! call obj%display( "test-5: STScalar field = " )
!```
INTERFACE
MODULE SUBROUTINE stsField_set5( obj, value, timeCompo )
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stsField_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all the entries by using given STScalar field
!
!# Introduction
! This routine set all entries of the component `timeCompo` STScalar
! field  to given scalar field `value`
!
! STScalar( timeCompo, : ) = value
!
!
!### Usage
!
!```fortran
  ! call scalarObj%initiate( param, dom )
  ! call scalarObj%set( value = 2.0_DFP )
  ! call obj%set( value=scalarObj, timeCompo=2 )
  ! call obj%display( "test-6: STScalar field = ")
  ! ierr = param%set( key="fieldType", value=FIELD_TYPE_CONSTANT)
  ! call scalarObj%deallocateData()
  ! call scalarObj%initiate( param, dom )
  ! call scalarObj%set( value=10.0_DFP )
  ! call obj%set( value=scalarObj, timeCompo=1 )
  ! call obj%display( "test-7: STScalar field = ")
!```

INTERFACE
MODULE SUBROUTINE stsField_set6( obj, value, timeCompo )
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  TYPE( ScalarField_ ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stsField_set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! This soubroutine sets the selected enties to a STScalar entry value( : )
! Effectively it does the following:
!
! STScalar( :, globalNode ) = value( : ), for entries in global nodes
!
!
!### Usage
!
!```fortran
  ! call reallocate( real2, 3, 4)
  ! real2( :, 1 ) = -1.0; real2( :, 2 ) = -2.0; real2( :, 3 ) = -3.0
  ! real2( :, 4 ) = -4.0
  ! call obj%set( value=real2, globalNode=[1,3,5,7] )
  ! call obj%display( "test-8: STScalar field = ")
!```

INTERFACE
MODULE SUBROUTINE stsField_set7(obj, value, globalNode)
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE stsField_set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! This routine sets all selected entries.
! STScalar( :, globalNode ) = value( :, : )
!
!
!### Usage
!
!```fortran
  ! call reallocate( real2, 3, 4)
  ! real2( :, 1 ) = -1.0; real2( :, 2 ) = -2.0; real2( :, 3 ) = -3.0
  ! real2( :, 4 ) = -4.0
  ! call obj%set( value=real2, globalNode=[1,3,5,7] )
  ! call obj%display( "test-8: STScalar field = ")
!```

INTERFACE
MODULE SUBROUTINE stsField_set8(obj, globalNode, value)
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
  REAL( DFP ), INTENT( IN ) :: value( :, : )
END SUBROUTINE stsField_set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! This routine sets the selected components of selected nodes to given value
!
! STScalar( timeCompo, globalNode ) = value( : )
!
!
!### Usage
!
!```fortran
  ! call reallocate( real1, 4)
  ! real1 = [1,10,100,1000]
  ! call obj%set( value=real1, globalNode=[1,3,5,7], timeCompo=1 )
  ! call obj%display( "test-9: STScalar field = " )
!```

INTERFACE
MODULE SUBROUTINE stsField_set9(obj, value, globalNode, timeCompo)
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stsField_set9
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
! STScalar( timeCompo, globalNode ) = value

INTERFACE
MODULE SUBROUTINE stsField_set10(obj, value, globalNode, timeCompo)
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stsField_set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! Set entries using the selected nodes using triplet.
!

INTERFACE
MODULE SUBROUTINE stsField_set11( obj, value, istart, iend, stride )
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
  REAL( DFP ), INTENT( IN ) :: value( : )
END SUBROUTINE stsField_set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: set the STScalar values using triplet
!
!# Introduction
! Set entries using the selected nodes using triplet.

INTERFACE
MODULE SUBROUTINE stsField_set12( obj, value, istart, iend, stride )
  CLASS( STScalarField_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( :, : )
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
END SUBROUTINE stsField_set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the single entry of the STScalar field

INTERFACE
MODULE SUBROUTINE stsField_get1( obj, value, globalNode, timeCompo )
  CLASS( STScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: globalNode
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: timeCompo
END SUBROUTINE stsField_get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine get all the entries by using given STScalar field

INTERFACE
MODULE SUBROUTINE stsField_get2( obj, value )
  CLASS( STScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value( :, : )
END SUBROUTINE stsField_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
MODULE SUBROUTINE stsField_get3(obj, value, globalNode)
  CLASS( STScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value(:,:)
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
END SUBROUTINE stsField_get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
MODULE SUBROUTINE stsField_get4(obj, value, globalNode, timeCompo)
  CLASS( STScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stsField_get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
MODULE SUBROUTINE stsField_get5(obj, value, globalNode, timeCompo)
  CLASS( STScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( INOUT ) :: value
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stsField_get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
MODULE SUBROUTINE stsField_get6( obj, value, istart, iend, stride )
  CLASS( STScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value( :, : )
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
END SUBROUTINE stsField_get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
MODULE SUBROUTINE stsField_get7( obj, value, istart, iend, stride, timeCompo )
  CLASS( STScalarField_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: value( : )
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
END SUBROUTINE stsField_get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPointer@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns pointer to a specific component

INTERFACE
MODULE FUNCTION stsField_getPointerOfComponent( obj, timeCompo ) RESULT( ans )
  CLASS( STScalarField_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: timeCompo
  REAL( DFP ), POINTER :: ans ( : )
END FUNCTION stsField_getPointerOfComponent
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STScalarField_Class