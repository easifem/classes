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

SUBMODULE( VTKFile_Class ) DataArrayMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                        WriteDataArrrayTag
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArrayTag
  TYPE( String ) :: names( 5 ), values( 5 )
  INTEGER( I4B ) :: n
  names( 1 ) = 'type'
  values( 1 ) = '"' // TRIM( dataType ) // '"'
  names( 2 ) = 'NumberOfComponents'
  values( 2 ) = '"' // TRIM(str(numberOfComponents, .true.)) // '"'
  names( 3 ) = 'Name'
  values( 3 ) = '"' // TRIM( name ) // '"'
  names( 4 ) = 'format'
  values( 4 ) = '"' // TRIM( DataFormatName( obj%DataFormat ) ) // '"'
  IF( INPUT( Default=.FALSE., option=isTuples ) ) THEN
    names( 2 ) = 'NumberOfTuples'
  END IF
  n = 4
  IF( INPUT( Default=.FALSE., option=isOffset ) ) THEN
    n = 5
    names( 5 ) = "offset"
    values( 5 ) = '"' // TRIM( str(obj%offset, .TRUE.) ) // '"'
    CALL obj%WriteSelfClosingTag( name=String('DataArray'), &
      & attrNames=names(1:n), attrValues=Values(1:n) )
  ELSE
    CALL obj%WriteTag( name=String('DataArray'), attrNames=names(1:n), &
      & attrValues=Values(1:n), content=content )
  END IF
END PROCEDURE VTKFile_WriteDataArrayTag

!----------------------------------------------------------------------------
!                                              WriteDataArrayLocationTag
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArrayLocationTag
  TYPE( String ) :: location0, action0
  !>
  location0 = location%upper()
  action0 = action%upper()
  !>
  SELECT CASE( TRIM( location0%chars() ) )
  CASE( 'CELL' )
    location0 = 'CellData'
  CASE( 'NODE' )
    location0 = 'PointData'
  END SELECT
  !>
  SELECT CASE( obj%DataStructureType )
  CASE(PARALLEL_VTK_RectilinearGrid, PARALLEL_VTK_StructuredGrid, &
    & PARALLEL_VTK_UnstructuredGrid )
    location0 = 'P'//location0
  END SELECT
  !>
  SELECT CASE( TRIM(action0%chars()) )
  CASE('OPEN')
    CALL obj%WriteStartTag( name=location0 )
  CASE('CLOSE')
    CALL obj%WriteEndTag( name=location0 )
  END SELECT
END PROCEDURE VTKFile_WriteDataArrayLocationTag

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Real32
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: numberOfComponents
  CHARACTER( LEN = 10 ) :: fmt
  !> main
  SELECT CASE( obj%DataFormat )
  CASE( VTK_ASCII )
    fmt = "ASCII"
    dataType = String('Float64')
    numberOfComponents = 1
    content = encodeVTKDataArray( x=x, fmt=fmt )
    CALL obj%WriteDataArrayTag( dataType=dataType, &
      & numberOfComponents=numberOfComponents, &
      & name=name, content=content, isTuples=isTuples )
  CASE( VTK_BINARY )
    fmt = "BINARY"
    dataType = String('Float64')
    numberOfComponents = 1
    content = encodeVTKDataArray( x=x, fmt=fmt )
    CALL obj%WriteDataArrayTag( dataType=dataType, &
      & numberOfComponents=numberOfComponents, &
      & name=name, content=content, isTuples=isTuples )
  CASE( VTK_APPENDED )
    dataType = String('Float64')
    numberOfComponents = 1
    CALL obj%WriteDataArrayTag( dataType=dataType, &
      & numberOfComponents=numberOfComponents, &
      & name=name, isTuples=isTuples, &
      & isOffset=.TRUE. )
    CALL obj%WriteToScratch( x )
    CALL Obj%UpdateOffset( nByte=SIZE( x, 1 ) * BYReal32 )
  END SELECT
END PROCEDURE VTKFile_WriteDataArray_Rank1_Real32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Real64
END PROCEDURE VTKFile_WriteDataArray_Rank1_Real64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Int8
END PROCEDURE VTKFile_WriteDataArray_Rank1_Int8

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Int16
END PROCEDURE VTKFile_WriteDataArray_Rank1_Int16

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Int32
END PROCEDURE VTKFile_WriteDataArray_Rank1_Int32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

#ifdef USE_Int64
MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Int64
END PROCEDURE VTKFile_WriteDataArray_Rank1_Int64
#endif

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

END SUBMODULE DataArrayMethods