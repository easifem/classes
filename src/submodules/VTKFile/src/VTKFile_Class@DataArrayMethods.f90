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
!                                                             WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_LocationTag
  TYPE( String ) :: loc, act
  !> main
  loc = location%upper()
  act = action%upper()
  SELECT CASE( TRIM(loc%chars()) )
  CASE( 'CELL' )
    loc = 'CellData'
  CASE( 'NODE' )
    loc = 'PointData'
  END SELECT
  !>
  SELECT CASE( obj%DataStructureType )
  CASE( PARALLEL_VTK_RectilinearGrid, PARALLEL_VTK_StructuredGrid, &
    & PARALLEL_VTK_UnstructuredGrid )
    loc = 'P'// loc
  END SELECT
  !>
  SELECT CASE( TRIM(act%chars()) )
  CASE( 'OPEN' )
    CALL obj%WriteStartTag( name=loc )
  CASE('CLOSE')
    CALL obj%WriteEndTag( name=loc )
  END SELECT
END PROCEDURE VTKFile_WriteDataArray_LocationTag

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
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Float32')
  isOffset = .FALSE.
  nByte =  SIZE( x, 1 ) * BYReal32
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank1_Real32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Real64
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Float64')
  isOffset = .FALSE.
  nByte =  SIZE( x, 1 ) * BYReal64
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank1_Real64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Int8
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int8')
  isOffset = .FALSE.
  nByte =  SIZE( x, 1 ) * BYInt8
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank1_Int8

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Int16
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int16')
  isOffset = .FALSE.
  nByte =  SIZE( x, 1 ) * BYInt16
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank1_Int16

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Int32
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int32')
  isOffset = .FALSE.
  nByte =  SIZE( x, 1 ) * BYInt32
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank1_Int32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

#ifdef USE_Int64
MODULE PROCEDURE VTKFile_WriteDataArray_Rank1_Int64
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int64')
  isOffset = .FALSE.
  nByte =  SIZE( x, 1 ) * BYInt64
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank1_Int64
#endif

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank2_Real32
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Float32')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYReal32
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank2_Real32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank2_Real64
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Float64')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYReal64
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank2_Real64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank2_Int8
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int8')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYInt8
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank2_Int8

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank2_Int16
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int16')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYInt16
#include "./VTKFile_WriteDataArray_Rank1234.inc"
END PROCEDURE VTKFile_WriteDataArray_Rank2_Int16

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank2_Int32
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int32')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYInt32
#include "./VTKFile_WriteDataArray_Rank1234.inc"
END PROCEDURE VTKFile_WriteDataArray_Rank2_Int32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

#ifdef USE_Int64
MODULE PROCEDURE VTKFile_WriteDataArray_Rank2_Int64
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int64')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYInt64
#include "./VTKFile_WriteDataArray_Rank1234.inc"
END PROCEDURE VTKFile_WriteDataArray_Rank2_Int64
#endif

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank3_Real32
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Float32')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYReal32
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank3_Real32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank3_Real64
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Float64')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYReal64
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank3_Real64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank3_Int8
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int8')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYInt8
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank3_Int8

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank3_Int16
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int16')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYInt16
#include "./VTKFile_WriteDataArray_Rank1234.inc"
END PROCEDURE VTKFile_WriteDataArray_Rank3_Int16

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank3_Int32
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int32')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYInt32
#include "./VTKFile_WriteDataArray_Rank1234.inc"
END PROCEDURE VTKFile_WriteDataArray_Rank3_Int32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

#ifdef USE_Int64
MODULE PROCEDURE VTKFile_WriteDataArray_Rank3_Int64
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int64')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYInt64
#include "./VTKFile_WriteDataArray_Rank1234.inc"
END PROCEDURE VTKFile_WriteDataArray_Rank3_Int64
#endif


!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank4_Real32
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Float32')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYReal32
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank4_Real32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank4_Real64
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Float64')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYReal64
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank4_Real64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank4_Int8
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int8')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYInt8
#include "./VTKFile_WriteDataArray_Rank1234.inc"

END PROCEDURE VTKFile_WriteDataArray_Rank4_Int8

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank4_Int16
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int16')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYInt16
#include "./VTKFile_WriteDataArray_Rank1234.inc"
END PROCEDURE VTKFile_WriteDataArray_Rank4_Int16

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_Rank4_Int32
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int32')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYInt32
#include "./VTKFile_WriteDataArray_Rank1234.inc"
END PROCEDURE VTKFile_WriteDataArray_Rank4_Int32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

#ifdef USE_Int64
MODULE PROCEDURE VTKFile_WriteDataArray_Rank4_Int64
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int64')
  isOffset = .FALSE.
  nByte =  SIZE( x ) * BYInt64
#include "./VTKFile_WriteDataArray_Rank1234.inc"
END PROCEDURE VTKFile_WriteDataArray_Rank4_Int64
#endif

!----------------------------------------------------------------------------
!                                                             WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Real32
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Float32')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYReal32
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Real32

!----------------------------------------------------------------------------
!                                                             WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Real64
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Float64')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYReal64
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Real64

!----------------------------------------------------------------------------
!                                                             WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int8
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int8')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt8
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int8

!----------------------------------------------------------------------------
!                                                             WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int16
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int16')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt16
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int16

!----------------------------------------------------------------------------
!                                                             WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int32
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int32')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt32
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int32

!----------------------------------------------------------------------------
!                                                             WriteDataArray
!----------------------------------------------------------------------------

#ifdef USE_Int64
MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int64
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int64')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt64
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank1_Int64
#endif

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Real32
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Float32')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYReal32
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Real32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Real64
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Float64')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYReal64
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Real64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int8
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int8')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt8
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int8

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int16
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int16')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt16
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int16

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int32
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int32')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt32
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

#ifdef USE_Int64
MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int64
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int64')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt64
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank2_Int64
#endif

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Real32
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Float32')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYReal32
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Real32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Real64
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Float64')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYReal64
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Real64

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int8
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int8')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt8
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int8

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int16
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int16')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt16
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int16

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int32
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int32')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt32
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int32

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

#ifdef USE_Int64
MODULE PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int64
  TYPE( String ) :: dataType, content
  INTEGER( I4B ) :: noc, nByte
  LOGICAL( LGT ) :: isOffset
  !> main
  dataType = String('Int64')
  isOffset = .FALSE.
  nByte =  (SIZE(x) + SIZE(y) + SIZE(z)) * BYInt64
  noc = 3
#include "./VTKFile_WriteDataArray_XYZ.inc"
END PROCEDURE VTKFile_WriteDataArray_XYZ_Rank3_Int64
#endif

!----------------------------------------------------------------------------
!                                                            WriteDataArray
!----------------------------------------------------------------------------

END SUBMODULE DataArrayMethods