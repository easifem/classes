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

!----------------------------------------------------------------------------
!                                           WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: This routine writes the location information
!
!# Introduction
!
! Write PointData or CellData open/close tag.
!
!@note
! This routine must be called before saving the data related to geometric
! mesh, this function initializes the saving of data variables indicating
! the *location* (node or cell centered) of variables that will be saved.
!@endnote
!
!@note
! A single file can contain both cell and node centered variables.
!@endnote
!
!
!### Usage
!
!```fortran
! TBA
!```

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_LocationTag( obj, location, action )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: location
    !! node or cell
  TYPE( String ), INTENT( IN ) :: action
    !! open, close
END SUBROUTINE VTKFile_WriteDataArray_LocationTag
END INTERFACE

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank1_Real32( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real32 ), INTENT( IN ) :: x( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank1_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank1_Real64( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real64 ), INTENT( IN ) :: x( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank1_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank1_Int8( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int8 ), INTENT( IN ) :: x( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank1_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank1_Int16( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int16 ), INTENT( IN ) :: x( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank1_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank1_Int32(obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int32 ), INTENT( IN ) :: x( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank1_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

#ifdef USE_Int64
INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank1_Int64(obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int64 ), INTENT( IN ) :: x( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank1_Int64
END INTERFACE
#endif

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank2_Real32( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real32 ), INTENT( IN ) :: x( 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank2_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank2_Real64( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real64 ), INTENT( IN ) :: x( 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank2_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank2_Int8( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int8 ), INTENT( IN ) :: x( 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank2_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank2_Int16( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int16 ), INTENT( IN ) :: x( 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank2_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank2_Int32(obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int32 ), INTENT( IN ) :: x( 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank2_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

#ifdef USE_Int64
INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank2_Int64(obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int64 ), INTENT( IN ) :: x( 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank2_Int64
END INTERFACE
#endif


!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank3_Real32( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real32 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank3_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank3_Real64( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real64 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank3_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank3_Int8( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int8 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank3_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank3_Int16( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int16 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank3_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank3_Int32(obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int32 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank3_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

#ifdef USE_Int64
INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank3_Int64(obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int64 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank3_Int64
END INTERFACE
#endif

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank4_Real32( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real32 ), INTENT( IN ) :: x( 1:, 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank4_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank4_Real64( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real64 ), INTENT( IN ) :: x( 1:, 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank4_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank4_Int8( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int8 ), INTENT( IN ) :: x( 1:, 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank4_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank4_Int16( obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int16 ), INTENT( IN ) :: x( 1:, 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank4_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                             WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank4_Int32(obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int32 ), INTENT( IN ) :: x( 1:, 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank4_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

#ifdef USE_Int64
INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Rank4_Int64(obj, name, x, &
  & isTuples, numberOfComponents )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int64 ), INTENT( IN ) :: x( 1:, 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: numberOfComponents
END SUBROUTINE VTKFile_WriteDataArray_Rank4_Int64
END INTERFACE
#endif

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank1_Real32( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real32 ), INTENT( IN ) :: x( 1: )
  REAL( Real32 ), INTENT( IN ) :: y( 1: )
  REAL( Real32 ), INTENT( IN ) :: z( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank1_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank1_Real64( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real64 ), INTENT( IN ) :: x( 1: )
  REAL( Real64 ), INTENT( IN ) :: y( 1: )
  REAL( Real64 ), INTENT( IN ) :: z( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank1_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank1_Int8( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int8 ), INTENT( IN ) :: x( 1: )
  INTEGER( Int8 ), INTENT( IN ) :: y( 1: )
  INTEGER( Int8 ), INTENT( IN ) :: z( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank1_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank1_Int16( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int16 ), INTENT( IN ) :: x( 1: )
  INTEGER( Int16 ), INTENT( IN ) :: y( 1: )
  INTEGER( Int16 ), INTENT( IN ) :: z( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank1_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank1_Int32( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int32 ), INTENT( IN ) :: x( 1: )
  INTEGER( Int32 ), INTENT( IN ) :: y( 1: )
  INTEGER( Int32 ), INTENT( IN ) :: z( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank1_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

#ifdef USE_Int64
INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank1_Int64( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int64 ), INTENT( IN ) :: x( 1: )
  INTEGER( Int64 ), INTENT( IN ) :: y( 1: )
  INTEGER( Int64 ), INTENT( IN ) :: z( 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank1_Int64
END INTERFACE
#endif

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank2_Real32( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real32 ), INTENT( IN ) :: x( 1:, 1: )
  REAL( Real32 ), INTENT( IN ) :: y( 1:, 1: )
  REAL( Real32 ), INTENT( IN ) :: z( 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank2_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank2_Real64( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real64 ), INTENT( IN ) :: x( 1:, 1: )
  REAL( Real64 ), INTENT( IN ) :: y( 1:, 1: )
  REAL( Real64 ), INTENT( IN ) :: z( 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank2_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank2_Int8( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int8 ), INTENT( IN ) :: x( 1:, 1: )
  INTEGER( Int8 ), INTENT( IN ) :: y( 1:, 1: )
  INTEGER( Int8 ), INTENT( IN ) :: z( 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank2_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank2_Int16( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int16 ), INTENT( IN ) :: x( 1:, 1: )
  INTEGER( Int16 ), INTENT( IN ) :: y( 1:, 1: )
  INTEGER( Int16 ), INTENT( IN ) :: z( 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank2_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank2_Int32( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int32 ), INTENT( IN ) :: x( 1:, 1: )
  INTEGER( Int32 ), INTENT( IN ) :: y( 1:, 1: )
  INTEGER( Int32 ), INTENT( IN ) :: z( 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank2_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

#ifdef USE_Int64
INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank2_Int64( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int64 ), INTENT( IN ) :: x( 1:, 1: )
  INTEGER( Int64 ), INTENT( IN ) :: y( 1:, 1: )
  INTEGER( Int64 ), INTENT( IN ) :: z( 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank2_Int64
END INTERFACE
#endif

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank3_Real32( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real32 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  REAL( Real32 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  REAL( Real32 ), INTENT( IN ) :: z( 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank3_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank3_Real64( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  REAL( Real64 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  REAL( Real64 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  REAL( Real64 ), INTENT( IN ) :: z( 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank3_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank3_Int8( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int8 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  INTEGER( Int8 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  INTEGER( Int8 ), INTENT( IN ) :: z( 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank3_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank3_Int16( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int16 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  INTEGER( Int16 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  INTEGER( Int16 ), INTENT( IN ) :: z( 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank3_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank3_Int32( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int32 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  INTEGER( Int32 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  INTEGER( Int32 ), INTENT( IN ) :: z( 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank3_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                            WriteDataArray@DataArrayMethods
!----------------------------------------------------------------------------

#ifdef USE_Int64
INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank3_Int64( obj, name, x, &
  & y, z, isTuples )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: name
  INTEGER( Int64 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  INTEGER( Int64 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  INTEGER( Int64 ), INTENT( IN ) :: z( 1:, 1:, 1: )
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTuples
END SUBROUTINE VTKFile_WriteDataArray_XYZ_Rank3_Int64
END INTERFACE
#endif
