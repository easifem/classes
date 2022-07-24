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
!                                    WriteDataArray@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteDataArray_Appended( obj )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE VTKFile_WriteDataArray_Appended
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch1( obj, x )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  CLASS( * ), INTENT( IN ) :: x( 1: )
END SUBROUTINE VTKFile_WriteToScratch1
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch2( obj, x )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  CLASS( * ), INTENT( IN ) :: x( 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch2
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch3( obj, x )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  CLASS( * ), INTENT( IN ) :: x( 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch3
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch4( obj, x )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  CLASS( * ), INTENT( IN ) :: x( 1:, 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch4
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch5( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real64 ), INTENT( IN ) :: x( 1: )
  REAL( Real64 ), INTENT( IN ) :: y( 1: )
  REAL( Real64 ), INTENT( IN ) :: z( 1: )
END SUBROUTINE VTKFile_WriteToScratch5
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch6( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real32 ), INTENT( IN ) :: x( 1: )
  REAL( Real32 ), INTENT( IN ) :: y( 1: )
  REAL( Real32 ), INTENT( IN ) :: z( 1: )
END SUBROUTINE VTKFile_WriteToScratch6
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch7( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int64 ), INTENT( IN ) :: x( 1: )
  INTEGER( Int64 ), INTENT( IN ) :: y( 1: )
  INTEGER( Int64 ), INTENT( IN ) :: z( 1: )
END SUBROUTINE VTKFile_WriteToScratch7
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch8( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int32 ), INTENT( IN ) :: x( 1: )
  INTEGER( Int32 ), INTENT( IN ) :: y( 1: )
  INTEGER( Int32 ), INTENT( IN ) :: z( 1: )
END SUBROUTINE VTKFile_WriteToScratch8
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch9( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int16 ), INTENT( IN ) :: x( 1: )
  INTEGER( Int16 ), INTENT( IN ) :: y( 1: )
  INTEGER( Int16 ), INTENT( IN ) :: z( 1: )
END SUBROUTINE VTKFile_WriteToScratch9
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch10( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int8 ), INTENT( IN ) :: x( 1: )
  INTEGER( Int8 ), INTENT( IN ) :: y( 1: )
  INTEGER( Int8 ), INTENT( IN ) :: z( 1: )
END SUBROUTINE VTKFile_WriteToScratch10
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch11( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real64 ), INTENT( IN ) :: x( 1:, 1: )
  REAL( Real64 ), INTENT( IN ) :: y( 1:, 1: )
  REAL( Real64 ), INTENT( IN ) :: z( 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch11
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch12( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real32 ), INTENT( IN ) :: x( 1:, 1: )
  REAL( Real32 ), INTENT( IN ) :: y( 1:, 1: )
  REAL( Real32 ), INTENT( IN ) :: z( 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch12
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch13( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int64 ), INTENT( IN ) :: x( 1:, 1: )
  INTEGER( Int64 ), INTENT( IN ) :: y( 1:, 1: )
  INTEGER( Int64 ), INTENT( IN ) :: z( 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch13
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch14( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int32 ), INTENT( IN ) :: x( 1:, 1: )
  INTEGER( Int32 ), INTENT( IN ) :: y( 1:, 1: )
  INTEGER( Int32 ), INTENT( IN ) :: z( 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch14
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch15( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int16 ), INTENT( IN ) :: x( 1:, 1: )
  INTEGER( Int16 ), INTENT( IN ) :: y( 1:, 1: )
  INTEGER( Int16 ), INTENT( IN ) :: z( 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch15
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch16( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int8 ), INTENT( IN ) :: x( 1:, 1: )
  INTEGER( Int8 ), INTENT( IN ) :: y( 1:, 1: )
  INTEGER( Int8 ), INTENT( IN ) :: z( 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch16
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch17( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real64 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  REAL( Real64 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  REAL( Real64 ), INTENT( IN ) :: z( 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch17
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch18( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real32 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  REAL( Real32 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  REAL( Real32 ), INTENT( IN ) :: z( 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch18
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch19( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int64 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  INTEGER( Int64 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  INTEGER( Int64 ), INTENT( IN ) :: z( 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch19
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch20( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int32 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  INTEGER( Int32 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  INTEGER( Int32 ), INTENT( IN ) :: z( 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch20
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch21( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int16 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  INTEGER( Int16 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  INTEGER( Int16 ), INTENT( IN ) :: z( 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch21
END INTERFACE

!----------------------------------------------------------------------------
!                                    WriteToScratch@DataArrayAppendedMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE VTKFile_WriteToScratch22( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( Int8 ), INTENT( IN ) :: x( 1:, 1:, 1: )
  INTEGER( Int8 ), INTENT( IN ) :: y( 1:, 1:, 1: )
  INTEGER( Int8 ), INTENT( IN ) :: z( 1:, 1:, 1: )
END SUBROUTINE VTKFile_WriteToScratch22
END INTERFACE
