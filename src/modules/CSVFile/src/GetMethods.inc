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
!                                                         getnrow@GetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION txt_getnrows( obj ) RESULT( ans )
  CLASS( CSVFile_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION txt_getnrows
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getncol@GetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION txt_getncols( obj ) RESULT( ans )
  CLASS( CSVFile_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION txt_getncols
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getChunkSize@GetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION txt_getChunkSize( obj ) RESULT( ans )
  CLASS( CSVFile_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION txt_getChunkSize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE txt_getDataTypes( obj, dataType )
  CLASS( CSVFile_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: dataType( : )
END SUBROUTINE txt_getDataTypes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_getValue( obj, irow, icol, val  )
  CLASS( CSVFile_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: irow
  INTEGER( I4B ), INTENT( IN ) :: icol
  CLASS( * ), INTENT( OUT ) :: val
END SUBROUTINE txt_getValue
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_getColumn( obj, icol, val )
  CLASS( CSVFile_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: icol
  CLASS( * ), INTENT( INOUT ) :: val( : )
END SUBROUTINE txt_getColumn
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_getRealColumn( obj, icol, val )
  CLASS( CSVFile_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: icol
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: val( : )
END SUBROUTINE txt_getRealColumn
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_getRealVectorColumn( obj, icol, val )
  CLASS( CSVFile_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: icol
  TYPE( RealVector_ ), INTENT( INOUT ) :: val
END SUBROUTINE txt_getRealVectorColumn
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_getIntColumn( obj, icol, val )
  CLASS( CSVFile_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: icol
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: val( : )
END SUBROUTINE txt_getIntColumn
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_getIntVectorColumn( obj, icol, val )
  CLASS( CSVFile_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: icol
  TYPE( IntVector_ ), INTENT( INOUT ) :: val
END SUBROUTINE txt_getIntVectorColumn
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_getStringColumn( obj, icol, val )
  CLASS( CSVFile_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: icol
  TYPE( String ), ALLOCATABLE, INTENT( INOUT ) :: val( : )
END SUBROUTINE txt_getStringColumn
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_getLogicalColumn( obj, icol, val )
  CLASS( CSVFile_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: icol
  LOGICAL( LGT ), ALLOCATABLE, INTENT( INOUT ) :: val( : )
END SUBROUTINE txt_getLogicalColumn
END INTERFACE

