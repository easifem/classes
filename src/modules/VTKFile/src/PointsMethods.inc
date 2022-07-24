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
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write Points

INTERFACE
MODULE SUBROUTINE VTKFile_WritePoints_1( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real64 ), INTENT( IN ) :: x( : )
  REAL( Real64 ), INTENT( IN ) :: y( : )
  REAL( Real64 ), INTENT( IN ) :: z( : )
END SUBROUTINE VTKFile_WritePoints_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write points

INTERFACE
MODULE SUBROUTINE VTKFile_WritePoints_2( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real32 ), INTENT( IN ) :: x( : )
  REAL( Real32 ), INTENT( IN ) :: y( : )
  REAL( Real32 ), INTENT( IN ) :: z( : )
END SUBROUTINE VTKFile_WritePoints_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write points

INTERFACE
MODULE SUBROUTINE VTKFile_WritePoints_3( obj, x )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real64 ), INTENT( IN ) :: x( 1:, 1: )
END SUBROUTINE VTKFile_WritePoints_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write points

INTERFACE
MODULE SUBROUTINE VTKFile_WritePoints_4( obj, x )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real32 ), INTENT( IN ) :: x( 1:, 1: )
END SUBROUTINE VTKFile_WritePoints_4
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write points

INTERFACE
MODULE SUBROUTINE VTKFile_WritePoints_5( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real64 ), INTENT( IN ) :: x( :, :, : )
  REAL( Real64 ), INTENT( IN ) :: y( :, :, : )
  REAL( Real64 ), INTENT( IN ) :: z( :, :, : )
END SUBROUTINE VTKFile_WritePoints_5
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write points

INTERFACE
MODULE SUBROUTINE VTKFile_WritePoints_6( obj, x, y, z )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real32 ), INTENT( IN ) :: x( :, :, : )
  REAL( Real32 ), INTENT( IN ) :: y( :, :, : )
  REAL( Real32 ), INTENT( IN ) :: z( :, :, : )
END SUBROUTINE VTKFile_WritePoints_6
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write points

INTERFACE
MODULE SUBROUTINE VTKFile_WritePoints_7( obj, x )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real64 ), INTENT( IN ) :: x( :, :, :, : )
END SUBROUTINE VTKFile_WritePoints_7
END INTERFACE

!----------------------------------------------------------------------------
!                                                WritePoints@PointsMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write points

INTERFACE
MODULE SUBROUTINE VTKFile_WritePoints_8( obj, x )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  REAL( Real32 ), INTENT( IN ) :: x( :, :, :, : )
END SUBROUTINE VTKFile_WritePoints_8
END INTERFACE
