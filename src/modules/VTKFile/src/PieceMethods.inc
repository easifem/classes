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
!                                                   WritePiece@PieceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: WritePiece

INTERFACE
MODULE SUBROUTINE VTKFile_WritePiece_1( obj, extent, srcFileName )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: extent( : )
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: srcFileName
END SUBROUTINE VTKFile_WritePiece_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   WritePiece@PieceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write Piece

INTERFACE
MODULE SUBROUTINE VTKFile_WritePiece_2( obj, nPoints, nCells )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nPoints
  INTEGER( I4B ), INTENT( IN ) :: nCells
END SUBROUTINE VTKFile_WritePiece_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   WritePiece@PieceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write Piece

INTERFACE
MODULE SUBROUTINE VTKFile_WritePiece_3( obj, nPoints, nVerts, nLines, &
  & nStrips, nPolys )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nPoints
  INTEGER( I4B ), INTENT( IN ) :: nVerts
  INTEGER( I4B ), INTENT( IN ) :: nLines
  INTEGER( I4B ), INTENT( IN ) :: nStrips
  INTEGER( I4B ), INTENT( IN ) :: nPolys
END SUBROUTINE VTKFile_WritePiece_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                   WritePiece@PieceMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 Sept 2021
! summary: Write piece

INTERFACE
MODULE SUBROUTINE VTKFile_WritePiece_4( obj )
  CLASS( VTKFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE VTKFile_WritePiece_4
END INTERFACE
