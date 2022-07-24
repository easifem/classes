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

SUBMODULE(VTKPlot_Class) StructuredGridMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x1y1
  !!
  REAL( DFP ) :: z( 1 )
  REAL( DFP ), ALLOCATABLE :: xx( :, :, : ), yy( :, :, : ), zz( :, :, : )
  !!
  z = 0.0_DFP
  CALL MeshGrid(x=xx, y=yy, z=zz, xgv=x, ygv=y, zgv=z)
  CALL obj%plot(x=xx, y=yy, z=zz, filename=filename)
  !!
  DEALLOCATE( xx, yy, zz )
  !!
END PROCEDURE vts_plot_x1y1

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x1y1z1
  !!
  REAL( DFP ), ALLOCATABLE :: xx( :, :, : ), yy( :, :, : ), zz( :, :, : )
  !!
  CALL MeshGrid(x=xx, y=yy, z=zz, xgv=x, ygv=y, zgv=z)
  CALL obj%plot(x=xx, y=yy, z=zz, filename=filename)
  DEALLOCATE( xx, yy, zz)
  !!
END PROCEDURE vts_plot_x1y1z1

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x2y2
  !!
  REAL( DFP ) :: z( 1 )
  REAL( DFP ), DIMENSION( SIZE(x,1), SIZE(x,2), 1 ) :: xx, yy, zz
  !!
  !! check
  !!
  z = 0.0_DFP
  xx( :, :, 1 ) = x
  yy( :, :, 1 ) = y
  zz = 0.0_DFP
  CALL obj%plot(x=xx, y=yy, z=zz, filename=filename)
  !!
END PROCEDURE vts_plot_x2y2

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x3y3z3
  !!
  CHARACTER( LEN = * ), PARAMETER :: myName = "vts_plot_x3y3z3"
  INTEGER(I4B) :: nx1, nx2, ny1, ny2, nz1, nz2
  TYPE( VTKFile_ ) :: aVTKfile
  !!
  !! check
  !!
  IF( ANY( SHAPE( x ) .NE. SHAPE( y ) ) .OR. &
    & ANY( SHAPE( y ) .NE. SHAPE( z ) ) .OR. &
    & ANY( SHAPE( z ) .NE. SHAPE( x ) ) ) THEN
    CALL e%raiseError(modName //'::'//myName// ' - '// &
      & 'Shape of x, y, and z should be the same.')
  END IF
  !!
  nx1 = 0; nx2 = SIZE( x,1 ) - 1
  ny1 = 0; ny2 = SIZE( x,2 ) - 1
  nz1 = 0; nz2 = SIZE( x,3 ) - 1
  !!
  CALL aVTKfile%InitiateVTKFile( &
    & filename=filename, &
    & mode="NEW", &
    & DataFormat=VTK_BINARY, &
    & DataStructureType=VTK_StructuredGrid, &
    & WholeExtent=[nx1, nx2, ny1, ny2, nz1, nz2])
  CALL aVTKfile%WritePiece(extent=[nx1, nx2, ny1, ny2, nz1, nz2])
  CALL aVTKfile%WritePoints(x=x, y=y, z=z)
  CALL aVTKfile%WritePiece()
  CALL aVTKfile%Deallocate()
  !!
END PROCEDURE vts_plot_x3y3z3

END SUBMODULE StructuredGridMethods