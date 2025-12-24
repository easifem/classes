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
USE VTKFile_Class, ONLY: VTKFile_, VTK_BINARY, VTK_StructuredGrid
USE GridPointUtility, ONLY: MeshGrid

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x1y1
REAL(DFP) :: z(1)
REAL(DFP), ALLOCATABLE :: xx(:, :, :), yy(:, :, :), zz(:, :, :)
z = 0.0_DFP
CALL MeshGrid(x=xx, y=yy, z=zz, xgv=x, ygv=y, zgv=z)
CALL obj%plot(x=xx, y=yy, z=zz, filename=filename)
DEALLOCATE (xx, yy, zz)
END PROCEDURE vts_plot_x1y1

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x1y1z1
REAL(DFP), ALLOCATABLE :: xx(:, :, :), yy(:, :, :), zz(:, :, :)
CALL MeshGrid(x=xx, y=yy, z=zz, xgv=x, ygv=y, zgv=z)
CALL obj%plot(x=xx, y=yy, z=zz, filename=filename)
DEALLOCATE (xx, yy, zz)
END PROCEDURE vts_plot_x1y1z1

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x1y1z1w1
INTEGER(I4B) :: nx1, nx2, ny1, ny2, nz1, nz2
TYPE(VTKFile_) :: aVTKfile
REAL(DFP), ALLOCATABLE :: xx(:, :, :), yy(:, :, :), zz(:, :, :)
CHARACTER(*), PARAMETER :: myName = "vts_plot_x1y1z1w1"

CALL MeshGrid(x=xx, y=yy, z=zz, xgv=x, ygv=y, zgv=z)

IF (ANY(SHAPE(xx) .NE. SHAPE(yy)) .OR. &
    ANY(SHAPE(yy) .NE. SHAPE(zz)) .OR. &
    ANY(SHAPE(zz) .NE. SHAPE(xx))) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
                    'Shape of xx, yy, and zz should be the same.')
END IF

nx1 = 0; nx2 = SIZE(xx, 1) - 1
ny1 = 0; ny2 = SIZE(xx, 2) - 1
nz1 = 0; nz2 = SIZE(xx, 3) - 1

CALL aVTKfile%InitiateVTKFile( &
  filename=filename, &
  mode="NEW", &
  DataFormat=VTK_BINARY, &
  DataStructureType=VTK_StructuredGrid, &
  WholeExtent=[nx1, nx2, ny1, ny2, nz1, nz2])

CALL aVTKfile%WritePiece(extent=[nx1, nx2, ny1, ny2, nz1, nz2])
CALL aVTKfile%WritePoints(x=xx, y=yy, z=zz)

CALL aVTKfile%WriteDataArray( &
  location=String("node"), &
  action=String("open"))

CALL aVTKfile%WriteDataArray( &
  name=String(TRIM(label)), &
  x=w)

CALL aVTKfile%WriteDataArray( &
  location=String("node"), &
  action=String("close"))

CALL aVTKfile%WritePiece()
CALL aVTKfile%DEALLOCATE()

DEALLOCATE (xx, yy, zz)
END PROCEDURE vts_plot_x1y1z1w1

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x3y3z3w2
INTEGER(I4B) :: nx1, nx2, ny1, ny2, nz1, nz2, ii
TYPE(VTKFile_) :: aVTKfile
CHARACTER(*), PARAMETER :: myName = "vts_plot_x3y3z3w2"

IF (ANY(SHAPE(x) .NE. SHAPE(y)) .OR. &
    ANY(SHAPE(y) .NE. SHAPE(z)) .OR. &
    ANY(SHAPE(z) .NE. SHAPE(x))) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
                    'Shape of x, y, and z should be the same.')
  RETURN
END IF

IF (SIZE(label) .NE. SIZE(w, 2)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
                    'Size of label should be same as size(w,2).')
  RETURN
END IF

nx1 = 0; nx2 = SIZE(x, 1) - 1
ny1 = 0; ny2 = SIZE(x, 2) - 1
nz1 = 0; nz2 = SIZE(x, 3) - 1

CALL aVTKfile%InitiateVTKFile( &
  filename=filename, &
  mode="NEW", &
  DataFormat=VTK_BINARY, &
  DataStructureType=VTK_StructuredGrid, &
  WholeExtent=[nx1, nx2, ny1, ny2, nz1, nz2])

CALL aVTKfile%WritePiece(extent=[nx1, nx2, ny1, ny2, nz1, nz2])
CALL aVTKfile%WritePoints(x=x, y=y, z=z)

CALL aVTKfile%WriteDataArray( &
  location=String("node"), &
  action=String("open"))

DO ii = 1, SIZE(w, 2)
  CALL aVTKfile%WriteDataArray( &
    name=label(ii), &
    x=w(:, ii))
END DO

CALL aVTKfile%WriteDataArray( &
  location=String("node"), &
  action=String("close"))

CALL aVTKfile%WritePiece()
CALL aVTKfile%DEALLOCATE()

END PROCEDURE vts_plot_x3y3z3w2

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x2y2
REAL(DFP) :: z(1)
REAL(DFP), DIMENSION(SIZE(x, 1), SIZE(x, 2), 1) :: xx, yy, zz
z = 0.0_DFP
xx(:, :, 1) = x
yy(:, :, 1) = y
zz = 0.0_DFP
CALL obj%plot(x=xx, y=yy, z=zz, filename=filename)
END PROCEDURE vts_plot_x2y2

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x2y2w2
REAL(DFP) :: z(1)
REAL(DFP), DIMENSION(SIZE(x, 1), SIZE(x, 2), 1) :: xx, yy, zz, ww
z = 0.0_DFP
xx(:, :, 1) = x
yy(:, :, 1) = y
ww(:, :, 1) = w
zz = 0.0_DFP
CALL obj%plot(x=xx, y=yy, z=zz, w=ww, label=label, filename=filename)
END PROCEDURE vts_plot_x2y2w2

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x2y2w2b
REAL(DFP) :: z(1)
REAL(DFP), DIMENSION(SIZE(x, 1), SIZE(x, 2), 1) :: xx, yy, zz
z = 0.0_DFP
xx(:, :, 1) = x
yy(:, :, 1) = y
zz = 0.0_DFP
CALL obj%plot(x=xx, y=yy, z=zz, w=w, label=label, filename=filename)
END PROCEDURE vts_plot_x2y2w2b

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x2y2w3
REAL(DFP) :: z(1)
REAL(DFP), DIMENSION(SIZE(x, 1), SIZE(x, 2), 1) :: xx, yy, zz
REAL(DFP), DIMENSION(SIZE(w, 1), SIZE(w, 2), 1, SIZE(w, 3)) :: ww
z = 0.0_DFP
xx(:, :, 1) = x
yy(:, :, 1) = y
ww(:, :, 1, :) = w
zz = 0.0_DFP
CALL obj%plot(x=xx, y=yy, z=zz, w=ww, label=label, filename=filename)
END PROCEDURE vts_plot_x2y2w3

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x3y3z3
CHARACTER(*), PARAMETER :: myName = "vts_plot_x3y3z3"
INTEGER(I4B) :: nx1, nx2, ny1, ny2, nz1, nz2
TYPE(VTKFile_) :: aVTKfile
IF (ANY(SHAPE(x) .NE. SHAPE(y)) .OR. &
  & ANY(SHAPE(y) .NE. SHAPE(z)) .OR. &
  & ANY(SHAPE(z) .NE. SHAPE(x))) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Shape of x, y, and z should be the same.')
END IF
nx1 = 0; nx2 = SIZE(x, 1) - 1
ny1 = 0; ny2 = SIZE(x, 2) - 1
nz1 = 0; nz2 = SIZE(x, 3) - 1
CALL aVTKfile%InitiateVTKFile( &
  filename=filename, &
  mode="NEW", &
  DataFormat=VTK_BINARY, &
  DataStructureType=VTK_StructuredGrid, &
  WholeExtent=[nx1, nx2, ny1, ny2, nz1, nz2])
CALL aVTKfile%WritePiece(extent=[nx1, nx2, ny1, ny2, nz1, nz2])
CALL aVTKfile%WritePoints(x=x, y=y, z=z)
CALL aVTKfile%WritePiece()
CALL aVTKfile%DEALLOCATE()
END PROCEDURE vts_plot_x3y3z3

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x3y3z3w3
CHARACTER(*), PARAMETER :: myName = "vts_plot_x3y3z3w3"
INTEGER(I4B) :: nx1, nx2, ny1, ny2, nz1, nz2
TYPE(VTKFile_) :: aVTKfile
REAL(DFP), ALLOCATABLE :: temp(:)

IF (ANY(SHAPE(x) .NE. SHAPE(y)) .OR. &
    ANY(SHAPE(y) .NE. SHAPE(z)) .OR. &
    ANY(SHAPE(z) .NE. SHAPE(x))) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Shape of x, y, and z should be the same.')
END IF
nx1 = 0; nx2 = SIZE(x, 1) - 1
ny1 = 0; ny2 = SIZE(x, 2) - 1
nz1 = 0; nz2 = SIZE(x, 3) - 1
CALL aVTKfile%InitiateVTKFile( &
  filename=filename, &
  mode="NEW", &
  DataFormat=VTK_BINARY, &
  DataStructureType=VTK_StructuredGrid, &
  WholeExtent=[nx1, nx2, ny1, ny2, nz1, nz2])
CALL aVTKfile%WritePiece(extent=[nx1, nx2, ny1, ny2, nz1, nz2])
CALL aVTKfile%WritePoints(x=x, y=y, z=z)

CALL aVTKfile%WriteDataArray( &
  location=String("node"), &
  action=String("open"))

temp = RESHAPE(w, [SIZE(w)])
CALL aVTKfile%WriteDataArray( &
  name=String(TRIM(label)), &
  x=temp)
IF (ALLOCATED(temp)) DEALLOCATE (temp)

CALL aVTKfile%WriteDataArray( &
  location=String("node"), &
  action=String("close"))

CALL aVTKfile%WritePiece()
CALL aVTKfile%DEALLOCATE()
END PROCEDURE vts_plot_x3y3z3w3

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x3y3z3w4
CHARACTER(*), PARAMETER :: myName = "vts_plot_x3y3z3w4"
INTEGER(I4B) :: nx1, nx2, ny1, ny2, nz1, nz2, ii
TYPE(VTKFile_) :: aVTKfile
REAL(DFP), ALLOCATABLE :: temp(:)

IF (ANY(SHAPE(x) .NE. SHAPE(y)) .OR. &
    ANY(SHAPE(y) .NE. SHAPE(z)) .OR. &
    ANY(SHAPE(z) .NE. SHAPE(x))) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
                    'Shape of x, y, and z should be the same.')
  RETURN
END IF

IF (SIZE(label) .NE. SIZE(w, 4)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
                    'The size of label should be same as size(w, 4)')
  RETURN
END IF

nx1 = 0; nx2 = SIZE(x, 1) - 1
ny1 = 0; ny2 = SIZE(x, 2) - 1
nz1 = 0; nz2 = SIZE(x, 3) - 1

CALL aVTKfile%InitiateVTKFile( &
  filename=filename, &
  mode="NEW", &
  DataFormat=VTK_BINARY, &
  DataStructureType=VTK_StructuredGrid, &
  WholeExtent=[nx1, nx2, ny1, ny2, nz1, nz2])

CALL aVTKfile%WritePiece(extent=[nx1, nx2, ny1, ny2, nz1, nz2])
CALL aVTKfile%WritePoints(x=x, y=y, z=z)

CALL aVTKfile%WriteDataArray( &
  location=String("node"), &
  action=String("open"))

DO ii = 1, SIZE(label)
  temp = RESHAPE(w(:, :, :, ii), [SIZE(x)])
  CALL aVTKfile%WriteDataArray( &
    name=String(label(ii)%chars()), &
    x=temp)
END DO

IF (ALLOCATED(temp)) DEALLOCATE (temp)

CALL aVTKfile%WriteDataArray( &
  location=String("node"), &
  action=String("close"))

CALL aVTKfile%WritePiece()
CALL aVTKfile%DEALLOCATE()
END PROCEDURE vts_plot_x3y3z3w4

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x1y1f
REAL(DFP), ALLOCATABLE :: xx(:, :, :), yy(:, :, :), zz(:, :, :)
REAL(DFP), PARAMETER :: zgv(1) = [0.0_DFP]

CALL MeshGrid(x=xx, y=yy, z=zz, xgv=x, ygv=y, zgv=[0.0_DFP])
CALL obj%plot(x=xx, y=yy, z=zz, f=f, filename=filename)
DEALLOCATE (xx, yy, zz)
END PROCEDURE vts_plot_x1y1f

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x2y2f
REAL(DFP), DIMENSION(SIZE(x, 1), SIZE(x, 2), 1) :: xx, yy, zz
xx(:, :, 1) = x
yy(:, :, 1) = y
zz = 0.0_DFP
CALL obj%plot(x=xx, y=yy, z=zz, f=f, filename=filename)
END PROCEDURE vts_plot_x2y2f

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_plot_x3y3z3f
CHARACTER(*), PARAMETER :: myName = "vts_plot_x3y3z3f"
REAL(DFP) :: arg(3)
REAL(DFP), DIMENSION(SIZE(x, 1), SIZE(x, 2), SIZE(x, 3)) :: func
INTEGER(I4B) :: nx1, nx2, ny1, ny2, nz1, nz2, ii, jj, kk
TYPE(VTKFile_) :: aVTKfile

IF (.NOT. ASSOCIATED(f)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
                    'function is not associated')
END IF

IF (ANY(SHAPE(x) .NE. SHAPE(y)) .OR. &
    ANY(SHAPE(y) .NE. SHAPE(z)) .OR. &
    ANY(SHAPE(z) .NE. SHAPE(x))) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
                    'Shape of x, y, and z should be the same.')
END IF

nx1 = 0; nx2 = SIZE(x, 1) - 1
ny1 = 0; ny2 = SIZE(x, 2) - 1
nz1 = 0; nz2 = SIZE(x, 3) - 1

CALL aVTKfile%InitiateVTKFile( &
  filename=filename, &
  mode="NEW", &
  DataFormat=VTK_BINARY, &
  DataStructureType=VTK_StructuredGrid, &
  WholeExtent=[nx1, nx2, ny1, ny2, nz1, nz2])

CALL aVTKfile%WritePiece(extent=[nx1, nx2, ny1, ny2, nz1, nz2])
CALL aVTKfile%WritePoints(x=x, y=y, z=z)

CALL aVTKfile%WriteDataArray( &
  location=String("node"), &
  action=String("open"))

DO kk = 1, SIZE(x, 3)
  DO jj = 1, SIZE(x, 2)
    DO ii = 1, SIZE(x, 1)
      arg(1) = x(ii, jj, kk)
      arg(2) = y(ii, jj, kk)
      arg(3) = z(ii, jj, kk)
      func(ii, jj, kk) = f(arg)
    END DO
  END DO
END DO

CALL aVTKfile%WriteDataArray( &
  name=String("f"), &
  x=func, &
  numberOfComponents=1)

CALL aVTKfile%WriteDataArray( &
  location=String("node"), &
  action=String("close"))

CALL aVTKfile%WritePiece()
CALL aVTKfile%DEALLOCATE()
END PROCEDURE vts_plot_x3y3z3f

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_surface_x1y1f
REAL(DFP), ALLOCATABLE :: xx(:, :, :), yy(:, :, :), zz(:, :, :)
REAL(DFP) :: arg(3)
INTEGER(I4B) :: ii, jj, kk

CALL MeshGrid(x=xx, y=yy, z=zz, xgv=x, ygv=y, zgv=[0.0_DFP])

DO kk = 1, SIZE(xx, 3)
  DO jj = 1, SIZE(xx, 2)
    DO ii = 1, SIZE(xx, 1)
      arg(1) = xx(ii, jj, kk)
      arg(2) = yy(ii, jj, kk)
      arg(3) = zz(ii, jj, kk)
      zz(ii, jj, kk) = f(arg)
    END DO
  END DO
END DO

CALL obj%plot(x=xx, y=yy, z=zz, filename=filename)
DEALLOCATE (xx, yy, zz)

END PROCEDURE vts_surface_x1y1f

!----------------------------------------------------------------------------
!                                                                      Plot
!----------------------------------------------------------------------------

MODULE PROCEDURE vts_surface_x2y2f
REAL(DFP), DIMENSION(SIZE(x, 1), SIZE(x, 2), 1) :: xx, yy, zz
REAL(DFP) :: arg(3)
INTEGER(I4B) :: ii, jj, kk

xx(:, :, 1) = x
yy(:, :, 1) = y
DO kk = 1, SIZE(xx, 3)
  DO jj = 1, SIZE(xx, 2)
    DO ii = 1, SIZE(xx, 1)
      arg(1) = xx(ii, jj, kk)
      arg(2) = yy(ii, jj, kk)
      arg(3) = zz(ii, jj, kk)
      zz(ii, jj, kk) = f(arg)
    END DO
  END DO
END DO
CALL obj%plot(x=xx, y=yy, z=zz, filename=filename)
END PROCEDURE vts_surface_x2y2f

END SUBMODULE StructuredGridMethods
