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

MODULE VTKPlot_Class
USE GlobalData
USE BaseType
USE ExceptionHandler_Class, ONLY: e
USE ParameterList, ONLY: ParameterList_
USE AbstractPlot_Class
USE VTKFile_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "VTKPlot_Class"

!----------------------------------------------------------------------------
!                                                                     VTKPlot_
!----------------------------------------------------------------------------

TYPE, EXTENDS(AbstractPlot_) :: VTKPlot_
CONTAINS
  PRIVATE
  !!
  !! @ConstructorMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => Plot_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => Plot_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => Plot_Display
  !!
  !! @StructuredGridMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x1y1
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x1y1f
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x1y1z1
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x2y2
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x2y2f
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x3y3z3
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x3y3z3f
  GENERIC, PUBLIC :: Plot => &
    & vts_plot_x1y1, &
    & vts_plot_x1y1f, &
    & vts_plot_x1y1z1, &
    & vts_plot_x2y2, &
    & vts_plot_x2y2f, &
    & vts_plot_x3y3z3, &
    & vts_plot_x3y3z3f
  PROCEDURE, PUBLIC, PASS(obj) :: vts_surface_x1y1f
  PROCEDURE, PUBLIC, PASS(obj) :: vts_surface_x2y2f
  GENERIC, PUBLIC :: Surface => &
    & vts_surface_x1y1f, &
    & vts_surface_x2y2f
  !!
  !! @Scatter3D
  !!
  PROCEDURE, PASS(obj) :: plot_scatter3D_1
  PROCEDURE, PASS(obj) :: plot_scatter3D_2
  PROCEDURE, PASS(obj) :: plot_scatter3D_3
  GENERIC, PUBLIC :: Scatter3D => plot_scatter3D_1, &
    & plot_scatter3D_2, plot_scatter3D_3
END TYPE VTKPlot_

PUBLIC :: VTKPlot_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: VTKPlotPointer_
  CLASS(VTKPlot_), POINTER :: ptr => NULL()
END TYPE VTKPlotPointer_

PUBLIC :: VTKPlotPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 July 2022
! summary: Initiate the plotting engine

INTERFACE
  MODULE SUBROUTINE Plot_Initiate(obj, param)
    CLASS(VTKPlot_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), OPTIONAL, INTENT(IN) :: param
  END SUBROUTINE Plot_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 July 2022
! summary: Deallocate the data stored inside the plot

INTERFACE
  MODULE SUBROUTINE Plot_Deallocate(obj)
    CLASS(VTKPlot_), INTENT(INOUT) :: obj
  END SUBROUTINE Plot_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 July 2022
! summary: Display the content of Plot

INTERFACE
  MODULE SUBROUTINE Plot_Display(obj, msg, unitno)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE Plot_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Create mesh grid using x and y, and plot structured grid

INTERFACE
  MODULE SUBROUTINE vts_plot_x1y1(obj, x, y, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: y(:)
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_plot_x1y1
END INTERFACE

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Create mesh grid using x, y, z, and plot structured grid

INTERFACE
  MODULE SUBROUTINE vts_plot_x1y1z1(obj, x, y, z, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: y(:)
    REAL(DFP), INTENT(IN) :: z(:)
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_plot_x1y1z1
END INTERFACE

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Create plot structured grid

INTERFACE
  MODULE SUBROUTINE vts_plot_x2y2(obj, x, y, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! x coordinate of mesh grid
    REAL(DFP), INTENT(IN) :: y(:, :)
    !! y coordinate of mesh grid
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_plot_x2y2
END INTERFACE

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: plot structured grid

INTERFACE
  MODULE SUBROUTINE vts_plot_x3y3z3(obj, x, y, z, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:, :, :)
    !! x ccoord of mesh grid
    REAL(DFP), INTENT(IN) :: y(:, :, :)
    !! y coord of mesh grid
    REAL(DFP), INTENT(IN) :: z(:, :, :)
    !! z coord of mesh grid
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_plot_x3y3z3
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Scatter3D@ScatterMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-10
! summary:  Scatter3D plot using VTK

INTERFACE
  MODULE SUBROUTINE plot_scatter3D_1(obj, x, y, z, label, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    !! x coordinates
    REAL(DFP), INTENT(IN) :: y(:)
    !! y coords
    REAL(DFP), INTENT(IN) :: z(:)
    !! z coords
    CHARACTER(*), INTENT(IN) :: label
    !! label
    CHARACTER(*), INTENT(IN) :: filename
    !! vtkfile name, this file will be opened and closed by this
    !! routine, the extension should be .vtp
  END SUBROUTINE plot_scatter3D_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Scatter3D@ScatterMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_scatter3D_2(obj, x, y, z, label, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    !! x coordinates
    REAL(DFP), INTENT(IN) :: y(:)
    !! y coordinates
    REAL(DFP), INTENT(IN) :: z(:, :)
    !! each column of z is considered as data
    !! for jth column data label will be label+j
    CHARACTER(*), INTENT(IN) :: label
    !! data label
    CHARACTER(*), INTENT(IN) :: filename
    !! vtkfile name, this file will be opened and closed by this
    !! routine, the extension should be .vtp
  END SUBROUTINE plot_scatter3D_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Scatter3D@ScatterMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_scatter3D_3(obj, x, y, label, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    !! x coordinates
    REAL(DFP), INTENT(IN) :: y(:)
    !! y coordinates
    CHARACTER(*), INTENT(IN) :: label
    !! dummy label
    CHARACTER(*), INTENT(IN) :: filename
    !! vtkfile name, this file will be opened and closed by this
    !! routine, the extension should be .vtp
  END SUBROUTINE plot_scatter3D_3
END INTERFACE

CONTAINS

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

SUBROUTINE vts_plot_x1y1f(obj, x, y, f, filename)
  USE BaseMethod, ONLY: MeshGrid
  CLASS(VTKPlot_), INTENT(IN) :: obj
  REAL(DFP), INTENT(IN) :: x(:)
  REAL(DFP), INTENT(IN) :: y(:)
  PROCEDURE(iface_SpaceFunction), POINTER :: f
  CHARACTER(*), INTENT(IN) :: filename
  !!
  REAL(DFP), ALLOCATABLE :: xx(:, :, :), yy(:, :, :), zz(:, :, :)
  !!
  CALL MeshGrid(x=xx, y=yy, z=zz, xgv=x, ygv=y, zgv=[0.0_DFP])
  CALL obj%plot(x=xx, y=yy, z=zz, f=f, filename=filename)
  DEALLOCATE (xx, yy, zz)
  !!
END SUBROUTINE vts_plot_x1y1f

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

SUBROUTINE vts_plot_x2y2f(obj, x, y, f, filename)
  CLASS(VTKPlot_), INTENT(IN) :: obj
  REAL(DFP), INTENT(IN) :: x(:, :)
  REAL(DFP), INTENT(IN) :: y(:, :)
  PROCEDURE(iface_SpaceFunction), POINTER :: f
  CHARACTER(*), INTENT(IN) :: filename
  !!
  !!
  REAL(DFP), DIMENSION(SIZE(x, 1), SIZE(x, 2), 1) :: xx, yy, zz
  !!
  xx(:, :, 1) = x
  yy(:, :, 1) = y
  zz = 0.0_DFP
  !!
  CALL obj%plot(x=xx, y=yy, z=zz, f=f, filename=filename)
  !!
END SUBROUTINE vts_plot_x2y2f

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

SUBROUTINE vts_plot_x3y3z3f(obj, x, y, z, f, filename)
  USE String_Class, ONLY: String
  CLASS(VTKPlot_), INTENT(IN) :: obj
  REAL(DFP), INTENT(IN) :: x(:, :, :)
  REAL(DFP), INTENT(IN) :: y(:, :, :)
  REAL(DFP), INTENT(IN) :: z(:, :, :)
  PROCEDURE(iface_SpaceFunction), POINTER :: f
  CHARACTER(*), INTENT(IN) :: filename
  !!
  !!
  CHARACTER(*), PARAMETER :: myName = "vts_plot_x3y3z3f"
  REAL(DFP) :: arg(3)
  REAL(DFP), DIMENSION(SIZE(x, 1), SIZE(x, 2), SIZE(x, 3)) :: func
  INTEGER(I4B) :: nx1, nx2, ny1, ny2, nz1, nz2, ii, jj, kk
  TYPE(VTKFile_) :: aVTKfile
  !!
  !! check
  !!
  IF (.NOT. ASSOCIATED(f)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'function is not associated')
  END IF
  !!
  !! check
  !!
  IF (ANY(SHAPE(x) .NE. SHAPE(y)) .OR. &
    & ANY(SHAPE(y) .NE. SHAPE(z)) .OR. &
    & ANY(SHAPE(z) .NE. SHAPE(x))) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Shape of x, y, and z should be the same.')
  END IF
  !!
  nx1 = 0; nx2 = SIZE(x, 1) - 1
  ny1 = 0; ny2 = SIZE(x, 2) - 1
  nz1 = 0; nz2 = SIZE(x, 3) - 1
  !!
  CALL aVTKfile%InitiateVTKFile( &
    & filename=filename, &
    & mode="NEW", &
    & DataFormat=VTK_BINARY, &
    & DataStructureType=VTK_StructuredGrid, &
    & WholeExtent=[nx1, nx2, ny1, ny2, nz1, nz2])
  CALL aVTKfile%WritePiece(extent=[nx1, nx2, ny1, ny2, nz1, nz2])
  CALL aVTKfile%WritePoints(x=x, y=y, z=z)
  !!
  CALL aVTKfile%WriteDataArray( &
    & location=String("node"), &
    & action=String("open"))
  !!
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
  !!
  CALL aVTKfile%WriteDataArray( &
    & name=String("f"), &
    & x=func, &
    & numberOfComponents=1)
  !!
  CALL aVTKfile%WriteDataArray( &
    & location=String("node"), &
    & action=String("close"))
  !!
  CALL aVTKfile%WritePiece()
  CALL aVTKfile%DEALLOCATE()
  !!
END SUBROUTINE vts_plot_x3y3z3f

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

SUBROUTINE vts_surface_x1y1f(obj, x, y, f, filename)
  USE BaseMethod, ONLY: MeshGrid
  CLASS(VTKPlot_), INTENT(IN) :: obj
  REAL(DFP), INTENT(IN) :: x(:)
  REAL(DFP), INTENT(IN) :: y(:)
  PROCEDURE(iface_SpaceFunction), POINTER :: f
  CHARACTER(*), INTENT(IN) :: filename
  !!
  REAL(DFP), ALLOCATABLE :: xx(:, :, :), yy(:, :, :), zz(:, :, :)
  REAL(DFP) :: arg(3)
  INTEGER(I4B) :: ii, jj, kk
  !!
  CALL MeshGrid(x=xx, y=yy, z=zz, xgv=x, ygv=y, zgv=[0.0_DFP])
  !!
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
  !!
  CALL obj%plot(x=xx, y=yy, z=zz, filename=filename)
  DEALLOCATE (xx, yy, zz)
  !!
END SUBROUTINE vts_surface_x1y1f

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

SUBROUTINE vts_surface_x2y2f(obj, x, y, f, filename)
  CLASS(VTKPlot_), INTENT(IN) :: obj
  REAL(DFP), INTENT(IN) :: x(:, :)
  REAL(DFP), INTENT(IN) :: y(:, :)
  PROCEDURE(iface_SpaceFunction), POINTER :: f
  CHARACTER(*), INTENT(IN) :: filename
  !!
  !!
  REAL(DFP), DIMENSION(SIZE(x, 1), SIZE(x, 2), 1) :: xx, yy, zz
  REAL(DFP) :: arg(3)
  INTEGER(I4B) :: ii, jj, kk
  !!
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
  !!
  CALL obj%plot(x=xx, y=yy, z=zz, filename=filename)
  !!
END SUBROUTINE vts_surface_x2y2f

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE VTKPlot_Class
