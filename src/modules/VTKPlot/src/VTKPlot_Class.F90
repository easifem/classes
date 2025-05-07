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
USE GlobalData, ONLY: DFP, I4B
USE BaseType, ONLY: iface_SpaceFunction
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE ParameterList, ONLY: ParameterList_
USE AbstractPlot_Class, ONLY: AbstractPlot_
IMPLICIT NONE
PRIVATE
PUBLIC :: VTKPlot_
PUBLIC :: VTKPlotPointer_

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
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x1y1z1w1
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x3y3z3w2
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x2y2
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x2y2w2
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x2y2w2b
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x2y2w3
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x2y2f
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x3y3z3
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x3y3z3w3
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x3y3z3w4
  PROCEDURE, PUBLIC, PASS(obj) :: vts_plot_x3y3z3f
  GENERIC, PUBLIC :: Plot => vts_plot_x1y1, vts_plot_x1y1f, &
    vts_plot_x1y1z1, vts_plot_x1y1z1w1, vts_plot_x3y3z3w2, &
    vts_plot_x2y2, vts_plot_x2y2w2, vts_plot_x2y2w2b, &
    vts_plot_x2y2w3, vts_plot_x2y2f, vts_plot_x3y3z3, &
    vts_plot_x3y3z3w3, vts_plot_x3y3z3w4, vts_plot_x3y3z3f

  PROCEDURE, PUBLIC, PASS(obj) :: vts_surface_x1y1f
  PROCEDURE, PUBLIC, PASS(obj) :: vts_surface_x2y2f
  GENERIC, PUBLIC :: Surface => vts_surface_x1y1f, vts_surface_x2y2f
  !!
  !! @Scatter3D
  !!
  PROCEDURE, PASS(obj) :: plot_scatter3D_1
  PROCEDURE, PASS(obj) :: plot_scatter3D_2
  PROCEDURE, PASS(obj) :: plot_scatter3D_3
  PROCEDURE, PASS(obj) :: plot_scatter3D_4
  GENERIC, PUBLIC :: Scatter3D => plot_scatter3D_1, plot_scatter3D_2, &
    plot_scatter3D_3, plot_scatter3D_4

END TYPE VTKPlot_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: VTKPlotPointer_
  CLASS(VTKPlot_), POINTER :: ptr => NULL()
END TYPE VTKPlotPointer_

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
! summary: Create mesh grid using x, y, z, and plot structured grid

INTERFACE
  MODULE SUBROUTINE vts_plot_x1y1z1w1(obj, x, y, z, w, label, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: y(:)
    REAL(DFP), INTENT(IN) :: z(:)
    REAL(DFP), INTENT(IN) :: w(:)
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_plot_x1y1z1w1
END INTERFACE

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Create mesh grid using x, y, z, and plot structured grid

INTERFACE
  MODULE SUBROUTINE vts_plot_x3y3z3w2(obj, x, y, z, w, label, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:, :, :)
    REAL(DFP), INTENT(IN) :: y(:, :, :)
    REAL(DFP), INTENT(IN) :: z(:, :, :)
    REAL(DFP), INTENT(IN) :: w(:, :)
    !! each columns represents a data set
    TYPE(string), INTENT(IN) :: label(:)
    !! label of each dataset
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_plot_x3y3z3w2
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
! summary: Create plot structured grid

INTERFACE
  MODULE SUBROUTINE vts_plot_x2y2w2(obj, x, y, w, label, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! x coordinate of mesh grid
    REAL(DFP), INTENT(IN) :: y(:, :)
    !! y coordinate of mesh grid
    REAL(DFP), INTENT(IN) :: w(:, :)
    !! z coordinate of mesh grid
    CHARACTER(*), INTENT(IN) :: label
    !! label of dataset
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_plot_x2y2w2
END INTERFACE

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Create plot structured grid

INTERFACE
  MODULE SUBROUTINE vts_plot_x2y2w2b(obj, x, y, w, label, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! x coordinate of mesh grid
    REAL(DFP), INTENT(IN) :: y(:, :)
    !! y coordinate of mesh grid
    REAL(DFP), INTENT(IN) :: w(:, :)
    !! each col of w denotes data
    !! number of cols of w should be same as size of label
    TYPE(String), INTENT(IN) :: label(:)
    !! label of dataset
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_plot_x2y2w2b
END INTERFACE

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Create plot structured grid

INTERFACE
  MODULE SUBROUTINE vts_plot_x2y2w3(obj, x, y, w, label, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! x coordinate of mesh grid
    REAL(DFP), INTENT(IN) :: y(:, :)
    !! y coordinate of mesh grid
    REAL(DFP), INTENT(IN) :: w(:, :, :)
    !! z coordinate of mesh grid
    TYPE(String), INTENT(IN) :: label(:)
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_plot_x2y2w3
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
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: plot structured grid

INTERFACE
  MODULE SUBROUTINE vts_plot_x3y3z3w3(obj, x, y, z, w, label, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:, :, :)
    !! x ccoord of mesh grid
    REAL(DFP), INTENT(IN) :: y(:, :, :)
    !! y coord of mesh grid
    REAL(DFP), INTENT(IN) :: z(:, :, :)
    !! z coord of mesh grid
    REAL(DFP), INTENT(IN) :: w(:, :, :)
    !! w coord of mesh grid
    CHARACTER(*), INTENT(IN) :: label
    !! label of dataset
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_plot_x3y3z3w3
END INTERFACE

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: plot structured grid

INTERFACE
  MODULE SUBROUTINE vts_plot_x3y3z3w4(obj, x, y, z, w, label, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:, :, :)
    !! x ccoord of mesh grid
    REAL(DFP), INTENT(IN) :: y(:, :, :)
    !! y coord of mesh grid
    REAL(DFP), INTENT(IN) :: z(:, :, :)
    !! z coord of mesh grid
    REAL(DFP), INTENT(IN) :: w(:, :, :, :)
    !! w coord of mesh grid
    TYPE(String), INTENT(IN) :: label(:)
    !! label of dataset
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_plot_x3y3z3w4
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
    !! x coordinates of all points
    REAL(DFP), INTENT(IN) :: y(:)
    !! y coords of all points
    REAL(DFP), INTENT(IN) :: z(:)
    !! z coords of all points
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
    !! x coordinates of all points
    REAL(DFP), INTENT(IN) :: y(:)
    !! y coordinates of all points
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

!----------------------------------------------------------------------------
!                                                 Scatter3D@ScatterMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-10
! summary:  Scatter3D plot using VTK

INTERFACE
  MODULE SUBROUTINE plot_scatter3D_4(obj, x, y, z, w, label, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    !! x coordinates of all points
    REAL(DFP), INTENT(IN) :: y(:)
    !! y coords of all points
    REAL(DFP), INTENT(IN) :: z(:)
    !! z coords of all points
    REAL(DFP), INTENT(IN) :: w(:, :)
    !! each column of w represents data on x,y,z
    CHARACTER(*), INTENT(IN) :: label
    !! label
    CHARACTER(*), INTENT(IN) :: filename
    !! vtkfile name, this file will be opened and closed by this
    !! routine, the extension should be .vtp
  END SUBROUTINE plot_scatter3D_4
END INTERFACE

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE vts_plot_x1y1f(obj, x, y, f, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: y(:)
    PROCEDURE(iface_SpaceFunction), POINTER :: f
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_plot_x1y1f
END INTERFACE

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE vts_plot_x2y2f(obj, x, y, f, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:, :)
    REAL(DFP), INTENT(IN) :: y(:, :)
    PROCEDURE(iface_SpaceFunction), POINTER :: f
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_plot_x2y2f
END INTERFACE

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE vts_plot_x3y3z3f(obj, x, y, z, f, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:, :, :)
    REAL(DFP), INTENT(IN) :: y(:, :, :)
    REAL(DFP), INTENT(IN) :: z(:, :, :)
    PROCEDURE(iface_SpaceFunction), POINTER :: f
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_plot_x3y3z3f
END INTERFACE

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE vts_surface_x1y1f(obj, x, y, f, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: y(:)
    PROCEDURE(iface_SpaceFunction), POINTER :: f
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_surface_x1y1f
END INTERFACE

!----------------------------------------------------------------------------
!                                                Plot@StructuredGridMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE vts_surface_x2y2f(obj, x, y, f, filename)
    CLASS(VTKPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:, :)
    REAL(DFP), INTENT(IN) :: y(:, :)
    PROCEDURE(iface_SpaceFunction), POINTER :: f
    CHARACTER(*), INTENT(IN) :: filename
  END SUBROUTINE vts_surface_x2y2f
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE VTKPlot_Class
