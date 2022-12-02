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

MODULE PLPlot_Class
USE GlobalData
USE BaseType
USE BaseMethod
USE ExceptionHandler_Class, ONLY: e
USE ParameterList, ONLY: ParameterList_
USE AbstractPlot_Class
USE EasyPlplot, ONLY: binData
IMPLICIT NONE
PRIVATE
PUBLIC :: binData
CHARACTER(LEN=*), PARAMETER :: modName = "PLPlot_Class"
CHARACTER(LEN=*), PARAMETER, PUBLIC :: PS_PLUS = "+"
CHARACTER(LEN=*), PARAMETER, PUBLIC :: PS_CROSS = "x"
CHARACTER(LEN=*), PARAMETER, PUBLIC :: PS_ASTERIC = "*"
CHARACTER(LEN=*), PARAMETER, PUBLIC :: PS_DOT = "."
CHARACTER(LEN=*), PARAMETER, PUBLIC :: PS_SQUARE = "s"
CHARACTER(LEN=*), PARAMETER, PUBLIC :: PS_STAR = "star"
CHARACTER(LEN=*), PARAMETER, PUBLIC :: PS_H_CIRCLE = "h_circle"
CHARACTER(LEN=*), PARAMETER, PUBLIC :: PS_H_SQUARE = "h_square"
CHARACTER(LEN=*), PARAMETER, PUBLIC :: PS_H_TRIAG_U = "h_triag_u"
CHARACTER(LEN=*), PARAMETER, PUBLIC :: PS_H_DIAMOND = "h_diamond"
CHARACTER(LEN=*), PARAMETER, PUBLIC :: PS_TRIAG_U = "triag_u"
CHARACTER(LEN=*), PARAMETER, PUBLIC :: PS_TRIAG_L = "triag_l"
CHARACTER(LEN=*), PARAMETER, PUBLIC :: PS_TRIAG_D = "triag_d"
CHARACTER(LEN=*), PARAMETER, PUBLIC :: PS_TRIAG_R = "triag_r"
CHARACTER(LEN=*), PARAMETER :: CODE_PS_PLUS = "#(140)"
CHARACTER(LEN=*), PARAMETER :: CODE_PS_CROSS = "#(141)"
CHARACTER(LEN=*), PARAMETER :: CODE_PS_ASTERIC = "#(142)"
CHARACTER(LEN=*), PARAMETER :: CODE_PS_DOT = "#(143)"
CHARACTER(LEN=*), PARAMETER :: CODE_PS_SQUARE = "#(144)"
CHARACTER(LEN=*), PARAMETER :: CODE_PS_STAR = "#(145)"
CHARACTER(LEN=*), PARAMETER :: CODE_PS_H_CIRCLE = "#(840)" !135
CHARACTER(LEN=*), PARAMETER :: CODE_PS_H_SQUARE = "#(841)"
CHARACTER(LEN=*), PARAMETER :: CODE_PS_H_TRIAG_U = "#(842)"
CHARACTER(LEN=*), PARAMETER :: CODE_PS_H_DIAMOND = "#(843)"
CHARACTER(LEN=*), PARAMETER :: CODE_PS_TRIAG_U = "#(852)"
CHARACTER(LEN=*), PARAMETER :: CODE_PS_TRIAG_L = "#(853)"
CHARACTER(LEN=*), PARAMETER :: CODE_PS_TRIAG_D = "#(854)"
CHARACTER(LEN=*), PARAMETER :: CODE_PS_TRIAG_R = "#(855)"

!----------------------------------------------------------------------------
!                                                                 PLPlot_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 11 July 2022
! summary: Interface to PLPLOT library.
!
!# Introduction
!
! Following pointStyle are allowed
!
! - "+" `ps_plus`
! - "x"  `ps_cross`
! - "*" `ps_asteric`
! - "." `ps_dot`
! - "s"  `pl_square`
! - "star" `ps_star`
! - "h_circle" `ps_h_circle`
! - "h_square" `ps_h_square`
! - "h_triag_u" `ps_h_triag_u`
! - "h_diamond"
! - "triag_u"
! - "triag_l"
! - "triag_d"
! - "triag_r"
!
! Following lineStyle
!
! - "-"
! - ":"
! - "--"

TYPE, EXTENDS(AbstractPlot_) :: PLPlot_
CONTAINS
  PRIVATE
  !!
  !! @ConstructorMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => Plot_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => Plot_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => Plot_Display
  !!
  !! @PlotMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Show => plot_Show
  PROCEDURE, PUBLIC, PASS(obj) :: Figure => plot_Figure
  PROCEDURE, PUBLIC, PASS(obj) :: Subplot => plot_Subplot
  !!
  !! @LinePlotMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Plot2D => plot_Plot2D
  PROCEDURE, PUBLIC, PASS(obj) :: ErrorBar => plot_ErrorBar
  PROCEDURE, PUBLIC, PASS(obj) :: Plot3D => plot_Plot3D
  PROCEDURE, PUBLIC, PASS(obj) :: line_plot_x1y1
  PROCEDURE, PUBLIC, PASS(obj) :: line_plot_x1y2
  GENERIC, PUBLIC :: LinePlot => &
    & line_plot_x1y1, &
    & line_plot_x1y2
  !!
  !! @ScatterMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Scatter => plot_Scatter
  !!
  !! @ContourMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Contour => plot_Contour
  PROCEDURE, PUBLIC, PASS(obj) :: Contourf => plot_Contourf
  !!
  !! @ColorbarMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Colorbar => plot_Colorbar
  PROCEDURE, PUBLIC, PASS(obj) :: Colorbar2 => plot_Colorbar2
  !!
  !! @BarMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Bar => plot_Bar
  PROCEDURE, PUBLIC, PASS(obj) :: Barh => plot_Barh
  PROCEDURE, PUBLIC, PASS(obj) :: Hist => plot_Hist
  !!
  !! @FillMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: FillBetween => plot_FillBetween
  PROCEDURE, PUBLIC, PASS(obj) :: FillBetweenx => plot_FillBetweenx
  !!
  !! @QuiverMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Quiver => plot_Quiver
  !!
  !! @SurfaceMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Surface => plot_Surface
  PROCEDURE, PUBLIC, PASS(obj) :: Wireframe => plot_Wireframe
  !!
  !! @SetMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: SetXlim => plot_SetXlim
  PROCEDURE, PUBLIC, PASS(obj) :: SetYlim => plot_SetYlim
  PROCEDURE, PUBLIC, PASS(obj) :: SetXYlim => plot_SetXYlim
  PROCEDURE, PUBLIC, PASS(obj) :: SetXYZlim => plot_SetXYZlim
  PROCEDURE, PUBLIC, PASS(obj) :: SetTitle => plot_SetTitle
  PROCEDURE, PUBLIC, PASS(obj) :: SetXLabel => plot_SetXLabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetYLabel => plot_SetYLabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetXYLabel => plot_SetXYLabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetXYZLabel => plot_SetXYZLabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetLabels => plot_SetLabels
  PROCEDURE, PUBLIC, PASS(obj) :: SetTicks => plot_SetTicks
  PROCEDURE, PUBLIC, PASS(obj) :: SetXTicks => plot_SetXTicks
  PROCEDURE, PUBLIC, PASS(obj) :: SetYTicks => plot_SetYTicks
  PROCEDURE, PUBLIC, PASS(obj) :: SetLegend => plot_SetLegend
  PROCEDURE, PUBLIC, PASS(obj) :: Set => plot_Set
END TYPE PLPlot_

PUBLIC :: PLPlot_

!----------------------------------------------------------------------------
!                                                            PLPlotPointer
!----------------------------------------------------------------------------

TYPE :: PLPlotPointer_
  CLASS(PLPlot_), POINTER :: ptr => NULL()
END TYPE PLPlotPointer_

PUBLIC :: PLPlotPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 July 2022
! summary: Initiate the plotting engine
!
!# Introduction
!
! This routine initiate the kernel.

INTERFACE
  MODULE SUBROUTINE Plot_Initiate(obj, param)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), OPTIONAL, INTENT(IN) :: param
  END SUBROUTINE Plot_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 July 2022
! summary: Deallocate the data stored inside the plot

INTERFACE
  MODULE SUBROUTINE Plot_Deallocate(obj)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  END SUBROUTINE Plot_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Display@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 July 2022
! summary: Display the content of Plot

INTERFACE
  MODULE SUBROUTINE Plot_Display(obj, msg, unitno)
    CLASS(PLPlot_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE Plot_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetDeviceName@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION GetDeviceName(filename) RESULT(ans)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    TYPE(String) :: ans
  END FUNCTION GetDeviceName
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Show@PlotMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Show(obj)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  END SUBROUTINE plot_Show
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Figure@PlotMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Figure(obj)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  END SUBROUTINE plot_Figure
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Subplot@PlotMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Subplot(obj, ncol, nrow, i, aspect, is3D)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ncol
    INTEGER(I4B), INTENT(IN) :: nrow
    INTEGER(I4B), INTENT(IN) :: i
    REAL(DFP), OPTIONAL, INTENT(IN) :: aspect
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: is3D
  END SUBROUTINE plot_Subplot
END INTERFACE

!----------------------------------------------------------------------------
!                                                   LinePlot@LinePlotMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE line_plot_x1y1(obj, &
    & x, y, filename, &
    & xmin, ymin, xmax, ymax, &
    & fontScaling, isWhiteOnBlack, &
    & isTransparent, colormap, figSize, &
    & lineColor, lineType, lineWidth, &
    & pointColor, pointType, pointSize, &
    & dx, dy, isLogX, isLogY, tickColor, tickWidth, &
    & xlabel, ylabel, title, labelColor)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: y(:)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    REAL(DFP), OPTIONAL, INTENT(IN) :: xmin, xmax
    REAL(DFP), OPTIONAL, INTENT(IN) :: ymin, ymax
    REAL(DFP), OPTIONAL, INTENT(IN) :: fontScaling
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isWhiteOnBlack
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTransparent
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: colormap
  !! "CoolWarm", "Gray", "BlueYellow", "BlueRed", "Radar"
  !! "HighFreq", "LowFreq"
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: figSize(:)
  !! figure size in pixels
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: lineColor
  !! "w", "k", "r", "g" etc.
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: lineType
  !! "-", ":", "--"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lineWidth
  !! line width
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: pointColor
  !! "w", "k", "r", "g" etc.
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: pointType
  !! "+", "x", "*", "."
    REAL(DFP), OPTIONAL, INTENT(IN) :: pointSize
  !! point size
    REAL(DFP), OPTIONAL, INTENT(IN) :: dx
  !! spacing between x ticks
    REAL(DFP), OPTIONAL, INTENT(IN) :: dy
  !! spacing between y ticks
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isLogX
  !! is x axis log
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isLogY
  !! is y axis log
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: tickColor
  !! color of x and y tick
    REAL(DFP), OPTIONAL, INTENT(IN) :: tickWidth
  !! tick width
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: xlabel
  !! x axis label
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: ylabel
  !! y axis label
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: title
  !! plot title
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: labelColor
  END SUBROUTINE line_plot_x1y1
END INTERFACE

! INTERFACE
! MODULE SUBROUTINE line_plot_x1y1( obj, x, y, filename, &
!   & xlabel, ylabel, title, lineWidth, xmin, ymin, xmax, ymax, &
!   & isPoint, pointSize, pointType )
!   CLASS( PLPlot_ ), INTENT( IN ) :: obj
!   REAL( DFP ), INTENT( IN ) :: x( : )
!   REAL( DFP ), INTENT( IN ) :: y( : )
!   CHARACTER( LEN = * ), INTENT( IN ) :: filename
!   CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: xlabel
!   CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: ylabel
!   CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: title
!   REAL( DFP ), OPTIONAL, INTENT( IN ) :: lineWidth
!   REAL( DFP ), OPTIONAL, INTENT( IN ) :: xmin, xmax
!   REAL( DFP ), OPTIONAL, INTENT( IN ) :: ymin, ymax
!   LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isPoint
!     !! If true we print points also
!   INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: pointSize
!   CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: pointType
! END SUBROUTINE line_plot_x1y1
! END INTERFACE

!----------------------------------------------------------------------------
!                                                  LinePlot@LinePlotMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE line_plot_x1y2(obj, x, y, filename, &
    & xlabel, ylabel, legendTexts, title, lineWidth, xmin, ymin, xmax, ymax, &
    & isPoint, pointSize, pointType)
    CLASS(PLPlot_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: y(:, :)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: xlabel
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: ylabel
    TYPE(String), OPTIONAL, INTENT(IN) :: legendTexts(:)
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: title
    REAL(DFP), OPTIONAL, INTENT(IN) :: lineWidth
    REAL(DFP), OPTIONAL, INTENT(IN) :: xmin, xmax
    REAL(DFP), OPTIONAL, INTENT(IN) :: ymin, ymax
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPoint
    !! If true we print points also
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: pointSize
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: pointType
  END SUBROUTINE line_plot_x1y2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Plot2D@LinePlotMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Plot2D(obj, x, y, lineColor, lineType, &
    & lineWidth, pointColor, pointType, pointSize)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: y(:)
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: lineColor
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: lineType
    REAL(DFP), OPTIONAL, INTENT(IN) :: lineWidth
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: pointColor
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: pointType
    REAL(DFP), OPTIONAL, INTENT(IN) :: pointSize
  END SUBROUTINE plot_Plot2D
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Errorbar@LinePlotMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_ErrorBar(obj, x, y, xerr, yerr, &
    & lineColor, lineType, lineWidth)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: y(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: xerr(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: yerr(:)
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: lineColor
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: lineType
    REAL(DFP), OPTIONAL, INTENT(IN) :: lineWidth
  END SUBROUTINE plot_ErrorBar
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Plot3D@LinePlotMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Plot3D(obj, x, y, z, lineColor, lineType, &
    & lineWidth, pointColor, pointType, pointSize)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: y(:)
    REAL(DFP), INTENT(IN) :: z(:)
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: lineColor
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: lineType
    REAL(DFP), OPTIONAL, INTENT(IN) :: lineWidth
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: pointColor
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: pointType
    REAL(DFP), OPTIONAL, INTENT(IN) :: pointSize
  END SUBROUTINE plot_Plot3D
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Scatter@ScatterMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Scatter(obj, x, y, c, s, pointColor, &
    & pointType, pointSize)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
  !! x data
    REAL(DFP), INTENT(IN) :: y(:)
  !! y data
    REAL(DFP), OPTIONAL, INTENT(IN) :: c(:)
  !! data for smooth coloring
    REAL(DFP), OPTIONAL, INTENT(IN) :: s(:)
  !! data for marker scaling
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: pointColor
  !! point color
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: pointType
  !! point type
    REAL(DFP), OPTIONAL, INTENT(IN) :: pointSize
  !! point size
  END SUBROUTINE plot_Scatter
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Contour@ContourMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Contour(obj, x, y, z, N, lineColor, &
    & lineType, lineWidth)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  !!
    REAL(DFP), INTENT(IN) :: x(:)
  !! x-coordinates of data
    REAL(DFP), INTENT(IN) :: y(:)
  !! y-coordinates of data
    REAL(DFP), INTENT(IN) :: z(:, :)
  !! Data for contouring
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: N
  !! Number of levels to use in contour
    CHARACTER(*), OPTIONAL, INTENT(IN) :: lineColor
  !! Color of contour lines
    CHARACTER(*), OPTIONAL, INTENT(IN) :: lineType
  !! Style of contour lines
    REAL(DFP), OPTIONAL, INTENT(IN) :: lineWidth
  !! Width of contour lines
  END SUBROUTINE plot_Contour
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Contourf@ContourMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Contourf(obj, x, y, z, N)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  !!
    REAL(DFP), INTENT(IN) :: x(:)
  !! x-coordinates of data
    REAL(DFP), INTENT(IN) :: y(:)
  !! y-coordinates of data
    REAL(DFP), INTENT(IN) :: z(:, :)
  !! Data for contouring
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: N
  END SUBROUTINE plot_Contourf
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Colorbar@ColorbarMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Colorbar(obj, z, N, leftLabel, rightLabel)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: z(:, :)
  !! data used for levels computation
    INTEGER(I4B), INTENT(IN) :: N
  !! number of levels to compute
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: leftLabel
  !! Label for left side of the colorbar
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: rightLabel
  !! Label for right side of color bar
  END SUBROUTINE plot_Colorbar
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Colorbar@ColorbarMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Colorbar2(obj, z, N, leftLabel, rightLabel)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: z(:, :)
  !! data used for levels computation
    INTEGER(I4B), INTENT(IN) :: N
  !! number of levels to compute
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: leftLabel
  !! Label for left side of the colorbar
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: rightLabel
  !! Label for right side of color bar
  END SUBROUTINE plot_Colorbar2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Bar@BarMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Bar(obj, x, y, c, relWidth, fillColor, &
    & fillPattern, lineColor, lineWidth)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
  !! x-positions of the bars' centers
    REAL(DFP), INTENT(IN) :: y(:)
  !! y-positions of the bars' tops
    REAL(DFP), INTENT(IN), OPTIONAL :: c(:)
  !! Color scale for bars
    REAL(DFP), INTENT(IN), OPTIONAL :: relWidth
  !! Relative width of bars (default 0.8)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fillColor
  !! Color of bar fills
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fillPattern
  !! Pattern of bar fills
    CHARACTER(*), OPTIONAL, INTENT(IN) :: lineColor
  !! Color of lines around bars
    REAL(DFP), OPTIONAL, INTENT(IN) :: lineWidth
  !! Width of lines around bars
  END SUBROUTINE plot_Bar
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Barh@BarMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Barh(obj, y, x, c, relWidth, fillColor, &
    & fillPattern, lineColor, lineWidth)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  !!
    REAL(DFP), INTENT(IN) :: y(:)
  !! y-positions of the bars' centers
    REAL(DFP), INTENT(IN) :: x(:)
  !! x-positions of the bars' tops
    REAL(DFP), OPTIONAL, INTENT(IN) :: c(:)
  !! Color scale for bars
    REAL(DFP), OPTIONAL, INTENT(IN) :: relWidth
  !! Relative width of bars
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fillColor
  !! Color of bar fills
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fillPattern
  !! Pattern of bar fills
    CHARACTER(*), OPTIONAL, INTENT(IN) :: lineColor
  !! Color of lines around bars
    REAL(DFP), OPTIONAL, INTENT(IN) :: lineWidth
  !! Width of lines around bars
  END SUBROUTINE plot_Barh
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Hist@BarMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE plot_Hist(obj, d, N, db, relWidth, fillColor, fillPattern, &
              & lineColor, lineWidth)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  !!
    REAL(DFP), INTENT(IN) :: d(:)
  !! Data for binning
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: N
  !! Number of bins
    REAL(DFP), OPTIONAL, INTENT(IN) :: db(2)
  !! Boundaries of bin range
    REAL(DFP), OPTIONAL, INTENT(IN) :: relWidth
  !! Relative width of bars (default 0.8)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fillColor
  !! Color of bar fills
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fillPattern
  !! Pattern of bar fills
    CHARACTER(*), OPTIONAL, INTENT(IN) :: lineColor
  !! Color of lines around bars
    REAL(DFP), OPTIONAL, INTENT(IN) :: lineWidth
  !! Width of lines around bars
  END SUBROUTINE plot_Hist
END INTERFACE

!----------------------------------------------------------------------------
!                                                    FillBetween@FillMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_FillBetween(obj, x, y1, y0, fillColor, &
    & fillPattern, lineWidth)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  !!
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: y1(:)
    REAL(DFP), INTENT(IN), OPTIONAL :: y0(:)
    CHARACTER(*), INTENT(IN), OPTIONAL :: fillColor
    CHARACTER(*), INTENT(IN), OPTIONAL :: fillPattern
    REAL(DFP), INTENT(IN), OPTIONAL :: lineWidth
  END SUBROUTINE plot_FillBetween
END INTERFACE

!----------------------------------------------------------------------------
!                                                    FillBetween@FillMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_FillBetweenx(obj, y, x1, x0, fillColor, &
    & fillPattern, lineWidth)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  !!
    REAL(DFP), INTENT(IN) :: y(:)
    REAL(DFP), INTENT(IN) :: x1(:)
    REAL(DFP), INTENT(IN), OPTIONAL :: x0(:)
    CHARACTER(*), INTENT(IN), OPTIONAL :: fillColor
    CHARACTER(*), INTENT(IN), OPTIONAL :: fillPattern
    REAL(DFP), INTENT(IN), OPTIONAL :: lineWidth
  END SUBROUTINE plot_FillBetweenx
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Quiver@QuiverMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Quiver(obj, x, y, u, v, s, c, scaling, &
    & lineColor, lineType, lineWidth)
  !!
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  !!
    REAL(DFP), INTENT(IN) :: x(:)
  !! x-positions of vectors
    REAL(DFP), INTENT(IN) :: y(:)
  !! y-positions of vectors
    REAL(DFP), INTENT(IN) :: u(:, :)
  !! u-components of vectors
    REAL(DFP), INTENT(IN) :: v(:, :)
  !! v-components of vectors
    REAL(DFP), INTENT(IN), OPTIONAL :: s(:, :)
  !! Scale of vectors
    REAL(DFP), INTENT(IN), OPTIONAL :: c(:, :)
  !! Color values for vectors
    REAL(DFP), INTENT(IN), OPTIONAL :: scaling
  !! Scaling of vectors
  !! < 0 = Automatic, then scaled
  !!   0 = Automatic
  !! > 0 = Directly scaled
    CHARACTER(*), INTENT(IN), OPTIONAL :: lineColor
  !! Color of vectors
    CHARACTER(*), INTENT(IN), OPTIONAL :: lineType
  !! Style of vectors' lines
    REAL(DFP), OPTIONAL, INTENT(IN) :: lineWidth
  !! Width of vectors' lines
  END SUBROUTINE plot_Quiver
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Surface@SurfaceMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Surface(obj, x, y, z, N, lineType)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
  !! x coord
    REAL(DFP), INTENT(IN) :: y(:)
  !! y coord
    REAL(DFP), INTENT(IN) :: z(:, :)
  !! data for contouring
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: N
  !! number of levels to use in surface colors
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: lineType
  !! Style for xy lines
  !! "-" = on
  !! "" = off
  END SUBROUTINE plot_Surface
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Wireframe@SurfaceMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Wireframe(obj, x, y, z, lineColor)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
  !! x coord
    REAL(DFP), INTENT(IN) :: y(:)
  !! y coord
    REAL(DFP), INTENT(IN) :: z(:, :)
  !! data for contouring
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: lineColor
  !! Color of contour lines
  END SUBROUTINE plot_Wireframe
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetXlim@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_SetXlim(obj, xmin, xmax)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: xmin
    REAL(DFP), INTENT(IN) :: xmax
  END SUBROUTINE plot_SetXlim
END INTERFACE

!----------------------------------------------------------------------------
!                                                         SetYlim@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_SetYlim(obj, ymin, ymax)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: ymin
    REAL(DFP), INTENT(IN) :: ymax
  END SUBROUTINE plot_SetYlim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetXYlim@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_SetXYlim(obj, x, y)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(2)
    REAL(DFP), INTENT(IN) :: y(2)
  END SUBROUTINE plot_SetXYlim
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetXYZlim@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_SetXYZlim(obj, x, y, z, altitude, azimuth, zoom)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  !!
    REAL(DFP), INTENT(IN) :: x(2)
  !! x-range of plot
    REAL(DFP), INTENT(IN) :: y(2)
  !! y-range of plot
    REAL(DFP), INTENT(IN) :: z(2)
  !! z-range of plot
    REAL(DFP), OPTIONAL, INTENT(IN) :: altitude
  !! Altitude angle of plot in degrees
    REAL(DFP), OPTIONAL, INTENT(IN) :: azimuth
  !! Azimuth angle of plot in degrees
    REAL(DFP), OPTIONAL, INTENT(IN) :: zoom
  !! Zoom ratio (default 1.0)
  END SUBROUTINE plot_SetXYZlim
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetXLabel@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_SetXLabel(obj, label, color)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: label
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: color
  END SUBROUTINE plot_SetXLabel
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetYLabel@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_SetYLabel(obj, label, color)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: label
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: color
  END SUBROUTINE plot_SetYLabel
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetXYZLabel@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_SetXYLabel(obj, xLabel, yLabel, color)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: xLabel
  !! Label for x-axis
    CHARACTER(*), INTENT(IN) :: yLabel
  !! Label for x-axis
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
  !! Color of labels
  END SUBROUTINE plot_SetXYLabel
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetXYZLabel@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_SetXYZLabel(obj, xLabel, yLabel, zLabel, color)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: xLabel
  !! Label for x-axis
    CHARACTER(*), INTENT(IN) :: yLabel
  !! Label for x-axis
    CHARACTER(*), INTENT(IN) :: zLabel
  !! Label for z-axis
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
  !! Color of labels
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetTitle@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_SetTitle(obj, label, color)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: label
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: color
  END SUBROUTINE plot_SetTitle
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetLabels@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_SetLabels(obj, xlabel, ylabel, title, color)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: xlabel
    CHARACTER(LEN=*), INTENT(IN) :: ylabel
    CHARACTER(LEN=*), INTENT(IN) :: title
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: color
  END SUBROUTINE plot_SetLabels
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetXticks@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_SetXticks(obj, d, islogScale, isPrimary, &
    & isSecondary, color, lineWidth)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  !!
    REAL(DFP), OPTIONAL, INTENT(IN) :: d
  !! Spacing between ticks
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isLogScale
  !! Flag for log-ticks and labels
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPrimary
  !! Draw primary axis
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSecondary
  !! Draw secondary axis
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: color
  !! Color code for ticks, box, and labels
    REAL(DFP), OPTIONAL, INTENT(IN) :: lineWidth
  !! Line width for ticks and box
  END SUBROUTINE plot_SetXticks
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetYticks@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_SetYticks(obj, d, islogScale, isPrimary, &
    & isSecondary, color, lineWidth)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  !!
    REAL(DFP), OPTIONAL, INTENT(IN) :: d
  !! Spacing between ticks
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isLogScale
  !! Flag for log-ticks and labels
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPrimary
  !! Draw primary axis
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSecondary
  !! Draw secondary axis
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: color
  !! Color code for ticks, box, and labels
    REAL(DFP), OPTIONAL, INTENT(IN) :: lineWidth
  !! Line width for ticks and box
  END SUBROUTINE plot_SetYticks
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetTicks@SetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE plot_SetTicks(obj, dx, dy, isLogX, isLogY, color, lineWidth)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  !!
    REAL(DFP), OPTIONAL, INTENT(IN) :: dx
  !! Spacing between ticks on x-axis
    REAL(DFP), OPTIONAL, INTENT(IN) :: dy
  !! Spacing between ticks on y-axis
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isLogX
  !! Flag for log-ticks and labels on x-axis
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isLogY
  !! Flag for log-ticks and labels on y-axis
    CHARACTER(*), INTENT(IN), OPTIONAL :: color
  !! Color code for ticks, box, and labels
    REAL(DFP), OPTIONAL, INTENT(IN) :: linewidth
  !! Line width for ticks and box
  END SUBROUTINE plot_SetTicks
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetLegend@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_SetLegend(obj, corner, series, lineWidths, &
    & pointScales, pointCounts, ncol)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
  !!
    CHARACTER(*), INTENT(IN) :: corner
  !! Corner for legend
    CHARACTER(*), INTENT(IN) :: series(:, :)
  !! Data series in rows
  !! [name,textColor,lineStyle,lineColor,pointStyle,pointColor,boxColor]
    REAL(DFP), OPTIONAL, INTENT(IN) :: lineWidths(:)
  !! Line widths for the plots
    REAL(DFP), OPTIONAL, INTENT(IN) :: pointScales(:)
  !! Marker sizes for the plots
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: pointCounts(:)
  !! Marker counts for the plots
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ncol
  !! Number of columns
  END SUBROUTINE plot_SetLegend
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE plot_Set(obj, device, fileName, fontScaling,&
    & isWhiteOnBlack, isTransparent, colormap, figSize, isFileFamily)
    CLASS(PLPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), OPTIONAL, INTENT(IN) :: device
  !! Output device to use
  !! * qtwidget
  !! * svgqt
  !! * pngqt
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fileName
  !! Name of file(s) to write to
  !! The text `%n` will be replaced with the figure number
    REAL(DFP), OPTIONAL, INTENT(IN) :: fontScaling
  !! Font scaling relative to default value
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isWhiteOnBlack
  !! Default foreground and background colors
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTransparent
  !! Transparent background
    CHARACTER(*), OPTIONAL, INTENT(IN) :: colormap
  !! Colormap to use
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: figSize(:)
  !! Size of figures to produce in pixels
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFileFamily
  END SUBROUTINE plot_Set
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE PLPlot_Class
