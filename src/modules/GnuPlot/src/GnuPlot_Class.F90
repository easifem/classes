! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

MODULE GnuPlot_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseMethod, ONLY: TOSTRING
USE BaseType, ONLY: RealVectorPointer_, &
                    RealMatrixPointer_
USE ExceptionHandler_Class, ONLY: e
USE String_Class, ONLY: String
USE StringUtility, ONLY: LowerCase
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table

USE UserFunction_Class, ONLY: UserFunction_

USE GnuPlotOpt_Class, ONLY: GnuPlotOpt_, &
                            DefaultGnuplotOpt_, &
                            GnuPlotLabel_, &
                            GnuPlotAxis_, &
                            GnuPlotTick_, &
                            GnuPlotPlotOpts_

IMPLICIT NONE

PRIVATE

PUBLIC :: GnuPlot_
PUBLIC :: GnuPlotPointer_

CHARACTER(*), PARAMETER :: modName = 'Gnuplot_Class'
INTEGER(I4B), PARAMETER :: NOT_INITIALIZED = -32000
TYPE(DefaultGnuplotOpt_), PARAMETER :: defaultOpt = DefaultGnuplotOpt_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary: GnuPlot_ class

TYPE :: GnuPlot_
  LOGICAL(LGT) :: isInitiated = .FALSE.

  TYPE(TxtFile_) :: pltfile

  TYPE(GnuPlotOpt_) :: opts

  TYPE(String) :: plotCommand(defaultOpt%maxNumberPlots)

  INTEGER(I4B) :: addPlotCount = 0

  ! DATA pointer
  TYPE(RealMatrixPointer_), ALLOCATABLE :: xMats(:), yMats(:), zMats(:)
  TYPE(RealVectorPointer_), ALLOCATABLE :: xVecs(:), yVecs(:), zVecs(:)

CONTAINS

  !! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  FINAL :: obj_Finalize

  !! plot lines
  PROCEDURE, PUBLIC, PASS(obj) :: multiplot => obj_multiplot
  PROCEDURE, PUBLIC, PASS(obj) :: plot1 => obj_plot1
  PROCEDURE, PUBLIC, PASS(obj) :: addPlot => obj_addPlot
  PROCEDURE, PUBLIC, PASS(obj) :: plot2 => obj_plot2
  PROCEDURE, PUBLIC, PASS(obj) :: plot3 => obj_plot3
  PROCEDURE, PUBLIC, PASS(obj) :: plot4 => obj_plot4
  PROCEDURE, PUBLIC, PASS(obj) :: plotFunc1 => obj_plotFunc1
  PROCEDURE, PUBLIC, PASS(obj) :: plotFunc2 => obj_plotFunc2
  PROCEDURE, PUBLIC, PASS(obj) :: plotFunc3 => obj_plotFunc3
  GENERIC, PUBLIC :: plot => plot1, plot2, plot3, plot4, &
    plotFunc1, plotFunc2, plotFunc3

  PROCEDURE, PUBLIC, PASS(obj) :: plotData1 => obj_plotData1
  GENERIC, PUBLIC :: plotData => plotData1

  !! 3d line plot
  PROCEDURE, PUBLIC, PASS(obj) :: plot3d_1 => obj_plot3d_vvv
  GENERIC, PUBLIC :: plot3d => plot3d_1

  !! surface plot
  PROCEDURE, PUBLIC, PASS(obj) :: surf1 => obj_surf1
  GENERIC, PUBLIC :: surf => surf1

  !! contour plot
  PROCEDURE, PUBLIC, PASS(obj) :: contour1 => obj_contour1
  PROCEDURE, PUBLIC, PASS(obj) :: contour2 => obj_contour2
  PROCEDURE, PUBLIC, PASS(obj) :: contour3 => obj_contour3
  GENERIC, PUBLIC :: contour => contour1, contour2, contour3

  ! @misc
  PROCEDURE, PUBLIC, PASS(obj) :: RunScript => obj_RunScript
  PROCEDURE, PUBLIC, PASS(obj) :: animationStart => obj_animationStart
  PROCEDURE, PUBLIC, PASS(obj) :: animationShow => obj_animationShow

  ! @Get (private)
  PROCEDURE, PASS(obj) :: SetPlotCommand => obj_SetPlotCommand
  ! @Write
  PROCEDURE, PUBLIC, PASS(obj) :: WritePlotSetup => obj_WritePlotSetup
  PROCEDURE, PUBLIC, PASS(obj) :: WriteDataBlock => obj_writeDataBlock_xy

  ! @TomlMethods
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml2 => &
    obj_ImportFromToml2

  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2

  ! @Set method through GnuplotOpt_
  PROCEDURE, PUBLIC, PASS(obj) :: SetTerm => obj_SetTerm

  PROCEDURE, PUBLIC, PASS(obj) :: SetTitle => obj_SetTitle

  PROCEDURE, PUBLIC, PASS(obj) :: SetAxisLabel => obj_SetAxisLabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetXLabel => obj_setxlabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetX2Label => obj_setx2label
  PROCEDURE, PUBLIC, PASS(obj) :: Setylabel => obj_setylabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetY2Label => obj_sety2label
  PROCEDURE, PUBLIC, PASS(obj) :: SetZLabel => obj_SetZLabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetCBLabel => obj_SetCBLabel

  PROCEDURE, PUBLIC, PASS(obj) :: SetAxisLim => obj_SetAxisLim
  PROCEDURE, PUBLIC, PASS(obj) :: SetXLim => obj_SetXLim
  PROCEDURE, PUBLIC, PASS(obj) :: SetX2Lim => obj_SetX2Lim
  PROCEDURE, PUBLIC, PASS(obj) :: SetYLim => obj_SetYLim
  PROCEDURE, PUBLIC, PASS(obj) :: SetY2Lim => obj_SetY2Lim
  PROCEDURE, PUBLIC, PASS(obj) :: SetZLim => obj_SetZLim
  PROCEDURE, PUBLIC, PASS(obj) :: SetCBLim => obj_SetCBLim

  PROCEDURE, PUBLIC, PASS(obj) :: SetPlotScale => obj_SetPlotScale
  PROCEDURE, PUBLIC, PASS(obj) :: SetXScale => obj_SetXScale
  PROCEDURE, PUBLIC, PASS(obj) :: SetX2Scale => obj_SetX2Scale
  PROCEDURE, PUBLIC, PASS(obj) :: SetYScale => obj_SetYScale
  PROCEDURE, PUBLIC, PASS(obj) :: SetY2Scale => obj_SetY2Scale
  PROCEDURE, PUBLIC, PASS(obj) :: SetZScale => obj_SetZScale
  PROCEDURE, PUBLIC, PASS(obj) :: SetCBScale => obj_SetCBScale

  PROCEDURE, PUBLIC, PASS(obj) :: SetFilename => obj_SetFilename
  PROCEDURE, PUBLIC, PASS(obj) :: SetOutput => obj_SetOutput
  PROCEDURE, PUBLIC, PASS(obj) :: SetCommandLine => obj_SetCommandLine
  PROCEDURE, PUBLIC, PASS(obj) :: SetOptions => obj_SetOptions
  PROCEDURE, PUBLIC, PASS(obj) :: SetScripts => obj_SetScripts

  PROCEDURE, PUBLIC, PASS(obj) :: Reset => obj_Reset
  PROCEDURE, PUBLIC, PASS(obj) :: SetUseDefaultPreset => &
    obj_SetUseDefaultPreset

END TYPE GnuPlot_

!----------------------------------------------------------------------------
!                                                              GnuPlotPointer
!----------------------------------------------------------------------------

TYPE :: GnuPlotPointer_
  CLASS(GnuPlot_), POINTER :: ptr => NULL()
END TYPE GnuPlotPointer_

!----------------------------------------------------------------------------
!                                             Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-06
! update: 2025-02-21
! summary:  Initialize the Gnuplot object

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2024-06-06
! update: 2025-02-21
! summary: Deallocate the Gnuplot object

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Finalize@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2025-05-07
! summary:  Finalize the Gnuplot object

INTERFACE
  MODULE SUBROUTINE obj_Finalize(obj)
    TYPE(GnuPlot_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Finalize
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Display@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-06
! summary: Display the info

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(GnuPlot_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                  multiplot@MultiPlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary:  initialize the multiplot

INTERFACE
  MODULE SUBROUTINE obj_multiplot(obj, dims)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dims(2)
  END SUBROUTINE obj_multiplot
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Plot@PlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-08
! update : 2025-02-18
! summary:  Plot 1D data

INTERFACE
  MODULE SUBROUTINE obj_plot1(obj, x1, y1, ls1, axes1, &
                              x2, y2, ls2, axes2, &
                              x3, y3, ls3, axes3, &
                              x4, y4, ls4, axes4)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x1(:)
    REAL(DFP), INTENT(IN), OPTIONAL :: y1(:)
    CHARACTER(*), INTENT(IN), OPTIONAL :: ls1
    CHARACTER(*), INTENT(IN), OPTIONAL :: axes1
    REAL(DFP), INTENT(IN), DIMENSION(:), OPTIONAL :: x2
    REAL(DFP), INTENT(IN), DIMENSION(:), OPTIONAL :: y2
    CHARACTER(*), INTENT(IN), OPTIONAL :: ls2
    CHARACTER(*), INTENT(IN), OPTIONAL :: axes2
    REAL(DFP), INTENT(IN), DIMENSION(:), OPTIONAL :: x3
    REAL(DFP), INTENT(IN), DIMENSION(:), OPTIONAL :: y3
    CHARACTER(*), INTENT(IN), OPTIONAL :: ls3
    CHARACTER(*), INTENT(IN), OPTIONAL :: axes3
    REAL(DFP), INTENT(IN), DIMENSION(:), OPTIONAL :: x4
    REAL(DFP), INTENT(IN), DIMENSION(:), OPTIONAL :: y4
    CHARACTER(*), INTENT(IN), OPTIONAL :: ls4
    CHARACTER(*), INTENT(IN), OPTIONAL :: axes4
  END SUBROUTINE obj_plot1
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_AddPlot(obj, x, y, ls, axes, append, dataFileName)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN), OPTIONAL :: y(:)
    CHARACTER(*), INTENT(IN), OPTIONAL :: ls
    CHARACTER(*), INTENT(IN), OPTIONAL :: axes
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: append
    CHARACTER(*), INTENT(IN), OPTIONAL :: dataFileName
  END SUBROUTINE obj_AddPlot
END INTERFACE

!----------------------------------------------------------------------------
!                                                           plot2@PlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-08
! update: 2025-02-18
! summary:  Plot 2D data

INTERFACE
  MODULE SUBROUTINE obj_plot2(obj, xv, ymat, lspec)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: xv(:)
    REAL(DFP), INTENT(IN) :: ymat(:, :)
    TYPE(String), INTENT(IN), OPTIONAL :: lspec
  END SUBROUTINE obj_plot2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         plot3@PlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-08
! update : 2025-02-18
! summary:  Plot 2D data

INTERFACE
  MODULE SUBROUTINE obj_plot3(obj, xmat, ymat, lspec)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: xmat(:, :)
    REAL(DFP), INTENT(IN) :: ymat(:, :)
    TYPE(String), INTENT(IN), OPTIONAL :: lspec
  END SUBROUTINE obj_plot3
END INTERFACE

!----------------------------------------------------------------------------
!                                                  plot_func1@PlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary:  plot with function

INTERFACE
  MODULE SUBROUTINE obj_plotFunc1(obj, yFunc, xMin, xMax, np, lspec)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    TYPE(UserFunction_), INTENT(INOUT) :: yFunc
    ! it should be a scalar function
    REAL(DFP), INTENT(IN) :: xMin, xMax
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: np
    TYPE(String), INTENT(IN), OPTIONAL :: lspec
  END SUBROUTINE obj_plotFunc1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      plot_func2@PlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary:  plot with function

INTERFACE
  MODULE SUBROUTINE obj_plotFunc2(obj, yFunc, xVec, lspec)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    TYPE(UserFunction_), INTENT(INOUT) :: yFunc
    ! it should be a scalar function
    REAL(DFP), INTENT(IN) :: xVec(:)
    TYPE(String), INTENT(IN), OPTIONAL :: lspec
  END SUBROUTINE obj_plotFunc2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-31
! summary:   plot with prescribed functions.
! two arguments will be passed to the functions
! when no function is provided two arguments vector will be used
! as x and y.
! two arguments vector should have the same size

INTERFACE
  MODULE SUBROUTINE obj_plotFunc3(obj, xFunc, yFunc, argVec1, argVec2, &
                                  lspec)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    TYPE(UserFunction_), OPTIONAL, POINTER, INTENT(INOUT) :: xFunc
    ! it should be a scalar function
    TYPE(UserFunction_), OPTIONAL, POINTER, INTENT(INOUT) :: yFunc
    ! it should be a scalar function
    REAL(DFP), INTENT(IN) :: argVec1(:), argVec2(:)
    TYPE(String), INTENT(IN), OPTIONAL :: lspec
  END SUBROUTINE obj_plotFunc3
END INTERFACE

!----------------------------------------------------------------------------
!                                                         plot3@PlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-28
! summary:  Plot

INTERFACE
  MODULE SUBROUTINE obj_plot4(obj)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_plot4
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-02-18
! summary:  plot data exporting png file

INTERFACE
  MODULE SUBROUTINE obj_plotData1(obj, filename, xdata, ydata, &
                                  xlim, ylim, xlabel, ylabel)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: filename
    REAL(DFP), INTENT(IN) :: xdata(:), ydata(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: xlim(2), ylim(2)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: xlabel, ylabel
  END SUBROUTINE obj_plotData1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 plot3d
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! update: 2025-02-21
! summary:  plot line in 3D
!..............................................................................
! lplot3d create a line plot in 3d
! datablock is used instead of  gnuplot inline file "-"
!..............................................................................

INTERFACE
  MODULE SUBROUTINE obj_plot3d_vvv(obj, x, y, z, lspec, paletteName)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: y(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: z(:)
    CHARACTER(*), INTENT(IN), OPTIONAL :: lspec
    CHARACTER(*), INTENT(IN), OPTIONAL :: paletteName
  END SUBROUTINE obj_plot3d_vvv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       surf
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! update: 2025-02-21
! summary:  plot surface
!..............................................................................
! splot create a surface plot
! datablock is used instead of  gnuplot inline file "-"
!..............................................................................

INTERFACE
  MODULE SUBROUTINE obj_surf1(obj, x, y, z, lspec, paletteName)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: y(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: z(:, :)
    CHARACTER(*), INTENT(IN), OPTIONAL :: lspec
    CHARACTER(*), INTENT(IN), OPTIONAL :: paletteName
  END SUBROUTINE obj_surf1
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-06
! summary:  Contour plot with gnuplot

INTERFACE
  MODULE SUBROUTINE obj_contour1(obj, x, y, z, lspec, &
                                 paletteName, fill)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(:, :)
    REAL(DFP), INTENT(IN), OPTIONAL :: y(:, :)
    REAL(DFP), INTENT(IN), OPTIONAL :: z(:, :)
    CHARACTER(*), INTENT(IN), OPTIONAL :: lspec
    CHARACTER(*), INTENT(IN), OPTIONAL :: paletteName
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: fill
  END SUBROUTINE obj_contour1
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-06
! summary:  Contour plot with gnuplot

INTERFACE
  MODULE SUBROUTINE obj_contour2(obj, x, y, z, lspec, &
                                 paletteName, fill)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: y(:)
    REAL(DFP), INTENT(IN), OPTIONAL :: z(:, :)
    CHARACTER(*), INTENT(IN), OPTIONAL :: lspec
    CHARACTER(*), INTENT(IN), OPTIONAL :: paletteName
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: fill
  END SUBROUTINE obj_contour2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-04-11
! summary:  Multiple Contours plot with gnuplot

INTERFACE
  MODULE SUBROUTINE obj_contour3(obj, x1, y1, z1, x2, y2, z2, &
                                 lspec1, lspec2, paletteName, fill)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(in) :: x1(:, :), x2(:, :)
    REAL(DFP), INTENT(in), OPTIONAL :: y1(:, :), y2(:, :)
    REAL(DFP), INTENT(in), OPTIONAL :: z1(:, :), z2(:, :)
    CHARACTER(*), INTENT(in), OPTIONAL :: lspec1, lspec2
    CHARACTER(*), INTENT(in), OPTIONAL :: paletteName
    LOGICAL(LGT), INTENT(in), OPTIONAL :: fill
  END SUBROUTINE obj_contour3
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetPlotCommand@UtilityMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-05-05
! summary:  Set Plot command with setting

INTERFACE
  MODULE SUBROUTINE obj_SetPlotCommand(obj, order, lspec, axes, &
                                       dataBlockName)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(*), INTENT(IN), OPTIONAL :: lspec
    CHARACTER(*), INTENT(IN), OPTIONAL :: axes
    CHARACTER(*), OPTIONAL, INTENT(IN) :: dataBlockName
  END SUBROUTINE obj_SetPlotCommand
END INTERFACE

!----------------------------------------------------------------------------
!                                               WritePlotSetup@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  process command
!
!# Introduction
!
!   obj subroutine writes all the data into plot file
!   to be read by gnuplot

INTERFACE
  MODULE SUBROUTINE obj_WritePlotSetup(obj)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_WritePlotSetup
END INTERFACE

!----------------------------------------------------------------------------
!                                               write_xydata@UtilityMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-02-21
! summary:  write_xydata to plt file
!
!# Introduction
! Writes set of xy data into a file

INTERFACE
  MODULE SUBROUTINE obj_WriteDataBlock_xy(obj, x, y)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: y(:)
  END SUBROUTINE obj_WriteDataBlock_xy
END INTERFACE

!----------------------------------------------------------------------------
!                                             reset_to_defaults@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:
! summary:  reset to defaults

! INTERFACE
!   MODULE SUBROUTINE obj_Reset(obj)
!     CLASS(GnuPlot_), INTENT(INOUT) :: obj
!   END SUBROUTINE obj_Reset
! END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetColorPalette
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary:  get color palettes hex code
!...............................................................................
! color_palettes create color palette as a
! string to be written into gnuplot script file
! the palettes credit goes to: Anna Schnider (https://github.com/aschn) and
! Hagen Wierstorf (https://github.com/hagenw)
!...............................................................................

INTERFACE
  MODULE FUNCTION GetColorPaletteScript(paletteName) RESULT(paletteScript)
    CHARACTER(*), INTENT(IN) :: paletteName
    CHARACTER(:), ALLOCATABLE :: paletteScript
  END FUNCTION GetColorPaletteScript
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 runScript
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary:  run raw gnuplot scripts stored in obj%txtscript
!..............................................................................
! runscript sends the the script string (txtstring) into a script
! file to be run by gnuplot
!..............................................................................

INTERFACE
  MODULE SUBROUTINE obj_RunScript(obj)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_RunScript
END INTERFACE

!----------------------------------------------------------------------------
!                                                              animationStart
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary: set the setting to start an animation
!-------------------------------------------------------------------------------
! obj_animation_start: set the setting to start an animation
! it simply set flags and open a script file to write data
!-------------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_animationStart(obj, pauseSeconds)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: pauseSeconds
  END SUBROUTINE obj_animationStart
END INTERFACE

!----------------------------------------------------------------------------
!                                                             animationShow
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary:  show animation
!-------------------------------------------------------------------------------
! sub_animation_show: simply resets the animation flags
! and finalize the plotting.
!-------------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_animationShow(obj)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_animationShow
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-23
! summary:  Import settings from toml

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-23
! summary:  Import settings from toml

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     set_filename@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set filename
!
!# Introduction
!
!Set a file name for plot command output
!obj file can be used later by gnuplot as an script file to reproduce the plot

INTERFACE
  MODULE SUBROUTINE obj_SetFilename(obj, name)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
  END SUBROUTINE obj_SetFilename
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-05-22
! summary:  set output

INTERFACE
  MODULE SUBROUTINE obj_SetOutput(obj, name)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
  END SUBROUTINE obj_SetOutput
END INTERFACE

!----------------------------------------------------------------------------
!                                                        setCommand
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-06-23
! summary:  set command which is executed at the end
!          if the length of chars is 0, then no command is executed

INTERFACE
  MODULE SUBROUTINE obj_SetCommandLine(obj, chars)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: chars
  END SUBROUTINE obj_SetCommandLine
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_options@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set options
!
!# Introduction
!
! Set the plot options. obj is a very powerfull procedure accepts many types
! of gnuplot command and customization
! If reset is false, then the new options are added to the existing options

INTERFACE
  MODULE SUBROUTINE obj_SetOptions(obj, optionStr, reset)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: optionStr
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: reset
  END SUBROUTINE obj_SetOptions
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_options@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  Set gnuplot scripts from a long string
! string will be split by semicolon

INTERFACE
  MODULE SUBROUTINE obj_SetScripts(obj, scriptStr, reset)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: scriptStr
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: reset
  END SUBROUTINE obj_SetScripts
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetTerm@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  Set gnuplot terminal

INTERFACE
  MODULE SUBROUTINE obj_SetTerm(obj, termType, termSize, &
                                termFont, termFontSize)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), OPTIONAL, INTENT(IN) :: termType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: termSize(2)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: termFont
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: termFontSize
  END SUBROUTINE obj_SetTerm
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_xlim@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set x limit
!
!# Introduction
!
!Set the x axis limits in form of [xmin, xmax]

INTERFACE
  MODULE SUBROUTINE obj_SetXLim(obj, lims)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
  END SUBROUTINE obj_SetXLim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_ylim@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! data: 2025-12-22
! summary:  set x2 limit
!
!# Introduction
!
!Set the y axis limits in form of [ymin, ymax]

INTERFACE
  MODULE SUBROUTINE obj_SetX2Lim(obj, lims)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
  END SUBROUTINE obj_SetX2Lim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_ylim@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set y limit
!
!# Introduction
!
!Set the y axis limits in form of [ymin, ymax]

INTERFACE
  MODULE SUBROUTINE obj_SetYLim(obj, lims)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
  END SUBROUTINE obj_SetYLim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_ylim@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! data: 2025-12-22
! summary:  set y2 limit
!
!# Introduction
!
!Set the y axis limits in form of [ymin, ymax]

INTERFACE
  MODULE SUBROUTINE obj_SetY2Lim(obj, lims)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
  END SUBROUTINE obj_SetY2Lim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_zlim@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set z limit
!
!# Introduction
!
!Set the z axis limits in form of [zmin, zmax]

INTERFACE
  MODULE SUBROUTINE obj_SetZLim(obj, lims)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
  END SUBROUTINE obj_SetZLim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_zlim@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-31
! summary:  set colorbar limits

INTERFACE
  MODULE SUBROUTINE obj_SetCBLim(obj, lims)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
  END SUBROUTINE obj_SetCBLim
END INTERFACE

!----------------------------------------------------------------------------
!                                                       set_axis@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  Set axis with specific direction

INTERFACE
  MODULE SUBROUTINE obj_SetAxisLim(obj, lims, direction)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
    CHARACTER(*), INTENT(IN), OPTIONAL :: direction
  END SUBROUTINE obj_SetAxisLim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_zlim@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary: Set plot scale

INTERFACE
  MODULE SUBROUTINE obj_SetPlotScale(obj, scaleChar, direction, logBase)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    CHARACTER(*), INTENT(IN) :: direction
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetPlotScale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetXScale(obj, scaleChar, logBase)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetXScale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetX2Scale(obj, scaleChar, logBase)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetX2Scale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetYScale(obj, scaleChar, logBase)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetYScale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetY2Scale(obj, scaleChar, logBase)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetY2Scale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetZScale(obj, scaleChar, logBase)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetZScale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetCBScale(obj, scaleChar, logBase)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetCBScale
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_plottitle@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set plot title

INTERFACE
  MODULE SUBROUTINE obj_SetTitle(obj, title, color, fontSize, fontName, &
                                 rotate)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetTitle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_xlabel@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set x label

INTERFACE
  MODULE SUBROUTINE obj_SetXLabel(obj, label, color, fontSize, fontName, &
                                  rotate)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetXLabel
END INTERFACE

!----------------------------------------------------------------------------
!                                                     set_x2label@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set x2 label

INTERFACE
  MODULE SUBROUTINE obj_SetX2Label(obj, label, color, fontSize, fontName, &
                                   rotate)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetX2Label
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_ylabel@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set y label

INTERFACE
  MODULE SUBROUTINE obj_SetYLabel(obj, label, color, fontSize, fontName, &
                                  rotate)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetYLabel
END INTERFACE

!----------------------------------------------------------------------------
!                                                     set_y2label@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set y2 label

INTERFACE
  MODULE SUBROUTINE obj_SetY2Label(obj, label, color, fontSize, fontName, &
                                   rotate)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetY2Label
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_zblabel@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22
! summary:  set z label

INTERFACE
  MODULE SUBROUTINE obj_SetZLabel(obj, label, color, fontSize, fontName, &
                                  rotate)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetZLabel
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_zblabel@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-31
! summary:  Set colorbar label

INTERFACE
  MODULE SUBROUTINE obj_SetCBLabel(obj, label, color, fontSize, fontName, &
                                   rotate)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetCBLabel
END INTERFACE

!----------------------------------------------------------------------------
!                                                       set_label@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
!> author: Shion Shimizu
! update: 2025-12-22

! summary:  set label
!
!# Introduction
!
! Set the text, color, font, size and rotation for labels including
! title, xlabel, x2label, ylabel, ....

INTERFACE
  MODULE SUBROUTINE obj_SetAxisLabel(obj, direction, label, color, &
                                     fontSize, fontName, rotate)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: direction
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetAxisLabel
END INTERFACE

!----------------------------------------------------------------------------
!                                             reset_to_defaults@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:
! summary:  reset to defaults

INTERFACE
  MODULE SUBROUTINE obj_Reset(obj)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Reset
END INTERFACE

!----------------------------------------------------------------------------
!                                         use_preset_configuration@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  set useDefaultPreset

INTERFACE
  MODULE SUBROUTINE obj_SetUseDefaultPreset(obj, abool)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: abool
  END SUBROUTINE obj_SetUseDefaultPreset
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GnuPlot_Class
