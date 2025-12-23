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
USE ExceptionHandler_Class, ONLY: e
USE String_Class, ONLY: String
USE StringUtility, ONLY: LowerCase
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table
IMPLICIT NONE
PRIVATE
PUBLIC :: GnuPlot_
PUBLIC :: GnuPlotOpt_

CHARACTER(*), PARAMETER :: modName = 'Gnuplot_Class'
INTEGER(I4B), PARAMETER :: NOT_INITIALIZED = -32000

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary: Gnuplot Default Options

TYPE GnuplotOpt_
  CHARACTER(4) :: termType = 'wxt'
  CHARACTER(15) :: termFont = 'Times New Roman'
  INTEGER(I4B) :: termFontSize = 10
  INTEGER(I4B) :: termSize(2) = [640, 480]
  CHARACTER(18) :: filename = "gnuplot_script"
  CHARACTER(11) :: dataStyle = "linespoints"
  CHARACTER(45) :: commentLine = &
                   '# -------------------------------------------'
  CHARACTER(17) :: commandLine = "gnuplot --persist"
  REAL(DFP) :: pauseSeconds = 2.0_DFP
END TYPE GnuplotOpt_

TYPE(GnuplotOpt_), PARAMETER :: defaultOpt = GnuplotOpt_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  Label_ for title and axis labels

TYPE :: Label_
  LOGICAL(LGT) :: isConfigured = .FALSE.
  CHARACTER(:), ALLOCATABLE :: text
  CHARACTER(:), ALLOCATABLE :: color
  CHARACTER(:), ALLOCATABLE :: fontname
  INTEGER(I4B) :: fontsize = NOT_INITIALIZED
  INTEGER(I4B) :: rotate = NOT_INITIALIZED
END TYPE Label_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  Tick_ for axis ticks

TYPE :: Tick_
  LOGICAL(LGT) :: isConfigured = .FALSE.
  INTEGER(I4B) :: plotscale = 0
  INTEGER(I4B) :: logBase = 10
  REAL(DFP) :: lims(2)
  ! CHARACTER(:), ALLOCATABLE :: color
  ! CHARACTER(:), ALLOCATABLE :: fontname
  ! INTEGER(I4B) :: fontsize = NOT_INITIALIZED
  ! INTEGER(I4B) :: rotate = NOT_INITIALIZED
END TYPE Tick_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  Axis_ type consist of label and tick

TYPE :: Axis_
  TYPE(String) :: name
  TYPE(Label_) :: label
  TYPE(Tick_) :: tick
END TYPE Axis_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary: GnuPlot_ class

TYPE :: GnuPlot_
  TYPE(TxtFile_) :: pltfile

  LOGICAL(LGT) :: runAfterWrite = .TRUE.

  LOGICAL(LGT) :: pauseAfterDraw = .FALSE.

  TYPE(Axis_) :: xaxis, yaxis, zaxis, x2axis, y2axis
  TYPE(Label_) :: title

  TYPE(String) :: filename
  ! the name of physical file
  ! to write the gnuplot script

  TYPE(String) :: commandline

  TYPE(String), ALLOCATABLE :: options(:)
  ! vector of strings for gnuplot options
  TYPE(String), ALLOCATABLE :: scripts(:)
  ! vector of strings for gnuplot scripts
  TYPE(String) :: dataStyle, termType, termFont
  ! datastyle: lines, points, linespoints
  ! termtype: wxt, qt, pngcairo, svg etc
  ! termfont: Times New Roman etc
  INTEGER(I4B) :: termFontSize
  INTEGER(I4B) :: termSize(2)

  LOGICAL(LGT) :: hasanimation = .FALSE.

  ! multiplot parameters
  LOGICAL(LGT) :: hasmultiplot = .FALSE.
  INTEGER :: multiplot_rows
  INTEGER :: multiplot_cols
  INTEGER :: multiplot_total_plots
  ! animation
  REAL(DFP) :: pause_seconds = 0
  ! keep plot on screen for this value in seconds
  INTEGER(I4B) :: frame_number
  ! frame number in animation
  ! use for debugging and error handling
  INTEGER(I4B) :: status = 0
  !Status from plot procedures
  LOGICAL(LGT) :: useDefaultTerm = .TRUE.
  ! TODO: separate some configs
  LOGICAL(LGT) :: useDefaultPreset = .TRUE.

  LOGICAL(LGT) :: hasCBRange = .FALSE.
  REAL(DFP) :: CBRange(2)
  CHARACTER(:), ALLOCATABLE :: cntrLevels_stmt
  CHARACTER(:), ALLOCATABLE :: pm3dOpts_stmt
  CHARACTER(:), ALLOCATABLE :: cbTicks_stmt

CONTAINS
  PRIVATE

  !! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  FINAL :: obj_Finalize

  !! plot lines
  PROCEDURE, PUBLIC, PASS(obj) :: multiplot => obj_multiplot
  PROCEDURE, PUBLIC, PASS(obj) :: plot1 => obj_plot1
  PROCEDURE, PUBLIC, PASS(obj) :: plot2 => obj_plot2
  PROCEDURE, PUBLIC, PASS(obj) :: plot3 => obj_plot3
  PROCEDURE, PUBLIC, PASS(obj) :: plot4 => obj_plot4
  GENERIC, PUBLIC :: plot => plot1, plot2, plot3, plot4

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

  !! @SET methods
  PROCEDURE, PUBLIC, PASS(obj) :: cntrLevels => obj_setCntrLevels
  PROCEDURE, PUBLIC, PASS(obj) :: cbTicks => obj_setCBTicks
  PROCEDURE, PUBLIC, PASS(obj) :: pm3dOpts => obj_setPm3dOpts
  PROCEDURE, PUBLIC, PASS(obj) :: cblim => obj_setCBLim

  PROCEDURE, PUBLIC, PASS(obj) :: SetTerm => obj_SetTerm

  PROCEDURE, PUBLIC, PASS(obj) :: SetTitle => obj_SetTitle

  PROCEDURE, PUBLIC, PASS(obj) :: SetAxisLabel => obj_SetAxisLabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetXLabel => obj_setxlabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetX2Label => obj_setx2label
  PROCEDURE, PUBLIC, PASS(obj) :: Setylabel => obj_setylabel
  PROCEDURE, PUBLIC, PASS(obj) :: SetY2Label => obj_sety2label
  PROCEDURE, PUBLIC, PASS(obj) :: SetZLabel => obj_setzlabel

  PROCEDURE, PUBLIC, PASS(obj) :: SetAxisLim => obj_SetAxisLim
  PROCEDURE, PUBLIC, PASS(obj) :: SetXLim => obj_SetXLim
  PROCEDURE, PUBLIC, PASS(obj) :: SetX2Lim => obj_SetX2Lim
  PROCEDURE, PUBLIC, PASS(obj) :: SetYLim => obj_SetYLim
  PROCEDURE, PUBLIC, PASS(obj) :: SetY2Lim => obj_SetY2Lim
  PROCEDURE, PUBLIC, PASS(obj) :: SetZLim => obj_SetZLim

  PROCEDURE, PUBLIC, PASS(obj) :: SetPlotScale => obj_SetPlotScale
  PROCEDURE, PUBLIC, PASS(obj) :: SetXScale => obj_SetXScale
  PROCEDURE, PUBLIC, PASS(obj) :: SetX2Scale => obj_SetX2Scale
  PROCEDURE, PUBLIC, PASS(obj) :: SetYScale => obj_SetYScale
  PROCEDURE, PUBLIC, PASS(obj) :: SetY2Scale => obj_SetY2Scale
  PROCEDURE, PUBLIC, PASS(obj) :: SetZScale => obj_SetZScale

  PROCEDURE, PUBLIC, PASS(obj) :: SetFilename => obj_SetFilename
  PROCEDURE, PUBLIC, PASS(obj) :: SetCommandLine => obj_SetCommandLine
  PROCEDURE, PUBLIC, PASS(obj) :: SetOptions => obj_SetOptions
  PROCEDURE, PUBLIC, PASS(obj) :: SetScripts => obj_SetScripts

  PROCEDURE, PUBLIC, PASS(obj) :: Reset => obj_Reset
  PROCEDURE, PUBLIC, PASS(obj) :: SetUseDefaultPreset => &
    obj_SetUseDefaultPreset

  ! @misc
  PROCEDURE, PUBLIC, PASS(obj) :: RunScript => obj_RunScript
  PROCEDURE, PUBLIC, PASS(obj) :: animationStart => obj_animationStart
  PROCEDURE, PUBLIC, PASS(obj) :: animationShow => obj_animationShow

  ! @Write
  PROCEDURE, PUBLIC, PASS(obj) :: WritePlotSetup => obj_WritePlotSetup
  PROCEDURE, PUBLIC, PASS(obj) :: WriteDataBlock => obj_writeDataBlock_xy

  ! @TomlMethods
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml2 => &
    obj_ImportFromToml2

  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2

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
!                                                     CntrLevels@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_setCntrLevels(obj, opts)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: opts
  END SUBROUTINE obj_setCntrLevels
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetPm3dOpts@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_setCBLim(obj, avec)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: avec(2)
  END SUBROUTINE obj_setCBLim
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetCBTicks@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_setCBTicks(obj, opts)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: opts
  END SUBROUTINE obj_setCBTicks
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetPm3dOpts@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_setPm3dOpts(obj, opts)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: opts
  END SUBROUTINE obj_setPm3dOpts
END INTERFACE

!----------------------------------------------------------------------------
!                                                  multiplot@MultiPlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary:  initialize the multiplot
!..............................................................................
! obj subroutine sets flag and number of rows and columns in case
! of multiplot layout
!..............................................................................

INTERFACE
  MODULE SUBROUTINE obj_multiplot(obj, rows, cols)
    CLASS(GnuPlot_) :: obj
    INTEGER(I4B), INTENT(IN) :: rows
    INTEGER(I4B), INTENT(IN) :: cols
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
    CLASS(GnuPlot_) :: obj
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
!                                                           plot2@PlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-08
! update: 2025-02-18
! summary:  Plot 2D data

INTERFACE
  MODULE SUBROUTINE obj_plot2(obj, xv, ymat, lspec)
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(IN) :: xmat(:, :)
    REAL(DFP), INTENT(IN) :: ymat(:, :)
    TYPE(String), INTENT(IN), OPTIONAL :: lspec
  END SUBROUTINE obj_plot3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           plot4@PlotMethods
!----------------------------------------------------------------------------

! TODO: Use with UserFunction class
!> author: Shion Shimizu
! date:   2024-09-22
! summary:  plot with function
!..............................................................................
! fplot, plot a function in the range xrange=[xmin, xamx] with np points
! if np is not sent, then np=50 is assumed!
! func is the name of function to be plotted
!..............................................................................

INTERFACE
  MODULE SUBROUTINE obj_plot4(obj, func, xrange, np)
    CLASS(GnuPlot_) :: obj
    INTERFACE
      FUNCTION func(x_)
        IMPORT DFP
        REAL(DFP), INTENT(IN) :: x_
        REAL(DFP) :: func
      END FUNCTION func
    END INTERFACE
    REAL(DFP), INTENT(IN) :: xrange(2)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: np
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(in) :: x1(:, :), x2(:, :)
    REAL(DFP), INTENT(in), OPTIONAL :: y1(:, :), y2(:, :)
    REAL(DFP), INTENT(in), OPTIONAL :: z1(:, :), z2(:, :)
    CHARACTER(*), INTENT(in), OPTIONAL :: lspec1, lspec2
    CHARACTER(*), INTENT(in), OPTIONAL :: paletteName
    LOGICAL(LGT), INTENT(in), OPTIONAL :: fill
  END SUBROUTINE obj_contour3
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
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(IN) :: name
  END SUBROUTINE obj_SetFilename
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(IN) :: lims(2)
  END SUBROUTINE obj_SetZLim
END INTERFACE

!----------------------------------------------------------------------------
!                                                       set_axis@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-12-22
! summary:  Set axis with specific direction

INTERFACE
  MODULE SUBROUTINE obj_SetAxisLim(obj, lims, direction)
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetXScale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetX2Scale(obj, scaleChar, logBase)
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetX2Scale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetYScale(obj, scaleChar, logBase)
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetYScale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetY2Scale(obj, scaleChar, logBase)
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetY2Scale
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetZScale(obj, scaleChar, logBase)
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(IN) :: scaleChar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: logBase
  END SUBROUTINE obj_SetZScale
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetZLabel
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
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(IN) :: direction
    CHARACTER(*), INTENT(IN) :: label
    CHARACTER(*), OPTIONAL, INTENT(IN) :: color
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate
  END SUBROUTINE obj_SetAxisLabel
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetPlotCommand@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  line specification
!
! Get the command script for plot

INTERFACE
  MODULE SUBROUTINE GetPlotCommand(order, plotCommand, lspec, axes_set)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(*), INTENT(IN), OPTIONAL :: lspec
    CHARACTER(*), INTENT(IN), OPTIONAL :: axes_set
    CHARACTER(*), INTENT(OUT) :: plotCommand
  END SUBROUTINE GetPlotCommand
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetAxesSetting@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  GetAxesSetting
!

INTERFACE
  MODULE SUBROUTINE GetAxesSetting(axes_set, axesSetting)
    CHARACTER(*), INTENT(IN) :: axes_set
    CHARACTER(*), INTENT(OUT) :: axesSetting
  END SUBROUTINE GetAxesSetting
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

INTERFACE
  MODULE SUBROUTINE obj_Reset(obj)
    CLASS(GnuPlot_) :: obj
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
    CLASS(GnuPlot_) :: obj
    LOGICAL(LGT), INTENT(IN) :: abool
  END SUBROUTINE obj_SetUseDefaultPreset
END INTERFACE

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
!
!----------------------------------------------------------------------------

END MODULE GnuPlot_Class
