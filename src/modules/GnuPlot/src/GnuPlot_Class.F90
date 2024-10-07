!
! This module is based on ogpf library made by Mohammad Rahmani
! Please check the LICENSE file for extra information
!

MODULE GnuPlot_Class
USE GlobalData, ONLY: I4B, DFP, sp => REAL32, dp => REAL64, &
                      LGT
USE BaseMethod, ONLY: TOSTRING
USE GridPointUtility, ONLY: Linspace
USE ParameterList, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
USE String_Class, ONLY: String
USE StringUtility, ONLY: LowerCase
USE AbstractPlot_Class
IMPLICIT NONE
PRIVATE
PUBLIC :: GnuPlot_
PUBLIC :: process_linespec
PUBLIC :: splitstring2array

CHARACTER(*), PARAMETER :: modName = 'Gnuplot_Class'
CHARACTER(*), PARAMETER :: md_name = 'ogpf libray'
CHARACTER(*), PARAMETER :: md_rev = 'Rev. 0.22 of March 9th, 2018'
CHARACTER(*), PARAMETER :: md_lic = 'Licence: MIT'
CHARACTER(*), PARAMETER :: gnuplot_term_type = 'wxt'
CHARACTER(*), PARAMETER :: gnuplot_term_font = 'Times New Roman,10'
CHARACTER(*), PARAMETER :: gnuplot_term_size = '640,480'
CHARACTER(*), PARAMETER :: gnuplot_output_filename = 'gnuplot_temp_script.plt'
CHARACTER(*), PARAMETER :: defaultFmtGnuplot = '(a)'
CHARACTER(*), PARAMETER :: commentLineGnuplot = &
                           '# -------------------------------------------'
CHARACTER(*), PARAMETER :: defaultPlotScale = "linear"
REAL(dfp), PARAMETER :: defaultPause = 2.0_DFP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! tplabel is a structure for gnuplot labels including
! title, xlabel, x2label, ylabel, ...
INTEGER, PARAMETER :: NOT_INITIALIZED = -32000
TYPE :: Label_
  LOGICAL(LGT) :: hasLabel = .FALSE.
  CHARACTER(:), ALLOCATABLE :: text
  CHARACTER(:), ALLOCATABLE :: color
  CHARACTER(:), ALLOCATABLE :: fontname
  INTEGER(I4B) :: fontsize = NOT_INITIALIZED
  INTEGER(I4B) :: rotate = NOT_INITIALIZED
END TYPE Label_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! the gpf class implement the object for using gnuplot from fortran in a semi-interactive mode!
! the fortran actually do the job and write out the commands and data in a single file and then
! calls the gnuplot by shell command to plot the data

TYPE, EXTENDS(AbstractPlot_) :: GnuPlot_
  TYPE(Label_) :: tpplottitle
  TYPE(Label_) :: tpxlabel
  TYPE(Label_) :: tpx2label
  TYPE(Label_) :: tpylabel
  TYPE(Label_) :: tpy2label
  TYPE(Label_) :: tpzlabel
  CHARACTER(:), ALLOCATABLE :: txtoptions
  ! a long string to store all type of gnuplot options
  CHARACTER(:), ALLOCATABLE :: txtscript
  ! a long string to store gnuplot script
  CHARACTER(:), ALLOCATABLE :: txtdatastyle
  ! lines, points, linepoints
  LOGICAL(LGT) :: hasxrange = .FALSE.
  LOGICAL(LGT) :: hasx2range = .FALSE.
  LOGICAL(LGT) :: hasyrange = .FALSE.
  LOGICAL(LGT) :: hasy2range = .FALSE.
  LOGICAL(LGT) :: haszrange = .FALSE.
  LOGICAL(LGT) :: hasoptions = .FALSE.
  LOGICAL(LGT) :: hasanimation = .FALSE.
  LOGICAL(LGT) :: hasfilename = .FALSE.
  LOGICAL(LGT) :: hasfileopen = .FALSE.
  REAL(DFP) :: xrange(2), yrange(2), zrange(2)
  REAL(DFP) :: x2range(2), y2range(2)
  CHARACTER(8) :: plotscale
  ! multiplot parameters
  LOGICAL(LGT) :: hasmultiplot = .FALSE.
  INTEGER :: multiplot_rows
  INTEGER :: multiplot_cols
  INTEGER :: multiplot_total_plots
  ! animation
  REAL(dfp) :: pause_seconds = 0
  ! keep plot on screen for this value in seconds
  INTEGER(i4b) :: frame_number
  ! frame number in animation
  ! use for debugging and error handling
  CHARACTER(:), ALLOCATABLE :: msg
  !Message from plot procedures
  INTEGER(i4b) :: status = 0
  !Status from plot procedures
  INTEGER(i4b) :: file_unit
  ! file unit identifier
  CHARACTER(:), ALLOCATABLE :: txtfilename
  ! the name of physical file
  ! to write the gnuplot script
  ! ogpf preset configuration (kind of gnuplot initialization)
  LOGICAL(LGT) :: preset_configuration = .TRUE.

  LOGICAL(LGT) :: hasCBRange = .FALSE.
  REAL(DFP) :: CBRange(2)
  CHARACTER(:), ALLOCATABLE :: cntrLevels_stmt
  CHARACTER(:), ALLOCATABLE :: pm3dOpts_stmt
  CHARACTER(:), ALLOCATABLE :: cbTicks_stmt

CONTAINS
  PRIVATE
  !!
  !! @ConstructorMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  !! set methods
  PROCEDURE, PUBLIC, PASS(obj) :: cntrLevels => obj_setCntrLevels
  PROCEDURE, PUBLIC, PASS(obj) :: cbTicks => obj_setCBTicks
  PROCEDURE, PUBLIC, PASS(obj) :: pm3dOpts => obj_setPm3dOpts

  PROCEDURE, PUBLIC, PASS(obj) :: cblim => obj_setCBLim

  !! plot
  PROCEDURE, PUBLIC, PASS(obj) :: multiplot => obj_multiplot
  PROCEDURE, PUBLIC, PASS(obj) :: plot1 => obj_plot1
  PROCEDURE, PUBLIC, PASS(obj) :: plot2 => obj_plot2
  PROCEDURE, PUBLIC, PASS(obj) :: plot3 => obj_plot3
  PROCEDURE, PUBLIC, PASS(obj) :: plot4 => obj_plot4
  GENERIC, PUBLIC :: plot => plot1, plot2, plot3, plot4

  !! 3d line plot
  PROCEDURE, PUBLIC, PASS(obj) :: plot3d_1 => obj_plot3d_vvv
  GENERIC, PUBLIC :: plot3d => plot3d_1

  !! surface plot
  PROCEDURE, PUBLIC, PASS(obj) :: surf1 => obj_surf1
  GENERIC, PUBLIC :: surf => surf1

  !! contour plot
  PROCEDURE, PUBLIC, PASS(obj) :: contour1 => obj_contour1
  PROCEDURE, PUBLIC, PASS(obj) :: contour2 => obj_contour2
  GENERIC, PUBLIC :: contour => contour1, contour2

  ! TODO: make a interface for these subroutines
  PROCEDURE, PASS(obj) :: preset_gnuplot_config

  PROCEDURE, PUBLIC, PASS(obj) :: create_outputfile
  PROCEDURE, PUBLIC, PASS(obj) :: processcmd
  PROCEDURE, PUBLIC, PASS(obj) :: finalize_plot
  !> 0.22
  PROCEDURE, PASS(obj) :: set_label
  ! public procedures
  PROCEDURE, PUBLIC, PASS(obj) :: options => set_options
  PROCEDURE, PUBLIC, PASS(obj) :: title => set_plottitle
  PROCEDURE, PUBLIC, PASS(obj) :: xlabel => set_xlabel
  PROCEDURE, PUBLIC, PASS(obj) :: x2label => set_x2label
  PROCEDURE, PUBLIC, PASS(obj) :: ylabel => set_ylabel
  PROCEDURE, PUBLIC, PASS(obj) :: y2label => set_y2label
  PROCEDURE, PUBLIC, PASS(obj) :: zlabel => set_zlabel
  PROCEDURE, PUBLIC, PASS(obj) :: axis => set_axis
  PROCEDURE, PUBLIC, PASS(obj) :: axis_sc => set_secondary_axis
  PROCEDURE, PUBLIC, PASS(obj) :: xlim => set_xlim
  PROCEDURE, PUBLIC, PASS(obj) :: ylim => set_ylim
  PROCEDURE, PUBLIC, PASS(obj) :: zlim => set_zlim
  PROCEDURE, PUBLIC, PASS(obj) :: filename => set_filename
  PROCEDURE, PUBLIC, PASS(obj) :: reset => reset_to_defaults
  PROCEDURE, PUBLIC, PASS(obj) :: preset => use_preset_configuration
  PROCEDURE, PUBLIC, PASS(obj) :: addScript => obj_addScript
  PROCEDURE, PUBLIC, PASS(obj) :: runScript => obj_runScript
  PROCEDURE, PUBLIC, PASS(obj) :: animationStart => obj_animationStart
  PROCEDURE, PUBLIC, PASS(obj) :: animationShow => obj_animationShow

  PROCEDURE, PUBLIC, PASS(obj) :: writeScript => obj_writeScript

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
! summary:  Initialize the Gnuplot object

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, param)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), OPTIONAL, INTENT(IN) :: param
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2024-06-06
! summary: Deallocate the Gnuplot object

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
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
    INTEGER(I4B), INTENT(in) :: rows
    INTEGER(I4B), INTENT(in) :: cols
  END SUBROUTINE obj_multiplot
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Plot@PlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-08
! summary:  Plot 1D data
! logScale option can be "semilogx", "semilogy" or "loglog"

INTERFACE
  MODULE SUBROUTINE obj_plot1(obj, x1, y1, ls1, axes1, &
                              x2, y2, ls2, axes2, &
                              x3, y3, ls3, axes3, &
                              x4, y4, ls4, axes4, &
                              logScale)
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
    CHARACTER(*), OPTIONAL, INTENT(IN) :: logScale
    !! default is no logscale
  END SUBROUTINE obj_plot1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           plot2@PlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-08
! summary:  Plot 2D data
! logScale option can be "semilogx", "semilogy" or "loglog"

INTERFACE
  MODULE SUBROUTINE obj_plot2(obj, xv, ymat, lspec, logScale)
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(IN) :: xv(:)
    REAL(DFP), INTENT(IN) :: ymat(:, :)
    CHARACTER(*), INTENT(IN), OPTIONAL :: lspec
    CHARACTER(*), OPTIONAL, INTENT(IN) :: logScale
    !! default is no logscale
  END SUBROUTINE obj_plot2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         plot3@PlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-08
! summary:  Plot 2D data
! logScale option can be "semilogx", "semilogy" or "loglog"

INTERFACE
  MODULE SUBROUTINE obj_plot3(obj, xmat, ymat, lspec, logScale)
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(in) :: xmat(:, :)
    REAL(DFP), INTENT(in) :: ymat(:, :)
    CHARACTER(*), INTENT(in), OPTIONAL :: lspec
    CHARACTER(*), OPTIONAL, INTENT(IN) :: logScale
    !! default is no logscale
  END SUBROUTINE obj_plot3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           plot4@PlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary:  plot with function
!..............................................................................
! fplot, plot a function in the range xrange=[xmin, xamx] with np points
! if np is not sent, then np=50 is assumed!
! func is the name of function to be plotted
!..............................................................................

INTERFACE
  MODULE SUBROUTINE obj_plot4(obj, func, xrange, np, logScale)
    CLASS(GnuPlot_) :: obj
    INTERFACE
      FUNCTION func(x_)
        IMPORT DFP
        REAL(DFP), INTENT(in) :: x_
        REAL(DFP) :: func
      END FUNCTION func
    END INTERFACE
    REAL(DFP), INTENT(in) :: xrange(2)
    INTEGER, OPTIONAL, INTENT(in) :: np
    CHARACTER(*), OPTIONAL, INTENT(IN) :: logScale
  END SUBROUTINE obj_plot4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 plot3d
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary:  plot line in 3D
!..............................................................................
! lplot3d create a line plot in 3d
! datablock is used instead of  gnuplot inline file "-"
!..............................................................................

INTERFACE
  MODULE SUBROUTINE obj_plot3d_vvv(obj, x, y, z, lspec, paletteName, logScale)
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(in) :: x(:)
    REAL(DFP), OPTIONAL, INTENT(in) :: y(:)
    REAL(DFP), OPTIONAL, INTENT(in) :: z(:)
    CHARACTER(*), INTENT(in), OPTIONAL :: lspec
    CHARACTER(*), INTENT(in), OPTIONAL :: paletteName
    CHARACTER(*), OPTIONAL, INTENT(IN) :: logScale
    !! default is no logscale
  END SUBROUTINE obj_plot3d_vvv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       surf
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary:  plot surface
!..............................................................................
! splot create a surface plot
! datablock is used instead of  gnuplot inline file "-"
!..............................................................................

INTERFACE
  MODULE SUBROUTINE obj_surf1(obj, x, y, z, lspec, paletteName, logScale)
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(in) :: x(:, :)
    REAL(DFP), OPTIONAL, INTENT(in) :: y(:, :)
    REAL(DFP), OPTIONAL, INTENT(in) :: z(:, :)
    CHARACTER(*), INTENT(in), OPTIONAL :: lspec
    CHARACTER(*), INTENT(in), OPTIONAL :: paletteName
    CHARACTER(*), OPTIONAL, INTENT(IN) :: logScale
  !! default is no logscale
  END SUBROUTINE obj_surf1
END INTERFACE

!----------------------------------------------------------------------------
!                                               finalize_plot@PlotMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-08-23
! summary:  To finalize writing of gnuplot commands/data
! and close the output file.

INTERFACE
  MODULE SUBROUTINE finalize_plot(obj)
    CLASS(GnuPlot_) :: obj
  END SUBROUTINE finalize_plot
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
    REAL(DFP), INTENT(in) :: x(:, :)
    REAL(DFP), INTENT(in), OPTIONAL :: y(:, :)
    REAL(DFP), INTENT(in), OPTIONAL :: z(:, :)
    CHARACTER(*), INTENT(in), OPTIONAL :: lspec
    CHARACTER(*), INTENT(in), OPTIONAL :: paletteName
    LOGICAL(LGT), INTENT(in), OPTIONAL :: fill
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
    REAL(DFP), INTENT(in) :: x(:)
    REAL(DFP), INTENT(in) :: y(:)
    REAL(DFP), INTENT(in), OPTIONAL :: z(:, :)
    CHARACTER(*), INTENT(in), OPTIONAL :: lspec
    CHARACTER(*), INTENT(in), OPTIONAL :: paletteName
    LOGICAL(LGT), INTENT(in), OPTIONAL :: fill
  END SUBROUTINE obj_contour2
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
  MODULE SUBROUTINE set_filename(obj, chars)
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(in) :: chars
  END SUBROUTINE set_filename
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

INTERFACE
  MODULE SUBROUTINE set_options(obj, stropt)
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(in) :: stropt
  END SUBROUTINE set_options
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_xlim@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set x limit
!
!# Introduction
!
!Set the x axis limits in form of [xmin, xmax]

INTERFACE
  MODULE SUBROUTINE set_xlim(obj, rng)
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(in) :: rng(2)
  END SUBROUTINE set_xlim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_ylim@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set y limit
!
!# Introduction
!
!Set the y axis limits in form of [ymin, ymax]

INTERFACE
  MODULE SUBROUTINE set_ylim(obj, rng)
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(in) :: rng(2)
  END SUBROUTINE set_ylim
END INTERFACE

!----------------------------------------------------------------------------
!                                                        set_zlim@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set z limit
!
!# Introduction
!
!Set the z axis limits in form of [zmin, zmax]

INTERFACE
  MODULE SUBROUTINE set_zlim(obj, rng)
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(in) :: rng(2)
  END SUBROUTINE set_zlim
END INTERFACE

!----------------------------------------------------------------------------
!                                                       set_axis@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set axis
!
!# Introduction
!
!Set the axes limits in form of [xmin, xmax, ymin, ymax, zmin, zmax]

INTERFACE
  MODULE SUBROUTINE set_axis(obj, rng)
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(in) :: rng(:)
  END SUBROUTINE set_axis
END INTERFACE

!----------------------------------------------------------------------------
!                                               set_secondary_axis@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set secondary axis
!
!# Introduction
!
!Set the secondary axes limits in form of [x2min, x2max, y2min, y2max]

INTERFACE
  MODULE SUBROUTINE set_secondary_axis(obj, rng)
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(in) :: rng(:)
  END SUBROUTINE set_secondary_axis
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_plottitle@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set plot title

INTERFACE
  MODULE SUBROUTINE set_plottitle(obj, chars, textcolor, font_size, &
                                  font_name, rotate)
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(in) :: chars
    CHARACTER(*), INTENT(in), OPTIONAL :: textcolor
    INTEGER, OPTIONAL :: font_size
    CHARACTER(*), INTENT(in), OPTIONAL :: font_name
    INTEGER, OPTIONAL :: rotate
  END SUBROUTINE set_plottitle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_xlabel@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set x label

INTERFACE
  MODULE SUBROUTINE set_xlabel(obj, chars, textcolor, font_size, &
                               font_name, rotate)
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(in) :: chars
    CHARACTER(*), INTENT(in), OPTIONAL :: textcolor
    INTEGER, OPTIONAL :: font_size
    CHARACTER(*), INTENT(in), OPTIONAL :: font_name
    INTEGER, OPTIONAL :: rotate
  END SUBROUTINE set_xlabel
END INTERFACE

!----------------------------------------------------------------------------
!                                                     set_x2label@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set x2 label

INTERFACE
  MODULE SUBROUTINE set_x2label(obj, chars, textcolor, font_size, &
                                font_name, rotate)
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(in) :: chars
    CHARACTER(*), INTENT(in), OPTIONAL :: textcolor
    INTEGER, OPTIONAL :: font_size
    CHARACTER(*), INTENT(in), OPTIONAL :: font_name
    INTEGER, OPTIONAL :: rotate
  END SUBROUTINE set_x2label
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_ylabel@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set y label

INTERFACE
  MODULE SUBROUTINE set_ylabel(obj, chars, textcolor, font_size, &
                               font_name, rotate)
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(in) :: chars
    CHARACTER(*), INTENT(in), OPTIONAL :: textcolor
    INTEGER, OPTIONAL :: font_size
    CHARACTER(*), INTENT(in), OPTIONAL :: font_name
    INTEGER, OPTIONAL :: rotate
  END SUBROUTINE set_ylabel
END INTERFACE

!----------------------------------------------------------------------------
!                                                     set_y2label@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set y2 label

INTERFACE
  MODULE SUBROUTINE set_y2label(obj, chars, textcolor, font_size, &
                                font_name, rotate)
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(in) :: chars
    CHARACTER(*), INTENT(in), OPTIONAL :: textcolor
    INTEGER, OPTIONAL :: font_size
    CHARACTER(*), INTENT(in), OPTIONAL :: font_name
    INTEGER, OPTIONAL :: rotate
  END SUBROUTINE set_y2label
END INTERFACE

!----------------------------------------------------------------------------
!                                                   set_zblabel@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set z label

INTERFACE
  MODULE SUBROUTINE set_zlabel(obj, chars, textcolor, font_size, &
                               font_name, rotate)
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(in) :: chars
    CHARACTER(*), INTENT(in), OPTIONAL :: textcolor
    INTEGER, OPTIONAL :: font_size
    CHARACTER(*), INTENT(in), OPTIONAL :: font_name
    INTEGER, OPTIONAL :: rotate
  END SUBROUTINE set_zlabel
END INTERFACE

!----------------------------------------------------------------------------
!                                                       set_label@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  set label
!
!# Introduction
!
! Set the text, color, font, size and rotation for labels including
! title, xlabel, x2label, ylabel, ....

INTERFACE
  MODULE SUBROUTINE set_label(obj, lblname, lbltext, lblcolor, font_size, &
                              font_name, rotate)
    CLASS(GnuPlot_) :: obj
    CHARACTER(*), INTENT(in) :: lblname
    CHARACTER(*), INTENT(in) :: lbltext
    CHARACTER(*), INTENT(in), OPTIONAL :: lblcolor
    CHARACTER(*), INTENT(in), OPTIONAL :: font_name
    INTEGER, OPTIONAL :: font_size
    INTEGER, OPTIONAL :: rotate
  END SUBROUTINE set_label
END INTERFACE

!----------------------------------------------------------------------------
!                                                     splitstr@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  splitstr, separate a string using ";" delimiters

INTERFACE
  MODULE PURE FUNCTION splitstr(chars) RESULT(spstr)
    CHARACTER(*), INTENT(in) :: chars
    CHARACTER(:), ALLOCATABLE :: spstr
  END FUNCTION splitstr
END INTERFACE

!----------------------------------------------------------------------------
!                                           splitstring2array@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  splitstring2array, separate a string using ";" delimiters

!..............................................................................
! splitstring splits a string to an array of
! substrings based on a selected delimiter
! note:
!    a. any facing space/blank in substrings will be removed
!    b. two adjacent delimiter treats as an empty substring between them
!    c. facing and trailing delimiter treats as empty substring at the fornt and end
!..............................................................................

INTERFACE
  MODULE SUBROUTINE splitstring2array(chars, strarray, delimiter)
    CHARACTER(*), INTENT(in) :: chars
    CHARACTER(80), ALLOCATABLE, INTENT(out) :: strarray(:)
    CHARACTER(1), OPTIONAL, INTENT(in) :: delimiter
  END SUBROUTINE splitstring2array
END INTERFACE

!----------------------------------------------------------------------------
!                                             process_linepec@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  line specification
!
!# Introduction
!
! process_linespec accepts the line specification and interpret it into
! a format to be sent to gnuplot

INTERFACE
  MODULE SUBROUTINE process_linespec(order, lsstring, lspec, axes_set)
    INTEGER, INTENT(in) :: order
    !1 for the first data series
    CHARACTER(*), INTENT(out) :: lsstring
    CHARACTER(*), INTENT(in), OPTIONAL :: lspec
    CHARACTER(*), INTENT(in), OPTIONAL :: axes_set
  END SUBROUTINE process_linespec
END INTERFACE

!----------------------------------------------------------------------------
!                                           process_axes_set@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  process_axes_set
!
!# Introduction
!
! process_axesspec accepts the axes set and interpret it into
! a format to be sent to gnuplot.
! the axes set can be one of the following set
! x1y1, x1y2, x2y1, x2y2

INTERFACE
  MODULE SUBROUTINE process_axes_set(axes_set, axes)
    CHARACTER(*), INTENT(in) :: axes_set
    CHARACTER(4), INTENT(out) :: axes
  END SUBROUTINE process_axes_set
END INTERFACE

!----------------------------------------------------------------------------
!                                           create_outputfile@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  create output file
!
!# Introduction
!
! Create an output file, assign a file_unit
! for writing the gnuplot commands

INTERFACE
  MODULE SUBROUTINE create_outputfile(obj)
    CLASS(GnuPlot_), INTENT(inout) :: obj
  END SUBROUTINE create_outputfile
END INTERFACE

!----------------------------------------------------------------------------
!                                                 processcmd@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  process command
!
!# Introduction
!
!   obj subroutine writes all the data into plot file
!   to be read by gnuplot

INTERFACE
  MODULE SUBROUTINE processcmd(obj)
    CLASS(GnuPlot_), INTENT(inout) :: obj
  END SUBROUTINE processcmd
END INTERFACE

!----------------------------------------------------------------------------
!                                               write_xydata@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  write xy data
!
!# Introduction
! Writes set of xy data into a file

INTERFACE
  MODULE SUBROUTINE write_xydata(file_unit, ndata, x, y)
    INTEGER, INTENT(in) :: file_unit
    INTEGER, INTENT(in) :: ndata
    REAL(DFP), INTENT(in) :: x(:)
    REAL(DFP), INTENT(in), OPTIONAL :: y(:)
  END SUBROUTINE write_xydata
END INTERFACE

!----------------------------------------------------------------------------
!                                                    hasTitle@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  check to see if the plot title (used as legend = key)

INTERFACE
  MODULE FUNCTION hasTitle(chars)
    CHARACTER(*), INTENT(in) :: chars
    LOGICAL :: hastitle
  END FUNCTION hasTitle
END INTERFACE

!----------------------------------------------------------------------------
!                                               write_label@UtilityMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-23
! summary:  write label
!
!# Introduction
!
!   obj subroutine writes the labels into plot file
!   to be read by gnuplot

INTERFACE
  MODULE SUBROUTINE write_label(obj, lblname)
    ! write_label
    CLASS(GnuPlot_) :: obj
    CHARACTER(*) :: lblname
  END SUBROUTINE write_label
END INTERFACE

!----------------------------------------------------------------------------
!                                             reset_to_defaults@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:
! summary:  reset to defaults

INTERFACE
  MODULE SUBROUTINE reset_to_defaults(obj)
    CLASS(GnuPlot_) :: obj
  END SUBROUTINE reset_to_defaults
END INTERFACE

!----------------------------------------------------------------------------
!                                         use_preset_configuration@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:
! summary:  use preset configuration

INTERFACE
  MODULE SUBROUTINE use_preset_configuration(obj, flag)
    CLASS(GnuPlot_) :: obj
    LOGICAL(LGT), INTENT(IN) :: flag
  END SUBROUTINE use_preset_configuration
END INTERFACE

!----------------------------------------------------------------------------
!                                                       preset_gnuplot_config
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary:  preset gnuplot config

INTERFACE
  MODULE SUBROUTINE preset_gnuplot_config(obj)
    CLASS(GnuPlot_) :: obj
  END SUBROUTINE preset_gnuplot_config
END INTERFACE

!----------------------------------------------------------------------------
!                                                         writeScript
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary:  write gnuplot script to file

INTERFACE
  MODULE SUBROUTINE obj_writeScript(obj, script, fmt)
    CLASS(GnuPlot_), INTENT(inout) :: obj
    CHARACTER(*), OPTIONAL, INTENT(IN) :: script
    CHARACTER(*), OPTIONAL, INTENT(IN) :: fmt
  END SUBROUTINE obj_writeScript
END INTERFACE

!----------------------------------------------------------------------------
!                                                             color_palettes
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
  MODULE FUNCTION color_palettes(paletteName) RESULT(paletteScript)
    CHARACTER(*), INTENT(in) :: paletteName
    CHARACTER(:), ALLOCATABLE :: paletteScript
  END FUNCTION color_palettes
END INTERFACE

!----------------------------------------------------------------------------
!                                                        addScript
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-09-22
! summary: create or eddit raw gnuplot script
! stored in obj%txtscript

!..............................................................................
! addscript: accepts all type of gnuplot command as a string and store it
! in global txtscript to be later sent to gnuplot
!..............................................................................

INTERFACE
  MODULE SUBROUTINE obj_addScript(obj, scripts)
    CLASS(GnuPlot_), INTENT(inout) :: obj
    CHARACTER(*), INTENT(in) :: scripts
  END SUBROUTINE obj_addScript
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
  MODULE SUBROUTINE obj_runScript(obj)
    CLASS(GnuPlot_), INTENT(inout) :: obj
  END SUBROUTINE obj_runScript
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
    CLASS(GnuPlot_), INTENT(inout) :: obj
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
    CLASS(GnuPlot_), INTENT(inout) :: obj
  END SUBROUTINE obj_animationShow
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GnuPlot_Class
