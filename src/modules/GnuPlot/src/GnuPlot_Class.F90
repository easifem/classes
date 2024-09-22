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
  PROCEDURE, PUBLIC, PASS(obj) :: plot1 => obj_plot1
  PROCEDURE, PUBLIC, PASS(obj) :: plot2 => obj_plot2
  PROCEDURE, PUBLIC, PASS(obj) :: plot3 => obj_plot3
  GENERIC, PUBLIC :: plot => plot1, plot2, plot3
  !! contour plot
  PROCEDURE, PUBLIC, PASS(obj) :: contour1 => obj_contour1
  PROCEDURE, PUBLIC, PASS(obj) :: contour2 => obj_contour2
  GENERIC, PUBLIC :: contour => contour1, contour2

  ! TODO: make a interface for these subroutines
  PROCEDURE, PASS(obj) :: preset_gnuplot_config
  PROCEDURE, PASS(obj) :: plot2d_vector_vs_vector
  PROCEDURE, PASS(obj) :: plot2d_matrix_vs_vector
  PROCEDURE, PASS(obj) :: plot2d_matrix_vs_matrix
  PROCEDURE, PASS(obj) :: semilogxv
  PROCEDURE, PASS(obj) :: semilogxm
  PROCEDURE, PASS(obj) :: semilogyv
  PROCEDURE, PASS(obj) :: semilogym
  PROCEDURE, PASS(obj) :: loglogv
  PROCEDURE, PASS(obj) :: loglogm

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
  PROCEDURE, PUBLIC, PASS(obj) :: multiplot => sub_multiplot
  GENERIC, PUBLIC :: old_plot => plot2d_vector_vs_vector, &
    plot2d_matrix_vs_vector, &
    plot2d_matrix_vs_matrix
  GENERIC, PUBLIC :: semilogx => semilogxv, semilogxm
  GENERIC, PUBLIC :: semilogy => semilogyv, semilogym
  GENERIC, PUBLIC :: loglog => loglogv, loglogm
  PROCEDURE, PUBLIC, PASS(obj) :: surf => splot
  ! 3D surface plot
  PROCEDURE, PUBLIC, PASS(obj) :: lplot => lplot3d
  ! 3D line plot
  PROCEDURE, PUBLIC, PASS(obj) :: old_contour => cplot
  ! contour plot
  PROCEDURE, PUBLIC, PASS(obj) :: fplot => function_plot
  PROCEDURE, PUBLIC, PASS(obj) :: addScript => obj_addScript
  PROCEDURE, PUBLIC, PASS(obj) :: runScript => obj_runScript
  PROCEDURE, PUBLIC, PASS(obj) :: animation_start => sub_animation_start
  PROCEDURE, PUBLIC, PASS(obj) :: animation_show => sub_animation_show

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
!                                                           Plot@PlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-08
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
! summary:  Plot 2D data

INTERFACE
  MODULE SUBROUTINE obj_plot2(obj, xv, ymat, lspec)
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(IN) :: xv(:)
    REAL(DFP), INTENT(IN) :: ymat(:, :)
    CHARACTER(*), INTENT(IN), OPTIONAL :: lspec
  END SUBROUTINE obj_plot2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         plot3@PlotMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-06-08
! summary:  Plot 2D data

INTERFACE
  MODULE SUBROUTINE obj_plot3(obj, xmat, ymat, lspec)
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(in) :: xmat(:, :)
    REAL(DFP), INTENT(in) :: ymat(:, :)
    CHARACTER(*), INTENT(in), OPTIONAL :: lspec
  END SUBROUTINE obj_plot3
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
  MODULE SUBROUTINE obj_contour1(obj, x, y, z, lspec, palette, fill)
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(in) :: x(:, :)
    REAL(DFP), INTENT(in), OPTIONAL :: y(:, :)
    REAL(DFP), INTENT(in), OPTIONAL :: z(:, :)
    CHARACTER(*), INTENT(in), OPTIONAL :: lspec
    CHARACTER(*), INTENT(in), OPTIONAL :: palette
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
  MODULE SUBROUTINE obj_contour2(obj, x, y, z, lspec, palette, fill)
    CLASS(GnuPlot_) :: obj
    REAL(DFP), INTENT(in) :: x(:)
    REAL(DFP), INTENT(in) :: y(:)
    REAL(DFP), INTENT(in), OPTIONAL :: z(:, :)
    CHARACTER(*), INTENT(in), OPTIONAL :: lspec
    CHARACTER(*), INTENT(in), OPTIONAL :: palette
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

CONTAINS

!..............................................................................
! obj subroutine sets flag and number of rows and columns in case
! of multiplot layout
!..............................................................................
SUBROUTINE sub_multiplot(obj, rows, cols)

  CLASS(GnuPlot_) :: obj
  INTEGER, INTENT(in) :: rows
  INTEGER, INTENT(in) :: cols

  ! ogpf does not support multiplot in animation mode
  IF (obj%hasanimation) THEN
    PRINT *, md_name//': ogpf does not support animation in multiplot mode'
    STOP
  END IF

  ! set multiplot cols and rows
  IF (rows > 0) THEN
    obj%multiplot_rows = rows
  ELSE

  END IF
  IF (cols > 0) THEN
    obj%multiplot_cols = cols
  ELSE

  END IF

  ! set the multiplot layout flag and plot numbers
  obj%hasmultiplot = .TRUE.
  obj%multiplot_total_plots = 0

  ! create the ouput file for writting gnuplot script
  CALL create_outputfile(obj)

END SUBROUTINE sub_multiplot

SUBROUTINE plot2d_vector_vs_vector(obj, x1, y1, ls1, axes1, &
                                   x2, y2, ls2, axes2, &
                                   x3, y3, ls3, axes3, &
                                   x4, y4, ls4, axes4)
  !..............................................................................
  ! obj procedure plots:
  !   1. A vector against another vector (xy plot)
  !   2. A vector versus its element indices (yi plot).
  !   3. Can accept up to 4 data sets as x,y pairs!
  ! Arguments
  ! xi, yi vectors of data series,
  ! lsi a string maximum 80 characters containing the line specification,
  ! legends, ...
  ! axesi is the axes for plotting: secondary axes are x2, and y2
  !..............................................................................

  CLASS(GnuPlot_) :: obj
  ! Input vector
  REAL(DFP), INTENT(in) :: x1(:) ! vector of data for x
  REAL(DFP), INTENT(in), OPTIONAL :: y1(:) ! vector of data for y
  CHARACTER(*), INTENT(in), OPTIONAL :: ls1 ! line specification
  CHARACTER(*), INTENT(in), OPTIONAL :: axes1

  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: x2
  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: y2
  CHARACTER(*), INTENT(in), OPTIONAL :: ls2
  CHARACTER(*), INTENT(in), OPTIONAL :: axes2

  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: x3
  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: y3
  CHARACTER(*), INTENT(in), OPTIONAL :: ls3
  CHARACTER(*), INTENT(in), OPTIONAL :: axes3

  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: x4
  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: y4
  CHARACTER(*), INTENT(in), OPTIONAL :: ls4
  CHARACTER(*), INTENT(in), OPTIONAL :: axes4

  !   Local variables
  !----------------------------------------------------------------------

  INTEGER :: nx1
  INTEGER :: ny1
  INTEGER :: nx2
  INTEGER :: ny2
  INTEGER :: nx3
  INTEGER :: ny3
  INTEGER :: nx4
  INTEGER :: ny4
  INTEGER :: number_of_plots
  CHARACTER(3) :: plottype
  INTEGER :: i
  CHARACTER(80) :: pltstring(4) ! Four 80 characters string

  !Initialize variables
  plottype = ''
  pltstring = ''

  !   Check the input
  nx1 = SIZE(x1)
  IF ((PRESENT(y1))) THEN
    ny1 = SIZE(y1)
    IF (nx1 .EQ. ny1) THEN
      plottype = 'xy1'
      number_of_plots = 1
    ELSE
                print*, md_name // ':plot2d_vector_vs_vector:' // 'length of x1 and y1 does not match'
      RETURN
    END IF
  ELSE !plot only x againest its element indices
    plottype = 'xi'
    number_of_plots = 1
  END IF

  !Process line spec and axes set for first data set if present
  CALL process_linespec(1, pltstring(1), ls1, axes1)

  IF (PRESENT(x2) .AND. PRESENT(y2)) THEN
    nx2 = SIZE(x2)
    ny2 = SIZE(y2)
    IF (nx2 .EQ. ny2) THEN
      plottype = 'xy2'
      number_of_plots = 2
    ELSE
      RETURN
    END IF
    !Process line spec for 2nd data set if present
    CALL process_linespec(2, pltstring(2), ls2, axes2)
  END IF

  IF (PRESENT(x3) .AND. PRESENT(y3)) THEN
    nx3 = SIZE(x3)
    ny3 = SIZE(y3)
    IF (nx3 .EQ. ny3) THEN
      plottype = 'xy3'
      number_of_plots = 3
    ELSE
      RETURN
    END IF
    !Process line spec for 3rd data set if present
    CALL process_linespec(3, pltstring(3), ls3, axes3)
  END IF

  IF (PRESENT(x4) .AND. PRESENT(y4)) THEN
    nx4 = SIZE(x4)
    ny4 = SIZE(y4)
    IF (nx4 .EQ. ny4) THEN
      plottype = 'xy4'
      number_of_plots = 4
    ELSE
      RETURN
    END IF
    !Process line spec for 4th data set if present
    CALL process_linespec(4, pltstring(4), ls4, axes4)
  END IF

  CALL create_outputfile(obj)

  ! Write plot title, axis labels and other annotations
  CALL processcmd(obj)

  ! Write plot command and line styles and legend if any
  IF (number_of_plots == 1) THEN
    WRITE (obj%file_unit, '(a)') TRIM(pltstring(1))
  ELSE
            write ( obj%file_unit, '(a)' )  ( trim(pltstring(i)) // ' \' , i=1, number_of_plots-1)
    WRITE (obj%file_unit, '(a)') TRIM(pltstring(number_of_plots))
  END IF
  ! Write xy data into file
  SELECT CASE (plottype)
  CASE ('xi')
    CALL write_xydata(obj%file_unit, nx1, x1)
  CASE ('xy1')
    CALL write_xydata(obj%file_unit, nx1, x1, y1)
  CASE ('xy2')
    CALL write_xydata(obj%file_unit, nx1, x1, y1)
    CALL write_xydata(obj%file_unit, nx2, x2, y2)
  CASE ('xy3')
    CALL write_xydata(obj%file_unit, nx1, x1, y1)
    CALL write_xydata(obj%file_unit, nx2, x2, y2)
    CALL write_xydata(obj%file_unit, nx3, x3, y3)
  CASE ('xy4')
    CALL write_xydata(obj%file_unit, nx1, x1, y1)
    CALL write_xydata(obj%file_unit, nx2, x2, y2)
    CALL write_xydata(obj%file_unit, nx3, x3, y3)
    CALL write_xydata(obj%file_unit, nx4, x4, y4)
  END SELECT

  !> Rev 0.2
  ! if there is no animation finalize
  IF (.NOT. (obj%hasanimation)) THEN
    CALL finalize_plot(obj)
  ELSE
    WRITE (obj%file_unit, '(a, F5.2)') 'pause ', obj%pause_seconds
  END IF

  !: End of plot2D_vector_vs_vector
END SUBROUTINE plot2d_vector_vs_vector

SUBROUTINE plot2d_matrix_vs_vector(obj, xv, ymat, lspec)
  !..............................................................................
  ! plot2D_matrix_vs_vector accepts a vector xv and a matrix ymat and plots
  ! columns of ymat against xv. lspec is an optional array defines the line
  ! specification for each data series. If a single element array is sent for
  ! lspec then all series are plotted using the same linespec
  !..............................................................................

  IMPLICIT NONE
  CLASS(GnuPlot_) :: obj
  ! Input arrays
  REAL(DFP), INTENT(in) :: xv(:)
  REAL(DFP), INTENT(in) :: ymat(:, :)
  CHARACTER(*), INTENT(in), OPTIONAL :: lspec
  !----------------------------------------------------------------------
  !       Local variables
  INTEGER :: nx
  INTEGER :: ny
  INTEGER :: ns
  INTEGER :: number_of_curves
  INTEGER :: i
  INTEGER :: j
  INTEGER :: ierr
  CHARACTER(80), ALLOCATABLE :: pltstring(:), lst(:)
  !

  !*******************************************************************************
  !   Check the input
  nx = SIZE(xv)
  ny = SIZE(ymat, dim=1)
  IF (.NOT. nx .EQ. ny) THEN
            print*, md_name // ':plot2d_matrix_vs_vector:' // 'The length of arrays does not match'
    RETURN
  END IF
  ! create the outfile to write the gnuplot script
  CALL create_outputfile(obj)

  ! Write titles and other annotations
  CALL processcmd(obj)

  ! Write plot command and line styles and legend if any
  number_of_curves = SIZE(ymat, dim=2)
  ALLOCATE (pltstring(number_of_curves), stat=ierr)
  IF (ierr /= 0) THEN
    PRINT *, 'allocation error'
    RETURN
  END IF

  ! assume no linespec is available
  pltstring(1:number_of_curves) = ''

  IF (PRESENT(lspec)) THEN

    CALL splitstring2array(lspec, lst, ';')
    ns = SIZE(lst, dim=1)

    IF (ns == number_of_curves) THEN
      ! there is a linespec for each curve
      pltstring = lst
    ELSEIF (ns < number_of_curves) THEN
      ! not enough linespec
      DO i = 1, ns
        pltstring(i) = lst(i)
      END DO
    ELSE ! ns > number_of curves
      PRINT *, 'ogpf: plot2d_matrix_vs_vector: wrong number of linespec'
      PRINT *, 'semicolon ";" acts as delimiter, check the linespec'
    END IF
  END IF

  IF (PRESENT(lspec)) THEN

    CALL process_linespec(1, pltstring(1), lst(1))
    ns = SIZE(lst)
    ! gpf will cylce through line specification, if number of specification passed
    ! is less than number of plots
    DO i = 1, number_of_curves
      j = MOD(i - 1, ns) + 1
      CALL process_linespec(i, pltstring(i), lst(j))
    END DO
  ELSE !No lspec is available
    pltstring(1) = ' plot "-" notitle,'
    pltstring(2:number_of_curves - 1) = '"-" notitle,'
    pltstring(number_of_curves) = '"-" notitle'
  END IF

  ! Write plot command and line styles and legend if any
        write ( obj%file_unit, '(a)' ) ( trim(pltstring(i)) // ' \' , i=1, number_of_curves-1)
  WRITE (obj%file_unit, '(a)') TRIM(pltstring(number_of_curves))

  ! Write data into script file
  DO j = 1, number_of_curves
    DO i = 1, nx
      WRITE (obj%file_unit, *) xv(i), ymat(i, j)
    END DO
    WRITE (obj%file_unit, '(a)') 'e' !end of jth set of data
  END DO

  !> Rev 0.2
  ! if there is no animation finalize
  IF (.NOT. (obj%hasanimation)) THEN
    CALL finalize_plot(obj)
  ELSE
    WRITE (obj%file_unit, '(a, F5.2)') 'pause ', obj%pause_seconds
  END IF

  !Release memory
  IF (ALLOCATED(pltstring)) THEN
    DEALLOCATE (pltstring)
  END IF
  !: End of plot2D_matrix_vs_vector
END SUBROUTINE plot2d_matrix_vs_vector

SUBROUTINE plot2d_matrix_vs_matrix(obj, xmat, ymat, lspec)
  !..............................................................................
  ! plot2D_matrix_vs_matrix accepts a matrix xmat and a matrix ymat and plots
  ! columns of ymat against columns of xmat. lspec is an optional array defines
  ! the line specification for each data series. If a single element array is
  ! sent for lspec then all series are plotted using the same linespec
  !..............................................................................

  IMPLICIT NONE
  CLASS(GnuPlot_) :: obj
  ! Input arrays
  REAL(DFP), INTENT(in) :: xmat(:, :)
  REAL(DFP), INTENT(in) :: ymat(:, :)
  CHARACTER(*), INTENT(in), OPTIONAL :: lspec
  !----------------------------------------------------------------------
  !       Local variables
  INTEGER :: mx, nx
  INTEGER :: my, ny
  INTEGER :: ns
  INTEGER :: number_of_curves
  INTEGER :: i
  INTEGER :: j
  INTEGER :: ierr
  CHARACTER(80), ALLOCATABLE :: pltstring(:), lst(:)
  !

  !*******************************************************************************
  !   Check the input
  ! check number of rows
  mx = SIZE(xmat, dim=1)
  my = SIZE(ymat, dim=1)
  IF (.NOT. mx .EQ. my) THEN
            print*, md_name // ':plot2d_matrix_vs_matrix:' // 'The length of arrays does not match'
    RETURN
  END IF
  ! check number of rows
  nx = SIZE(xmat, dim=2)
  ny = SIZE(ymat, dim=2)
  IF (.NOT. nx .EQ. ny) THEN
   PRINT *, 'GnuPlot_ error: The number of columns are different, check xmat, ymat'
    RETURN
  END IF

  ! create the outfile to write the gnuplot script
  CALL create_outputfile(obj)

  ! Write titles and other annotations
  CALL processcmd(obj)

  ! Write plot command and line styles and legend if any
  number_of_curves = SIZE(ymat, dim=2)
  ALLOCATE (pltstring(number_of_curves), stat=ierr)
  IF (ierr /= 0) THEN
    PRINT *, 'allocation error'
    RETURN
  END IF

  ! assume no linespec is available
  pltstring(1:number_of_curves) = ''

  IF (PRESENT(lspec)) THEN

    CALL splitstring2array(lspec, lst, ';')
    ns = SIZE(lst, dim=1)

    IF (ns == number_of_curves) THEN
      ! there is a linespec for each curve
      pltstring = lst
    ELSEIF (ns < number_of_curves) THEN
      ! not enough linespec
      DO i = 1, ns
        pltstring(i) = lst(i)
      END DO
    ELSE ! ns > number_of curves
   PRINT *, md_name//': plot2d_matrix_vs_matrix:'//' wrong number of linespec'
      PRINT *, 'semicolon ";" acts as delimiter, check the linespec'
    END IF
  END IF

  IF (PRESENT(lspec)) THEN

    CALL process_linespec(1, pltstring(1), lst(1))
    ns = SIZE(lst)
    ! GnuPlot_ will cylce through line specification, if number of specification passed
    ! is less than number of plots
    DO i = 1, number_of_curves
      j = MOD(i - 1, ns) + 1
      CALL process_linespec(i, pltstring(i), lst(j))
    END DO
  ELSE !No lspec is available
    pltstring(1) = ' plot "-" notitle,'
    pltstring(2:number_of_curves - 1) = '"-" notitle,'
    pltstring(number_of_curves) = '"-" notitle'
  END IF

  ! Write plot command and line styles and legend if any
        write ( obj%file_unit, '(a)' ) ( trim(pltstring(i)) // ' \' , i=1, number_of_curves-1)
  WRITE (obj%file_unit, '(a)') TRIM(pltstring(number_of_curves))

  ! Write data into script file
  DO j = 1, number_of_curves
    DO i = 1, mx
      WRITE (obj%file_unit, *) xmat(i, j), ymat(i, j)
    END DO
    WRITE (obj%file_unit, '(a)') 'e' !end of jth set of data
  END DO

  !> Rev 0.2
  ! if there is no animation finalize
  IF (.NOT. (obj%hasanimation)) THEN
    CALL finalize_plot(obj)
  ELSE
    WRITE (obj%file_unit, '(a, F5.2)') 'pause ', obj%pause_seconds
  END IF

  !Release memory
  IF (ALLOCATED(pltstring)) THEN
    DEALLOCATE (pltstring)
  END IF
  !: End of plot2D_matrix_vs_vector
END SUBROUTINE plot2d_matrix_vs_matrix

SUBROUTINE splot(obj, x, y, z, lspec, palette)
  !..............................................................................
  ! splot create a surface plot
  ! datablock is used instead of  gnuplot inline file "-"
  !..............................................................................

  CLASS(GnuPlot_) :: obj
  ! Input vector
  REAL(DFP), INTENT(in) :: x(:, :)
  REAL(DFP), INTENT(in), OPTIONAL :: y(:, :)
  REAL(DFP), INTENT(in), OPTIONAL :: z(:, :)
  CHARACTER(*), INTENT(in), OPTIONAL :: lspec
  CHARACTER(*), INTENT(in), OPTIONAL :: palette

  !   Local variables
  !----------------------------------------------------------------------
  INTEGER :: ncx
  INTEGER :: nrx
  INTEGER :: i
  INTEGER :: j
  LOGICAL :: xyz_data
  CHARACTER(80) :: pltstring
  CHARACTER(*), PARAMETER :: datablock = '$xyz'

  pltstring = ''
  !   Check the input data
  ncx = SIZE(x, dim=2)
  nrx = SIZE(x, dim=1)
  IF (PRESENT(y) .AND. PRESENT(z)) THEN
    xyz_data = .TRUE.
  ELSEIF (PRESENT(y)) THEN
    PRINT *, "GnuPlot_ error: Z matrix was not sent to 3D plot routine"
    RETURN
  ELSE
    xyz_data = .FALSE.
  END IF

  ! set default line style for 3D plot, can be overwritten
  obj%txtdatastyle = 'lines'
  ! create the script file for writting gnuplot commands and data
  CALL create_outputfile(obj)

  ! Write titles and other annotations
  CALL processcmd(obj)

  ! Write xy data into file
  WRITE (obj%file_unit, '(a)') '#data x y z'
  ! Rev 0.20
  ! write the $xyz datablocks
  WRITE (obj%file_unit, '(a)') datablock//' << EOD'
  IF (xyz_data) THEN
    DO j = 1, ncx
      DO i = 1, nrx
        WRITE (obj%file_unit, *) x(i, j), y(i, j), z(i, j)
      END DO
      WRITE (obj%file_unit, '(a)') !put an empty line
    END DO
    WRITE (obj%file_unit, '(a)') 'EOD' !end of datablock
  ELSE !only Z has been sent (i.e. single matrix data)
    DO j = 1, ncx
      DO i = 1, nrx
        WRITE (obj%file_unit, *) i, j, x(i, j)
      END DO
      WRITE (obj%file_unit, '(a)') !put an empty line
    END DO
    WRITE (obj%file_unit, '(a)') 'EOD' !end of datablock
  END IF

  !write the color palette into gnuplot script file
  IF (PRESENT(palette)) THEN
    WRITE (obj%file_unit, '(a)') color_palettes(palette)
    WRITE (obj%file_unit, '(a)') 'set pm3d' ! a conflict with lspec
  END IF

  IF (PRESENT(lspec)) THEN
    IF (hastitle(lspec)) THEN
      pltstring = 'splot '//datablock//' '//TRIM(lspec)
    ELSE
      pltstring = 'splot '//datablock//' notitle '//TRIM(lspec)
    END IF
  ELSE
    pltstring = 'splot '//datablock//' notitle '
  END IF

  WRITE (obj%file_unit, '(a)') TRIM(pltstring)

  !> Rev 0.2: animation
  ! if there is no animation finalize
  IF (.NOT. (obj%hasanimation)) THEN
    CALL finalize_plot(obj)
  ELSE
    WRITE (obj%file_unit, '(a, F5.2)') 'pause ', obj%pause_seconds
  END IF

  !: End of splot
END SUBROUTINE splot

!..............................................................................
!   Rev 0.19
!   cplot creates a contour plot based on the three dimensional data
!..............................................................................

SUBROUTINE cplot(obj, x, y, z, lspec, palette)
  CLASS(GnuPlot_) :: obj
  ! Input vector
  REAL(DFP), INTENT(in) :: x(:, :)
  REAL(DFP), INTENT(in), OPTIONAL :: y(:, :)
  REAL(DFP), INTENT(in), OPTIONAL :: z(:, :)
  CHARACTER(*), INTENT(in), OPTIONAL :: lspec
  CHARACTER(*), INTENT(in), OPTIONAL :: palette

  INTEGER :: ncx
  INTEGER :: nrx
  INTEGER :: i
  INTEGER :: j
  LOGICAL :: xyz_data
  CHARACTER(80) :: pltstring
  CHARACTER(*), PARAMETER :: datablock = '$xyz'
  !       character(*), parameter ::  cntr_table = '$xyz_contour'

  pltstring = ''
  !   Check the input data
  ncx = SIZE(x, dim=2)
  nrx = SIZE(x, dim=1)
  IF (PRESENT(y) .AND. PRESENT(z)) THEN
    xyz_data = .TRUE.
  ELSEIF (PRESENT(y)) THEN
    PRINT *, "GnuPlot_ error: Z matrix was not sent to 3D plot routine"
    RETURN
  ELSE
    xyz_data = .FALSE.
  END IF

  ! set default line style for 3D plot, can be overwritten
  obj%txtdatastyle = 'lines'
  ! create the script file for writting gnuplot commands and data
  CALL create_outputfile(obj)

  ! Write titles and other annotations
  CALL processcmd(obj)

  ! Write xy data into file
  WRITE (obj%file_unit, '(a)') '#data x y z'
  ! write the $xyz datablocks
  WRITE (obj%file_unit, '(a)') datablock//' << EOD'
  IF (xyz_data) THEN
    DO j = 1, ncx
      DO i = 1, nrx
        WRITE (obj%file_unit, fmt=*) x(i, j), y(i, j), z(i, j)
      END DO
      WRITE (obj%file_unit, '(a)') !put an empty line
    END DO
    WRITE (obj%file_unit, '(a)') 'EOD' !end of datablock
  ELSE !only Z has been sent (i.e. single matrix data)
    DO j = 1, ncx
      DO i = 1, nrx
        WRITE (obj%file_unit, fmt=*) i, j, x(i, j)
      END DO
      WRITE (obj%file_unit, '(a)') !put an empty line
    END DO
    WRITE (obj%file_unit, '(a)') 'EOD' !end of datablock
  END IF

  ! create the contour lines
  WRITE (obj%file_unit, '(a)') ! empty line
  WRITE (obj%file_unit, '(a)') '# create the contour'
  WRITE (obj%file_unit, '(a)') 'set contour base'
  WRITE (obj%file_unit, '(a)') 'set cntrparam levels 14'
  WRITE (obj%file_unit, '(a)') 'unset surface'
  WRITE (obj%file_unit, '(a)') 'set view map'

  !write the color palette into gnuplot script file
  IF (PRESENT(palette)) THEN
    WRITE (obj%file_unit, '(a)') color_palettes(palette)
    WRITE (obj%file_unit, '(a)') 'set pm3d' ! a conflict with lspec
  END IF

  WRITE (obj%file_unit, '(a)') ! empty line

  IF (PRESENT(lspec)) THEN
    IF (hastitle(lspec)) THEN
      pltstring = 'splot '//datablock//' '//TRIM(lspec)
    ELSE
      pltstring = 'splot '//datablock//' notitle '//TRIM(lspec)
    END IF
  ELSE
    pltstring = 'splot '//datablock//' notitle '
  END IF

  WRITE (obj%file_unit, '(a)') TRIM(pltstring)

  !> Rev 0.20
  ! if there is no animation finalize
  IF (.NOT. (obj%hasanimation)) THEN
    CALL finalize_plot(obj)
  ELSE
    WRITE (obj%file_unit, '(a, F5.2)') 'pause ', obj%pause_seconds
  END IF

  !: End of cplot
END SUBROUTINE cplot

SUBROUTINE lplot3d(obj, x, y, z, lspec, palette)
  !..............................................................................
  ! lplot3d create a line plot in 3d
  ! datablock is used instead of  gnuplot inline file "-"
  !..............................................................................

  CLASS(GnuPlot_) :: obj
  ! Input vector
  REAL(DFP), INTENT(in) :: x(:)
  REAL(DFP), INTENT(in), OPTIONAL :: y(:)
  REAL(DFP), INTENT(in), OPTIONAL :: z(:)
  CHARACTER(*), INTENT(in), OPTIONAL :: lspec
  CHARACTER(*), INTENT(in), OPTIONAL :: palette

  !   Local variables
  !----------------------------------------------------------------------
  INTEGER :: ncx
  INTEGER :: nrx
  INTEGER :: i
  INTEGER :: j
  LOGICAL :: xyz_data
  CHARACTER(80) :: pltstring
  CHARACTER(*), PARAMETER :: datablock = '$xyz'

  pltstring = ''
  !   Check the input data
  nrx = SIZE(x)
  IF (PRESENT(y) .AND. PRESENT(z)) THEN
    xyz_data = .TRUE.
  ELSEIF (PRESENT(y)) THEN
    PRINT *, "GnuPlot_ error: Z matrix was not sent to 3D plot routine"
    RETURN
  ELSE
    xyz_data = .FALSE.
  END IF

  ! set default line style for 3D plot, can be overwritten
  obj%txtdatastyle = 'lines'
  ! create the script file for writing gnuplot commands and data
  CALL create_outputfile(obj)

  ! Write titles and other annotations
  CALL processcmd(obj)

  ! Write xy data into file
  WRITE (obj%file_unit, '(a)') '#data x y z'
  ! Rev 0.20
  ! write the $xyz datablocks
  WRITE (obj%file_unit, '(a)') datablock//' << EOD'
  IF (xyz_data) THEN
    DO i = 1, nrx
      WRITE (obj%file_unit, *) x(i), y(i), z(i)
    END DO
    WRITE (obj%file_unit, '(a)') !put an empty line
    WRITE (obj%file_unit, '(a)') 'EOD' !end of datablock
  ELSE !only Z has been sent (i.e. single matrix data)
    DO i = 1, nrx
      WRITE (obj%file_unit, *) i, x(i)
    END DO
    WRITE (obj%file_unit, '(a)') !put an empty line
    WRITE (obj%file_unit, '(a)') 'EOD' !end of datablock
  END IF

  !write the color palette into gnuplot script file
  IF (PRESENT(palette)) THEN
    WRITE (obj%file_unit, '(a)') color_palettes(palette)
    WRITE (obj%file_unit, '(a)') 'set pm3d' ! a conflict with lspec
  END IF

  IF (PRESENT(lspec)) THEN
    IF (hastitle(lspec)) THEN
      pltstring = 'splot '//datablock//' '//TRIM(lspec)//'with lines'
    ELSE
      pltstring = 'splot '//datablock//' notitle '//TRIM(lspec)//'with lines'
    END IF
  ELSE
    pltstring = 'splot '//datablock//' notitle with lines'
  END IF

  WRITE (obj%file_unit, '(a)') TRIM(pltstring)

  !> Rev 0.2: animation
  ! if there is no animation finalize
  IF (.NOT. (obj%hasanimation)) THEN
    CALL finalize_plot(obj)
  ELSE
    WRITE (obj%file_unit, '(a, F5.2)') 'pause ', obj%pause_seconds
  END IF

  !: End of lplot3d
END SUBROUTINE lplot3d

SUBROUTINE function_plot(obj, func, xrange, np)
  !..............................................................................
  ! fplot, plot a function in the range xrange=[xmin, xamx] with np points
  ! if np is not sent, then np=50 is assumed!
  ! func is the name of function to be plotted
  !..............................................................................

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

  INTEGER :: n
  INTEGER :: i
  INTEGER :: alloc_err
  REAL(DFP), ALLOCATABLE :: x(:)
  REAL(DFP), ALLOCATABLE :: y(:)

  IF (PRESENT(np)) THEN
    n = np
  ELSE
    n = 50
  END IF
  ALLOCATE (x(1:n), y(1:n), stat=alloc_err)
  IF (alloc_err /= 0) THEN
    STOP "Allocation error in fplot procedure..."
  END IF
  !Create set of xy data
  x = Linspace(xrange(1), xrange(2), n)
  y = [(func(x(i)), i=1, n)]

  CALL plot2d_vector_vs_vector(obj, x, y)

  ! cleanup memory
  IF (ALLOCATED(x)) DEALLOCATE (x)
  IF (ALLOCATED(y)) DEALLOCATE (y)

END SUBROUTINE function_plot

SUBROUTINE semilogxv(obj, x1, y1, ls1, axes1, &
                     x2, y2, ls2, axes2, &
                     x3, y3, ls3, axes3, &
                     x4, y4, ls4, axes4)
  !..............................................................................
  !   obj procedure is the same as plotXY with logarithmic x1 and x2 axes
  !..............................................................................

  CLASS(GnuPlot_) :: obj
  ! Input vector
  REAL(DFP), INTENT(in) :: x1(:) ! vector of data for x
  REAL(DFP), INTENT(in), OPTIONAL :: y1(:) ! vector of data for y
  CHARACTER(*), INTENT(in), OPTIONAL :: ls1 ! line specification
  CHARACTER(*), INTENT(in), OPTIONAL :: axes1

  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: x2
  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: y2
  CHARACTER(*), INTENT(in), OPTIONAL :: ls2
  CHARACTER(*), INTENT(in), OPTIONAL :: axes2

  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: x3
  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: y3
  CHARACTER(*), INTENT(in), OPTIONAL :: ls3
  CHARACTER(*), INTENT(in), OPTIONAL :: axes3

  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: x4
  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: y4
  CHARACTER(*), INTENT(in), OPTIONAL :: ls4
  CHARACTER(*), INTENT(in), OPTIONAL :: axes4
  obj%plotscale = 'semilogx'
  CALL plot2d_vector_vs_vector(obj, &
                               x1, y1, ls1, axes1, &
                               x2, y2, ls2, axes2, &
                               x3, y3, ls3, axes3, &
                               x4, y4, ls4, axes4)
  ! Set the plot scale as linear. It means log scale is off
  obj%plotscale = 'linear'

END SUBROUTINE semilogxv

!..............................................................................
SUBROUTINE semilogyv(obj, x1, y1, ls1, axes1, &
                     x2, y2, ls2, axes2, &
                     x3, y3, ls3, axes3, &
                     x4, y4, ls4, axes4)
  !..............................................................................
  !   obj procedure is the same as plotXY with logarithmic y1 and y2 axes
  !..............................................................................

  CLASS(GnuPlot_) :: obj
  ! Input vector
  REAL(DFP), INTENT(in) :: x1(:) ! vector of data for x
  REAL(DFP), INTENT(in), OPTIONAL :: y1(:) ! vector of data for y
  CHARACTER(*), INTENT(in), OPTIONAL :: ls1 ! line specification
  CHARACTER(*), INTENT(in), OPTIONAL :: axes1

  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: x2
  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: y2
  CHARACTER(*), INTENT(in), OPTIONAL :: ls2
  CHARACTER(*), INTENT(in), OPTIONAL :: axes2

  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: x3
  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: y3
  CHARACTER(*), INTENT(in), OPTIONAL :: ls3
  CHARACTER(*), INTENT(in), OPTIONAL :: axes3

  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: x4
  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: y4
  CHARACTER(*), INTENT(in), OPTIONAL :: ls4
  CHARACTER(*), INTENT(in), OPTIONAL :: axes4

  obj%plotscale = 'semilogy'
  CALL plot2d_vector_vs_vector(obj, &
                               x1, y1, ls1, axes1, &
                               x2, y2, ls2, axes2, &
                               x3, y3, ls3, axes3, &
                               x4, y4, ls4, axes4)
  ! Set the plot scale as linear. It means log scale is off
  obj%plotscale = 'linear'

END SUBROUTINE semilogyv

SUBROUTINE loglogv(obj, x1, y1, ls1, axes1, &
                   x2, y2, ls2, axes2, &
                   x3, y3, ls3, axes3, &
                   x4, y4, ls4, axes4)
  !..............................................................................
  !   obj procedure is the same as plotXY with logarithmic x1, y1, x2, y2 axes
  !..............................................................................

  CLASS(GnuPlot_) :: obj
  ! Input vector
  REAL(DFP), INTENT(in) :: x1(:) ! vector of data for x
  REAL(DFP), INTENT(in), OPTIONAL :: y1(:) ! vector of data for y
  CHARACTER(*), INTENT(in), OPTIONAL :: ls1 ! line specification
  CHARACTER(*), INTENT(in), OPTIONAL :: axes1

  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: x2
  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: y2
  CHARACTER(*), INTENT(in), OPTIONAL :: ls2
  CHARACTER(*), INTENT(in), OPTIONAL :: axes2

  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: x3
  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: y3
  CHARACTER(*), INTENT(in), OPTIONAL :: ls3
  CHARACTER(*), INTENT(in), OPTIONAL :: axes3

  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: x4
  REAL(DFP), INTENT(in), DIMENSION(:), OPTIONAL :: y4
  CHARACTER(*), INTENT(in), OPTIONAL :: ls4
  CHARACTER(*), INTENT(in), OPTIONAL :: axes4

  obj%plotscale = 'loglog'
  CALL plot2d_vector_vs_vector(obj, &
                               x1, y1, ls1, axes1, &
                               x2, y2, ls2, axes2, &
                               x3, y3, ls3, axes3, &
                               x4, y4, ls4, axes4)
  ! Set the plot scale as linear. It means log scale is off
  obj%plotscale = 'linear'

END SUBROUTINE loglogv

SUBROUTINE semilogxm(obj, xv, ymat, lspec)
  !..............................................................................
  !Plots a matrix against a vector with logarithmic x-axis
  !For more information see plot2D_matrix_vs_vector procedure
  !Everything is the same except the x-axis scale
  !..............................................................................

  IMPLICIT NONE
  CLASS(GnuPlot_) :: obj
  ! Input arrays
  REAL(DFP), INTENT(in) :: xv(:)
  REAL(DFP), INTENT(in) :: ymat(:, :)
  CHARACTER(*), INTENT(in), OPTIONAL :: lspec

  obj%plotscale = 'semilogx'
  CALL plot2d_matrix_vs_vector(obj, xv, ymat, lspec)
  ! Set the plot scale as linear. It means log scale is off
  obj%plotscale = 'linear'

END SUBROUTINE semilogxm

SUBROUTINE semilogym(obj, xv, ymat, lspec)
  !..............................................................................
  !Plots a matrix against a vector with logarithmic y-axis
  !For more information see plot2D_matrix_vs_vector procedure
  !Everything is the same except the x-axis scale
  !..............................................................................

  IMPLICIT NONE
  CLASS(GnuPlot_) :: obj
  ! Input arrays
  REAL(DFP), INTENT(in) :: xv(:)
  REAL(DFP), INTENT(in) :: ymat(:, :)
  CHARACTER(*), INTENT(in), OPTIONAL :: lspec

  obj%plotscale = 'semilogy'
  CALL plot2d_matrix_vs_vector(obj, xv, ymat, lspec)
  ! Set the plot scale as linear. It means log scale is off
  obj%plotscale = 'linear'

END SUBROUTINE semilogym

SUBROUTINE loglogm(obj, xv, ymat, lspec)
  !..............................................................................
  !Plots a matrix against a vector with logarithmic x-axis and y-axis
  !For more information see plot2D_matrix_vs_vector procedure
  !Everything is the same except the axes scale
  !..............................................................................

  IMPLICIT NONE
  CLASS(GnuPlot_) :: obj
  ! Input arrays
  REAL(DFP), INTENT(in) :: xv(:)
  REAL(DFP), INTENT(in) :: ymat(:, :)
  CHARACTER(*), INTENT(in), OPTIONAL :: lspec

  obj%plotscale = 'loglog'
  CALL plot2d_matrix_vs_vector(obj, xv, ymat, lspec)
  ! Set the plot scale as linear. It means log scale is off
  obj%plotscale = 'linear'

END SUBROUTINE loglogm

    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Three: Animation Routines
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE sub_animation_start(obj, pause_seconds)
  !-------------------------------------------------------------------------------
  ! sub_animation_start: set the setting to start an animation
  ! it simply set flags and open a script file to write data
  !-------------------------------------------------------------------------------
  CLASS(GnuPlot_) :: obj
  REAL, INTENT(in), OPTIONAL :: pause_seconds

  ! ogpf does not support multiplot with animation at the same time
  IF (obj%hasmultiplot) THEN
    PRINT *, md_name//': does not support animation in multiplot mode!'
    STOP
  END IF

  IF (PRESENT(pause_seconds)) THEN
    obj%pause_seconds = pause_seconds
  ELSE
    obj%pause_seconds = 2 ! delay in second
  END IF

  obj%frame_number = 0

  ! create the ouput file for writting gnuplot script
  CALL create_outputfile(obj)
  obj%hasfileopen = .TRUE.
  obj%hasanimation = .TRUE.

END SUBROUTINE sub_animation_start

SUBROUTINE sub_animation_show(obj)
  !-------------------------------------------------------------------------------
  ! sub_animation_show: simply resets the animation flags
  ! and finalize the plotting.
  !-------------------------------------------------------------------------------

  CLASS(GnuPlot_) :: obj

  obj%frame_number = 0
  obj%hasanimation = .FALSE.

  CALL finalize_plot(obj)

END SUBROUTINE sub_animation_show

END MODULE GnuPlot_Class
