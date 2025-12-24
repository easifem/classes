!-------------------------------------------------------------------------------
!    GnuPlot Interface
!-------------------------------------------------------------------------------
!    Purpose:   Object Based Interface to GnuPlot from Fortran (ogpf)
!    Platform:  Windows XP/Vista/7/10
!               (It should work on other platforms, see the finalize_plot subroutine below)
!    Language:  Fortran 2003 and 2008
!    Requires:  1. Fortran 2003 compiler (e.g gfortran 5, IVF 12.1, ...)
!                  There is only two more features needs Fortran 2008 standard
!                  execute_command_line and passing internal function as argument.
!               2. gnuplot 5 and higher (other previous version can be used
!    Author:    Mohammad Rahmani
!               Chem Eng Dep., Amirkabir Uni. of Tech
!               Tehran, Ir
!               url:    aut.ac.ir/m.rahmani
!               github: github.com/kookma
!               email:  m[dot]rahmani[at]aut[dot]ac[dot]ir
!
!
! Acknowledgement:
! Special thanks to Hagen Wierstorf (http://www.gnuplotting.org)
! For vluable codes and examples on using gnuplot
! Some examples and color palletes are provided by gnuplotting.
!

MODULE OGPF
USE GlobalData, ONLY: wp => DFP, sp => REAL32, dp => REAL64
IMPLICIT NONE
PRIVATE

PUBLIC :: GPF

! PUBLIC arange, linspace, meshgrid, wp
! PUBLIC num2str

! Library information
CHARACTER(len=*), PARAMETER :: md_name = 'ogpf libray'
CHARACTER(len=*), PARAMETER :: md_rev = 'Rev. 0.22 of March 9th, 2018'
CHARACTER(len=*), PARAMETER :: md_lic = 'Licence: MIT'

! ogpf Configuration parameters
! The terminal and font have been set for Windows operating system
! Correct to meet the requirements on other OS like Linux and Mac.
CHARACTER(len=*), PARAMETER :: gnuplot_term_type = 'wxt'
! Output terminal
CHARACTER(len=*), PARAMETER :: gnuplot_term_font = 'Times New Roman,10'
! font
CHARACTER(len=*), PARAMETER :: gnuplot_term_size = '640,480'
!'960,840'                  ! plot window size
CHARACTER(len=*), PARAMETER :: gnuplot_output_filename = 'ogpf_temp_script.plt'
! temporary file for output
! extra configuration can be set using ogpf object

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! module procedure
INTERFACE num2str ! convert integer, real, double precision into string
  MODULE PROCEDURE num2str_i4
  MODULE PROCEDURE num2str_r4
  MODULE PROCEDURE num2str_r8
END INTERFACE

!> 0.22
! tplabel is a structure for gnuplot labels including
! title, xlabel, x2label, ylabel, ...
INTEGER, PARAMETER, PRIVATE :: NOT_INITIALIZED = -32000
TYPE tplabel
  LOGICAL :: has_label = .FALSE.
  CHARACTER(len=:), ALLOCATABLE :: lbltext
  CHARACTER(len=:), ALLOCATABLE :: lblcolor
  CHARACTER(len=:), ALLOCATABLE :: lblfontname
  INTEGER :: lblfontsize = NOT_INITIALIZED
  INTEGER :: lblrotate = NOT_INITIALIZED
END TYPE tplabel

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
! the gpf class implement the object for using gnuplot from fortran in a semi-interactive mode!
! the fortran actually do the job and write out the commands and data in a single file and then
! calls the gnuplot by shell command to plot the data

TYPE :: GPF
  !> 0.22
  TYPE(tplabel) :: tpplottitle
  TYPE(tplabel) :: tpxlabel
  TYPE(tplabel) :: tpx2label
  TYPE(tplabel) :: tpylabel
  TYPE(tplabel) :: tpy2label
  TYPE(tplabel) :: tpzlabel
  CHARACTER(len=:), ALLOCATABLE :: txtoptions
  ! a long string to store all type of gnuplot options
  CHARACTER(len=:), ALLOCATABLE :: txtscript
  ! a long string to store gnuplot script
  CHARACTER(len=:), ALLOCATABLE :: txtdatastyle
  ! lines, points, linepoints
  LOGICAL :: hasxrange = .FALSE.
  LOGICAL :: hasx2range = .FALSE.
  LOGICAL :: hasyrange = .FALSE.
  LOGICAL :: hasy2range = .FALSE.
  LOGICAL :: haszrange = .FALSE.
  LOGICAL :: hasoptions = .FALSE.
  LOGICAL :: hasanimation = .FALSE.
  LOGICAL :: hasfilename = .FALSE.
  LOGICAL :: hasfileopen = .FALSE.
  REAL(wp) :: xrange(2), yrange(2), zrange(2)
  REAL(wp) :: x2range(2), y2range(2)
  CHARACTER(len=8) :: plotscale
  ! multiplot parameters
  LOGICAL :: hasmultiplot = .FALSE.
  INTEGER :: multiplot_rows
  INTEGER :: multiplot_cols
  INTEGER :: multiplot_total_plots
  ! animation
  REAL :: pause_seconds = 0
  ! keep plot on screen for this value in seconds
  INTEGER :: frame_number
  ! frame number in animation
  ! use for debugging and error handling
  CHARACTER(len=:), ALLOCATABLE :: msg
  !Message from plot procedures
  INTEGER :: status = 0
  !Status from plot procedures
  INTEGER :: file_unit
  ! file unit identifier
  CHARACTER(len=:), ALLOCATABLE :: txtfilename
  ! the name of physical file
  ! to write the gnuplot script
  ! ogpf preset configuration (kind of gnuplot initialization)
  LOGICAL :: preset_configuration = .TRUE.
CONTAINS
  PRIVATE
  ! local private procedures
  PROCEDURE, PASS, PRIVATE :: preset_gnuplot_config
  PROCEDURE, PASS, PRIVATE :: plot2d_vector_vs_vector
  PROCEDURE, PASS, PRIVATE :: plot2d_matrix_vs_vector
  PROCEDURE, PASS, PRIVATE :: plot2d_matrix_vs_matrix
  PROCEDURE, PASS, PRIVATE :: semilogxv
  PROCEDURE, PASS, PRIVATE :: semilogxm
  PROCEDURE, PASS, PRIVATE :: semilogyv
  PROCEDURE, PASS, PRIVATE :: semilogym
  PROCEDURE, PASS, PRIVATE :: loglogv
  PROCEDURE, PASS, PRIVATE :: loglogm
  !> 0.22
  PROCEDURE, PASS, PRIVATE :: set_label
  ! public procedures
  PROCEDURE, PASS, PUBLIC :: options => set_options
  PROCEDURE, PASS, PUBLIC :: title => set_plottitle
  PROCEDURE, PASS, PUBLIC :: xlabel => set_xlabel
  PROCEDURE, PASS, PUBLIC :: x2label => set_x2label
  PROCEDURE, PASS, PUBLIC :: ylabel => set_ylabel
  PROCEDURE, PASS, PUBLIC :: y2label => set_y2label
  PROCEDURE, PASS, PUBLIC :: zlabel => set_zlabel
  PROCEDURE, PASS, PUBLIC :: axis => set_axis
  PROCEDURE, PASS, PUBLIC :: axis_sc => set_secondary_axis
  PROCEDURE, PASS, PUBLIC :: xlim => set_xlim
  PROCEDURE, PASS, PUBLIC :: ylim => set_ylim
  PROCEDURE, PASS, PUBLIC :: zlim => set_zlim
  PROCEDURE, PASS, PUBLIC :: filename => set_filename
  PROCEDURE, PASS, PUBLIC :: reset => reset_to_defaults
  PROCEDURE, PASS, PUBLIC :: preset => use_preset_configuration
  PROCEDURE, PASS, PUBLIC :: multiplot => sub_multiplot
  GENERIC, PUBLIC :: plot => plot2d_vector_vs_vector, &
    plot2d_matrix_vs_vector, &
    plot2d_matrix_vs_matrix
  GENERIC, PUBLIC :: semilogx => semilogxv, semilogxm
  GENERIC, PUBLIC :: semilogy => semilogyv, semilogym
  GENERIC, PUBLIC :: loglog => loglogv, loglogm
  PROCEDURE, PASS, PUBLIC :: surf => splot
  ! 3D surface plot
  PROCEDURE, PASS, PUBLIC :: lplot => lplot3d
  ! 3D line plot
  PROCEDURE, PASS, PUBLIC :: contour => cplot
  ! contour plot
  PROCEDURE, PASS, PUBLIC :: fplot => function_plot
  PROCEDURE, PASS, PUBLIC :: add_script => addscript
  PROCEDURE, PASS, PUBLIC :: run_script => runscript
  PROCEDURE, PASS, PUBLIC :: animation_start => sub_animation_start
  PROCEDURE, PASS, PUBLIC :: animation_show => sub_animation_show
END TYPE GPF

CONTAINS

!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!> Section One: Set/Get Methods for ogpf object
!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE use_preset_configuration(this, flag)
  !..............................................................................
  !Set a flag to tell ogpf if the customized gnuplot configuration should
  !be used
  !..............................................................................

  CLASS(gpf) :: this
  LOGICAL, INTENT(in) :: flag

  ! default is true
  this%preset_configuration = flag

END SUBROUTINE use_preset_configuration

SUBROUTINE set_filename(this, string)
  !..............................................................................
  !Set a file name for plot command output
  !This file can be used later by gnuplot as an script file to reproduce the plot
  !..............................................................................

  CLASS(gpf) :: this
  CHARACTER(len=*), INTENT(in) :: string

  this%txtfilename = TRIM(string)
  this%hasfilename = .TRUE.

END SUBROUTINE set_filename

SUBROUTINE set_options(this, stropt)
  !..............................................................................
  ! Set the plot options. This is a very powerfull procedure accepts many types
  ! of gnuplot command and customization
  !..............................................................................

  CLASS(gpf) :: this
  CHARACTER(len=*), INTENT(in) :: stropt

  IF (.NOT. ALLOCATED(this%txtoptions)) this%txtoptions = ''
  IF (LEN_TRIM(this%txtoptions) == 0) THEN
    this%txtoptions = '' ! initialize string
  END IF
  IF (LEN_TRIM(stropt) > 0) THEN
    this%txtoptions = this%txtoptions//splitstr(stropt)
  END IF

  this%hasoptions = .TRUE.

END SUBROUTINE set_options

SUBROUTINE set_xlim(this, rng)
  !..............................................................................
  !Set the x axis limits in form of [xmin, xmax]
  !..............................................................................

  CLASS(gpf) :: this
  REAL(wp), INTENT(in) :: rng(2)
  this%hasxrange = .TRUE.
  this%xrange = rng

END SUBROUTINE

SUBROUTINE set_ylim(this, rng)
  !..............................................................................
  !Set the y axis limits in form of [ymin, ymax]
  !..............................................................................

  CLASS(gpf) :: this
  REAL(wp), INTENT(in) :: rng(2)
  this%hasyrange = .TRUE.
  this%yrange = rng

END SUBROUTINE

SUBROUTINE set_zlim(this, rng)
  !..............................................................................
  !Set the z axis limits in form of [zmin, zmax]
  !..............................................................................

  CLASS(gpf) :: this
  REAL(wp), INTENT(in) :: rng(2)
  this%haszrange = .TRUE.
  this%zrange = rng

END SUBROUTINE

SUBROUTINE set_axis(this, rng)
  !..............................................................................
  !Set the axes limits in form of [xmin, xmax, ymin, ymax, zmin, zmax]
  !..............................................................................

  CLASS(gpf) :: this
  REAL(wp), INTENT(in) :: rng(:)
  INTEGER :: n
  n = SIZE(rng, dim=1)
  SELECT CASE (n)
  CASE (2) !Only the range for x-axis has been sent
    this%hasxrange = .TRUE.
    this%xrange = rng(1:2)
  CASE (4)
    this%hasxrange = .TRUE.
    this%hasyrange = .TRUE.
    this%xrange = rng(1:2)
    this%yrange = rng(3:4)
  CASE (6)
    this%hasxrange = .TRUE.
    this%hasyrange = .TRUE.
    this%haszrange = .TRUE.
    this%xrange = rng(1:2)
    this%yrange = rng(3:4)
    this%zrange = rng(5:6)
  CASE default
    PRINT *, 'gpf error: wrong axis range setting!'
    RETURN
  END SELECT

END SUBROUTINE set_axis

SUBROUTINE set_secondary_axis(this, rng)
  !..............................................................................
  !Set the secondary axes limits in form of [x2min, x2max, y2min, y2max]
  !..............................................................................

  CLASS(gpf) :: this
  REAL(wp), INTENT(in) :: rng(:)
  INTEGER :: n
  n = SIZE(rng, dim=1)
  SELECT CASE (n)
  CASE (2) !Only the range for x2-axis has been sent
    this%hasx2range = .TRUE.
    this%x2range = rng(1:2)
  CASE (4)
    this%hasx2range = .TRUE.
    this%hasy2range = .TRUE.
    this%x2range = rng(1:2)
    this%y2range = rng(3:4)
  CASE default
    PRINT *, 'gpf error: wrong axis range setting!'
    RETURN
  END SELECT

END SUBROUTINE set_secondary_axis

    subroutine set_plottitle(this, string, textcolor, font_size, font_name, rotate)
  !..............................................................................
  !Set the plot title
  !..............................................................................
  CLASS(gpf) :: this
  CHARACTER(len=*), INTENT(in) :: string
  CHARACTER(len=*), INTENT(in), OPTIONAL :: textcolor
  INTEGER, OPTIONAL :: font_size
  CHARACTER(len=*), INTENT(in), OPTIONAL :: font_name
  INTEGER, OPTIONAL :: rotate

        call this%set_label('plot_title', string, textcolor, font_size, font_name, rotate)

END SUBROUTINE set_plottitle

SUBROUTINE set_xlabel(this, string, textcolor, font_size, font_name, rotate)
  !..............................................................................
  !Set the xlabel
  !..............................................................................
  CLASS(gpf) :: this
  CHARACTER(len=*), INTENT(in) :: string
  CHARACTER(len=*), INTENT(in), OPTIONAL :: textcolor
  INTEGER, OPTIONAL :: font_size
  CHARACTER(len=*), INTENT(in), OPTIONAL :: font_name
  INTEGER, OPTIONAL :: rotate

CALL this%set_label('xlabel', string, textcolor, font_size, font_name, rotate)

END SUBROUTINE set_xlabel

SUBROUTINE set_x2label(this, string, textcolor, font_size, font_name, rotate)
  !..............................................................................
  !Set the x2label
  !..............................................................................
  CLASS(gpf) :: this
  CHARACTER(len=*), INTENT(in) :: string
  CHARACTER(len=*), INTENT(in), OPTIONAL :: textcolor
  INTEGER, OPTIONAL :: font_size
  CHARACTER(len=*), INTENT(in), OPTIONAL :: font_name
  INTEGER, OPTIONAL :: rotate

        call this%set_label('x2label', string, textcolor, font_size, font_name, rotate)

END SUBROUTINE set_x2label

SUBROUTINE set_ylabel(this, string, textcolor, font_size, font_name, rotate)
  !..............................................................................
  !Set the ylabel
  !..............................................................................
  CLASS(gpf) :: this
  CHARACTER(len=*), INTENT(in) :: string
  CHARACTER(len=*), INTENT(in), OPTIONAL :: textcolor
  INTEGER, OPTIONAL :: font_size
  CHARACTER(len=*), INTENT(in), OPTIONAL :: font_name
  INTEGER, OPTIONAL :: rotate

CALL this%set_label('ylabel', string, textcolor, font_size, font_name, rotate)

END SUBROUTINE set_ylabel

SUBROUTINE set_y2label(this, string, textcolor, font_size, font_name, rotate)
  !..............................................................................
  !Set the y2label
  !..............................................................................
  CLASS(gpf) :: this
  CHARACTER(len=*), INTENT(in) :: string
  CHARACTER(len=*), INTENT(in), OPTIONAL :: textcolor
  INTEGER, OPTIONAL :: font_size
  CHARACTER(len=*), INTENT(in), OPTIONAL :: font_name
  INTEGER, OPTIONAL :: rotate

        call this%set_label('y2label', string, textcolor, font_size, font_name, rotate)

END SUBROUTINE set_y2label

SUBROUTINE set_zlabel(this, string, textcolor, font_size, font_name, rotate)
  !..............................................................................
  !Set the zlabel
  !..............................................................................
  CLASS(gpf) :: this
  CHARACTER(len=*), INTENT(in) :: string
  CHARACTER(len=*), INTENT(in), OPTIONAL :: textcolor
  INTEGER, OPTIONAL :: font_size
  CHARACTER(len=*), INTENT(in), OPTIONAL :: font_name
  INTEGER, OPTIONAL :: rotate

CALL this%set_label('zlabel', string, textcolor, font_size, font_name, rotate)

END SUBROUTINE set_zlabel

!> 0.22

    subroutine set_label(this, lblname, lbltext, lblcolor, font_size, font_name, rotate)
  !..............................................................................
  ! Set the text, color, font, size and rotation for labels including
  ! title, xlabel, x2label, ylabel, ....
  !..............................................................................

  CLASS(gpf) :: this
  CHARACTER(len=*), INTENT(in) :: lblname
  CHARACTER(len=*), INTENT(in) :: lbltext
  CHARACTER(len=*), INTENT(in), OPTIONAL :: lblcolor
  CHARACTER(len=*), INTENT(in), OPTIONAL :: font_name
  INTEGER, OPTIONAL :: font_size
  INTEGER, OPTIONAL :: rotate

  ! local variable
  TYPE(tplabel) :: label

  label%has_label = .TRUE.
  label%lbltext = TRIM(lbltext)

  IF (PRESENT(lblcolor)) THEN
    label%lblcolor = lblcolor
  END IF

  IF (PRESENT(font_name)) THEN
    label%lblfontname = font_name
  ELSE
    IF (.NOT. ALLOCATED(label%lblfontname)) THEN
      label%lblfontname = ''
    END IF
  END IF

  IF (PRESENT(font_size)) THEN
    label%lblfontsize = font_size
  END IF

  IF (PRESENT(rotate)) THEN
    label%lblrotate = rotate
  END IF

  SELECT CASE (lblname)
  CASE ('xlabel')
    this%tpxlabel = label
  CASE ('x2label')
    this%tpx2label = label
  CASE ('ylabel')
    this%tpylabel = label
  CASE ('y2label')
    this%tpy2label = label
  CASE ('zlabel')
    this%tpzlabel = label
  CASE ('plot_title')
    this%tpplottitle = label
  END SELECT

END SUBROUTINE set_label

SUBROUTINE reset_to_defaults(this)
  !..............................................................................
  !Reset all ogpf properties (params to their default values
  !...............................................................................
  CLASS(gpf) :: this

  this%preset_configuration = .TRUE.
  this%txtfilename = gnuplot_output_filename

  IF (ALLOCATED(this%txtoptions)) DEALLOCATE (this%txtoptions)
  IF (ALLOCATED(this%txtscript)) DEALLOCATE (this%txtscript)
  IF (ALLOCATED(this%txtdatastyle)) DEALLOCATE (this%txtdatastyle)
  IF (ALLOCATED(this%msg)) DEALLOCATE (this%msg)

  this%hasoptions = .FALSE.

  this%hasxrange = .FALSE.
  this%hasx2range = .FALSE.
  this%hasyrange = .FALSE.
  this%hasy2range = .FALSE.
  this%haszrange = .FALSE.

  this%pause_seconds = 0.0
  this%status = 0
  this%hasanimation = .FALSE.
  this%hasfileopen = .FALSE.
  this%hasmultiplot = .FALSE.

  this%plotscale = ''
  this%tpplottitle%has_label = .FALSE.
  this%tpxlabel%has_label = .FALSE.
  this%tpx2label%has_label = .FALSE.
  this%tpylabel%has_label = .FALSE.
  this%tpy2label%has_label = .FALSE.
  this%tpzlabel%has_label = .FALSE.

END SUBROUTINE reset_to_defaults

    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Two: Main Plotting Routines
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE sub_multiplot(this, rows, cols)
  !..............................................................................
  ! This subroutine sets flag and number of rows and columns in case
  ! of multiplot layout
  !..............................................................................

  CLASS(gpf) :: this
  INTEGER, INTENT(in) :: rows
  INTEGER, INTENT(in) :: cols

  ! ogpf does not support multiplot in animation mode
  IF (this%hasanimation) THEN
    PRINT *, md_name//': ogpf does not support animation in multiplot mode'
    STOP
  END IF

  ! set multiplot cols and rows
  IF (rows > 0) THEN
    this%multiplot_rows = rows
  ELSE

  END IF
  IF (cols > 0) THEN
    this%multiplot_cols = cols
  ELSE

  END IF

  ! set the multiplot layout flag and plot numbers
  this%hasmultiplot = .TRUE.
  this%multiplot_total_plots = 0

  ! create the ouput file for writting gnuplot script
  CALL create_outputfile(this)

END SUBROUTINE sub_multiplot

SUBROUTINE plot2d_vector_vs_vector(this, x1, y1, ls1, axes1, &
                                   x2, y2, ls2, axes2, &
                                   x3, y3, ls3, axes3, &
                                   x4, y4, ls4, axes4)
  !..............................................................................
  ! This procedure plots:
  !   1. A vector against another vector (xy plot)
  !   2. A vector versus its element indices (yi plot).
  !   3. Can accept up to 4 data sets as x,y pairs!
  ! Arguments
  ! xi, yi vectors of data series,
  ! lsi a string maximum 80 characters containing the line specification,
  ! legends, ...
  ! axesi is the axes for plotting: secondary axes are x2, and y2
  !..............................................................................

  CLASS(gpf) :: this
  ! Input vector
  REAL(wp), INTENT(in) :: x1(:) ! vector of data for x
  REAL(wp), INTENT(in), OPTIONAL :: y1(:) ! vector of data for y
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls1 ! line specification
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes1

  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: x2
  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: y2
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls2
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes2

  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: x3
  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: y3
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls3
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes3

  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: x4
  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: y4
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls4
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes4

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
  CHARACTER(len=3) :: plottype
  INTEGER :: i
  CHARACTER(len=80) :: pltstring(4) ! Four 80 characters string

  !Initialize variables
  plottype = ''
  pltstring = ''

  !   Check the input
  nx1 = SIZE(x1)
  IF ((PRESENT(y1))) THEN
    ny1 = SIZE(y1)
    IF (checkdim(nx1, ny1)) THEN
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
    IF (checkdim(nx2, ny2)) THEN
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
    IF (checkdim(nx3, ny3)) THEN
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
    IF (checkdim(nx4, ny4)) THEN
      plottype = 'xy4'
      number_of_plots = 4
    ELSE
      RETURN
    END IF
    !Process line spec for 4th data set if present
    CALL process_linespec(4, pltstring(4), ls4, axes4)
  END IF

  CALL create_outputfile(this)

  ! Write plot title, axis labels and other annotations
  CALL processcmd(this)

  ! Write plot command and line styles and legend if any
  IF (number_of_plots == 1) THEN
    WRITE (this%file_unit, '(a)') TRIM(pltstring(1))
  ELSE
            write ( this%file_unit, '(a)' )  ( trim(pltstring(i)) // ' \' , i=1, number_of_plots-1)
    WRITE (this%file_unit, '(a)') TRIM(pltstring(number_of_plots))
  END IF
  ! Write xy data into file
  SELECT CASE (plottype)
  CASE ('xi')
    CALL write_xydata(this%file_unit, nx1, x1)
  CASE ('xy1')
    CALL write_xydata(this%file_unit, nx1, x1, y1)
  CASE ('xy2')
    CALL write_xydata(this%file_unit, nx1, x1, y1)
    CALL write_xydata(this%file_unit, nx2, x2, y2)
  CASE ('xy3')
    CALL write_xydata(this%file_unit, nx1, x1, y1)
    CALL write_xydata(this%file_unit, nx2, x2, y2)
    CALL write_xydata(this%file_unit, nx3, x3, y3)
  CASE ('xy4')
    CALL write_xydata(this%file_unit, nx1, x1, y1)
    CALL write_xydata(this%file_unit, nx2, x2, y2)
    CALL write_xydata(this%file_unit, nx3, x3, y3)
    CALL write_xydata(this%file_unit, nx4, x4, y4)
  END SELECT

  !> Rev 0.2
  ! if there is no animation finalize
  IF (.NOT. (this%hasanimation)) THEN
    CALL finalize_plot(this)
  ELSE
    WRITE (this%file_unit, '(a, F5.2)') 'pause ', this%pause_seconds
  END IF

  !: End of plot2D_vector_vs_vector
END SUBROUTINE plot2d_vector_vs_vector

SUBROUTINE plot2d_matrix_vs_vector(this, xv, ymat, lspec)
  !..............................................................................
  ! plot2D_matrix_vs_vector accepts a vector xv and a matrix ymat and plots
  ! columns of ymat against xv. lspec is an optional array defines the line
  ! specification for each data series. If a single element array is sent for
  ! lspec then all series are plotted using the same linespec
  !..............................................................................

  IMPLICIT NONE
  CLASS(gpf) :: this
  ! Input arrays
  REAL(wp), INTENT(in) :: xv(:)
  REAL(wp), INTENT(in) :: ymat(:, :)
  CHARACTER(len=*), INTENT(in), OPTIONAL :: lspec
  !----------------------------------------------------------------------
  !       Local variables
  INTEGER :: nx
  INTEGER :: ny
  INTEGER :: ns
  INTEGER :: number_of_curves
  INTEGER :: i
  INTEGER :: j
  INTEGER :: ierr
  CHARACTER(len=80), ALLOCATABLE :: pltstring(:), lst(:)
  !

  !*******************************************************************************
  !   Check the input
  nx = SIZE(xv)
  ny = SIZE(ymat, dim=1)
  IF (.NOT. checkdim(nx, ny)) THEN
            print*, md_name // ':plot2d_matrix_vs_vector:' // 'The length of arrays does not match'
    RETURN
  END IF
  ! create the outfile to write the gnuplot script
  CALL create_outputfile(this)

  ! Write titles and other annotations
  CALL processcmd(this)

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
        write ( this%file_unit, '(a)' ) ( trim(pltstring(i)) // ' \' , i=1, number_of_curves-1)
  WRITE (this%file_unit, '(a)') TRIM(pltstring(number_of_curves))

  ! Write data into script file
  DO j = 1, number_of_curves
    DO i = 1, nx
      WRITE (this%file_unit, *) xv(i), ymat(i, j)
    END DO
    WRITE (this%file_unit, '(a)') 'e' !end of jth set of data
  END DO

  !> Rev 0.2
  ! if there is no animation finalize
  IF (.NOT. (this%hasanimation)) THEN
    CALL finalize_plot(this)
  ELSE
    WRITE (this%file_unit, '(a, F5.2)') 'pause ', this%pause_seconds
  END IF

  !Release memory
  IF (ALLOCATED(pltstring)) THEN
    DEALLOCATE (pltstring)
  END IF
  !: End of plot2D_matrix_vs_vector
END SUBROUTINE plot2d_matrix_vs_vector

SUBROUTINE plot2d_matrix_vs_matrix(this, xmat, ymat, lspec)
  !..............................................................................
  ! plot2D_matrix_vs_matrix accepts a matrix xmat and a matrix ymat and plots
  ! columns of ymat against columns of xmat. lspec is an optional array defines
  ! the line specification for each data series. If a single element array is
  ! sent for lspec then all series are plotted using the same linespec
  !..............................................................................

  IMPLICIT NONE
  CLASS(gpf) :: this
  ! Input arrays
  REAL(wp), INTENT(in) :: xmat(:, :)
  REAL(wp), INTENT(in) :: ymat(:, :)
  CHARACTER(len=*), INTENT(in), OPTIONAL :: lspec
  !----------------------------------------------------------------------
  !       Local variables
  INTEGER :: mx, nx
  INTEGER :: my, ny
  INTEGER :: ns
  INTEGER :: number_of_curves
  INTEGER :: i
  INTEGER :: j
  INTEGER :: ierr
  CHARACTER(len=80), ALLOCATABLE :: pltstring(:), lst(:)
  !

  !*******************************************************************************
  !   Check the input
  ! check number of rows
  mx = SIZE(xmat, dim=1)
  my = SIZE(ymat, dim=1)
  IF (.NOT. checkdim(mx, my)) THEN
            print*, md_name // ':plot2d_matrix_vs_matrix:' // 'The length of arrays does not match'
    RETURN
  END IF
  ! check number of rows
  nx = SIZE(xmat, dim=2)
  ny = SIZE(ymat, dim=2)
  IF (.NOT. checkdim(nx, ny)) THEN
   PRINT *, 'gpf error: The number of columns are different, check xmat, ymat'
    RETURN
  END IF

  ! create the outfile to write the gnuplot script
  CALL create_outputfile(this)

  ! Write titles and other annotations
  CALL processcmd(this)

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
        write ( this%file_unit, '(a)' ) ( trim(pltstring(i)) // ' \' , i=1, number_of_curves-1)
  WRITE (this%file_unit, '(a)') TRIM(pltstring(number_of_curves))

  ! Write data into script file
  DO j = 1, number_of_curves
    DO i = 1, mx
      WRITE (this%file_unit, *) xmat(i, j), ymat(i, j)
    END DO
    WRITE (this%file_unit, '(a)') 'e' !end of jth set of data
  END DO

  !> Rev 0.2
  ! if there is no animation finalize
  IF (.NOT. (this%hasanimation)) THEN
    CALL finalize_plot(this)
  ELSE
    WRITE (this%file_unit, '(a, F5.2)') 'pause ', this%pause_seconds
  END IF

  !Release memory
  IF (ALLOCATED(pltstring)) THEN
    DEALLOCATE (pltstring)
  END IF
  !: End of plot2D_matrix_vs_vector
END SUBROUTINE plot2d_matrix_vs_matrix

SUBROUTINE splot(this, x, y, z, lspec, palette)
  !..............................................................................
  ! splot create a surface plot
  ! datablock is used instead of  gnuplot inline file "-"
  !..............................................................................

  CLASS(gpf) :: this
  ! Input vector
  REAL(wp), INTENT(in) :: x(:, :)
  REAL(wp), INTENT(in), OPTIONAL :: y(:, :)
  REAL(wp), INTENT(in), OPTIONAL :: z(:, :)
  CHARACTER(len=*), INTENT(in), OPTIONAL :: lspec
  CHARACTER(len=*), INTENT(in), OPTIONAL :: palette

  !   Local variables
  !----------------------------------------------------------------------
  INTEGER :: ncx
  INTEGER :: nrx
  INTEGER :: i
  INTEGER :: j
  LOGICAL :: xyz_data
  CHARACTER(len=80) :: pltstring
  CHARACTER(len=*), PARAMETER :: datablock = '$xyz'

  pltstring = ''
  !   Check the input data
  ncx = SIZE(x, dim=2)
  nrx = SIZE(x, dim=1)
  IF (PRESENT(y) .AND. PRESENT(z)) THEN
    xyz_data = .TRUE.
  ELSEIF (PRESENT(y)) THEN
    PRINT *, "gpf error: Z matrix was not sent to 3D plot routine"
    RETURN
  ELSE
    xyz_data = .FALSE.
  END IF

  ! set default line style for 3D plot, can be overwritten
  this%txtdatastyle = 'lines'
  ! create the script file for writting gnuplot commands and data
  CALL create_outputfile(this)

  ! Write titles and other annotations
  CALL processcmd(this)

  ! Write xy data into file
  WRITE (this%file_unit, '(a)') '#data x y z'
  ! Rev 0.20
  ! write the $xyz datablocks
  WRITE (this%file_unit, '(a)') datablock//' << EOD'
  IF (xyz_data) THEN
    DO j = 1, ncx
      DO i = 1, nrx
        WRITE (this%file_unit, *) x(i, j), y(i, j), z(i, j)
      END DO
      WRITE (this%file_unit, '(a)') !put an empty line
    END DO
    WRITE (this%file_unit, '(a)') 'EOD' !end of datablock
  ELSE !only Z has been sent (i.e. single matrix data)
    DO j = 1, ncx
      DO i = 1, nrx
        WRITE (this%file_unit, *) i, j, x(i, j)
      END DO
      WRITE (this%file_unit, '(a)') !put an empty line
    END DO
    WRITE (this%file_unit, '(a)') 'EOD' !end of datablock
  END IF

  !write the color palette into gnuplot script file
  IF (PRESENT(palette)) THEN
    WRITE (this%file_unit, '(a)') color_palettes(palette)
    WRITE (this%file_unit, '(a)') 'set pm3d' ! a conflict with lspec
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

  WRITE (this%file_unit, '(a)') TRIM(pltstring)

  !> Rev 0.2: animation
  ! if there is no animation finalize
  IF (.NOT. (this%hasanimation)) THEN
    CALL finalize_plot(this)
  ELSE
    WRITE (this%file_unit, '(a, F5.2)') 'pause ', this%pause_seconds
  END IF

  !: End of splot
END SUBROUTINE splot

SUBROUTINE cplot(this, x, y, z, lspec, palette,)
  !..............................................................................
  !   Rev 0.19
  !   cplot creates a contour plot based on the three dimensional data
  !..............................................................................

  CLASS(gpf) :: this
  ! Input vector
  REAL(wp), INTENT(in) :: x(:, :)
  REAL(wp), INTENT(in), OPTIONAL :: y(:, :)
  REAL(wp), INTENT(in), OPTIONAL :: z(:, :)
  CHARACTER(len=*), INTENT(in), OPTIONAL :: lspec
  CHARACTER(len=*), INTENT(in), OPTIONAL :: palette

  !   Local variables
  !----------------------------------------------------------------------

  INTEGER :: ncx
  INTEGER :: nrx
  INTEGER :: i
  INTEGER :: j
  LOGICAL :: xyz_data
  CHARACTER(len=80) :: pltstring
  CHARACTER(len=*), PARAMETER :: datablock = '$xyz'
  !       character(len=*), parameter ::  cntr_table = '$xyz_contour'

  pltstring = ''
  !   Check the input data
  ncx = SIZE(x, dim=2)
  nrx = SIZE(x, dim=1)
  IF (PRESENT(y) .AND. PRESENT(z)) THEN
    xyz_data = .TRUE.
  ELSEIF (PRESENT(y)) THEN
    PRINT *, "gpf error: Z matrix was not sent to 3D plot routine"
    RETURN
  ELSE
    xyz_data = .FALSE.
  END IF

  ! set default line style for 3D plot, can be overwritten
  this%txtdatastyle = 'lines'
  ! create the script file for writting gnuplot commands and data
  CALL create_outputfile(this)

  ! Write titles and other annotations
  CALL processcmd(this)

  ! Write xy data into file
  WRITE (this%file_unit, '(a)') '#data x y z'
  ! write the $xyz datablocks
  WRITE (this%file_unit, '(a)') datablock//' << EOD'
  IF (xyz_data) THEN
    DO j = 1, ncx
      DO i = 1, nrx
        WRITE (this%file_unit, fmt=*) x(i, j), y(i, j), z(i, j)
      END DO
      WRITE (this%file_unit, '(a)') !put an empty line
    END DO
    WRITE (this%file_unit, '(a)') 'EOD' !end of datablock
  ELSE !only Z has been sent (i.e. single matrix data)
    DO j = 1, ncx
      DO i = 1, nrx
        WRITE (this%file_unit, fmt=*) i, j, x(i, j)
      END DO
      WRITE (this%file_unit, '(a)') !put an empty line
    END DO
    WRITE (this%file_unit, '(a)') 'EOD' !end of datablock
  END IF

  ! create the contour lines
  WRITE (this%file_unit, '(a)') ! empty line
  WRITE (this%file_unit, '(a)') '# create the contour'
  WRITE (this%file_unit, '(a)') 'set contour base'
  WRITE (this%file_unit, '(a)') 'set cntrparam levels 14'
  WRITE (this%file_unit, '(a)') 'unset surface'
  WRITE (this%file_unit, '(a)') 'set view map'

  !write the color palette into gnuplot script file
  IF (PRESENT(palette)) THEN
    WRITE (this%file_unit, '(a)') color_palettes(palette)
    WRITE (this%file_unit, '(a)') 'set pm3d' ! a conflict with lspec
  END IF

  WRITE (this%file_unit, '(a)') ! empty line

  IF (PRESENT(lspec)) THEN
    IF (hastitle(lspec)) THEN
      pltstring = 'splot '//datablock//' '//TRIM(lspec)
    ELSE
      pltstring = 'splot '//datablock//' notitle '//TRIM(lspec)
    END IF
  ELSE
    pltstring = 'splot '//datablock//' notitle '
  END IF

  WRITE (this%file_unit, '(a)') TRIM(pltstring)

  !> Rev 0.20
  ! if there is no animation finalize
  IF (.NOT. (this%hasanimation)) THEN
    CALL finalize_plot(this)
  ELSE
    WRITE (this%file_unit, '(a, F5.2)') 'pause ', this%pause_seconds
  END IF

  !: End of cplot
END SUBROUTINE cplot

SUBROUTINE lplot3d(this, x, y, z, lspec, palette)
  !..............................................................................
  ! lplot3d create a line plot in 3d
  ! datablock is used instead of  gnuplot inline file "-"
  !..............................................................................

  CLASS(gpf) :: this
  ! Input vector
  REAL(wp), INTENT(in) :: x(:)
  REAL(wp), INTENT(in), OPTIONAL :: y(:)
  REAL(wp), INTENT(in), OPTIONAL :: z(:)
  CHARACTER(len=*), INTENT(in), OPTIONAL :: lspec
  CHARACTER(len=*), INTENT(in), OPTIONAL :: palette

  !   Local variables
  !----------------------------------------------------------------------
  INTEGER :: ncx
  INTEGER :: nrx
  INTEGER :: i
  INTEGER :: j
  LOGICAL :: xyz_data
  CHARACTER(len=80) :: pltstring
  CHARACTER(len=*), PARAMETER :: datablock = '$xyz'

  pltstring = ''
  !   Check the input data
  nrx = SIZE(x)
  IF (PRESENT(y) .AND. PRESENT(z)) THEN
    xyz_data = .TRUE.
  ELSEIF (PRESENT(y)) THEN
    PRINT *, "gpf error: Z matrix was not sent to 3D plot routine"
    RETURN
  ELSE
    xyz_data = .FALSE.
  END IF

  ! set default line style for 3D plot, can be overwritten
  this%txtdatastyle = 'lines'
  ! create the script file for writing gnuplot commands and data
  CALL create_outputfile(this)

  ! Write titles and other annotations
  CALL processcmd(this)

  ! Write xy data into file
  WRITE (this%file_unit, '(a)') '#data x y z'
  ! Rev 0.20
  ! write the $xyz datablocks
  WRITE (this%file_unit, '(a)') datablock//' << EOD'
  IF (xyz_data) THEN
    DO i = 1, nrx
      WRITE (this%file_unit, *) x(i), y(i), z(i)
    END DO
    WRITE (this%file_unit, '(a)') !put an empty line
    WRITE (this%file_unit, '(a)') 'EOD' !end of datablock
  ELSE !only Z has been sent (i.e. single matrix data)
    DO i = 1, nrx
      WRITE (this%file_unit, *) i, x(i)
    END DO
    WRITE (this%file_unit, '(a)') !put an empty line
    WRITE (this%file_unit, '(a)') 'EOD' !end of datablock
  END IF

  !write the color palette into gnuplot script file
  IF (PRESENT(palette)) THEN
    WRITE (this%file_unit, '(a)') color_palettes(palette)
    WRITE (this%file_unit, '(a)') 'set pm3d' ! a conflict with lspec
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

  WRITE (this%file_unit, '(a)') TRIM(pltstring)

  !> Rev 0.2: animation
  ! if there is no animation finalize
  IF (.NOT. (this%hasanimation)) THEN
    CALL finalize_plot(this)
  ELSE
    WRITE (this%file_unit, '(a, F5.2)') 'pause ', this%pause_seconds
  END IF

  !: End of lplot3d
END SUBROUTINE lplot3d

SUBROUTINE function_plot(this, func, xrange, np)
  !..............................................................................
  ! fplot, plot a function in the range xrange=[xmin, xamx] with np points
  ! if np is not sent, then np=50 is assumed!
  ! func is the name of function to be plotted
  !..............................................................................

  CLASS(gpf) :: this
  INTERFACE
    FUNCTION func(x)
      IMPORT :: wp
      REAL(wp), INTENT(in) :: x
      REAL(wp) :: func
    END FUNCTION func
  END INTERFACE
  REAL(wp), INTENT(in) :: xrange(2)
  INTEGER, OPTIONAL, INTENT(in) :: np

  INTEGER :: n
  INTEGER :: i
  INTEGER :: alloc_err
  REAL(wp), ALLOCATABLE :: x(:)
  REAL(wp), ALLOCATABLE :: y(:)

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
  x = linspace(xrange(1), xrange(2), n)
  y = [(func(x(i)), i=1, n)]

  CALL plot2d_vector_vs_vector(this, x, y)

  ! cleanup memory
  IF (ALLOCATED(x)) DEALLOCATE (x)
  IF (ALLOCATED(y)) DEALLOCATE (y)

END SUBROUTINE function_plot

SUBROUTINE semilogxv(this, x1, y1, ls1, axes1, &
                     x2, y2, ls2, axes2, &
                     x3, y3, ls3, axes3, &
                     x4, y4, ls4, axes4)
  !..............................................................................
  !   This procedure is the same as plotXY with logarithmic x1 and x2 axes
  !..............................................................................

  CLASS(gpf) :: this
  ! Input vector
  REAL(wp), INTENT(in) :: x1(:) ! vector of data for x
  REAL(wp), INTENT(in), OPTIONAL :: y1(:) ! vector of data for y
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls1 ! line specification
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes1

  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: x2
  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: y2
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls2
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes2

  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: x3
  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: y3
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls3
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes3

  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: x4
  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: y4
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls4
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes4
  this%plotscale = 'semilogx'
  CALL plot2d_vector_vs_vector(this, &
                               x1, y1, ls1, axes1, &
                               x2, y2, ls2, axes2, &
                               x3, y3, ls3, axes3, &
                               x4, y4, ls4, axes4)
  ! Set the plot scale as linear. It means log scale is off
  this%plotscale = 'linear'

END SUBROUTINE semilogxv

!..............................................................................
SUBROUTINE semilogyv(this, x1, y1, ls1, axes1, &
                     x2, y2, ls2, axes2, &
                     x3, y3, ls3, axes3, &
                     x4, y4, ls4, axes4)
  !..............................................................................
  !   This procedure is the same as plotXY with logarithmic y1 and y2 axes
  !..............................................................................

  CLASS(gpf) :: this
  ! Input vector
  REAL(wp), INTENT(in) :: x1(:) ! vector of data for x
  REAL(wp), INTENT(in), OPTIONAL :: y1(:) ! vector of data for y
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls1 ! line specification
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes1

  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: x2
  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: y2
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls2
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes2

  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: x3
  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: y3
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls3
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes3

  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: x4
  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: y4
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls4
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes4

  this%plotscale = 'semilogy'
  CALL plot2d_vector_vs_vector(this, &
                               x1, y1, ls1, axes1, &
                               x2, y2, ls2, axes2, &
                               x3, y3, ls3, axes3, &
                               x4, y4, ls4, axes4)
  ! Set the plot scale as linear. It means log scale is off
  this%plotscale = 'linear'

END SUBROUTINE semilogyv

SUBROUTINE loglogv(this, x1, y1, ls1, axes1, &
                   x2, y2, ls2, axes2, &
                   x3, y3, ls3, axes3, &
                   x4, y4, ls4, axes4)
  !..............................................................................
  !   This procedure is the same as plotXY with logarithmic x1, y1, x2, y2 axes
  !..............................................................................

  CLASS(gpf) :: this
  ! Input vector
  REAL(wp), INTENT(in) :: x1(:) ! vector of data for x
  REAL(wp), INTENT(in), OPTIONAL :: y1(:) ! vector of data for y
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls1 ! line specification
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes1

  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: x2
  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: y2
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls2
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes2

  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: x3
  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: y3
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls3
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes3

  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: x4
  REAL(wp), INTENT(in), DIMENSION(:), OPTIONAL :: y4
  CHARACTER(len=*), INTENT(in), OPTIONAL :: ls4
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes4

  this%plotscale = 'loglog'
  CALL plot2d_vector_vs_vector(this, &
                               x1, y1, ls1, axes1, &
                               x2, y2, ls2, axes2, &
                               x3, y3, ls3, axes3, &
                               x4, y4, ls4, axes4)
  ! Set the plot scale as linear. It means log scale is off
  this%plotscale = 'linear'

END SUBROUTINE loglogv

SUBROUTINE semilogxm(this, xv, ymat, lspec)
  !..............................................................................
  !Plots a matrix against a vector with logarithmic x-axis
  !For more information see plot2D_matrix_vs_vector procedure
  !Everything is the same except the x-axis scale
  !..............................................................................

  IMPLICIT NONE
  CLASS(gpf) :: this
  ! Input arrays
  REAL(wp), INTENT(in) :: xv(:)
  REAL(wp), INTENT(in) :: ymat(:, :)
  CHARACTER(len=*), INTENT(in), OPTIONAL :: lspec

  this%plotscale = 'semilogx'
  CALL plot2d_matrix_vs_vector(this, xv, ymat, lspec)
  ! Set the plot scale as linear. It means log scale is off
  this%plotscale = 'linear'

END SUBROUTINE semilogxm

SUBROUTINE semilogym(this, xv, ymat, lspec)
  !..............................................................................
  !Plots a matrix against a vector with logarithmic y-axis
  !For more information see plot2D_matrix_vs_vector procedure
  !Everything is the same except the x-axis scale
  !..............................................................................

  IMPLICIT NONE
  CLASS(gpf) :: this
  ! Input arrays
  REAL(wp), INTENT(in) :: xv(:)
  REAL(wp), INTENT(in) :: ymat(:, :)
  CHARACTER(len=*), INTENT(in), OPTIONAL :: lspec

  this%plotscale = 'semilogy'
  CALL plot2d_matrix_vs_vector(this, xv, ymat, lspec)
  ! Set the plot scale as linear. It means log scale is off
  this%plotscale = 'linear'

END SUBROUTINE semilogym

SUBROUTINE loglogm(this, xv, ymat, lspec)
  !..............................................................................
  !Plots a matrix against a vector with logarithmic x-axis and y-axis
  !For more information see plot2D_matrix_vs_vector procedure
  !Everything is the same except the axes scale
  !..............................................................................

  IMPLICIT NONE
  CLASS(gpf) :: this
  ! Input arrays
  REAL(wp), INTENT(in) :: xv(:)
  REAL(wp), INTENT(in) :: ymat(:, :)
  CHARACTER(len=*), INTENT(in), OPTIONAL :: lspec

  this%plotscale = 'loglog'
  CALL plot2d_matrix_vs_vector(this, xv, ymat, lspec)
  ! Set the plot scale as linear. It means log scale is off
  this%plotscale = 'linear'

END SUBROUTINE loglogm

    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Three: Animation Routines
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE sub_animation_start(this, pause_seconds)
  !-------------------------------------------------------------------------------
  ! sub_animation_start: set the setting to start an animation
  ! it simply set flags and open a script file to write data
  !-------------------------------------------------------------------------------
  CLASS(gpf) :: this
  REAL, INTENT(in), OPTIONAL :: pause_seconds

  ! ogpf does not support multiplot with animation at the same time
  IF (this%hasmultiplot) THEN
    PRINT *, md_name//': does not support animation in multiplot mode!'
    STOP
  END IF

  IF (PRESENT(pause_seconds)) THEN
    this%pause_seconds = pause_seconds
  ELSE
    this%pause_seconds = 2 ! delay in second
  END IF

  this%frame_number = 0

  ! create the ouput file for writting gnuplot script
  CALL create_outputfile(this)
  this%hasfileopen = .TRUE.
  this%hasanimation = .TRUE.

END SUBROUTINE sub_animation_start

SUBROUTINE sub_animation_show(this)
  !-------------------------------------------------------------------------------
  ! sub_animation_show: simply resets the animation flags
  ! and finalize the plotting.
  !-------------------------------------------------------------------------------

  CLASS(gpf) :: this

  this%frame_number = 0
  this%hasanimation = .FALSE.

  CALL finalize_plot(this)

END SUBROUTINE sub_animation_show

    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Four: Gnuplot direct scriptting
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE addscript(this, strcmd)
  !..............................................................................
  ! addscript: accepts all type of gnuplot command as a string and store it
  ! in global txtscript to be later sent to gnuplot
  !..............................................................................

  CLASS(gpf) :: this
  CHARACTER(len=*), INTENT(in) :: strcmd

  IF (.NOT. ALLOCATED(this%txtscript)) this%txtscript = ''
  IF (LEN_TRIM(this%txtscript) == 0) THEN
    this%txtscript = '' ! initialize string
  END IF
  IF (LEN_TRIM(strcmd) > 0) THEN
    this%txtscript = this%txtscript//splitstr(strcmd)
  END IF

END SUBROUTINE addscript

SUBROUTINE runscript(this)
  !..............................................................................
  ! runscript sends the the script string (txtstring) into a script
  ! file to be run by gnuplot
  !..............................................................................

  CLASS(gpf) :: this

  !REV 0.18: a dedicated subroutine is used to create the output file
  CALL create_outputfile(this)

  !write the script
  CALL processcmd(this)
  WRITE (unit=this%file_unit, fmt='(a)') this%txtscript

  ! close the file and call gnuplot
  CALL finalize_plot(this)

END SUBROUTINE runscript

    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Five: gnuplot command processing and data writing to script file
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE process_axes_set(axes_set, axes)
  !..............................................................................
  ! process_axesspec accepts the axes set and interpret it into
  ! a format to be sent to gnuplot.
  ! the axes set can be one of the following set
  ! x1y1, x1y2, x2y1, x2y2
  !..............................................................................

  CHARACTER(len=*), INTENT(in) :: axes_set
  CHARACTER(len=4), INTENT(out) :: axes

  IF (LEN_TRIM(ADJUSTL(axes_set)) == 0) THEN
    axes = ''
    RETURN
  END IF

  SELECT CASE (lcase(TRIM(ADJUSTL(axes_set))))
  CASE ('x1y1')
    axes = 'x1y1'
  CASE ('x1y2')
    axes = 'x1y2'
  CASE ('x2y1')
    axes = 'x2y1'
  CASE ('x2y2')
    axes = 'x2y2'
  CASE default ! wrong strings
                print*, md_name // ':process_axes_set:' // ' wrong axes set is sent.'// new_line(' ') &
      //'axes set can be on of: x1y1, x1y2, x2y1, x2y2'
    axes = ''
    RETURN
  END SELECT

END SUBROUTINE process_axes_set

SUBROUTINE process_linespec(order, lsstring, lspec, axes_set)
  !..............................................................................
  ! process_linespec accepts the line specification and interpret it into
  ! a format to be sent to gnuplot
  !..............................................................................

  INTEGER, INTENT(in) :: order !1 for the first data series
  CHARACTER(len=*), INTENT(out) :: lsstring
  CHARACTER(len=*), INTENT(in), OPTIONAL :: lspec
  CHARACTER(len=*), INTENT(in), OPTIONAL :: axes_set

  !local variables
  CHARACTER(len=4) :: axes
  CHARACTER(len=10) :: axes_setting

  !check the axes set
  axes_setting = ''
  IF (PRESENT(axes_set)) THEN
    CALL process_axes_set(axes_set, axes)
    IF (LEN(TRIM(axes)) > 0) THEN
      axes_setting = ' axes '//axes
    END IF
  END IF

  SELECT CASE (order)
  CASE (1)
    IF (PRESENT(lspec)) THEN
      IF (hastitle(lspec)) THEN
        lsstring = 'plot "-" '//TRIM(lspec)//axes_setting
      ELSE
        lsstring = 'plot "-" notitle '//TRIM(lspec)//axes_setting
      END IF
    ELSE
      lsstring = 'plot "-" notitle'//axes_setting
    END IF
  CASE default !e.g. 2, 3, 4, ...
    IF (PRESENT(lspec)) THEN
      IF (hastitle(lspec)) THEN
        lsstring = ', "-" '//TRIM(lspec)//axes_setting
      ELSE
        lsstring = ', "-" notitle '//TRIM(lspec)//axes_setting
      END IF
    ELSE
      lsstring = ', "-" notitle'//axes_setting
    END IF
  END SELECT
END SUBROUTINE process_linespec

SUBROUTINE processcmd(this)
  !..............................................................................
  !   This subroutine writes all the data into plot file
  !   to be read by gnuplot
  !..............................................................................

  CLASS(gpf) :: this

  ! write the plot style for data
  ! this is used only when 3D plots (splot, cplot) is used
  IF (ALLOCATED(this%txtdatastyle)) THEN
    WRITE (this%file_unit, '("set style data ", a)') this%txtdatastyle
    WRITE (this%file_unit, '(a)')
  END IF

  ! Write options
  IF (this%hasoptions) THEN
    WRITE (this%file_unit, '(" ")')
    WRITE (this%file_unit, '("# options")')
    WRITE (this%file_unit, '(a)') this%txtoptions
    WRITE (this%file_unit, '(a)')
  END IF

  ! Check with plot scale: i.e linear, logx, logy, or log xy
  WRITE (this%file_unit, '(" ")')
  WRITE (this%file_unit, '("# plot scale")')
  SELECT CASE (this%plotscale)
  CASE ('semilogx')
    WRITE (this%file_unit, '("set logscale  x")')
  CASE ('semilogy')
    WRITE (this%file_unit, '("set logscale  y")')
  CASE ('loglog')
    WRITE (this%file_unit, '("set logscale  xy")')
  CASE default !for no setting
    !pass
  END SELECT

        !!>0.22
  ! write annotation
  WRITE (this%file_unit, '(" ")')
  WRITE (this%file_unit, '("# Annotation: title and labels")')
  CALL write_label(this, 'plot_title')
  CALL write_label(this, 'xlabel')
  CALL write_label(this, 'x2label')
  CALL write_label(this, 'ylabel')
  CALL write_label(this, 'y2label')
  CALL write_label(this, 'zlabel')

  ! axes range
  WRITE (this%file_unit, '(" ")')
  WRITE (this%file_unit, '("# axes setting")')
  IF (this%hasxrange) THEN
    WRITE (this%file_unit, '("set xrange [",G0,":",G0,"]")') this%xrange
  END IF
  IF (this%hasyrange) THEN
    WRITE (this%file_unit, '("set yrange [",G0,":",G0,"]")') this%yrange
  END IF
  IF (this%haszrange) THEN
    WRITE (this%file_unit, '("set zrange [",G0,":",G0,"]")') this%zrange
  END IF

  ! secondary axes range
  IF (this%hasx2range) THEN
    WRITE (this%file_unit, '("set x2range [",G0,":",G0,"]")') this%x2range
  END IF
  IF (this%hasy2range) THEN
    WRITE (this%file_unit, '("set y2range [",G0,":",G0,"]")') this%y2range
  END IF
  ! finish by new line
  WRITE (this%file_unit, '(a)') ! emptyline

END SUBROUTINE processcmd

SUBROUTINE write_label(this, lblname)
  !..............................................................................
  !   This subroutine writes the labels into plot file
  !   to be read by gnuplot
  !..............................................................................

  ! write_label
  CLASS(gpf) :: this
  CHARACTER(len=*) :: lblname

  ! local var
  CHARACTER(len=:), ALLOCATABLE :: lblstring
  CHARACTER(len=:), ALLOCATABLE :: lblset
  TYPE(tplabel) :: label

  SELECT CASE (lblname)
  CASE ('xlabel')
    IF (.NOT. (this%tpxlabel%has_label)) THEN
      RETURN ! there is no label
    END IF
    lblset = 'set xlabel "'
    label = this%tpxlabel
  CASE ('x2label')
    IF (.NOT. (this%tpx2label%has_label)) THEN
      RETURN ! there is no label
    END IF
    lblset = 'set x2label "'
    label = this%tpx2label
  CASE ('ylabel')
    IF (.NOT. (this%tpylabel%has_label)) THEN
      RETURN ! there is no label
    END IF
    lblset = 'set ylabel "'
    label = this%tpylabel
  CASE ('y2label')
    IF (.NOT. (this%tpy2label%has_label)) THEN
      RETURN ! there is no label
    END IF
    lblset = 'set y2label "'
    label = this%tpy2label
  CASE ('zlabel')
    IF (.NOT. (this%tpzlabel%has_label)) THEN
      RETURN ! there is no label
    END IF
    lblset = 'set zlabel "'
    label = this%tpzlabel
  CASE ('plot_title')
    IF (.NOT. (this%tpplottitle%has_label)) THEN
      RETURN ! there is no label
    END IF
    lblset = 'set title "'
    label = this%tpplottitle
  END SELECT

  lblstring = ''
  ! if there is a label continue to set it
  lblstring = lblstring//lblset//TRIM(label%lbltext)//'"'
  IF (ALLOCATED(label%lblcolor)) THEN
    lblstring = lblstring//' tc "'//TRIM(label%lblcolor)//'"'
  END IF
  ! set font and size
  IF (ALLOCATED(this%tpxlabel%lblfontname)) THEN
    lblstring = lblstring//' font "'//TRIM(label%lblfontname)//','
    IF (label%lblfontsize /= NOT_INITIALIZED) THEN
      lblstring = lblstring//num2str(label%lblfontsize)//'"'
    ELSE
      lblstring = lblstring//'"'
    END IF
  ELSE ! check if only font size has been given
    IF (label%lblfontsize /= NOT_INITIALIZED) THEN
      lblstring = lblstring//' font ",'//num2str(label%lblfontsize)//'"'
    END IF
  END IF
  ! set rotation
  IF (label%lblrotate /= NOT_INITIALIZED) THEN
    lblstring = lblstring//' rotate by '//num2str(label%lblrotate)
  END IF

  ! write to ogpf script file
  WRITE (this%file_unit, '(a)') lblstring

END SUBROUTINE write_label

FUNCTION color_palettes(palette_name) RESULT(str)
  !...............................................................................
  ! color_palettes create color palette as a
  ! string to be written into gnuplot script file
  ! the palettes credit goes to: Anna Schnider (https://github.com/aschn) and
  ! Hagen Wierstorf (https://github.com/hagenw)
  !...............................................................................
  CHARACTER(len=*), INTENT(in) :: palette_name
  CHARACTER(len=:), ALLOCATABLE :: str

  ! local variables
  CHARACTER(len=1) :: strnumber
  CHARACTER(len=11) :: strblank
  INTEGER :: j
  INTEGER :: maxcolors

  ! define the color palettes
  CHARACTER(len=:), ALLOCATABLE :: pltname
  CHARACTER(len=7) :: palette(10) ! palettes with maximum 9 colors

  maxcolors = 8 ! default number of discrete colors
  palette = ''
  SELECT CASE (lcase(TRIM(ADJUSTL(palette_name))))
  CASE ('set1')
    pltname = 'set1'
    palette(1:maxcolors) = [ &
                           "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", &
                           "#FF7F00", "#FFFF33", "#A65628", "#F781BF"]
  CASE ('set2')
    pltname = 'set2'
    palette(1:maxcolors) = [ &
                           "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", &
                           "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3"]
  CASE ('set3')
    pltname = 'set3'
    palette(1:maxcolors) = [ &
                           "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", &
                           "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5"]
  CASE ('palette1')
    pltname = 'palette1'
    palette(1:maxcolors) = [ &
                           "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", &
                           "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC"]
  CASE ('palette2')
    pltname = 'palette2'
    palette(1:maxcolors) = [ &
                           "#B3E2CD", "#FDCDAC", "#CDB5E8", "#F4CAE4", &
                           "#D6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC"]
  CASE ('paired')
    pltname = 'paired'
    palette(1:maxcolors) = [ &
                           "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", &
                           "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00"]
  CASE ('dark2')
    pltname = 'dark2'
    palette(1:maxcolors) = [ &
                           "#1B9E77", "#D95F02", "#7570B3", "#E7298A", &
                           "#66A61E", "#E6AB02", "#A6761D", "#666666"]
  CASE ('accent')
    pltname = 'accent'
    palette(1:maxcolors) = [ &
                           "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", &
                           "#386CB0", "#F0027F", "#BF5B17", "#666666"]
  CASE ('jet')
    ! Matlab jet palette
    maxcolors = 9
    pltname = 'jet'
    palette(1:maxcolors) = [ &
                           '#000090', '#000fff', '#0090ff', '#0fffee', &
                        '#90ff70', '#ffee00', '#ff7000', '#ee0000', '#7f0000']

  CASE ('vik')
    maxcolors = 10
    pltname = 'vik'
    palette = ["#001261", "#033E7D", "#1E6F9D", "#71A8C4", "#C9DDE7", &
               "#EACEBD", "#D39774", "#BE6533", "#8B2706", "#590008"]
  CASE default
    PRINT *, md_name//": color_palettes: wrong palette name"
    PRINT *, 'gnuplot default palette will be used!'
    str = ' ' ! empty palette is returned!
    RETURN
  END SELECT

  ! generate the gnuplot palette as a single multiline string
  str = '# Define the '//pltname//' pallete'//NEW_LINE(' ')
  str = str//'set palette defined ( \'//NEW_LINE(' ')
  strblank = '           ' ! pad certain number of paces
  DO j = 1, maxcolors - 1
    WRITE (unit=strnumber, fmt='(I1)') j - 1
    str = str//strblank//strnumber//' "'//palette(j)//'",\'//NEW_LINE(' ')
  END DO

  j = maxcolors - 1
  WRITE (strnumber, fmt='(I1)') j
  str = str//strblank//strnumber//' "'//palette(j)//'" )'//NEW_LINE(' ')

END FUNCTION color_palettes

SUBROUTINE write_xydata(file_unit, ndata, x, y)
  !..............................................................................
  ! Writes set of xy data into a file
  !..............................................................................

  INTEGER, INTENT(in) :: file_unit
  INTEGER, INTENT(in) :: ndata
  REAL(wp), INTENT(in) :: x(:)
  REAL(wp), INTENT(in), OPTIONAL :: y(:)

  INTEGER :: i

  ! TODO (Mohammad#1#12/22/17): The format string shall be modified to write the
  ! number in more suitable form
  ! Rev 0.18
  IF (PRESENT(y)) THEN !both x and y are present, data are xy set
    DO i = 1, ndata
      WRITE (file_unit, *) x(i), y(i)
    END DO
  ELSE !only x is passed, data are index-x set
    DO i = 1, ndata
      WRITE (file_unit, *) x(i)
    END DO
  END IF
  WRITE (file_unit, '(a)') 'e' !end of set of data

END SUBROUTINE write_xydata

SUBROUTINE create_outputfile(this)
  !..............................................................................
  ! Create an output file, assign a file_unit
  ! for writing the gnuplot commands
  !..............................................................................

  ! Rev 0.18
  CLASS(gpf), INTENT(inout) :: this

  IF (this%hasfileopen) THEN
    ! there is nothing to do, file has been already open!
    RETURN
  END IF

  !> Rev 0.2 animation

  ! animation handling
  IF (this%hasanimation) THEN
    this%frame_number = this%frame_number + 1 ! for future use
  END IF

  ! Open the output file

  IF (.NOT. (this%hasfilename)) THEN ! check if no file has been set by user
    this%txtfilename = gnuplot_output_filename
  END IF

        open ( newunit = this%file_unit, file = this%txtfilename, status = 'replace', iostat = this%status )

  IF (this%status /= 0) THEN
    PRINT *, "md_helperproc, create_outputfile: cannot open file for output"
    STOP
  END IF

  ! Set the gnuplot terminal, write ogpf configuration (customized setting)
  ! Can be overwritten by options

  ! write signature
  WRITE (this%file_unit, '(a)') '# '//md_name
  WRITE (this%file_unit, '(a)') '# '//md_rev
  WRITE (this%file_unit, '(a)') '# '//md_lic
  WRITE (this%file_unit, '(a)') ! emptyline

  ! write the global settings
  WRITE (this%file_unit, '(a)') '# gnuplot global setting'
  WRITE (unit=this%file_unit, fmt='(a)') 'set term '//gnuplot_term_type// &
    ' size '//gnuplot_term_size//' enhanced font "'// &
    gnuplot_term_font//'"'// &
    ' title "'//md_name//': '//md_rev//'"' ! library name and version

  ! write the preset configuration for gnuplot (ogpf customized settings)
  IF (this%preset_configuration) THEN
    CALL this%preset_gnuplot_config()
  END IF
  ! write multiplot setting
  IF (this%hasmultiplot) THEN
    WRITE (this%file_unit, fmt='(a, I2, a, I2)') 'set multiplot layout ', &
      this%multiplot_rows, ',', this%multiplot_cols
  END IF
  ! set flag true for file is opened
  this%hasfileopen = .TRUE.

END SUBROUTINE create_outputfile

SUBROUTINE preset_gnuplot_config(this)
  !..............................................................................
  ! To write the preset configuration for gnuplot (ogpf customized settings)
  !..............................................................................
  CLASS(gpf) :: this

  WRITE (this%file_unit, fmt='(a)')
  WRITE (this%file_unit, fmt='(a)') '# ogpf extra configuration'
        write(this%file_unit, fmt='(a)') '# -------------------------------------------'

  ! color definition
  WRITE (this%file_unit, fmt='(a)') '# color definitions'
write(this%file_unit, fmt='(a)') 'set style line 1 lc rgb "#800000" lt 1 lw 2'
write(this%file_unit, fmt='(a)') 'set style line 2 lc rgb "#ff0000" lt 1 lw 2'
write(this%file_unit, fmt='(a)') 'set style line 3 lc rgb "#ff4500" lt 1 lw 2'
write(this%file_unit, fmt='(a)') 'set style line 4 lc rgb "#ffa500" lt 1 lw 2'
write(this%file_unit, fmt='(a)') 'set style line 5 lc rgb "#006400" lt 1 lw 2'
write(this%file_unit, fmt='(a)') 'set style line 6 lc rgb "#0000ff" lt 1 lw 2'
write(this%file_unit, fmt='(a)') 'set style line 7 lc rgb "#9400d3" lt 1 lw 2'
  WRITE (this%file_unit, fmt='(a)')
  ! axes setting
  WRITE (this%file_unit, fmt='(a)') '# Axes'
  WRITE (this%file_unit, fmt='(a)') 'set border linewidth 1.15'
  WRITE (this%file_unit, fmt='(a)') 'set tics nomirror'
  WRITE (this%file_unit, fmt='(a)')

  WRITE (this%file_unit, fmt='(a)') '# grid'
  WRITE (this%file_unit, fmt='(a)') '# Add light grid to plot'
        write(this%file_unit, fmt='(a)') 'set style line 102 lc rgb "#d6d7d9" lt 0 lw 1'
  WRITE (this%file_unit, fmt='(a)') 'set grid back ls 102'
  WRITE (this%file_unit, fmt='(a)')
  ! set the plot style
  WRITE (this%file_unit, fmt='(a)') '# plot style'
  WRITE (this%file_unit, fmt='(a)') 'set style data linespoints'
  WRITE (this%file_unit, fmt='(a)')

        write(this%file_unit, fmt='(a)') '# -------------------------------------------'
  WRITE (this%file_unit, fmt='(a)') ''

END SUBROUTINE preset_gnuplot_config

SUBROUTINE finalize_plot(this)
  !..............................................................................
  ! To finalize the writing of gnuplot commands/data and close the output file.
  !..............................................................................
  CLASS(gpf) :: this

  ! check for multiplots
  IF (this%hasmultiplot) THEN
            if (this%multiplot_total_plots < this%multiplot_rows * this%multiplot_cols - 1 ) then
      ! increment the number of plots
      this%multiplot_total_plots = this%multiplot_total_plots + 1
      RETURN ! do not finalize plot, still there is places in multiplot
    ELSE
      ! close multiplot
      WRITE (this%file_unit, fmt='(a)') 'unset multiplot'
      ! reset multiplot flag
      this%hasmultiplot = .FALSE.

    END IF
  END IF

  WRITE (this%file_unit, fmt='(a)') 'pause mouse close'
  CLOSE (unit=this%file_unit) ! close the script file
  this%hasfileopen = .FALSE. ! reset file open flag
  this%hasanimation = .FALSE.
  ! Use shell command to run gnuplot
  IF (get_os_type() == 1) THEN
    CALL execute_command_line('wgnuplot -persist '//this%txtfilename) !   Now plot the results
  ELSE
    CALL execute_command_line('gnuplot -persist '//this%txtfilename) !   Now plot the results
  END IF
CONTAINS
  INTEGER FUNCTION get_os_type() RESULT(r)
            !! Returns one of OS_WINDOWS, others
            !! At first, the environment variable `OS` is checked, which is usually
            !! found on Windows.
            !! Copy from fpm/fpm_environment: https://github.com/fortran-lang/fpm/blob/master/src/fpm_environment.f90
    CHARACTER(len=32) :: val
    INTEGER :: length, rc

    INTEGER, PARAMETER :: OS_OTHERS = 0
    INTEGER, PARAMETER :: OS_WINDOWS = 1

    r = OS_OTHERS
    ! Check environment variable `OS`.
    CALL GET_ENVIRONMENT_VARIABLE('OS', val, length, rc)

    IF (rc == 0 .AND. length > 0 .AND. INDEX(val, 'Windows_NT') > 0) THEN
      r = OS_WINDOWS
      RETURN
    END IF

  END FUNCTION

END SUBROUTINE finalize_plot

    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Six: Utility and helper procedures
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION hastitle(string)
  !..............................................................................
  ! check to see if the plot title (used as legend = key)
  !..............................................................................

  CHARACTER(len=*), INTENT(in) :: string
  LOGICAL :: hastitle
  INTEGER :: idx1
  INTEGER :: idx2

  idx1 = INDEX(lcase(string), 'title') !Check if title is passed
  idx2 = INDEX(' '//lcase(string), ' t ') !Check if the abbreviated title 't' is passed. Extra space is added
  ! at the beginning of string to find starting 't'
  IF (idx1 /= 0 .OR. idx2 /= 0) THEN
    hastitle = .TRUE.
  ELSE
    hastitle = .FALSE.
  END IF

END FUNCTION hastitle

FUNCTION checkdim(nx, ny)
  !..............................................................................
  ! checkdim checks the equality of dimensions of two vector
  !..............................................................................

  INTEGER, INTENT(in) :: nx
  INTEGER, INTENT(in) :: ny
  LOGICAL :: checkdim
  IF (nx /= ny) THEN
    checkdim = .FALSE.
  ELSE
    checkdim = .TRUE.
  END IF

END FUNCTION checkdim

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!> Section Seven: String utility Routines
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PURE FUNCTION splitstr(str) RESULT(spstr)
  !..............................................................................
  !splitstr, separate a string using ";" delimiters
  !..............................................................................

  CHARACTER(len=*), INTENT(in) :: str

  ! local variables
  CHARACTER, PARAMETER :: delimiter = ';'
  CHARACTER(len=:), ALLOCATABLE :: spstr
  INTEGER :: n
  INTEGER :: m
  INTEGER :: k

  k = LEN_TRIM(str) !length with removed trailing blanks
  n = SCAN(str, delimiter)
  IF (n == 0) THEN ! This is a single statement
    spstr = ADJUSTL(str)//NEW_LINE(' ')
    RETURN
  END IF

  ! for two or more statements separated by ;
  spstr = ''
  m = 1
  DO WHILE (n /= 0 .AND. m < k)
    IF (n /= 1) THEN
      spstr = spstr//ADJUSTL(str(m:m + n - 2))//NEW_LINE(' ')
    END IF
    m = n + m
    n = SCAN(str(m:k), delimiter)
  END DO
  IF (m < k) THEN !write the last statement
    spstr = spstr//ADJUSTL(str(m:k))//NEW_LINE(' ')
  END IF
END FUNCTION splitstr

SUBROUTINE splitstring2array(strin, strarray, delimiter)
  !..............................................................................
  ! splitstring splits a string to an array of
  ! substrings based on a selected delimiter
  ! note:
  !    a. any facing space/blank in substrings will be removed
  !    b. two adjacent delimiter treats as an empty substring between them
  !    c. facing and trailing delimiter treats as empty substring at the fornt and end
  !..............................................................................

  CHARACTER(len=*), INTENT(in) :: strin
  CHARACTER(len=80), ALLOCATABLE, INTENT(out) :: strarray(:)
  CHARACTER(len=1), OPTIONAL, INTENT(in) :: delimiter

  ! local variables
  INTEGER :: m, n
  INTEGER :: i, idx
  CHARACTER(len=LEN(strin)) :: strtmp
  CHARACTER(len=1) :: delimiter_

  ! 0. check the existance of delimiter
  IF (PRESENT(delimiter)) THEN
    delimiter_ = delimiter
  ELSE
    delimiter_ = ';'
  END IF

  ! 1. remove initial blanks if any
  strtmp = TRIM(ADJUSTL(strin))

  ! 2. count the number substrings separated by delimiter
  n = COUNT([(strtmp(i:i) == delimiter_, i=1, LEN_TRIM(strtmp))])

  ! 3. allocate the output string array
  ALLOCATE (strarray(n + 1))

  ! 4. extract substrings and store in array one by one
  m = 1
  DO i = 1, n
    idx = INDEX(strtmp(m:), delimiter_)
    strarray(i) = ADJUSTL(strtmp(m:m + idx - 2))
    m = m + idx
  END DO
  strarray(n + 1) = ADJUSTL(strtmp(m:))

END SUBROUTINE splitstring2array

FUNCTION lcase(strinput)
  !..............................................................................
  ! Return the string (strInput) in lowercase
  !..............................................................................

  CHARACTER(len=*), INTENT(in) :: strinput
  CHARACTER(len=LEN(strinput)) :: lcase
  INTEGER :: i
  INTEGER :: n
  CHARACTER(1) :: chr

  DO i = 1, LEN(strinput)
    chr = strinput(i:i)
    n = ICHAR(chr)
    IF (n >= 65 .AND. n <= 90) THEN
      lcase(i:i) = CHAR(n + 32)
    ELSE
      lcase(i:i) = chr
    END IF
  END DO
END FUNCTION lcase

FUNCTION num2str_i4(number_in)
  !..............................................................................
  ! num2str_int: converts integer number to string
  !..............................................................................

  INTEGER(kind=KIND(1)), INTENT(in) :: number_in
  CHARACTER(len=:), ALLOCATABLE :: num2str_i4

  ! local variable
  CHARACTER(len=RANGE(number_in)) :: strnm
  WRITE (unit=strnm, fmt='(I0)') number_in
  num2str_i4 = TRIM(strnm)

END FUNCTION num2str_i4

FUNCTION num2str_r4(number_in, strfmt)
  !..............................................................................
  ! num2str_r4: converts single precision real number to string
  ! strfmt is the optional format string
  !..............................................................................

  REAL(kind=sp), INTENT(in) :: number_in
  CHARACTER(len=*), INTENT(in), OPTIONAL :: strfmt
  CHARACTER(len=:), ALLOCATABLE :: num2str_r4

  ! local variable
  CHARACTER(len=RANGE(number_in)) :: strnm

  IF (PRESENT(strfmt)) THEN
    WRITE (unit=strnm, fmt='('//TRIM(strfmt)//')') number_in
  ELSE
    WRITE (unit=strnm, fmt='(G0)') number_in
  END IF

  num2str_r4 = TRIM(strnm)

END FUNCTION num2str_r4

FUNCTION num2str_r8(number_in, strfmt)
  !..............................................................................
  ! num2str_real: converts double precision real number to string
  ! strfmt is the optional format string
  !..............................................................................

  REAL(kind=dp), INTENT(in) :: number_in
  CHARACTER(len=*), INTENT(in), OPTIONAL :: strfmt
  CHARACTER(len=:), ALLOCATABLE :: num2str_r8

  ! local variable
  CHARACTER(len=RANGE(number_in)) :: strnm

  IF (PRESENT(strfmt)) THEN
    WRITE (unit=strnm, fmt='('//TRIM(strfmt)//')') number_in
  ELSE
    WRITE (unit=strnm, fmt='(G0)') number_in
  END IF

  num2str_r8 = TRIM(strnm)

END FUNCTION num2str_r8

    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Eight: Math helper function
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION arange(xa, xb, dx)
  !..............................................................................
  !   returns a vector in the form of [xa, xa+dx, xa+2*dx, ...]
  !   the number of elements is calculated as m = n+ 1,
  !   where n= int ( (xa-xb)/dx) ).
  !   arange is similar to colon in Matlab and arange in Python!
  !
  !   NOTE:
  !    - If n calculated as zero, result is [xa]
  !    - If n calculated as Inf (dx=0), a fatal error will be raised
  !    - If n calculated as negative value (e.g xa<xb or dx<0.0), a
  !      fatal error will be raised
  !..............................................................................

  REAL(wp), INTENT(in) :: xa
  REAL(wp), INTENT(in) :: xb
  REAL(wp), INTENT(in), OPTIONAL :: dx
  REAL(wp), ALLOCATABLE :: arange(:)

  !   Local vars
  REAL(wp) :: dxl
  INTEGER :: i
  INTEGER :: n
  INTEGER :: ierr

  ! check the presence of dx and its correctness
  IF (PRESENT(dx)) THEN
    dxl = dx
    IF (ABS(dx) <= TINY(dx)) THEN
      PRINT *, "arange procedure: Fatal Error: wrong dx, use a dx > 0.0 "
      STOP
    END IF
  ELSE
    dxl = 1.0_WP
  END IF

  IF ((xa < xb) .AND. (dx < 0.0_WP)) THEN
    PRINT *, "arange procedure: Fatal Error: wrong dx, use a dx > 0.0 "
    STOP
  END IF

  n = INT((xb - xa) / dxl) ! n+1 is the number of elements

  ALLOCATE (arange(n), stat=ierr)

  IF (ierr /= 0) THEN
PRINT *, "arange procedure: Fatal Error, allocation failed in arange function"
    STOP
  END IF

  arange = [(xa + i * dxl, i=0, n)]

END FUNCTION arange

FUNCTION linspace(a, b, n_elements)
  !..............................................................................
  !   returns a linearly spaced vector with n points in [a, b]
  !   if n is omitted, 100 points will be considered
  !..............................................................................

  REAL(wp), INTENT(in) :: a
  REAL(wp), INTENT(in) :: b
  INTEGER, INTENT(in), OPTIONAL :: n_elements
  REAL(wp), ALLOCATABLE :: linspace(:)

  !   Local vars
  REAL(wp) :: dx
  INTEGER :: i
  INTEGER :: n
  INTEGER :: ierr

  IF (PRESENT(n_elements)) THEN
    IF (n_elements <= 1) THEN
                print*, "linspace procedure: Error: wrong value of n_elements, use an n_elements > 1"
      STOP
    END IF
    n = n_elements
  ELSE
    n = 100
  END IF

  ALLOCATE (linspace(n), stat=ierr)
  IF (ierr /= 0) THEN
            print*, "linspace procedure: Fatal Error, Allocation failed in linspace function"
    STOP
  END IF

  dx = (b - a) / REAL((n - 1), wp)
  linspace = [(i * dx + a, i=0, n - 1)]

END FUNCTION linspace

SUBROUTINE meshgrid(x, y, xgv, ygv, ierr)
  !..............................................................................
  !meshgrid generate mesh grid over a rectangular domain of [xmin xmax, ymin, ymax]
  ! Inputs:
  !     xgv, ygv are grid vectors in form of full grid data
  ! Outputs:
  !     X and Y are matrix each of size [ny by nx] contains the grid data.
  !     The coordinates of point (i,j) is [X(i,j), Y(i,j)]
  !     ierr: The error flag
  !     """
  !     # Example
  !     # call meshgrid(X, Y, [0.,1.,2.,3.],[5.,6.,7.,8.])
  !     # X
  !     # [0.0, 1.0, 2.0, 3.0,
  !     #  0.0, 1.0, 2.0, 3.0,
  !     #  0.0, 1.0, 2.0, 3.0,
  !     #  0.0, 1.0, 2.0, 3.0]
  !     #
  !     #Y
  !     #[ 5.0, 5.0, 5.0, 5.0,
  !     #  6.0, 6.0, 6.0, 6.0,
  !     #  7.0, 7.0, 7.0, 7.0,
  !     #  8.0, 8.0, 8.0, 8.0]
  !..............................................................................
  ! Rev 0.2, Feb 2018
  ! New feature added: xgv and ygv as full grid vector are accepted now

  ! Arguments
  REAL(wp), INTENT(out), ALLOCATABLE :: x(:, :)
  REAL(wp), INTENT(out), ALLOCATABLE :: y(:, :)
  REAL(wp), INTENT(in) :: xgv(:) ! x grid vector [start, stop, step] or [start, stop]
  REAL(wp), INTENT(in), OPTIONAL :: ygv(:) ! y grid vector [start, stop, step] or [start, stop]
  INTEGER, INTENT(out), OPTIONAL :: ierr ! the error value

  ! Local variables
  INTEGER :: sv
  INTEGER :: nx
  INTEGER :: ny
  LOGICAL :: only_xgv_available

  ! Initial setting
  only_xgv_available = .FALSE.
  sv = 0 !Assume no error

  nx = SIZE(xgv, dim=1)

  IF (PRESENT(ygv)) THEN
    ny = SIZE(ygv, dim=1)
  ELSE
    only_xgv_available = .TRUE.
    ny = nx
  END IF

  ALLOCATE (x(ny, nx), y(ny, nx), stat=sv)
  IF (sv /= 0) THEN
    PRINT *, "allocataion erro in meshgrid"
    STOP
  END IF

  x(1, :) = xgv
  x(2:ny, :) = SPREAD(xgv, dim=1, ncopies=ny - 1)

  IF (only_xgv_available) THEN
    y = TRANSPOSE(x)
  ELSE
    y(:, 1) = ygv
    y(:, 2:nx) = SPREAD(ygv, dim=2, ncopies=nx - 1)
  END IF

  IF (PRESENT(ierr)) THEN
    ierr = sv
  END IF

END SUBROUTINE meshgrid

!End of ogpf
END MODULE OGPF

