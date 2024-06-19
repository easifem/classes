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
CHARACTER(*), PARAMETER :: gnuplot_output_filename = 'ogpf_temp_script.plt'

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
  PROCEDURE, PUBLIC, PASS(obj) :: getColorPalettes
  PROCEDURE, PUBLIC, PASS(obj) :: checkTitle
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
  PROCEDURE, PUBLIC, PASS(obj) :: add_script => addscript
  PROCEDURE, PUBLIC, PASS(obj) :: run_script => runscript
  PROCEDURE, PUBLIC, PASS(obj) :: animation_start => sub_animation_start
  PROCEDURE, PUBLIC, PASS(obj) :: animation_show => sub_animation_show

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
!                                                          set_cntrLevels
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_setCntrLevels(obj, opts)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: opts
  END SUBROUTINE obj_setCntrLevels
END INTERFACE

!----------------------------------------------------------------------------
!                                                          obj_setPm3dOpts
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_setCBLim(obj, avec)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: avec(2)
  END SUBROUTINE obj_setCBLim
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setCBTicks
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_setCBTicks(obj, opts)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: opts
  END SUBROUTINE obj_setCBTicks
END INTERFACE

!----------------------------------------------------------------------------
!                                                          obj_setPm3dOpts
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_setPm3dOpts(obj, opts)
    CLASS(GnuPlot_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: opts
  END SUBROUTINE obj_setPm3dOpts
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Plot1
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
!                                                                 plot2
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
!                                                                 plot3
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

CONTAINS
!
!Set a flag to tell ogpf if the customized gnuplot configuration should
!be used
!

SUBROUTINE use_preset_configuration(obj, flag)

  CLASS(GnuPlot_) :: obj
  LOGICAL, INTENT(in) :: flag

  ! default is true
  obj%preset_configuration = flag

END SUBROUTINE use_preset_configuration

SUBROUTINE set_filename(obj, chars)
  !..............................................................................
  !Set a file name for plot command output
  !obj file can be used later by gnuplot as an script file to reproduce the plot
  !..............................................................................

  CLASS(GnuPlot_) :: obj
  CHARACTER(*), INTENT(in) :: chars

  obj%txtfilename = TRIM(chars)
  obj%hasfilename = .TRUE.

END SUBROUTINE set_filename

SUBROUTINE set_options(obj, stropt)
  !..............................................................................
  ! Set the plot options. obj is a very powerfull procedure accepts many types
  ! of gnuplot command and customization
  !..............................................................................

  CLASS(GnuPlot_) :: obj
  CHARACTER(*), INTENT(in) :: stropt

  IF (.NOT. ALLOCATED(obj%txtoptions)) obj%txtoptions = ''
  IF (LEN_TRIM(obj%txtoptions) == 0) THEN
    obj%txtoptions = '' ! initialize chars
  END IF
  IF (LEN_TRIM(stropt) > 0) THEN
    obj%txtoptions = obj%txtoptions//splitstr(stropt)
  END IF

  obj%hasoptions = .TRUE.

END SUBROUTINE set_options

SUBROUTINE set_xlim(obj, rng)
  !..............................................................................
  !Set the x axis limits in form of [xmin, xmax]
  !..............................................................................

  CLASS(GnuPlot_) :: obj
  REAL(DFP), INTENT(in) :: rng(2)
  obj%hasxrange = .TRUE.
  obj%xrange = rng

END SUBROUTINE

SUBROUTINE set_ylim(obj, rng)
  !..............................................................................
  !Set the y axis limits in form of [ymin, ymax]
  !..............................................................................

  CLASS(GnuPlot_) :: obj
  REAL(DFP), INTENT(in) :: rng(2)
  obj%hasyrange = .TRUE.
  obj%yrange = rng

END SUBROUTINE

SUBROUTINE set_zlim(obj, rng)
  !..............................................................................
  !Set the z axis limits in form of [zmin, zmax]
  !..............................................................................

  CLASS(GnuPlot_) :: obj
  REAL(DFP), INTENT(in) :: rng(2)
  obj%haszrange = .TRUE.
  obj%zrange = rng

END SUBROUTINE

SUBROUTINE set_axis(obj, rng)
  !..............................................................................
  !Set the axes limits in form of [xmin, xmax, ymin, ymax, zmin, zmax]
  !..............................................................................

  CLASS(GnuPlot_) :: obj
  REAL(DFP), INTENT(in) :: rng(:)
  INTEGER :: n
  n = SIZE(rng, dim=1)
  SELECT CASE (n)
  CASE (2) !Only the range for x-axis has been sent
    obj%hasxrange = .TRUE.
    obj%xrange = rng(1:2)
  CASE (4)
    obj%hasxrange = .TRUE.
    obj%hasyrange = .TRUE.
    obj%xrange = rng(1:2)
    obj%yrange = rng(3:4)
  CASE (6)
    obj%hasxrange = .TRUE.
    obj%hasyrange = .TRUE.
    obj%haszrange = .TRUE.
    obj%xrange = rng(1:2)
    obj%yrange = rng(3:4)
    obj%zrange = rng(5:6)
  CASE default
    PRINT *, 'GnuPlot_ error: wrong axis range setting!'
    RETURN
  END SELECT

END SUBROUTINE set_axis

SUBROUTINE set_secondary_axis(obj, rng)
  !..............................................................................
  !Set the secondary axes limits in form of [x2min, x2max, y2min, y2max]
  !..............................................................................

  CLASS(GnuPlot_) :: obj
  REAL(DFP), INTENT(in) :: rng(:)
  INTEGER :: n
  n = SIZE(rng, dim=1)
  SELECT CASE (n)
  CASE (2) !Only the range for x2-axis has been sent
    obj%hasx2range = .TRUE.
    obj%x2range = rng(1:2)
  CASE (4)
    obj%hasx2range = .TRUE.
    obj%hasy2range = .TRUE.
    obj%x2range = rng(1:2)
    obj%y2range = rng(3:4)
  CASE default
    PRINT *, 'GnuPlot_ error: wrong axis range setting!'
    RETURN
  END SELECT

END SUBROUTINE set_secondary_axis

SUBROUTINE set_plottitle(obj, chars, textcolor, font_size, font_name, rotate)
  !..............................................................................
  !Set the plot title
  !..............................................................................
  CLASS(GnuPlot_) :: obj
  CHARACTER(*), INTENT(in) :: chars
  CHARACTER(*), INTENT(in), OPTIONAL :: textcolor
  INTEGER, OPTIONAL :: font_size
  CHARACTER(*), INTENT(in), OPTIONAL :: font_name
  INTEGER, OPTIONAL :: rotate

        call obj%set_label('plot_title', chars, textcolor, font_size, font_name, rotate)

END SUBROUTINE set_plottitle

SUBROUTINE set_xlabel(obj, chars, textcolor, font_size, font_name, rotate)
  !..............................................................................
  !Set the xlabel
  !..............................................................................
  CLASS(GnuPlot_) :: obj
  CHARACTER(*), INTENT(in) :: chars
  CHARACTER(*), INTENT(in), OPTIONAL :: textcolor
  INTEGER, OPTIONAL :: font_size
  CHARACTER(*), INTENT(in), OPTIONAL :: font_name
  INTEGER, OPTIONAL :: rotate

  CALL obj%set_label('xlabel', chars, textcolor, font_size, font_name, rotate)

END SUBROUTINE set_xlabel

SUBROUTINE set_x2label(obj, chars, textcolor, font_size, font_name, rotate)
  !..............................................................................
  !Set the x2label
  !..............................................................................
  CLASS(GnuPlot_) :: obj
  CHARACTER(*), INTENT(in) :: chars
  CHARACTER(*), INTENT(in), OPTIONAL :: textcolor
  INTEGER, OPTIONAL :: font_size
  CHARACTER(*), INTENT(in), OPTIONAL :: font_name
  INTEGER, OPTIONAL :: rotate

 CALL obj%set_label('x2label', chars, textcolor, font_size, font_name, rotate)

END SUBROUTINE set_x2label

SUBROUTINE set_ylabel(obj, chars, textcolor, font_size, font_name, rotate)
  !..............................................................................
  !Set the ylabel
  !..............................................................................
  CLASS(GnuPlot_) :: obj
  CHARACTER(*), INTENT(in) :: chars
  CHARACTER(*), INTENT(in), OPTIONAL :: textcolor
  INTEGER, OPTIONAL :: font_size
  CHARACTER(*), INTENT(in), OPTIONAL :: font_name
  INTEGER, OPTIONAL :: rotate

  CALL obj%set_label('ylabel', chars, textcolor, font_size, font_name, rotate)

END SUBROUTINE set_ylabel

SUBROUTINE set_y2label(obj, chars, textcolor, font_size, font_name, rotate)
  !..............................................................................
  !Set the y2label
  !..............................................................................
  CLASS(GnuPlot_) :: obj
  CHARACTER(*), INTENT(in) :: chars
  CHARACTER(*), INTENT(in), OPTIONAL :: textcolor
  INTEGER, OPTIONAL :: font_size
  CHARACTER(*), INTENT(in), OPTIONAL :: font_name
  INTEGER, OPTIONAL :: rotate

 CALL obj%set_label('y2label', chars, textcolor, font_size, font_name, rotate)

END SUBROUTINE set_y2label

SUBROUTINE set_zlabel(obj, chars, textcolor, font_size, font_name, rotate)
  !..............................................................................
  !Set the zlabel
  !..............................................................................
  CLASS(GnuPlot_) :: obj
  CHARACTER(*), INTENT(in) :: chars
  CHARACTER(*), INTENT(in), OPTIONAL :: textcolor
  INTEGER, OPTIONAL :: font_size
  CHARACTER(*), INTENT(in), OPTIONAL :: font_name
  INTEGER, OPTIONAL :: rotate

  CALL obj%set_label('zlabel', chars, textcolor, font_size, font_name, rotate)

END SUBROUTINE set_zlabel

!..............................................................................
! Set the text, color, font, size and rotation for labels including
! title, xlabel, x2label, ylabel, ....
!..............................................................................
SUBROUTINE set_label(obj, lblname, lbltext, lblcolor, font_size, &
                     font_name, rotate)

  CLASS(GnuPlot_) :: obj
  CHARACTER(*), INTENT(in) :: lblname
  CHARACTER(*), INTENT(in) :: lbltext
  CHARACTER(*), INTENT(in), OPTIONAL :: lblcolor
  CHARACTER(*), INTENT(in), OPTIONAL :: font_name
  INTEGER, OPTIONAL :: font_size
  INTEGER, OPTIONAL :: rotate

  ! local variable
  TYPE(Label_) :: label

  label%hasLabel = .TRUE.
  label%text = TRIM(lbltext)

  IF (PRESENT(lblcolor)) THEN
    label%color = lblcolor
  END IF

  IF (PRESENT(font_name)) THEN
    label%fontname = font_name
  ELSE
    IF (.NOT. ALLOCATED(label%fontname)) THEN
      label%fontname = ''
    END IF
  END IF

  IF (PRESENT(font_size)) THEN
    label%fontsize = font_size
  END IF

  IF (PRESENT(rotate)) THEN
    label%rotate = rotate
  END IF

  SELECT CASE (lblname)
  CASE ('xlabel')
    obj%tpxlabel = label
  CASE ('x2label')
    obj%tpx2label = label
  CASE ('ylabel')
    obj%tpylabel = label
  CASE ('y2label')
    obj%tpy2label = label
  CASE ('zlabel')
    obj%tpzlabel = label
  CASE ('plot_title')
    obj%tpplottitle = label
  END SELECT

END SUBROUTINE set_label

!..............................................................................
!Reset all oGnuPlot_ properties (params to their default values
!...............................................................................
SUBROUTINE reset_to_defaults(obj)
  CLASS(GnuPlot_) :: obj

  obj%preset_configuration = .TRUE.
  obj%txtfilename = gnuplot_output_filename

  IF (ALLOCATED(obj%txtoptions)) DEALLOCATE (obj%txtoptions)
  IF (ALLOCATED(obj%txtscript)) DEALLOCATE (obj%txtscript)
  IF (ALLOCATED(obj%txtdatastyle)) DEALLOCATE (obj%txtdatastyle)
  IF (ALLOCATED(obj%msg)) DEALLOCATE (obj%msg)

  obj%hasoptions = .FALSE.

  obj%hasxrange = .FALSE.
  obj%hasx2range = .FALSE.
  obj%hasyrange = .FALSE.
  obj%hasy2range = .FALSE.
  obj%haszrange = .FALSE.

  obj%pause_seconds = 0.0
  obj%status = 0
  obj%hasanimation = .FALSE.
  obj%hasfileopen = .FALSE.
  obj%hasmultiplot = .FALSE.

  obj%plotscale = ''
  obj%tpplottitle%hasLabel = .FALSE.
  obj%tpxlabel%hasLabel = .FALSE.
  obj%tpx2label%hasLabel = .FALSE.
  obj%tpylabel%hasLabel = .FALSE.
  obj%tpy2label%hasLabel = .FALSE.
  obj%tpzlabel%hasLabel = .FALSE.

END SUBROUTINE reset_to_defaults

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

    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !!> Section Four: Gnuplot direct scriptting
    !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE addscript(obj, strcmd)
  !..............................................................................
  ! addscript: accepts all type of gnuplot command as a string and store it
  ! in global txtscript to be later sent to gnuplot
  !..............................................................................

  CLASS(GnuPlot_) :: obj
  CHARACTER(*), INTENT(in) :: strcmd

  IF (.NOT. ALLOCATED(obj%txtscript)) obj%txtscript = ''
  IF (LEN_TRIM(obj%txtscript) == 0) THEN
    obj%txtscript = '' ! initialize string
  END IF
  IF (LEN_TRIM(strcmd) > 0) THEN
    obj%txtscript = obj%txtscript//splitstr(strcmd)
  END IF

END SUBROUTINE addscript

SUBROUTINE runscript(obj)
  !..............................................................................
  ! runscript sends the the script string (txtstring) into a script
  ! file to be run by gnuplot
  !..............................................................................

  CLASS(GnuPlot_) :: obj

  !REV 0.18: a dedicated subroutine is used to create the output file
  CALL create_outputfile(obj)

  !write the script
  CALL processcmd(obj)
  WRITE (unit=obj%file_unit, fmt='(a)') obj%txtscript

  ! close the file and call gnuplot
  CALL finalize_plot(obj)

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

  CHARACTER(*), INTENT(in) :: axes_set
  CHARACTER(4), INTENT(out) :: axes

  IF (LEN_TRIM(ADJUSTL(axes_set)) == 0) THEN
    axes = ''
    RETURN
  END IF

  SELECT CASE (LowerCase(TRIM(ADJUSTL(axes_set))))
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
  CHARACTER(*), INTENT(out) :: lsstring
  CHARACTER(*), INTENT(in), OPTIONAL :: lspec
  CHARACTER(*), INTENT(in), OPTIONAL :: axes_set

  !local variables
  CHARACTER(4) :: axes
  CHARACTER(10) :: axes_setting

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

SUBROUTINE processcmd(obj)
  !..............................................................................
  !   obj subroutine writes all the data into plot file
  !   to be read by gnuplot
  !..............................................................................

  CLASS(GnuPlot_) :: obj

  ! write the plot style for data
  ! obj is used only when 3D plots (splot, cplot) is used
  IF (ALLOCATED(obj%txtdatastyle)) THEN
    WRITE (obj%file_unit, '("set style data ", a)') obj%txtdatastyle
    WRITE (obj%file_unit, '(a)')
  END IF

  ! Write options
  IF (obj%hasoptions) THEN
    WRITE (obj%file_unit, '(" ")')
    WRITE (obj%file_unit, '("# options")')
    WRITE (obj%file_unit, '(a)') obj%txtoptions
    WRITE (obj%file_unit, '(a)')
  END IF

  ! Check with plot scale: i.e linear, logx, logy, or log xy
  WRITE (obj%file_unit, '(" ")')
  WRITE (obj%file_unit, '("# plot scale")')
  SELECT CASE (obj%plotscale)
  CASE ('semilogx')
    WRITE (obj%file_unit, '("set logscale  x")')
  CASE ('semilogy')
    WRITE (obj%file_unit, '("set logscale  y")')
  CASE ('loglog')
    WRITE (obj%file_unit, '("set logscale  xy")')
  CASE default !for no setting
    !pass
  END SELECT

        !!>0.22
  ! write annotation
  WRITE (obj%file_unit, '(" ")')
  WRITE (obj%file_unit, '("# Annotation: title and labels")')
  CALL write_label(obj, 'plot_title')
  CALL write_label(obj, 'xlabel')
  CALL write_label(obj, 'x2label')
  CALL write_label(obj, 'ylabel')
  CALL write_label(obj, 'y2label')
  CALL write_label(obj, 'zlabel')

  ! axes range
  WRITE (obj%file_unit, '(" ")')
  WRITE (obj%file_unit, '("# axes setting")')
  IF (obj%hasxrange) THEN
    WRITE (obj%file_unit, '("set xrange [",G0,":",G0,"]")') obj%xrange
  END IF
  IF (obj%hasyrange) THEN
    WRITE (obj%file_unit, '("set yrange [",G0,":",G0,"]")') obj%yrange
  END IF
  IF (obj%haszrange) THEN
    WRITE (obj%file_unit, '("set zrange [",G0,":",G0,"]")') obj%zrange
  END IF

  ! secondary axes range
  IF (obj%hasx2range) THEN
    WRITE (obj%file_unit, '("set x2range [",G0,":",G0,"]")') obj%x2range
  END IF
  IF (obj%hasy2range) THEN
    WRITE (obj%file_unit, '("set y2range [",G0,":",G0,"]")') obj%y2range
  END IF
  ! finish by new line
  WRITE (obj%file_unit, '(a)') ! emptyline

END SUBROUTINE processcmd

SUBROUTINE write_label(obj, lblname)
  !..............................................................................
  !   obj subroutine writes the labels into plot file
  !   to be read by gnuplot
  !..............................................................................

  ! write_label
  CLASS(GnuPlot_) :: obj
  CHARACTER(*) :: lblname

  ! local var
  CHARACTER(:), ALLOCATABLE :: lblstring
  CHARACTER(:), ALLOCATABLE :: lblset
  TYPE(Label_) :: label

  SELECT CASE (lblname)
  CASE ('xlabel')
    IF (.NOT. (obj%tpxlabel%hasLabel)) THEN
      RETURN ! there is no label
    END IF
    lblset = 'set xlabel "'
    label = obj%tpxlabel
  CASE ('x2label')
    IF (.NOT. (obj%tpx2label%hasLabel)) THEN
      RETURN ! there is no label
    END IF
    lblset = 'set x2label "'
    label = obj%tpx2label
  CASE ('ylabel')
    IF (.NOT. (obj%tpylabel%hasLabel)) THEN
      RETURN ! there is no label
    END IF
    lblset = 'set ylabel "'
    label = obj%tpylabel
  CASE ('y2label')
    IF (.NOT. (obj%tpy2label%hasLabel)) THEN
      RETURN ! there is no label
    END IF
    lblset = 'set y2label "'
    label = obj%tpy2label
  CASE ('zlabel')
    IF (.NOT. (obj%tpzlabel%hasLabel)) THEN
      RETURN ! there is no label
    END IF
    lblset = 'set zlabel "'
    label = obj%tpzlabel
  CASE ('plot_title')
    IF (.NOT. (obj%tpplottitle%hasLabel)) THEN
      RETURN ! there is no label
    END IF
    lblset = 'set title "'
    label = obj%tpplottitle
  END SELECT

  lblstring = ''
  ! if there is a label continue to set it
  lblstring = lblstring//lblset//TRIM(label%text)//'"'
  IF (ALLOCATED(label%color)) THEN
    lblstring = lblstring//' tc "'//TRIM(label%color)//'"'
  END IF
  ! set font and size
  IF (ALLOCATED(obj%tpxlabel%fontname)) THEN
    lblstring = lblstring//' font "'//TRIM(label%fontname)//','
    IF (label%fontsize /= NOT_INITIALIZED) THEN
      lblstring = lblstring//tostring(label%fontsize)//'"'
    ELSE
      lblstring = lblstring//'"'
    END IF
  ELSE ! check if only font size has been given
    IF (label%fontsize /= NOT_INITIALIZED) THEN
      lblstring = lblstring//' font ",'//tostring(label%fontsize)//'"'
    END IF
  END IF
  ! set rotation
  IF (label%rotate /= NOT_INITIALIZED) THEN
    lblstring = lblstring//' rotate by '//tostring(label%rotate)
  END IF

  ! write to ogpf script file
  WRITE (obj%file_unit, '(a)') lblstring

END SUBROUTINE write_label

FUNCTION getColorPalettes(obj, palette_name) RESULT(chars)
  CLASS(GnuPlot_), INTENT(inout) :: obj
  CHARACTER(*), INTENT(in) :: palette_name
  CHARACTER(:), ALLOCATABLE :: chars

  chars = color_palettes(palette_name)
END FUNCTION getColorPalettes

FUNCTION color_palettes(palette_name) RESULT(str)
  !...............................................................................
  ! color_palettes create color palette as a
  ! string to be written into gnuplot script file
  ! the palettes credit goes to: Anna Schnider (https://github.com/aschn) and
  ! Hagen Wierstorf (https://github.com/hagenw)
  !...............................................................................
  CHARACTER(*), INTENT(in) :: palette_name
  CHARACTER(:), ALLOCATABLE :: str

  ! local variables
  CHARACTER(1) :: strnumber
  CHARACTER(11) :: strblank
  INTEGER :: j
  INTEGER :: maxcolors

  ! define the color palettes
  CHARACTER(:), ALLOCATABLE :: pltname
  CHARACTER(7) :: palette(10) ! palettes with maximum 9 colors

  maxcolors = 8 ! default number of discrete colors
  palette = ''
  SELECT CASE (LowerCase(TRIM(ADJUSTL(palette_name))))
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
  REAL(DFP), INTENT(in) :: x(:)
  REAL(DFP), INTENT(in), OPTIONAL :: y(:)

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

SUBROUTINE create_outputfile(obj)
  !..............................................................................
  ! Create an output file, assign a file_unit
  ! for writing the gnuplot commands
  !..............................................................................

  ! Rev 0.18
  CLASS(GnuPlot_), INTENT(inout) :: obj

  IF (obj%hasfileopen) THEN
    ! there is nothing to do, file has been already open!
    RETURN
  END IF

  !> Rev 0.2 animation

  ! animation handling
  IF (obj%hasanimation) THEN
    obj%frame_number = obj%frame_number + 1 ! for future use
  END IF

  ! Open the output file

  IF (.NOT. (obj%hasfilename)) THEN ! check if no file has been set by user
    obj%txtfilename = gnuplot_output_filename
  END IF

        open ( newunit = obj%file_unit, file = obj%txtfilename, status = 'replace', iostat = obj%status )

  IF (obj%status /= 0) THEN
    PRINT *, "md_helperproc, create_outputfile: cannot open file for output"
    STOP
  END IF

  ! Set the gnuplot terminal, write oGnuPlot_ configuration (customized setting)
  ! Can be overwritten by options

  ! write signature
  WRITE (obj%file_unit, '(a)') '# '//md_name
  WRITE (obj%file_unit, '(a)') '# '//md_rev
  WRITE (obj%file_unit, '(a)') '# '//md_lic
  WRITE (obj%file_unit, '(a)') ! emptyline

  ! write the global settings
  WRITE (obj%file_unit, '(a)') '# gnuplot global setting'
  WRITE (unit=obj%file_unit, fmt='(a)') 'set term '//gnuplot_term_type// &
    ' size '//gnuplot_term_size//' enhanced font "'// &
    gnuplot_term_font//'"'// &
    ' title "'//md_name//': '//md_rev//'"' ! library name and version

  ! write the preset configuration for gnuplot (ogpf customized settings)
  IF (obj%preset_configuration) THEN
    CALL obj%preset_gnuplot_config()
  END IF
  ! write multiplot setting
  IF (obj%hasmultiplot) THEN
    WRITE (obj%file_unit, fmt='(a, I2, a, I2)') 'set multiplot layout ', &
      obj%multiplot_rows, ',', obj%multiplot_cols
  END IF
  ! set flag true for file is opened
  obj%hasfileopen = .TRUE.

END SUBROUTINE create_outputfile

SUBROUTINE preset_gnuplot_config(obj)
  !..............................................................................
  ! To write the preset configuration for gnuplot (ogpf customized settings)
  !..............................................................................
  CLASS(GnuPlot_) :: obj

  WRITE (obj%file_unit, fmt='(a)')
  WRITE (obj%file_unit, fmt='(a)') '# ogpf extra configuration'
        write(obj%file_unit, fmt='(a)') '# -------------------------------------------'

  ! color definition
  WRITE (obj%file_unit, fmt='(a)') '# color definitions'
WRITE (obj%file_unit, fmt='(a)') 'set style line 1 lc rgb "#800000" lt 1 lw 2'
WRITE (obj%file_unit, fmt='(a)') 'set style line 2 lc rgb "#ff0000" lt 1 lw 2'
WRITE (obj%file_unit, fmt='(a)') 'set style line 3 lc rgb "#ff4500" lt 1 lw 2'
WRITE (obj%file_unit, fmt='(a)') 'set style line 4 lc rgb "#ffa500" lt 1 lw 2'
WRITE (obj%file_unit, fmt='(a)') 'set style line 5 lc rgb "#006400" lt 1 lw 2'
WRITE (obj%file_unit, fmt='(a)') 'set style line 6 lc rgb "#0000ff" lt 1 lw 2'
WRITE (obj%file_unit, fmt='(a)') 'set style line 7 lc rgb "#9400d3" lt 1 lw 2'
  WRITE (obj%file_unit, fmt='(a)')
  ! axes setting
  WRITE (obj%file_unit, fmt='(a)') '# Axes'
  WRITE (obj%file_unit, fmt='(a)') 'set border linewidth 1.15'
  WRITE (obj%file_unit, fmt='(a)') 'set tics nomirror'
  WRITE (obj%file_unit, fmt='(a)')

  WRITE (obj%file_unit, fmt='(a)') '# grid'
  WRITE (obj%file_unit, fmt='(a)') '# Add light grid to plot'
        write(obj%file_unit, fmt='(a)') 'set style line 102 lc rgb "#d6d7d9" lt 0 lw 1'
  WRITE (obj%file_unit, fmt='(a)') 'set grid back ls 102'
  WRITE (obj%file_unit, fmt='(a)')
  ! set the plot style
  WRITE (obj%file_unit, fmt='(a)') '# plot style'
  WRITE (obj%file_unit, fmt='(a)') 'set style data linespoints'
  WRITE (obj%file_unit, fmt='(a)')

        write(obj%file_unit, fmt='(a)') '# -------------------------------------------'
  WRITE (obj%file_unit, fmt='(a)') ''

END SUBROUTINE preset_gnuplot_config

SUBROUTINE finalize_plot(obj)
  !..............................................................................
  ! To finalize the writing of gnuplot commands/data and close the output file.
  !..............................................................................
  CLASS(GnuPlot_) :: obj

  ! check for multiplots
  IF (obj%hasmultiplot) THEN
            if (obj%multiplot_total_plots < obj%multiplot_rows * obj%multiplot_cols - 1 ) then
      ! increment the number of plots
      obj%multiplot_total_plots = obj%multiplot_total_plots + 1
      RETURN ! do not finalize plot, still there is places in multiplot
    ELSE
      ! close multiplot
      WRITE (obj%file_unit, fmt='(a)') 'unset multiplot'
      ! reset multiplot flag
      obj%hasmultiplot = .FALSE.

    END IF
  END IF

  WRITE (obj%file_unit, fmt='(a)') 'pause mouse close'
  CLOSE (unit=obj%file_unit) ! close the script file
  obj%hasfileopen = .FALSE. ! reset file open flag
  obj%hasanimation = .FALSE.
  ! Use shell command to run gnuplot
  IF (get_os_type() == 1) THEN
    CALL execute_command_line('wgnuplot -persist '//obj%txtfilename) !   Now plot the results
  ELSE
    CALL execute_command_line('gnuplot -persist '//obj%txtfilename) !   Now plot the results
  END IF
CONTAINS
  INTEGER FUNCTION get_os_type() RESULT(r)
            !! Returns one of OS_WINDOWS, others
            !! At first, the environment variable `OS` is checked, which is usually
            !! found on Windows.
            !! Copy from fpm/fpm_environment: https://github.com/fortran-lang/fpm/blob/master/src/fpm_environment.f90
    CHARACTER(32) :: val
    INTEGER :: length, rc

    INTEGER, PARAMETER :: OS_OTHERS = 0
    INTEGER, PARAMETER :: OS_WINDOWS = 1

    r = OS_OTHERS
    ! Check environment variable `OS`.
    CALL GET_ENVIRONMENT_VARIABLE('OS', val, length, rc)

    IF (rc .EQ. 0 .AND. length > 0 .AND. INDEX(val, 'Windows_NT') > 0) THEN
      r = OS_WINDOWS
      RETURN
    END IF

  END FUNCTION

END SUBROUTINE finalize_plot

! TODO: improve by using StringUtility
FUNCTION checkTitle(obj, chars) RESULT(isOk)
  CLASS(GnuPlot_), INTENT(in) :: obj
  CHARACTER(*), INTENT(in) :: chars
  LOGICAL :: isOk

  isOk = hasTitle(chars)
END FUNCTION checkTitle

FUNCTION hasTitle(chars)
  !..............................................................................
  ! check to see if the plot title (used as legend = key)
  !..............................................................................

  CHARACTER(*), INTENT(in) :: chars
  LOGICAL :: hastitle
  INTEGER :: idx1
  INTEGER :: idx2

  idx1 = INDEX(LowerCase(chars), 'title')
  !Check if title is passed
  idx2 = INDEX(' '//LowerCase(chars), ' t ')
  !Check if the abbreviated title 't' is passed. Extra space is added
  ! at the beginning of chars to find starting 't'
  IF (idx1 /= 0 .OR. idx2 /= 0) THEN
    hastitle = .TRUE.
  ELSE
    hastitle = .FALSE.
  END IF

END FUNCTION hasTitle

! TODO: replace these utility with String_Class methods
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!> Section Seven: String utility Routines
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PURE FUNCTION splitstr(chars) RESULT(spstr)
  !..............................................................................
  !splitstr, separate a string using ";" delimiters
  !..............................................................................

  CHARACTER(*), INTENT(in) :: chars

  ! local variables
  CHARACTER, PARAMETER :: delimiter = ';'
  CHARACTER(:), ALLOCATABLE :: spstr
  INTEGER :: n
  INTEGER :: m
  INTEGER :: k

  k = LEN_TRIM(chars) !length with removed trailing blanks
  n = SCAN(chars, delimiter)
  IF (n == 0) THEN ! obj is a single statement
    spstr = ADJUSTL(chars)//NEW_LINE(' ')
    RETURN
  END IF

  ! for two or more statements separated by ;
  spstr = ''
  m = 1
  DO WHILE (n /= 0 .AND. m < k)
    IF (n /= 1) THEN
      spstr = spstr//ADJUSTL(chars(m:m + n - 2))//NEW_LINE(' ')
    END IF
    m = n + m
    n = SCAN(chars(m:k), delimiter)
  END DO
  IF (m < k) THEN !write the last statement
    spstr = spstr//ADJUSTL(chars(m:k))//NEW_LINE(' ')
  END IF
END FUNCTION splitstr

SUBROUTINE splitstring2array(chars, strarray, delimiter)
  !..............................................................................
  ! splitstring splits a string to an array of
  ! substrings based on a selected delimiter
  ! note:
  !    a. any facing space/blank in substrings will be removed
  !    b. two adjacent delimiter treats as an empty substring between them
  !    c. facing and trailing delimiter treats as empty substring at the fornt and end
  !..............................................................................

  CHARACTER(*), INTENT(in) :: chars
  CHARACTER(80), ALLOCATABLE, INTENT(out) :: strarray(:)
  CHARACTER(1), OPTIONAL, INTENT(in) :: delimiter

  ! local variables
  INTEGER :: m, n
  INTEGER :: i, idx
  CHARACTER(LEN(chars)) :: strtmp
  CHARACTER(1) :: delimiter_

  ! 0. check the existance of delimiter
  IF (PRESENT(delimiter)) THEN
    delimiter_ = delimiter
  ELSE
    delimiter_ = ';'
  END IF

  ! 1. remove initial blanks if any
  strtmp = TRIM(ADJUSTL(chars))

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

END MODULE GnuPlot_Class
