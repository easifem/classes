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

SUBMODULE(GnuPlot_Class) PlotMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Plot1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plot1
CHARACTER(*), PARAMETER :: myName = "obj_plot1()"

INTEGER :: nx1, ny1, nx2, ny2, nx3, ny3, nx4, ny4, number_of_plots, i
CHARACTER(3) :: plottype
CHARACTER(80) :: pltstring(4) ! Four 80 characters string
LOGICAL(LGT) :: acase, isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

!Initialize variables
plottype = ''
pltstring = ''

!   Check the input
nx1 = SIZE(x1)
acase = PRESENT(y1)

IF (acase) THEN
  ny1 = SIZE(y1)

  isok = nx1 .EQ. ny1

  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                     '[INTERNAL ERROR] :: length of x1 and y1 does not match')
    RETURN
  END IF

  plottype = 'xy1'
  number_of_plots = 1

ELSE !plot only x againest its element indices
  plottype = 'xi'
  number_of_plots = 1
END IF

!Process line spec and axes set for first data set if present
CALL process_linespec(1, pltstring(1), ls1, axes1)

acase = PRESENT(x2) .AND. PRESENT(y2)
IF (acase) THEN
  nx2 = SIZE(x2)
  ny2 = SIZE(y2)

  isok = nx2 .EQ. ny2
  IF (.NOT. isok) RETURN

  plottype = 'xy2'
  number_of_plots = 2

  !Process line spec for 2nd data set if present
  CALL process_linespec(2, pltstring(2), ls2, axes2)
END IF

acase = PRESENT(x3) .AND. PRESENT(y3)

IF (acase) THEN
  nx3 = SIZE(x3)
  ny3 = SIZE(y3)

  isok = nx3 .EQ. ny3
  IF (.NOT. isok) RETURN

  plottype = 'xy3'
  number_of_plots = 3

  !Process line spec for 3rd data set if present
  CALL process_linespec(3, pltstring(3), ls3, axes3)
END IF

acase = PRESENT(x4) .AND. PRESENT(y4)
IF (acase) THEN
  nx4 = SIZE(x4)
  ny4 = SIZE(y4)

  isok = nx4 .EQ. ny4
  IF (.NOT. isok) RETURN

  plottype = 'xy4'
  number_of_plots = 4

  !Process line spec for 4th data set if present
  CALL process_linespec(4, pltstring(4), ls4, axes4)
END IF

CALL create_outputfile(obj)

IF (PRESENT(logScale)) THEN
  obj%plotscale = logScale
END IF
! Write plot title, axis labels and other annotations
CALL processcmd(obj)

obj%plotscale = defaultPlotScale

! Write plot command and line styles and legend if any
isok = number_of_plots .EQ. 1
IF (isok) THEN
  WRITE (obj%file_unit, '(a)') TRIM(pltstring(1))
ELSE
  WRITE (obj%file_unit, '(a)') (TRIM(pltstring(i))//' \', &
                                i=1, number_of_plots - 1)
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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_plot1

!----------------------------------------------------------------------------
!                                                                 plot2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plot2
CHARACTER(*), PARAMETER :: myName = "obj_plot2"

INTEGER :: nx, ny, ns, number_of_curves, ii, jj, ierr
CHARACTER(80), ALLOCATABLE :: pltstring(:), lst(:)

nx = SIZE(xv)
ny = SIZE(ymat, dim=1)
IF (.NOT. nx .EQ. ny) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: The length of xv and ymat does not match')
END IF

CALL create_outputfile(obj)
IF (PRESENT(logScale)) THEN
  obj%plotscale = logScale
END IF
! Write plot title, axis labels and other annotations
CALL processcmd(obj)

obj%plotscale = defaultPlotScale

number_of_curves = SIZE(ymat, dim=2)
ALLOCATE (pltstring(number_of_curves), stat=ierr)
IF (ierr /= 0) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Failed to allocate memory for pltstring')
END IF

pltstring(1:number_of_curves) = ''

IF (PRESENT(lspec)) THEN

  CALL splitstring2array(lspec, lst, ';')
  ns = SIZE(lst, dim=1)

  IF (ns == number_of_curves) THEN
    pltstring = lst
  ELSEIF (ns < number_of_curves) THEN
    DO ii = 1, ns
      pltstring(ii) = lst(ii)
    END DO
  ELSE ! ns > number_of curves
    CALL e%RaiseWarning(modName//'::'//myName//' - '// &
      & '[WARNING]:: wrong number of linespec,'// &
      'semicolon ";" acts as delimiter')
  END IF
END IF

IF (PRESENT(lspec)) THEN

  CALL process_linespec(1, pltstring(1), lst(1))
  ns = SIZE(lst)
  ! gpf will cylce through line specification, if number of specification passed
  ! is less than number of plots
  DO ii = 1, number_of_curves
    jj = MOD(ii - 1, ns) + 1
    CALL process_linespec(ii, pltstring(ii), lst(jj))
  END DO
ELSE !No lspec is available
  pltstring(1) = ' plot "-" notitle,'
  pltstring(2:number_of_curves - 1) = '"-" notitle,'
  pltstring(number_of_curves) = '"-" notitle'
END IF

! Write plot command and line styles and legend if any
DO ii = 1, number_of_curves - 1
  CALL obj%writeScript(script=TRIM(pltstring(ii))//' \')
END DO
CALL obj%writeScript(script=TRIM(pltstring(number_of_curves)))

! Write data into script file
DO jj = 1, number_of_curves
  DO ii = 1, nx
    WRITE (obj%file_unit, *) xv(ii), ymat(ii, jj)
  END DO
  WRITE (obj%file_unit, '(a)') 'e' !end of jth set of data
END DO

IF (.NOT. (obj%hasanimation)) THEN
  CALL finalize_plot(obj)
ELSE
  WRITE (obj%file_unit, '(a, F5.2)') 'pause ', obj%pause_seconds
END IF

!Release memory
IF (ALLOCATED(pltstring)) THEN
  DEALLOCATE (pltstring)
END IF

END PROCEDURE obj_plot2

!----------------------------------------------------------------------------
!                                                                 plot3
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plot3
CHARACTER(*), PARAMETER :: myName = "obj_plot3"
INTEGER(I4B) :: mx, nx, my, ny, ns, number_of_curves, &
                ii, jj, ierr
CHARACTER(80), ALLOCATABLE :: pltstring(:), lst(:)

mx = SIZE(xmat, dim=1)
my = SIZE(ymat, dim=1)
IF (.NOT. mx .EQ. my) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: The row length of xmat and ymat does not match')
END IF

nx = SIZE(xmat, dim=2)
ny = SIZE(ymat, dim=2)
IF (.NOT. nx .EQ. ny) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: The column length of xmat and ymat does not match')
END IF

CALL create_outputfile(obj)
IF (PRESENT(logScale)) THEN
  obj%plotscale = logScale
END IF
! Write plot title, axis labels and other annotations
CALL processcmd(obj)

obj%plotscale = defaultPlotScale

number_of_curves = SIZE(ymat, dim=2)
ALLOCATE (pltstring(number_of_curves), stat=ierr)
IF (ierr /= 0) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Failed to allocate memory for pltstring')
END IF

pltstring(1:number_of_curves) = ''

IF (PRESENT(lspec)) THEN

  CALL splitstring2array(lspec, lst, ';')
  ns = SIZE(lst, dim=1)

  IF (ns == number_of_curves) THEN
    ! there is a linespec for each curve
    pltstring = lst
  ELSEIF (ns < number_of_curves) THEN
    ! not enough linespec
    DO ii = 1, ns
      pltstring(ii) = lst(ii)
    END DO
  ELSE ! ns > number_of curves
    CALL e%RaiseWarning(modName//'::'//myName//' - '// &
      & '[WARNING]:: wrong number of linespec,'// &
      'semicolon ";" acts as delimiter')
  END IF
END IF

IF (PRESENT(lspec)) THEN

  CALL process_linespec(1, pltstring(1), lst(1))
  ns = SIZE(lst)
  ! GnuPlot_ will cylce through line specification, if number of specification passed
  ! is less than number of plots
  DO ii = 1, number_of_curves
    jj = MOD(ii - 1, ns) + 1
    CALL process_linespec(ii, pltstring(ii), lst(jj))
  END DO
ELSE !No lspec is available
  pltstring(1) = ' plot "-" notitle,'
  pltstring(2:number_of_curves - 1) = '"-" notitle,'
  pltstring(number_of_curves) = '"-" notitle'
END IF

DO ii = 1, number_of_curves - 1
  CALL obj%writeScript(script=TRIM(pltstring(ii))//' \')
END DO
CALL obj%writeScript(script=TRIM(pltstring(number_of_curves)))

DO jj = 1, number_of_curves
  DO ii = 1, mx
    WRITE (obj%file_unit, *) xmat(ii, jj), ymat(ii, jj)
  END DO
  WRITE (obj%file_unit, '(a)') 'e' !end of jth set of data
END DO

IF (.NOT. (obj%hasanimation)) THEN
  CALL finalize_plot(obj)
ELSE
  WRITE (obj%file_unit, '(a, F5.2)') 'pause ', obj%pause_seconds
END IF

IF (ALLOCATED(pltstring)) THEN
  DEALLOCATE (pltstring)
END IF

END PROCEDURE obj_plot3

!----------------------------------------------------------------------------
!                                                                    plot4
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plot4
CHARACTER(*), PARAMETER :: myName = "obj_plot4"
INTEGER :: np0, ii, alloc_err
REAL(DFP), ALLOCATABLE :: x(:)
REAL(DFP), ALLOCATABLE :: y(:)

IF (PRESENT(np)) THEN
  np0 = np
ELSE
  np0 = 50
END IF

ALLOCATE (x(1:np0), y(1:np0), stat=alloc_err)

IF (alloc_err /= 0) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Failed to allocate memory for x and y')
END IF
!Create set of xy data
x = Linspace(xrange(1), xrange(2), np0)
y = [(func(x(ii)), ii=1, np0)]

CALL obj%plot(x, y, logScale=logScale)

! cleanup memory
IF (ALLOCATED(x)) DEALLOCATE (x)
IF (ALLOCATED(y)) DEALLOCATE (y)

END PROCEDURE obj_plot4

!----------------------------------------------------------------------------
!                                                              finalize_plot
!----------------------------------------------------------------------------

MODULE PROCEDURE finalize_plot
LOGICAL(LGT) :: isok
INTEGER(I4B) :: a, b

! check for multiplots
IF (obj%hasmultiplot) THEN
  a = obj%multiplot_total_plots
  b = obj%multiplot_rows * obj%multiplot_cols - 1
  isok = a .LT. b

  IF (isok) THEN
    ! increment the number of plots
    obj%multiplot_total_plots = obj%multiplot_total_plots + 1
    RETURN ! do not finalize plot, still there is places in multiplot
  END IF

  ! close multiplot
  WRITE (obj%file_unit, fmt='(a)') 'unset multiplot'
  ! reset multiplot flag
  obj%hasmultiplot = .FALSE.

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

END PROCEDURE finalize_plot

END SUBMODULE PlotMethods
