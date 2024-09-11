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

! Write plot title, axis labels and other annotations
CALL processcmd(obj)

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
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This module has not been implemented yet')
! TODO: Implement obj_plot2
END PROCEDURE obj_plot2

!----------------------------------------------------------------------------
!                                                                 plot3
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_plot3
CHARACTER(*), PARAMETER :: myName = "obj_plot3"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This module has not been implemented yet')
! TODO: Implement obj_plot3
END PROCEDURE obj_plot3

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
