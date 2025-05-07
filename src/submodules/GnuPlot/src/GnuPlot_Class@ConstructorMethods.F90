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

SUBMODULE(GnuPlot_Class) ConstructorMethods
USE BaseMethod, ONLY: Display
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate

IF (obj%pltfile%IsOpen()) RETURN

IF (obj%hasanimation) THEN
  obj%frame_number = obj%frame_number + 1
END IF

obj%plotEngine = PLOT_ENGINE_GNUPLOT
CALL obj%pltfile%Initiate(filename=obj%txtfilename, &
                          status="REPLACE")
CALL obj%pltfile%OPEN()

CALL WriteSignature(obj%pltfile)
CALL WriteDefaultConfig(obj%pltfile, obj%preset_configuration)
IF (obj%hasmultiplot) CALL writeMultiPlotConfig(obj%pltfile, &
                                       obj%multiplot_rows, obj%multiplot_cols)

obj%hasfileopen = .TRUE.

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
LOGICAL(LGT) :: finished

IF (obj%hasanimation) THEN
  CALL obj%pltfile%WRITE("pause "//tostring(obj%pause_seconds))
  RETURN
END IF

IF (obj%hasmultiplot) THEN
  CALL CheckMultiPlot(obj, finished)
  IF (.NOT. finished) RETURN
END IF

obj%plotEngine = PLOT_ENGINE_PLPLOT

IF (obj%pltfile%IsOpen()) THEN
  CALL obj%pltfile%DEALLOCATE()
  obj%hasfileopen = .FALSE.
  obj%hasanimation = .FALSE.
END IF

CALL execute_command_line('gnuplot -persist '//obj%txtfilename)

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Finalize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Finalize
CALL obj%DEALLOCATE()
END PROCEDURE obj_Finalize

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
CALL Display("# PLOT ENGINE : GNUPLOT", msg, unitno)
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE WriteSignature(obj)
  TYPE(TxtFile_) :: obj

  CALL obj%WRITE("# "//modName//" in EASIFEM")
  CALL obj%WriteBlank()

END SUBROUTINE WriteSignature

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE WriteDefaultConfig(obj, preset)
  TYPE(TxtFile_) :: obj
  LOGICAL(LGT) :: preset

  CALL obj%WRITE("# global setting")
  CALL obj%WriteBlank()

  CALL obj%WRITE("set term "//gnuplot_term_type// &
                 " size "//gnuplot_term_size//" enhanced font "// &
                 '"'//gnuplot_term_font//'"')

  IF (preset) THEN
    CALL obj%WriteBlank()
    CALL obj%WRITE('# ogpf extra configuration')
    CALL obj%WRITE(commentLineGnuplot)

    ! color definition
    CALL obj%WRITE('# color definitions')
    CALL obj%WRITE('set style line 1 lc rgb "#800000" lt 1 lw 2')
    CALL obj%WRITE('set style line 2 lc rgb "#ff0000" lt 1 lw 2')
    CALL obj%WRITE('set style line 3 lc rgb "#ff4500" lt 1 lw 2')
    CALL obj%WRITE('set style line 4 lc rgb "#ffa500" lt 1 lw 2')
    CALL obj%WRITE('set style line 5 lc rgb "#006400" lt 1 lw 2')
    CALL obj%WRITE('set style line 6 lc rgb "#0000ff" lt 1 lw 2')
    CALL obj%WRITE('set style line 7 lc rgb "#9400d3" lt 1 lw 2')
    CALL obj%WriteBlank()
    ! axes setting
    CALL obj%WRITE('# Axes')
    CALL obj%WRITE('set border linewidth 1.15')
    CALL obj%WRITE('set tics nomirror')
    CALL obj%WriteBlank()

    CALL obj%WRITE('# grid')
    CALL obj%WRITE('# Add light grid to plot')
    CALL obj%WRITE('set style line 102 lc rgb "#d6d7d9" lt 0 lw 1')
    CALL obj%WRITE('set grid back ls 102')
    CALL obj%WriteBlank()
    ! set the plot style
    CALL obj%WRITE('# plot style')
    CALL obj%WRITE('set style data linespoints')
    CALL obj%WriteBlank()

    CALL obj%WRITE(commentLineGnuplot)
    CALL obj%WriteBlank()
  END IF

END SUBROUTINE WriteDefaultConfig

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE WriteMultiPlotConfig(obj, nrow, ncol)
  TYPE(TxtFile_) :: obj
  INTEGER(I4B), INTENT(IN) :: nrow, ncol

  CALL obj%WRITE("set multiplot layout "// &
                 tostring(nrow)//", "//tostring(ncol))

END SUBROUTINE WriteMultiPlotConfig

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE CheckMultiPlot(obj, finished)
  CLASS(GnuPlot_), INTENT(inout) :: obj
  LOGICAL(LGT), INTENT(out) :: finished
  INTEGER(I4B) :: ntotal, ncurrent

  ncurrent = obj%multiplot_total_plots
  ntotal = obj%multiplot_rows * obj%multiplot_cols - 1
  finished = ncurrent .GT. ntotal

  IF (.NOT. finished) THEN
    obj%multiplot_total_plots = obj%multiplot_total_plots + 1
    RETURN
  END IF

  CALL obj%pltfile%WRITE("unset multiplot")
  obj%hasmultiplot = .FALSE.

END SUBROUTINE CheckMultiPlot

END SUBMODULE ConstructorMethods
