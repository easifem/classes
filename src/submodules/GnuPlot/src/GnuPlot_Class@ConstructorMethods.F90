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

CALL obj%pltfile%Initiate(filename=obj%filename//".plt", &
                          status="REPLACE")
CALL obj%pltfile%OPEN()

CALL Help_WriteSignature(obj%pltfile)

CALL Help_WriteTerm(obj)

IF (obj%useDefaultPreset) CALL Help_WriteDefaultPreset(obj%pltfile)

IF (obj%hasmultiplot) CALL Help_WriteMultiPlotConfig( &
  obj%pltfile, obj%multiplot_rows, obj%multiplot_cols)

IF (obj%runAfterWrite .AND. LEN(obj%commandline%chars()) .EQ. 0) &
  obj%commandline = defaultOpt%commandLine

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

! TODO: fix mismatch of subroutine name
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

IF (obj%pauseAfterDraw) THEN
  CALL obj%pltfile%WriteBlank()
  CALL obj%pltfile%WRITE("pause mouse keypress")
  CALL obj%pltfile%WRITE("if (exists('MOUSE_CHAR') && MOUSE_CHAR eq 'c' || MOUSE_KEY == -1) {")
  CALL obj%pltfile%WRITE("    print 'Exiting...'")
  CALL obj%pltfile%WRITE("    quit")
  CALL obj%pltfile%WRITE("   } else {")
  call obj%pltfile%WRITE("    print 'Press c or q to quit, any other key to refresh'")
  CALL obj%pltfile%WRITE("   replot")
  CALL obj%pltfile%WRITE("   load '"//obj%filename//".plt'")
  CALL obj%pltfile%WRITE("  }")
  obj%commandline = "gnuplot "
END IF

IF (obj%pltfile%IsOpen()) THEN
  CALL obj%pltfile%DEALLOCATE()
  obj%hasanimation = .FALSE.
END IF

IF (obj%runAfterWrite) &
  CALL execute_command_line(obj%commandline//" "//obj%filename//".plt")

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Finalize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Finalize
! WARN: This causes a repeatation of gnuplot execution
! CALL obj%DEALLOCATE()
END PROCEDURE obj_Finalize

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
CALL Display("#GNUPLOT CLASS", msg, unitno)
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_WriteSignature(obj)
  TYPE(TxtFile_), INTENT(INOUT) :: obj

  CALL obj%WRITE("# "//modName//" in EASIFEM")
  CALL obj%WriteBlank()

END SUBROUTINE Help_WriteSignature

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_WriteTerm(obj)
  TYPE(GnuPlot_), INTENT(INOUT) :: obj

  IF (obj%useDefaultTerm) THEN
    CALL obj%pltfile%WRITE("set term "//defaultOpt%termType//" size " &
                           //tostring(defaultOpt%termSize(1))//"," &
                           //tostring(defaultOpt%termSize(2)) &
                           //" enhanced font "// &
                           '"'//defaultOpt%termFont//','// &
                           tostring(defaultOpt%termFontSize)//'"')
  ELSE
    CALL obj%pltfile%WRITE("set term "//obj%termType//" size " &
                           //tostring(obj%termSize(1))//"," &
                           //tostring(obj%termSize(2)) &
                           //" enhanced font "// &
                           '"'//obj%termFont//','// &
                           tostring(obj%termFontSize)//'"')
  END IF

  CALL obj%pltfile%WriteBlank()

END SUBROUTINE Help_WriteTerm

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_WriteDefaultPreset(obj)
  TYPE(TxtFile_), INTENT(INOUT) :: obj

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

END SUBROUTINE Help_WriteDefaultPreset

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_WriteMultiPlotConfig(obj, nrow, ncol)
  TYPE(TxtFile_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: nrow, ncol

  CALL obj%WRITE("set multiplot layout "// &
                 tostring(nrow)//", "//tostring(ncol))

END SUBROUTINE Help_WriteMultiPlotConfig

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
