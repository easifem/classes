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

SUBMODULE(Gnuplot_Class) UtilityMethods
USE InputUtility, ONLY: Input
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_GetAxesSetting(axes, axesStatement)
  CHARACTER(*), PARAMETER :: myName = "Help_GetAxesSetting"
  CHARACTER(*), INTENT(IN) :: axes
  TYPE(string), INTENT(OUT) :: axesStatement
  CHARACTER(*), PARAMETER :: prefix = ' axes '
  LOGICAL(LGT) :: isok

  axesStatement = ""

  isok = LEN(axes) .GT. 0
  IF (.NOT. isok) RETURN

  SELECT CASE (LowerCase(TRIM(ADJUSTL(axes))))
  CASE ('x1y1')
    axesStatement = prefix//'x1y1'
  CASE ('x1y2')
    axesStatement = prefix//'x1y2'
  CASE ('x2y1')
    axesStatement = prefix//'x2y1'
  CASE ('x2y2')
    axesStatement = prefix//'x2y2'
  CASE default
    CALL e%raiseWarning(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: wrong axes set is sent. &
      &    axes set can be on of: x1y1, x1y2, x2y1, x2y2')
  END SELECT

END SUBROUTINE Help_GetAxesSetting

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_GetUsingStatement(obj, usingStatement)
  TYPE(GnuPlotPlotOpts_), INTENT(INOUT) :: obj
  TYPE(String), INTENT(INOUT) :: usingStatement

  IF (.NOT. obj%scaleData) RETURN

  usingStatement = " using "
  usingStatement = usingStatement//"($1*"// &
                   obj%dataScale(1)//"):"
  usingStatement = usingStatement//"($2*"// &
                   obj%dataScale(2)//") "

END SUBROUTINE Help_GetUsingStatement

!----------------------------------------------------------------------------
!                                                         obj_SetPlotCommand
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetPlotCommand
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetPlotCommand()"
#endif
TYPE(String) :: dataBlock0, lspec0, &
                usingStatement, axesStatement

axesStatement = ""
IF (PRESENT(axes)) CALL Help_GetAxesSetting(axes, axesStatement)

usingStatement = " "
CALL Help_GetUsingStatement(obj%opts%plotOpts, usingStatement)

dataBlock0 = Input(default='"-"', option=dataBlockName)

lspec0 = Input(default="", option=lspec)

SELECT CASE (order)
CASE (1)

  obj%plotCommand(1) = 'plot '//dataBlock0//usingStatement// &
                       lspec0//axesStatement

CASE DEFAULT

  obj%plotCommand(order) = ', '//dataBlock0//usingStatement// &
                           lspec0//axesStatement

END SELECT

END PROCEDURE obj_SetPlotCommand

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WritePlotSetup
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WritePlotSetup()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

!----------------------------------
!                      data style
!----------------------------------

CALL Help_WriteDataStyle()

!----------------------------------
!                         options
!----------------------------------

CALL Help_WriteOptions()

!----------------------------------
!                            title
!----------------------------------

CALL obj%pltfile%WRITE("# title")
CALL Help_WriteLabelSetup(obj%opts%title, isTitle=.TRUE.)

!----------------------------------
!                            labels
!----------------------------------

CALL obj%pltfile%WRITE("# labels")
CALL Help_WriteLabelSetup(obj%opts%xaxis%label, &
                          direction="x", isTitle=.FALSE.)
CALL Help_WriteLabelSetup(obj%opts%yaxis%label, &
                          direction="y", isTitle=.FALSE.)
CALL Help_WriteLabelSetup(obj%opts%zaxis%label, &
                          direction="z", isTitle=.FALSE.)
CALL Help_WriteLabelSetup(obj%opts%x2axis%label, &
                          direction="x2", isTitle=.FALSE.)
CALL Help_WriteLabelSetup(obj%opts%y2axis%label, &
                          direction="y2", isTitle=.FALSE.)
CALL Help_WriteLabelSetup(obj%opts%cbAxis%label, &
                          direction="cb", isTitle=.FALSE.)

!----------------------------------
!                  write Tick Setup
!----------------------------------
CALL obj%pltfile%WRITE("# ticks")
!! primary axes
CALL Help_WriteTickSetup(tick=obj%opts%xaxis%tick, direction="x")
CALL Help_WriteTickSetup(tick=obj%opts%yaxis%tick, direction="y")
CALL Help_WriteTickSetup(tick=obj%opts%zaxis%tick, direction="z")

!! secondary axes
CALL Help_WriteTickSetup(tick=obj%opts%x2axis%tick, direction="x2")
CALL Help_WriteTickSetup(tick=obj%opts%y2axis%tick, direction="y2")

!! colorbar axis
CALL Help_WriteTickSetup(tick=obj%opts%cbAxis%tick, direction="cb")

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif

CONTAINS

SUBROUTINE Help_WriteDataStyle()

  IF (obj%opts%plotOpts%datastyle%LEN() .GT. 0) THEN
    CALL obj%pltfile%WRITE("# data style")
    CALL obj%pltfile%WRITE("set style data "//obj%opts%plotOpts%datastyle)
    CALL obj%pltfile%WriteBlank()
  END IF

END SUBROUTINE Help_WriteDataStyle

SUBROUTINE Help_WriteOptions()
  INTEGER(I4B) :: ii

  IF (.NOT. ALLOCATED(obj%opts%options)) RETURN

  CALL obj%pltfile%WRITE("# options")
  DO ii = 1, SIZE(obj%opts%options)
    CALL obj%pltfile%WRITE(obj%opts%options(ii)%chars())
  END DO
  CALL obj%pltfile%WriteBlank()

END SUBROUTINE Help_WriteOptions

SUBROUTINE Help_WriteLabelSetup(label, direction, isTitle)
  TYPE(GnuPlotLabel_), INTENT(IN) :: label
  CHARACTER(*), OPTIONAL, INTENT(IN) :: direction
  LOGICAL(LGT), INTENT(IN) :: isTitle
  CHARACTER(:), ALLOCATABLE :: lblstring

  IF (.NOT. label%isConfigured) RETURN

  lblstring = ''
  IF (isTitle) THEN
    lblstring = 'set title "'//TRIM(label%text)//'"'
  ELSE
    lblstring = "set "//TRIM(direction)//'label "'// &
                TRIM(label%text)//'"'
  END IF

  !! color
  IF (ALLOCATED(label%color)) &
    lblstring = lblstring//' tc "'//TRIM(label%color)//'"'

  !! fontname and size
  IF (ALLOCATED(label%fontname)) THEN
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

  !! rotation
  IF (label%rotate /= NOT_INITIALIZED) &
    lblstring = lblstring//' rotate by '//tostring(label%rotate)

  IF (obj%pltfile%IsOpen()) CALL obj%pltfile%WRITE(lblstring)
  CALL obj%pltfile%WriteBlank()

END SUBROUTINE Help_WriteLabelSetup

SUBROUTINE Help_WriteTickSetup(tick, direction)
  TYPE(GnuPlotTick_), INTENT(IN) :: tick
  CHARACTER(*), INTENT(IN) :: direction

  IF (.NOT. tick%isConfigured) RETURN

  SELECT CASE (tick%plotscale)
  CASE (1) ! autoscale
    CALL obj%pltfile%WRITE("set autoscale "//TRIM(direction))
    RETURN
  CASE (2) ! log scale
    CALL obj%pltfile%WRITE("set logscale "//TRIM(direction) &
                           //" "//tostring(tick%logbase))
  CASE DEFAULT ! linear scale
  END SELECT

  CALL obj%pltfile%WRITE("set "//TRIM(direction)//"range "// &
                         "["//tostring(tick%lims(1))//":" &
                         //tostring(tick%lims(2))//"]")
  CALL obj%pltfile%WriteBlank()

END SUBROUTINE Help_WriteTickSetup

END PROCEDURE obj_WritePlotSetup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteDataBlock_xy
INTEGER(I4B) :: ii, ndata

ndata = SIZE(x)
DO ii = 1, ndata
  CALL obj%pltfile%WRITE([x(ii), y(ii)], orient="ROW")
END DO
CALL obj%pltfile%WRITE("e")

END PROCEDURE obj_WriteDataBlock_xy

!----------------------------------------------------------------------------
!                                                            color_palettes
!----------------------------------------------------------------------------

MODULE PROCEDURE GetColorPaletteScript
CHARACTER(11) :: strblank
INTEGER(I4B) :: jj
INTEGER(I4B) :: maxcolors

CHARACTER(:), ALLOCATABLE :: pltname
CHARACTER(7) :: palette(10) ! palettes with maximum 9 colors

maxcolors = 8 ! default number of discrete colors
palette = ''

#include "./include/colorPalettes.F90"

! generate the gnuplot palette as a single multiline string
paletteScript = '# Define the '//pltname//' pallete'//NEW_LINE(' ')
paletteScript = paletteScript//'set palette defined ( \'//NEW_LINE(' ')
strblank = '           ' ! pad certain number of paces

DO jj = 1, maxcolors - 1
  paletteScript = paletteScript//strblank//tostring(jj - 1)// &
                  ' "'//palette(jj)//'",\'//NEW_LINE(' ')
END DO

jj = maxcolors - 1
paletteScript = paletteScript//strblank//tostring(jj)// &
                ' "'//palette(jj)//'" )'//NEW_LINE(' ')

END PROCEDURE GetColorPaletteScript

!----------------------------------------------------------------------------
!                                                                 runscript
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RunScript
INTEGER(I4B) :: ii

CALL obj%Initiate()

CALL obj%WritePlotSetup()

DO ii = 1, SIZE(obj%opts%scripts)
  CALL obj%pltfile%WRITE(obj%opts%scripts(ii)%chars())
END DO

CALL obj%DEALLOCATE()

END PROCEDURE obj_RunScript

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE UtilityMethods
