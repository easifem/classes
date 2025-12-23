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
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetAxesSetting
CHARACTER(*), PARAMETER :: myName = 'GetAxesSetting()'
CHARACTER(*), PARAMETER :: prefix = ' axes '
LOGICAL(LGT) :: isok
CHARACTER(:), ALLOCATABLE :: astr

astr = LowerCase(TRIM(ADJUSTL(axes_set)))

isok = LEN(astr) .EQ. 0
IF (isok) THEN
  axesSetting = ''
  RETURN
END IF

SELECT CASE (astr)
CASE ('x1y1')
  axesSetting = prefix//'x1y1'
CASE ('x1y2')
  axesSetting = prefix//'x1y2'
CASE ('x2y1')
  axesSetting = prefix//'x2y1'
CASE ('x2y2')
  axesSetting = prefix//'x2y2'
CASE default
  ! wrong strings
  CALL e%raiseWarning(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: wrong axes set is sent. &
    &    axes set can be on of: x1y1, x1y2, x2y1, x2y2')
  axesSetting = ''
  RETURN
END SELECT

END PROCEDURE GetAxesSetting

!----------------------------------------------------------------------------
!                                                         GetPlotCommand
!----------------------------------------------------------------------------

MODULE PROCEDURE GetPlotCommand
CHARACTER(10) :: axes_setting
LOGICAL(LGT) :: acase

!check the axes set
axes_setting = ""
IF (PRESENT(axes_set)) &
  CALL GetAxesSetting(axes_set, axes_setting)

acase = PRESENT(lspec)

SELECT CASE (order)
CASE (1)

  IF (acase) THEN

    plotCommand = 'plot "-" '//TRIM(lspec)//axes_setting

    RETURN
  END IF

  plotCommand = 'plot "-" '//axes_setting

CASE DEFAULT

  IF (acase) THEN

    plotCommand = ', "-" '//TRIM(lspec)//axes_setting

    RETURN
  END IF

  plotCommand = ', "-" '//axes_setting

END SELECT

END PROCEDURE GetPlotCommand

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WritePlotSetup

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
CALL Help_WriteLabelSetup(obj%title, isTitle=.TRUE.)

!----------------------------------
!                            labels
!----------------------------------

CALL obj%pltfile%WRITE("# labels")
CALL Help_WriteLabelSetup(obj%xaxis%label, &
                          direction="x", isTitle=.FALSE.)
CALL Help_WriteLabelSetup(obj%yaxis%label, &
                          direction="y", isTitle=.FALSE.)
CALL Help_WriteLabelSetup(obj%zaxis%label, &
                          direction="z", isTitle=.FALSE.)
CALL Help_WriteLabelSetup(obj%x2axis%label, &
                          direction="x2", isTitle=.FALSE.)
CALL Help_WriteLabelSetup(obj%y2axis%label, &
                          direction="y2", isTitle=.FALSE.)

!----------------------------------
!                  write Tick Setup
!----------------------------------
CALL obj%pltfile%WRITE("# ticks")
!! primary axes
CALL Help_WriteTickSetup(tick=obj%xaxis%tick, direction="x")
CALL Help_WriteTickSetup(tick=obj%yaxis%tick, direction="y")
CALL Help_WriteTickSetup(tick=obj%zaxis%tick, direction="z")

!! secondary axes
CALL Help_WriteTickSetup(tick=obj%x2axis%tick, direction="x2")
CALL Help_WriteTickSetup(tick=obj%y2axis%tick, direction="y2")

CONTAINS

SUBROUTINE Help_WriteDataStyle()

  IF (obj%datastyle%LEN() .GT. 0) THEN
    CALL obj%pltfile%WRITE("# data style")
    CALL obj%pltfile%WRITE("set style data "//obj%datastyle%chars())
    CALL obj%pltfile%WriteBlank()
  END IF

END SUBROUTINE Help_WriteDataStyle

SUBROUTINE Help_WriteOptions()
  INTEGER(I4B) :: ii

  IF (.NOT. ALLOCATED(obj%options)) RETURN

  CALL obj%pltfile%WRITE("# options")
  DO ii = 1, SIZE(obj%options)
    CALL obj%pltfile%WRITE(obj%options(ii)%chars())
  END DO
  CALL obj%pltfile%WriteBlank()

END SUBROUTINE Help_WriteOptions

SUBROUTINE Help_WriteLabelSetup(label, direction, isTitle)
  TYPE(Label_), INTENT(IN) :: label
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
  TYPE(Tick_), INTENT(IN) :: tick
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

DO ii = 1, SIZE(obj%scripts)
  CALL obj%pltfile%WRITE(obj%scripts(ii)%chars())
END DO

CALL obj%DEALLOCATE()

END PROCEDURE obj_RunScript

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE UtilityMethods
