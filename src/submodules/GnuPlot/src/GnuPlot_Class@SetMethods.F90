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

SUBMODULE(GnuPlot_Class) SetMethods
USE String_Class, ONLY: StrJoin
USE StringUtility, ONLY: UpperCase
USE InputUtility, ONLY: Input
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetPm3dOpts
obj%pm3dOpts_stmt = "set pm3d "//TRIM(opts)
END PROCEDURE obj_SetPm3dOpts

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCBTicks
obj%cbTicks_stmt = "set cbtics "//TRIM(opts)
END PROCEDURE obj_SetCBTicks

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCntrLevels
obj%cntrLevels_stmt = "set cntrparam levels "//TRIM(opts)
END PROCEDURE obj_SetCntrLevels

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCBLim
obj%hasCBRange = .TRUE.
obj%CBRange = avec
END PROCEDURE obj_SetCBLim

!----------------------------------------------------------------------------
!                                                           set_filename
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFilename
obj%filename = TRIM(name)
END PROCEDURE obj_SetFilename

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCommandline

IF (LEN(chars) .EQ. 0) THEN
  obj%commandline = ""
  obj%runAfterWrite = .FALSE.
  RETURN
END IF

obj%commandline = TRIM(chars)
obj%runAfterWrite = .TRUE.

END PROCEDURE obj_SetCommandline

!----------------------------------------------------------------------------
!                                                                set_options
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOptions
TYPE(String) :: tmpStr
LOGICAL(LGT) :: reset0

IF (optionStr%LEN() .EQ. 0) RETURN

reset0 = Input(default=.TRUE., option=.TRUE.)

IF (reset) THEN
  CALL optionStr%Split(tokens=obj%options, sep=";")
ELSE
  tmpStr = ""
  IF (ALLOCATED(obj%options)) tmpStr = StrJoin(obj%options, sep=";")
  tmpStr = tmpStr//";"//optionStr
  CALL tmpStr%Split(tokens=obj%options, sep=";")
END IF

END PROCEDURE obj_SetOptions

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetScripts
TYPE(String) :: tmpStr
LOGICAL(LGT) :: reset0

IF (scriptStr%LEN() .EQ. 0) RETURN

reset0 = Input(default=.TRUE., option=.TRUE.)

IF (reset) THEN
  CALL scriptstr%Split(tokens=obj%scripts, sep=";")
ELSE
  tmpStr = ""
  IF (ALLOCATED(obj%scripts)) tmpStr = StrJoin(obj%scripts, sep=";")
  tmpStr = tmpStr//";"//scriptStr
  CALL tmpStr%Split(tokens=obj%scripts, sep=";")
END IF

END PROCEDURE obj_SetScripts

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTerm

obj%useDefaultTerm = .FALSE.

obj%termType = Input(default=defaultOpt%termType, option=termType)
obj%termSize = Input(default=defaultOpt%termSize, option=termSize)
obj%termFont = Input(default=defaultOpt%termFont, option=termFont)
obj%termFontSize = Input(default=defaultOpt%termFontSize, &
                         option=termFontSize)

END PROCEDURE obj_SetTerm

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetXLim
obj%xaxis%tick%isConfigured = .TRUE.
obj%xaxis%tick%lims = lims
END PROCEDURE obj_SetXLim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetX2Lim
obj%x2axis%tick%isConfigured = .TRUE.
obj%x2axis%tick%lims = lims
END PROCEDURE obj_SetX2Lim

!----------------------------------------------------------------------------
!                                                                   set_ylim
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetYLim
obj%yaxis%tick%isConfigured = .TRUE.
obj%yaxis%tick%lims = lims
END PROCEDURE obj_SetYLim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetY2Lim
obj%y2axis%tick%isConfigured = .TRUE.
obj%y2axis%tick%lims = lims
END PROCEDURE obj_SetY2Lim

!----------------------------------------------------------------------------
!                                                                   set_zlim
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetZLim
obj%zaxis%tick%isConfigured = .TRUE.
obj%zaxis%tick%lims = lims
END PROCEDURE obj_SetZLim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAxisLim
CHARACTER(*), PARAMETER :: myName = "obj_SetAxisLim"

SELECT CASE (UpperCase(direction))
CASE ("X")
  CALL obj%SetXLim(lims)
CASE ("X2")
  CALL obj%SetX2Lim(lims)
CASE ("Y")
  CALL obj%SetYLim(lims)
CASE ("Y2")
  CALL obj%SetY2Lim(lims)
CASE ("Z")
  CALL obj%SetZLim(lims)
CASE DEFAULT
  CALL e%RaiseWarning(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: unknown direction for label')
END SELECT

END PROCEDURE obj_SetAxisLim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetXScale
CALL Help_SetPlotScale(obj%xaxis%tick, scaleChar, logBase)
END PROCEDURE obj_SetXScale

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetX2Scale
CALL Help_SetPlotScale(obj%x2axis%tick, scaleChar, logBase)
END PROCEDURE obj_SetX2Scale

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetYScale
CALL Help_SetPlotScale(obj%yaxis%tick, scaleChar, logBase)
END PROCEDURE obj_SetYScale

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetY2Scale
CALL Help_SetPlotScale(obj%y2axis%tick, scaleChar, logBase)
END PROCEDURE obj_SetY2Scale

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetZScale
CALL Help_SetPlotScale(obj%zaxis%tick, scaleChar, logBase)
END PROCEDURE obj_SetZScale

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetPlotScale
CHARACTER(*), PARAMETER :: myName = "obj_SetPlotScale"

SELECT CASE (UpperCase(direction))
CASE ("X")
  CALL obj%SetXScale(scaleChar, logBase)
CASE ("X2")
  CALL obj%SetX2Scale(scaleChar, logBase)
CASE ("Y")
  CALL obj%SetYScale(scaleChar, logBase)
CASE ("Y2")
  CALL obj%SetY2Scale(scaleChar, logBase)
CASE ("Z")
  CALL obj%SetZScale(scaleChar, logBase)
CASE DEFAULT
  CALL e%RaiseWarning(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: unknown direction for plot scale')
END SELECT

END PROCEDURE obj_SetPlotScale

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_SetPlotScale(obj, scaleChar, logBase)
  TYPE(Tick_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: scaleChar
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: logBase

  SELECT CASE (UpperCase(scaleChar))
  CASE ("AUTO", "A")
    obj%isConfigured = .TRUE.
    obj%plotscale = 1_I4B
  CASE ("LOG", "L")
    obj%isConfigured = .TRUE.
    obj%plotscale = 2_I4B
    obj%logBase = Input(default=10_I4B, option=logBase)
  CASE DEFAULT
    obj%plotscale = 0_I4B ! no scale
  END SELECT

END SUBROUTINE Help_SetPlotScale

!----------------------------------------------------------------------------
!                                                             set_plottitle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTitle
CALL Help_SetLabel(obj%title, title, color, fontSize, fontName, rotate)
END PROCEDURE obj_SetTitle

!----------------------------------------------------------------------------
!                                                                 set_xlabel
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetXLabel
CALL Help_SetLabel(obj%xaxis%label, label, color, fontSize, fontName, rotate)
END PROCEDURE obj_SetXLabel

!----------------------------------------------------------------------------
!                                                                set_x2label
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetX2Label
CALL Help_SetLabel(obj%x2axis%label, label, color, fontSize, fontName, rotate)
END PROCEDURE obj_SetX2Label

!----------------------------------------------------------------------------
!                                                                 set_ylabel
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetYLabel
CALL Help_SetLabel(obj%yaxis%label, label, color, fontSize, fontName, rotate)
END PROCEDURE obj_SetYLabel

!----------------------------------------------------------------------------
!                                                                set_y2label
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetY2Label
CALL Help_SetLabel(obj%y2axis%label, label, color, fontSize, fontName, rotate)
END PROCEDURE obj_SetY2Label

!----------------------------------------------------------------------------
!                                                                 set_zlabel
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetZLabel
CALL Help_SetLabel(obj%zaxis%label, label, color, fontSize, fontName, rotate)
END PROCEDURE obj_SetZLabel

!----------------------------------------------------------------------------
!                                                                 set_label
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAxisLabel
CHARACTER(*), PARAMETER :: myName = "obj_SetLabel"

SELECT CASE (UpperCase(direction))
CASE ("X")
  CALL obj%SetXLabel(label, color, fontSize, fontName, rotate)
CASE ("X2")
  CALL obj%SetX2Label(label, color, fontSize, fontName, rotate)
CASE ("Y")
  CALL obj%SetYLabel(label, color, fontSize, fontName, rotate)
CASE ("Y2")
  CALL obj%SetY2Label(label, color, fontSize, fontName, rotate)
CASE ("Z")
  CALL obj%SetZLabel(label, color, fontSize, fontName, rotate)
CASE default
  CALL e%RaiseWarning(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: unknown direction for label')
END SELECT

END PROCEDURE obj_SetAxisLabel

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_SetLabel(obj, label, color, fontSize, fontName, rotate)
  TYPE(Label_) :: obj
  CHARACTER(*), INTENT(IN) :: label
  CHARACTER(*), OPTIONAL, INTENT(IN) :: color
  CHARACTER(*), OPTIONAL, INTENT(IN) :: fontName
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: fontSize
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: rotate

  IF (LEN_TRIM(label) .EQ. 0) RETURN

  obj%isConfigured = .TRUE.
  obj%text = TRIM(label)

  IF (PRESENT(color)) obj%color = color

  IF (PRESENT(fontName)) THEN
    obj%fontname = fontName
  ELSE
    IF (.NOT. ALLOCATED(obj%fontname)) THEN
      obj%fontname = ''
    END IF
  END IF

  IF (PRESENT(fontSize)) obj%fontsize = fontSize

  IF (PRESENT(rotate)) obj%rotate = rotate

END SUBROUTINE Help_SetLabel

!----------------------------------------------------------------------------
!                                                       reset_to_defaults
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Reset

obj%useDefaultTerm = .TRUE.
obj%useDefaultPreset = .TRUE.
obj%filename = defaultOpt%filename

IF (ALLOCATED(obj%options)) DEALLOCATE (obj%options)
IF (ALLOCATED(obj%scripts)) DEALLOCATE (obj%scripts)
obj%dataStyle = ""

obj%pause_seconds = 0.0_DFP
obj%status = 0
obj%hasanimation = .FALSE.
obj%hasmultiplot = .FALSE.

obj%commandline = defaultOpt%commandline
obj%runAfterWrite = .TRUE.
obj%pauseAfterDraw = .FALSE.

END PROCEDURE obj_Reset

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetUseDefaultPreset
obj%useDefaultPreset = abool
END PROCEDURE obj_SetUseDefaultPreset

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
