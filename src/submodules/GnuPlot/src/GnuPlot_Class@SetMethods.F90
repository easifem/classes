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

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                           set_filename
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFilename
CALL obj%opts%SetFilename(name)
END PROCEDURE obj_SetFilename

!----------------------------------------------------------------------------
!                                                           set_output
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOutput
CALL obj%opts%SetOutput(name)
END PROCEDURE obj_SetOutput

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCommandline
CALL obj%opts%SetCommandLine(chars)
END PROCEDURE obj_SetCommandline

!----------------------------------------------------------------------------
!                                                                set_options
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOptions
CALL obj%opts%SetOptions(optionStr, reset)
END PROCEDURE obj_SetOptions

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetScripts
CALL obj%opts%SetScripts(scriptStr, reset)
END PROCEDURE obj_SetScripts

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTerm
CALL obj%opts%SetTerm(termType, termSize, termFont, termFontSize)
END PROCEDURE obj_SetTerm

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetXLim
CALL obj%opts%SetXLim(lims)
END PROCEDURE obj_SetXLim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetX2Lim
CALL obj%opts%SetX2Lim(lims)
END PROCEDURE obj_SetX2Lim

!----------------------------------------------------------------------------
!                                                                   set_ylim
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetYLim
CALL obj%opts%SetYLim(lims)
END PROCEDURE obj_SetYLim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetY2Lim
CALL obj%opts%SetY2Lim(lims)
END PROCEDURE obj_SetY2Lim

!----------------------------------------------------------------------------
!                                                                   set_zlim
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetZLim
CALL obj%opts%SetZLim(lims)
END PROCEDURE obj_SetZLim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCBLim
CALL obj%opts%SetCBLim(lims)
END PROCEDURE obj_SetCBLim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAxisLim
CALL obj%opts%SetAxisLim(lims, direction)
END PROCEDURE obj_SetAxisLim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetXScale
CALL obj%opts%SetXScale(scaleChar, logBase)
END PROCEDURE obj_SetXScale

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetX2Scale
CALL obj%opts%SetX2Scale(scaleChar, logBase)
END PROCEDURE obj_SetX2Scale

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetYScale
CALL obj%opts%SetYScale(scaleChar, logBase)
END PROCEDURE obj_SetYScale

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetY2Scale
CALL obj%opts%SetY2Scale(scaleChar, logBase)
END PROCEDURE obj_SetY2Scale

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetZScale
CALL obj%opts%SetZScale(scaleChar, logBase)
END PROCEDURE obj_SetZScale

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCBScale
CALL obj%opts%SetCBScale(scaleChar, logBase)
END PROCEDURE obj_SetCBScale

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetPlotScale
CALL obj%opts%SetPlotScale(scaleChar, direction, logBase)
END PROCEDURE obj_SetPlotScale

!----------------------------------------------------------------------------
!                                                             set_plottitle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTitle
CALL obj%opts%SetTitle(title, color, fontSize, fontName, rotate)
END PROCEDURE obj_SetTitle

!----------------------------------------------------------------------------
!                                                                 set_xlabel
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetXLabel
CALL obj%opts%SetXLabel(label, color, fontSize, fontName, rotate)
END PROCEDURE obj_SetXLabel

!----------------------------------------------------------------------------
!                                                                set_x2label
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetX2Label
CALL obj%opts%SetX2Label(label, color, fontSize, fontName, rotate)
END PROCEDURE obj_SetX2Label

!----------------------------------------------------------------------------
!                                                                 set_ylabel
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetYLabel
CALL obj%opts%SetYLabel(label, color, fontSize, fontName, rotate)
END PROCEDURE obj_SetYLabel

!----------------------------------------------------------------------------
!                                                                set_y2label
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetY2Label
CALL obj%opts%SetY2Label(label, color, fontSize, fontName, rotate)
END PROCEDURE obj_SetY2Label

!----------------------------------------------------------------------------
!                                                                 set_zlabel
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetZLabel
CALL obj%opts%SetZLabel(label, color, fontSize, fontName, rotate)
END PROCEDURE obj_SetZLabel

!----------------------------------------------------------------------------
!                                                                 set_zlabel
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCBLabel
CALL obj%opts%SetCBLabel(label, color, fontSize, fontName, rotate)
END PROCEDURE obj_SetCBLabel

!----------------------------------------------------------------------------
!                                                                 set_label
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAxisLabel
CALL obj%opts%SetAxisLabel(direction, label, color, &
                           fontSize, fontName, rotate)
END PROCEDURE obj_SetAxisLabel

!----------------------------------------------------------------------------
!                                                       reset_to_defaults
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Reset
CALL obj%opts%Reset()
END PROCEDURE obj_Reset

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetUseDefaultPreset
CALL obj%opts%SetUseDefaultPreset(abool)
END PROCEDURE obj_SetUseDefaultPreset

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
