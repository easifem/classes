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

SUBMODULE(PLPlot_Class) SetMethods
USE BaseMethod
USE EasyPlplot
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetXlim
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_SetXlim
  CALL xlim(xl=xmin, xh=xmax)
END PROCEDURE plot_SetXlim

!----------------------------------------------------------------------------
!                                                                 SetYlim
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_SetYlim
  CALL ylim(yl=ymin, yh=ymax)
END PROCEDURE plot_SetYlim

!----------------------------------------------------------------------------
!                                                                 SetXYlim
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_SetXYlim
  CALL xylim(xb=x, yb=y)
END PROCEDURE plot_SetXYlim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_SetXYZlim
  CALL xyzlim(xb=x, yb=y, zb=z, altitude=altitude, &
    & azimuth=azimuth, zoom=zoom)
END PROCEDURE plot_SetXYZlim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_SetXLabel
  CALL xlabel(label=label, color=color)
END PROCEDURE plot_SetXLabel

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_SetYLabel
  CALL ylabel(label=label, color=color)
END PROCEDURE plot_SetYLabel

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_SetXYLabel
  CALL obj%SetXLabel( label=xLabel, color=color )
  CALL obj%SetYLabel( label=yLabel, color=color )
END PROCEDURE plot_SetXYLabel

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_SetXYZLabel
  CALL box(xLabel=xLabel, ylabel=yLabel, zLabel=zLabel, color=color)
END PROCEDURE plot_SetXYZLabel

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_SetTitle
  CALL title(label=label, color=color)
END PROCEDURE plot_SetTitle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_SetLabels
  CALL labels(xLabel=xLabel, yLabel=yLabel, &
    & plotLabel=title, color=color )
END PROCEDURE plot_SetLabels

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_SetXticks
  CALL xticks(d=d, logScale=isLogScale, &
    & primary=isPrimary, secondary=isSecondary, &
    & color=color, lineWidth=lineWidth)
END PROCEDURE plot_SetXticks

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_SetYticks
  CALL yticks(d=d, logScale=isLogScale, &
    & primary=isPrimary, secondary=isSecondary, &
    & color=color, lineWidth=lineWidth)
END PROCEDURE plot_SetYticks

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_SetTicks
  CALL ticks(dx=dx, dy=dy, logx=isLogX, logy=isLogY, &
    & color=color, lineWidth=lineWidth)
END PROCEDURE plot_SetTicks

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_SetLegend
  CALL legend(corner=corner, series=series, lineWidths=lineWidths, &
  & markScales=pointScales, markCounts=pointCounts, ncol=ncol )
END PROCEDURE plot_SetLegend

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Set
  CALL setup( &
    & device=device, &
    & fileName=fileName, &
    & fontScaling=fontScaling, &
    & whiteOnBlack=isWhiteOnBlack, &
    & transparent=isTransparent, &
    & colormap=colormap, &
    & figSize=figSize, &
    & isFileFamily=isFileFamily )
END PROCEDURE plot_Set

END SUBMODULE SetMethods