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

#ifdef USE_PLPLOT
SUBMODULE(PLPlot_Class) LinePlotMethods
USE BaseMethod
USE EasyPlplot
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   LinePlot
!----------------------------------------------------------------------------

MODULE PROCEDURE line_plot_x1y1
#ifdef USE_PLPLOT
REAL(DFP) :: xmin0, xmax0, ymin0, ymax0, lineWidth0
TYPE(String) :: device, xlabel0, ylabel0, title0, pointType0
  !!
device = GetDeviceName(filename)
  !!
CALL obj%Set( &
  & device=device%chars(), &
  & filename=filename, &
  & fontScaling=fontScaling, &
  & isWhiteOnBlack=isWhiteOnBlack, &
  & isTransparent=isTransparent, &
  & colormap=colormap, &
  & figSize=figSize, &
  & isFileFamily=.FALSE.)
  !!
! CALL obj%Figure()
CALL obj%Subplot(ncol=1_I4B, nrow=1_I4B, i=1_I4B)
  !!
xmin0 = MINVAL(x); xmin0 = xmin0 - ABS(xmin0) * 0.1_DFP
xmin0 = INPUT(option=xmin, default=xmin0)
xmax0 = MAXVAL(x); xmax0 = xmax0 + ABS(xmax0) * 0.1_DFP
xmax0 = INPUT(option=xmax, default=xmax0)
  !!
ymin0 = MINVAL(y); ymin0 = ymin0 - ABS(ymin0) * 0.1_DFP
ymin0 = INPUT(option=ymin, default=ymin0)
ymax0 = MAXVAL(y); ymax0 = ymax0 + ABS(ymax0) * 0.1_DFP
ymax0 = INPUT(option=ymax, default=ymax0)
  !!
CALL obj%SetXYlim(x=[xmin0, xmax0], y=[ymin0, ymax0])
  !!
CALL obj%Plot2D( &
  & x=x, &
  & y=y, &
  & lineColor=lineColor, &
  & lineType=lineType, &
  & lineWidth=lineWidth, &
  & pointColor=pointColor, &
  & pointType=pointType, &
  & pointSize=pointSize)
  !!
CALL obj%SetTicks( &
  & dx=dx, &
  & dy=dy, &
  & isLogX=isLogX, &
  & isLogY=isLogY, &
  & color=tickColor, &
  & lineWidth=tickWidth)
  !!
xlabel0 = INPUT(option=xlabel, default="")
ylabel0 = INPUT(option=ylabel, default="")
title0 = INPUT(option=title, default="")
  !!
CALL obj%SetLabels( &
  & xlabel=xlabel0%chars(), &
  & ylabel=ylabel0%chars(), &
  & title=title0%chars(), &
  & color=labelColor)
  !!
CALL PLEND
! CALL obj%Show()
#endif
END PROCEDURE line_plot_x1y1

!----------------------------------------------------------------------------
!                                                                   LinePlot
!----------------------------------------------------------------------------

MODULE PROCEDURE line_plot_x1y2
#ifdef USE_PLPLOT
REAL(DFP) :: xmin0, xmax0, ymin0, ymax0, lineWidth0
REAL(DFP) :: legend_width, legend_height
TYPE(String) :: extn, driver, xlabel0, ylabel0, title0, pointType0
INTEGER(I4B) :: ii
INTEGER(I4B), DIMENSION(SIZE(y, 2)) :: opt_array, text_colors, &
& box_colors, box_patterns, line_colors, &
& line_styles, symbol_colors, symbol_numbers
REAL(DFP), DIMENSION(SIZE(y, 2)) :: symbol_scales, line_widths, &
  & box_line_widths, box_scales
CHARACTER(LEN=20), DIMENSION(SIZE(y, 2)) :: symbols
CHARACTER(LEN=80), DIMENSION(SIZE(y, 2)) :: legend_text
  !!
xlabel0 = INPUT(option=xlabel, default="X-Axis")
ylabel0 = INPUT(option=ylabel, default="Y-Axis")
title0 = INPUT(option=title, default="Title")
lineWidth0 = INPUT(option=lineWidth, default=2.0_DFP)
pointType0 = INPUT(option=pointType, default="#(135)")
  !!
IF (PRESENT(xmin)) THEN
  xmin0 = xmin
ELSE
  xmin0 = MINVAL(x)
  xmin0 = xmin0 - ABS(xmin0) * 0.1
END IF
  !!
IF (PRESENT(xmax)) THEN
  xmax0 = xmax
ELSE
  xmax0 = MAXVAL(x)
  xmax0 = xmax0 + ABS(xmax0) * 0.1
END IF
  !!
IF (PRESENT(ymin)) THEN
  ymin0 = ymin
ELSE
  ymin0 = MINVAL(y)
  ymin0 = ymin0 - ABS(ymin0) * 0.1
END IF
  !!
IF (PRESENT(ymax)) THEN
  ymax0 = ymax
ELSE
  ymax0 = MAXVAL(y)
  ymax0 = ymax0 + ABS(ymax0) * 0.1
END IF
  !!
IF (PRESENT(legendTexts)) THEN
  DO ii = 1, SIZE(legendTexts)
    legend_text(ii) = legendTexts(ii)%chars()
  END DO
ELSE
  DO ii = 1, SIZE(legend_text)
    legend_text(ii) = "data_"//tostring(ii)
  END DO
END IF
  !!
extn = getExtension(filename)
  !!
SELECT CASE (extn%chars())
CASE ("pdf")
  driver = "pdf"
  ! driver = "pdfcairo"
CASE ("png")
  driver = "pngqt"
  ! driver = "pngcairo"
CASE ("ps")
  driver = "ps"
  ! driver = "pscairo"
CASE ("eps")
  driver = "epscairo"
CASE ("svg")
  driver = "svg"
CASE ("jpeg", "jpg")
  driver = "jpgqt"
END SELECT
  !!
CALL PLSDEV(driver%chars())
CALL PLSFNAM(TRIM(filename))
CALL PLSCOLBG(255, 255, 255)
CALL PLINIT
CALL PLSCOL0(0, 0, 0, 0)
CALL PLCOL0(0)
CALL PLENV(xmin0, xmax0, ymin0, ymax0, 0, 0)
CALL PLBOX('bcnst', 0.0_DFP, 0, 'bcnstv', 0.0_DFP, 0)
CALL PLLAB(xlabel0%chars(), ylabel0%chars(), title0%chars())
CALL PLWIDTH(lineWidth0)
DO ii = 1, SIZE(y, 2)
  CALL PLCOL0(ii)
  line_colors(ii) = ii
  symbol_colors(ii) = ii
  CALL PLLINE(x, y(:, ii))
  IF (PRESENT(isPoint)) THEN
    CALL PLSTRING(x, y(:, ii), pointType0%chars())
  END IF
END DO
  !!
opt_array = PL_LEGEND_LINE
line_styles = 1
line_widths = 1
symbol_scales = 1.0
symbol_numbers = 1
text_colors = 0
DO ii = 1, SIZE(symbols)
  symbols(ii) = ""
END DO
  !!
CALL PLLEGEND( &
  & legend_width, &
  & legend_height, &
  & PL_LEGEND_BACKGROUND + PL_LEGEND_BOUNDING_BOX, &
  & 0, &
  & 0.0_DFP, &
  & 0.0_DFP, &
  & 0.10_DFP, &
  & 15, &
  & 0, &
  & 1, &
  & 0, &
  & 0, &
  & opt_array, &
  & 1.0_DFP, &
  & 1.0_DFP, &
  & 2.0_DFP, &
  & 1.0_DFP, &
  & text_colors, &
  & legend_text, &
  & box_colors, &
  & box_patterns, &
  & box_scales, &
  & box_line_widths, &
  & line_colors, &
  & line_styles, &
  & line_widths, &
  & symbol_colors, &
  & symbol_scales, &
  & symbol_numbers, &
  & symbols)
CALL PLCOL0(0)
CALL PLEND
#endif
END PROCEDURE line_plot_x1y2

!----------------------------------------------------------------------------
!                                                                    Plot2D
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Plot2D
CALL Plot( &
  & x=x, &
  & y=y, &
  & lineColor=lineColor, &
  & lineStyle=lineType, &
  & lineWidth=lineWidth, &
  & markColor=pointColor, &
  & markStyle=pointType, &
  & markSize=pointSize)
END PROCEDURE plot_Plot2D

!----------------------------------------------------------------------------
!                                                                 Errorbar
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Errorbar
CALL errorbar(&
  & x=x, &
  & y=y, &
  & xerr=xerr, &
  & yerr=yerr, &
  & lineColor=lineColor, &
  & lineStyle=lineType, &
  & lineWidth=lineWidth)
END PROCEDURE plot_Errorbar

!----------------------------------------------------------------------------
!                                                                 Plot3D
!----------------------------------------------------------------------------

MODULE PROCEDURE plot_Plot3D
CALL Plot3( &
  & x=x, &
  & y=y, &
  & z=z, &
  & lineColor=lineColor, &
  & lineStyle=lineType, &
  & lineWidth=lineWidth, &
  & markColor=pointColor, &
  & markStyle=pointType, &
  & markSize=pointSize)
END PROCEDURE plot_Plot3D

END SUBMODULE LinePlotMethods
#endif
