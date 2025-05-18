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
!
!> author: Vikas Sharma, Ph. D.
! date: 15 Aug 2022
! summary: Wrapper module for plplot
!
!# Introduction
!
! This module is slightly modified from its original version.
! https://github.com/zoziha/easy_plplot
!
MODULE EasyPlplot
USE GlobalData, ONLY: wp => DFP, I4B, LGT
USE plplot
USE EasyPlplot_Utilities

IMPLICIT NONE
PRIVATE

#ifdef USE_PLPLOT
INTEGER(I4B), PARAMETER :: pp = plflt
#else
INTEGER(I4B), PARAMETER :: pp = wp
#endif

CHARACTER(*), PARAMETER :: default_dev = 'qtwidget'
!! Default output device

CHARACTER(*), PARAMETER, PUBLIC :: PS_PLUS = "+"
CHARACTER(*), PARAMETER, PUBLIC :: PS_CROSS = "x"
CHARACTER(*), PARAMETER, PUBLIC :: PS_ASTERIC = "*"
CHARACTER(*), PARAMETER, PUBLIC :: PS_DOT = "."
CHARACTER(*), PARAMETER, PUBLIC :: PS_SQUARE = "s"
CHARACTER(*), PARAMETER, PUBLIC :: PS_STAR = "star"
CHARACTER(*), PARAMETER, PUBLIC :: PS_H_CIRCLE = "h_circle"
CHARACTER(*), PARAMETER, PUBLIC :: PS_H_SQUARE = "h_square"
CHARACTER(*), PARAMETER, PUBLIC :: PS_H_TRIAG_U = "h_triag_u"
CHARACTER(*), PARAMETER, PUBLIC :: PS_H_DIAMOND = "h_diamond"
CHARACTER(*), PARAMETER, PUBLIC :: PS_TRIAG_U = "triag_u"
CHARACTER(*), PARAMETER, PUBLIC :: PS_TRIAG_L = "triag_l"
CHARACTER(*), PARAMETER, PUBLIC :: PS_TRIAG_D = "triag_d"
CHARACTER(*), PARAMETER, PUBLIC :: PS_TRIAG_R = "triag_r"
CHARACTER(*), PARAMETER :: CODE_PS_PLUS = "#(140)"
CHARACTER(*), PARAMETER :: CODE_PS_CROSS = "#(141)"
CHARACTER(*), PARAMETER :: CODE_PS_ASTERIC = "#(142)"
CHARACTER(*), PARAMETER :: CODE_PS_DOT = "#(143)"
CHARACTER(*), PARAMETER :: CODE_PS_SQUARE = "#(144)"
CHARACTER(*), PARAMETER :: CODE_PS_STAR = "#(145)"
CHARACTER(*), PARAMETER :: CODE_PS_H_CIRCLE = "#(840)" !135
CHARACTER(*), PARAMETER :: CODE_PS_H_SQUARE = "#(841)"
CHARACTER(*), PARAMETER :: CODE_PS_H_TRIAG_U = "#(842)"
CHARACTER(*), PARAMETER :: CODE_PS_H_DIAMOND = "#(843)"
CHARACTER(*), PARAMETER :: CODE_PS_TRIAG_U = "#(852)"
CHARACTER(*), PARAMETER :: CODE_PS_TRIAG_L = "#(853)"
CHARACTER(*), PARAMETER :: CODE_PS_TRIAG_D = "#(854)"
CHARACTER(*), PARAMETER :: CODE_PS_TRIAG_R = "#(855)"

! Library State
LOGICAL(LGT) :: didShow = .FALSE.
!! Flag for library display status
REAL(pp) :: fontScale = 1.0_PP
!! Font scale factor to resetPen
LOGICAL(LGT) :: blackOnWhite = .TRUE.
!! Reverse black and white
LOGICAL(LGT) :: transparentBackground = .FALSE.
!! Transparent background

! Interfaces
INTERFACE localize
  MODULE PROCEDURE localize_1
  MODULE PROCEDURE localize_2
END INTERFACE

! Exports
PUBLIC :: setup
! interface created
PUBLIC :: show
! interface created
PUBLIC :: figure
! interface created
PUBLIC :: subplot
! interface created
PUBLIC :: xylim, xlim, ylim, xyzlim !
! interface created
PUBLIC :: labels, xlabel, ylabel, title
! interface created
PUBLIC :: ticks, xticks, yticks, box
! interface created
PUBLIC :: legend
! interface created

! public :: mixval, linspace
PUBLIC :: binData
!!
PUBLIC :: plot
! interface created
PUBLIC :: plot3
! interface created
PUBLIC :: scatter
! interface created
PUBLIC :: errorbar
! interface created
!!
PUBLIC :: contour
! interface created
PUBLIC :: contourf
! interface created
PUBLIC :: colorbar
! interface created
PUBLIC :: colorbar2
! interface created
PUBLIC :: bar
! interface created
PUBLIC :: barh
! interface created
PUBLIC :: hist
! interface created
PUBLIC :: fillBetween
! interface created
PUBLIC :: fillBetweenx
! interface created
PUBLIC :: quiver
! interface created
PUBLIC :: surface
! interface created
PUBLIC :: wireframe
! interface created

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Count data in each bin
FUNCTION binData(d, N, db, normalize) RESULT(o)
  REAL(wp), DIMENSION(:), INTENT(in) :: d
  !! Data for binning
  INTEGER, INTENT(in), OPTIONAL :: N
  !! Number of bins
  REAL(wp), DIMENSION(2), INTENT(in), OPTIONAL :: db
  !! Boundaries of bin range
  INTEGER, INTENT(in), OPTIONAL :: normalize
  !! Normalization type (1=sum, 2=bin size, 3=maxval)
  REAL(wp), DIMENSION(:, :), ALLOCATABLE :: o

  REAL(wp), DIMENSION(:), ALLOCATABLE :: b
  INTEGER :: Nl, k

  Nl = 10
  IF (PRESENT(N)) Nl = N

  IF (PRESENT(db)) THEN
    b = linspace(db(1), db(2), Nl + 1)
  ELSE
    b = linspace(MINVAL(d) - EPSILON(1.0_WP), &
      & MAXVAL(d) + EPSILON(1.0_WP), Nl + 1)
  END IF

  ALLOCATE (o(Nl, 2))
  o(:, 1) = (b(1:Nl) + b(2:Nl + 1)) / 2.0_WP

  DO k = 1, Nl
    o(k, 2) = REAL(COUNT(d >= b(k) .AND. d <= b(k + 1)), wp)
  END DO

  IF (PRESENT(normalize)) THEN
    SELECT CASE (normalize)
    CASE (1)
      o(:, 2) = o(:, 2) / SUM(o(:, 2))
    CASE (2)
      DO k = 1, Nl
        o(k, 2) = o(k, 2) / (b(k + 1) - b(k))
      END DO
    CASE (3)
      o(:, 2) = o(:, 2) / MAXVAL(o(:, 2))
    END SELECT
  END IF
END FUNCTION binData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION localize_1(A) RESULT(o)
  REAL(wp), DIMENSION(:), INTENT(in) :: A
  REAL(pp), DIMENSION(:), ALLOCATABLE :: o
  INTEGER :: N, k

  N = SIZE(A)
  ALLOCATE (o(N))
  DO CONCURRENT(k=1:N)
    o(k) = REAL(A(k), pp)
  END DO
END FUNCTION localize_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION localize_2(A) RESULT(o)
  REAL(wp), DIMENSION(:, :), INTENT(in) :: A
  REAL(pp), DIMENSION(:, :), ALLOCATABLE :: o
  INTEGER :: N, M, i, j

  N = SIZE(A, 1)
  M = SIZE(A, 2)
  ALLOCATE (o(N, M))
  DO CONCURRENT(i=1:N, j=1:M)
    o(i, j) = REAL(A(i, j), pp)
  END DO
END FUNCTION localize_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Axes and Figure Routines
!! Create a new figure
SUBROUTINE figure
  LOGICAL, SAVE :: isFirst = .TRUE.

  IF (.NOT. isFirst) THEN
    CALL pleop()
  ELSE
    isFirst = .FALSE.
  END IF

  CALL plbop()
  CALL plssub(1, 1)
  CALL pladv(1)
  CALL resetPen()
END SUBROUTINE figure

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Create a set of axes on a figure
SUBROUTINE subplot(ny, nx, i, aspect, is3d)
  INTEGER, INTENT(in) :: nx
  !! Number of subplot columns
  INTEGER, INTENT(in) :: ny
  !! Number of subplot rows
  INTEGER, INTENT(in) :: i
  !! Subplot to use
  REAL(wp), INTENT(in), OPTIONAL :: aspect
  !! Aspect ratio of the axes
  LOGICAL, INTENT(in), OPTIONAL :: is3d

  LOGICAL :: is3dl

  CALL plssub(nx, ny)
  CALL pladv(i)
  CALL resetPen()

  is3dl = .FALSE.
  IF (PRESENT(is3d)) is3dl = is3d

  IF (is3dl) THEN
    CALL plvpor(0.0_PP, 1.0_PP, 0.0_PP, 1.0_PP)
  ELSE
    IF (PRESENT(aspect)) THEN
      CALL plvasp(REAL(aspect, pp))
    ELSE
      CALL plvsta()
    END IF
  END IF

  CALL defaultLim()
END SUBROUTINE subplot

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE defaultLim
  REAL(pp), PARAMETER :: eps = EPSILON(1.0_PP)

  CALL plwind(-eps, eps, -eps, eps)
END SUBROUTINE defaultLim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the x and y ranges of the plot
SUBROUTINE xylim(xb, yb)
  REAL(wp), DIMENSION(2), INTENT(in) :: xb
  !! x-range of plot
  REAL(wp), DIMENSION(2), INTENT(in) :: yb
  !! y-range of plot

  REAL(pp), DIMENSION(2) :: xbl, ybl

  xbl = localize(xb)
  ybl = localize(yb)

  CALL plwind(xbl(1), xbl(2), ybl(1), ybl(2))
END SUBROUTINE xylim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the limits of the x-axis
SUBROUTINE xlim(xl, xh)
  REAL(wp), INTENT(in) :: xl, xh

  REAL(pp) :: x1, x2, y1, y2

  CALL plgvpw(x1, x2, y1, y2)
  CALL plwind(REAL(xl, pp), REAL(xh, pp), y1, y2)
END SUBROUTINE xlim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the limits of the y-axis
SUBROUTINE ylim(yl, yh)
  REAL(wp), INTENT(in) :: yl, yh

  REAL(pp) :: x1, x2, y1, y2

  CALL plgvpw(x1, x2, y1, y2)
  CALL plwind(x1, x2, REAL(yl, pp), REAL(yh, pp))
END SUBROUTINE ylim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the limits for a 3d plot
SUBROUTINE xyzlim(xb, yb, zb, altitude, azimuth, zoom)
  REAL(wp), DIMENSION(2), INTENT(in) :: xb
  !! x-range of plot
  REAL(wp), DIMENSION(2), INTENT(in) :: yb
  !! y-range of plot
  REAL(wp), DIMENSION(2), INTENT(in) :: zb
  !! z-range of plot
  REAL(wp), INTENT(in), OPTIONAL :: altitude
  !! Altitude angle of plot in degrees
  REAL(wp), INTENT(in), OPTIONAL :: azimuth
  !! Azimuth angle of plot in degrees
  REAL(wp), INTENT(in), OPTIONAL :: zoom
  !! Zoom ratio (default 1.0)

  REAL(pp) :: al, az, zm

  al = 45.0_PP
  IF (PRESENT(altitude)) al = REAL(altitude, pp)
  az = 60.0_PP
  IF (PRESENT(azimuth)) az = REAL(azimuth, pp)
  zm = 1.0_PP
  IF (PRESENT(zoom)) zm = REAL(zoom, pp)

  CALL plwind(-1.0_PP, 1.0_PP, -1.0_PP, 1.5_PP)
  CALL plw3d(zm, zm, 1.2_PP * zm, &
  & REAL(xb(1), pp), REAL(xb(2), pp), &
  & REAL(yb(1), pp), REAL(yb(2), pp), &
  & REAL(zb(1), pp), REAL(zb(2), pp), al, az)
END SUBROUTINE xyzlim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the ticks for the axes
SUBROUTINE ticks(dx, dy, logx, logy, color, lineWidth)
  REAL(wp), INTENT(in), OPTIONAL :: dx
  !! Spacing between ticks on x-axis
  REAL(wp), INTENT(in), OPTIONAL :: dy
  !! Spacing between ticks on y-axis
  LOGICAL, INTENT(in), OPTIONAL :: logx
  !! Flag for log-ticks and labels on x-axis
  LOGICAL, INTENT(in), OPTIONAL :: logy
  !! Flag for log-ticks and labels on y-axis
  CHARACTER(*), INTENT(in), OPTIONAL :: color
  !! Color code for ticks, box, and labels
  REAL(wp), OPTIONAL :: linewidth
  !! Line width for ticks and box

  REAL(pp) :: dxl, dyl
  CHARACTER(10) :: xopts, yopts

  dxl = 0.0_PP
  IF (PRESENT(dx)) dxl = REAL(dx, pp)

  dyl = 0.0_PP
  IF (PRESENT(dy)) dyl = REAL(dy, pp)

  xopts = 'bcnst'
  IF (PRESENT(logx)) THEN
    IF (logx) xopts = 'bcnstl'
  END IF

  yopts = 'bcnstv'
  IF (PRESENT(logy)) THEN
    IF (logy) yopts = 'bcnstvl'
  END IF

  CALL resetPen()

  IF (PRESENT(color)) CALL setColor(color)
  IF (PRESENT(lineWidth)) CALL setLineWidth(lineWidth)

  CALL plbox(xopts, dxl, 0, yopts, dyl, 0)
  CALL resetPen()
END SUBROUTINE ticks

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set x,y and plot labels
SUBROUTINE box(xLabel, yLabel, zLabel, color)
  CHARACTER(*), INTENT(in) :: xLabel
  !! Label for x-axis
  CHARACTER(*), INTENT(in) :: yLabel
  !! Label for x-axis
  CHARACTER(*), INTENT(in) :: zLabel
  !! Label for z-axis
  CHARACTER(*), INTENT(in), OPTIONAL :: color
  !! Color of labels

  IF (PRESENT(color)) CALL setColor(color)
  CALL plbox3('bnstu', xLabel, 0.0_PP, 0, 'bnstu', yLabel, &
    & 0.0_PP, 0, 'bnstu', zLabel, 0.0_PP, 0)
  CALL resetPen()
END SUBROUTINE box

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the ticks for the x-axis
SUBROUTINE xticks(d, logScale, primary, secondary, color, lineWidth)
  REAL(wp), INTENT(in), OPTIONAL :: d
  !! Spacing between ticks
  LOGICAL, INTENT(in), OPTIONAL :: logScale
  !! Flag for log-ticks and labels
  LOGICAL, INTENT(in), OPTIONAL :: primary
  !! Draw primary axis
  LOGICAL, INTENT(in), OPTIONAL :: secondary
  !! Draw secondary axis
  CHARACTER(*), INTENT(in), OPTIONAL :: color
  !! Color code for ticks, box, and labels
  REAL(wp), OPTIONAL :: linewidth
  !! Line width for ticks and box
  REAL(pp) :: dxl, dyl
! interface created
  CHARACTER(10) :: xopts, yopts

  dxl = 0.0_PP
  dyl = 0.0_PP
  IF (PRESENT(d)) dxl = REAL(d, pp)

  xopts = 'nst'

  IF (PRESENT(primary)) THEN
    IF (primary) xopts = TRIM(xopts)//'b'
  ELSE
    xopts = TRIM(xopts)//'b'
  END IF

  IF (PRESENT(secondary)) THEN
    IF (secondary) xopts = TRIM(xopts)//'c'
  ELSE
    xopts = TRIM(xopts)//'c'
  END IF

  IF (PRESENT(logScale)) THEN
    IF (logScale) xopts = TRIM(xopts)//'l'
  END IF
  yopts = ''

  IF (PRESENT(color)) CALL setColor(color)
  IF (PRESENT(lineWidth)) CALL setLineWidth(lineWidth)
  CALL plbox(xopts, dxl, 0, yopts, dyl, 0)
  CALL resetPen()
END SUBROUTINE xticks

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the ticks for the y-axis
SUBROUTINE yticks(d, logScale, primary, secondary, color, lineWidth)
  REAL(wp), INTENT(in), OPTIONAL :: d
  !! Spacing between ticks
  LOGICAL, INTENT(in), OPTIONAL :: logScale
  !! Flag for log-ticks and labels
  LOGICAL, INTENT(in), OPTIONAL :: primary
  !! Draw primary axis
  LOGICAL, INTENT(in), OPTIONAL :: secondary
  !! Draw secondary axis
  CHARACTER(*), INTENT(in), OPTIONAL :: color
  !! Color code for ticks, box, and labels
  REAL(wp), OPTIONAL :: linewidth
  !! Line width for ticks and box
  REAL(pp) :: dxl, dyl
  CHARACTER(10) :: xopts, yopts

  dxl = 0.0_PP
  dyl = 0.0_PP
  IF (PRESENT(d)) dyl = REAL(d, pp)

  yopts = 'nst'

  IF (PRESENT(primary)) THEN
    IF (primary) yopts = TRIM(xopts)//'b'
  ELSE
    yopts = TRIM(yopts)//'b'
  END IF

  IF (PRESENT(secondary)) THEN
    IF (secondary) yopts = TRIM(yopts)//'c'
  ELSE
    yopts = TRIM(yopts)//'c'
  END IF

  IF (PRESENT(logScale)) THEN
    IF (logScale) yopts = TRIM(yopts)//'l'
  END IF
  xopts = ''

  IF (PRESENT(color)) CALL setColor(color)
  IF (PRESENT(lineWidth)) CALL setLineWidth(lineWidth)
  CALL plbox(xopts, dxl, 0, yopts, dyl, 0)
  CALL resetPen()
END SUBROUTINE yticks

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set x,y and plot labels
SUBROUTINE labels(xLabel, yLabel, plotLabel, color)
  CHARACTER(*), INTENT(in) :: xLabel
  !! Label for x-axis
  CHARACTER(*), INTENT(in) :: yLabel
  !! Label for y-axis
  CHARACTER(*), INTENT(in) :: plotLabel
  !! Label entire plot
  CHARACTER(*), INTENT(in), OPTIONAL :: color
  !! Color of labels

  IF (PRESENT(color)) CALL setColor(color)
  CALL pllab(xLabel, yLabel, plotLabel)
  CALL resetPen()
END SUBROUTINE labels

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set x-label
SUBROUTINE xlabel(label, color)
  CHARACTER(*), INTENT(in) :: label
  !! Label for axis
  CHARACTER(*), INTENT(in), OPTIONAL :: color
  !! Color of labels

  IF (PRESENT(color)) CALL setColor(color)
  CALL plmtex('b', 3.0_PP, 0.5_PP, 0.5_PP, label)
  CALL resetPen()
END SUBROUTINE xlabel

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set y-label
SUBROUTINE ylabel(label, color)
  CHARACTER(*), INTENT(in) :: label
  !! Label for axis
  CHARACTER(*), INTENT(in), OPTIONAL :: color
  !! Color of labels

  IF (PRESENT(color)) CALL setColor(color)
  CALL plmtex('l', 5.0_PP, 0.5_PP, 0.5_PP, label)
  CALL resetPen()
END SUBROUTINE ylabel

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set plot title
SUBROUTINE title(label, color)
  CHARACTER(*), INTENT(in) :: label
  !! Label for plot
  CHARACTER(*), INTENT(in), OPTIONAL :: color
  !! Color of labels

  IF (PRESENT(color)) CALL setColor(color)
  CALL plmtex('t', 1.5_PP, 0.5_PP, 0.5_PP, label)
  CALL resetPen()
END SUBROUTINE title

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Add a colorbar to the top of the plot
SUBROUTINE colorbar(z, N, leftLabel, rightLabel)
  REAL(wp), DIMENSION(:, :), INTENT(in) :: z
  !! Data used for levels computation
  INTEGER, INTENT(in) :: N
  !! Number of levels to compute
  CHARACTER(*), INTENT(in), OPTIONAL :: leftLabel
  !! Label for left side of colorbar
  CHARACTER(*), INTENT(in), OPTIONAL :: rightLabel
  !! Label for right side of colorbar

  REAL(pp), DIMENSION(:, :), ALLOCATABLE :: values
  CHARACTER(64), DIMENSION(2) :: labels

  REAL(pp) :: fill_width
  REAL(pp) :: cont_width
  INTEGER :: cont_color
  REAL(pp) :: colorbar_width
  REAL(pp) :: colorbar_height
  INTEGER :: k

  values = RESHAPE( &
    & REAL([(REAL(k - 1, wp) / REAL(N - 1, wp) * (MAXVAL(z) - MINVAL(z)) + &
    & MINVAL(z), k=1, N)], pp), &
    & [N, 1])

  fill_width = 2.0_PP
  cont_width = 0.0_PP
  cont_color = 1
  labels = ''
  IF (PRESENT(leftLabel)) labels(1) = leftLabel
  IF (PRESENT(rightLabel)) labels(2) = rightLabel

  CALL plcolorbar(colorbar_width, colorbar_height,&
    & IOR(PL_COLORBAR_GRADIENT, PL_COLORBAR_SHADE_LABEL), &
    & PL_POSITION_TOP,&
    & 0.0_PP, 0.01_PP, 0.75_PP, 0.05_PP,&
    & 0, 1, 1, 0.0_PP, 0.0_PP, &
    & cont_color, cont_width, &
    & [PL_COLORBAR_LABEL_LEFT, PL_COLORBAR_LABEL_RIGHT], labels, &
    & ['bcvmt'], [0.0_PP], [0], [SIZE(values)], values)
END SUBROUTINE colorbar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Add a colorbar to the top of the plot
SUBROUTINE colorbar2(z, N, leftLabel, rightLabel)
  REAL(wp), DIMENSION(:, :), INTENT(in) :: z
  !! Data used for levels computation
  INTEGER, INTENT(in) :: N
  !! Number of levels to compute
  CHARACTER(*), INTENT(in), OPTIONAL :: leftLabel
  !! Label for left side of colorbar
  CHARACTER(*), INTENT(in), OPTIONAL :: rightLabel
  !! Label for right side of colorbar

  REAL(pp), DIMENSION(:, :), ALLOCATABLE :: values
  CHARACTER(64), DIMENSION(2) :: labels

  REAL(pp) :: fill_width
  REAL(pp) :: cont_width
  INTEGER :: cont_color
  REAL(pp) :: colorbar_width
  REAL(pp) :: colorbar_height
  INTEGER :: k

  values = RESHAPE( &
    & REAL([(REAL(k - 1, wp) / REAL(N - 1, wp) * (MAXVAL(z) - MINVAL(z)) + &
    & MINVAL(z), k=1, N)], pp), &
    & [N, 1])

  fill_width = 2.0_PP
  cont_width = 0.0_PP
  cont_color = 1
  labels = ''
  IF (PRESENT(leftLabel)) labels(1) = leftLabel
  IF (PRESENT(rightLabel)) labels(2) = rightLabel

  CALL plcolorbar(colorbar_width, colorbar_height,&
    & IOR(PL_COLORBAR_GRADIENT, PL_COLORBAR_SHADE_LABEL), PL_POSITION_RIGHT,&
    & 0.01_PP, 0.0_PP, 0.05_PP, 0.75_PP,&
    & 0, 1, 1, 0.0_PP, 0.0_PP, &
    & cont_color, cont_width, &
    & [PL_COLORBAR_LABEL_BOTTOM, PL_COLORBAR_LABEL_TOP], labels, &
    & ['bcvmt'], [0.0_PP], [0], [SIZE(values)], values)
END SUBROUTINE colorbar2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Create legend for plot data
!!
!! FIXME: Text sizing should be modifiable
SUBROUTINE legend(corner, series, lineWidths, markScales, markCounts, ncol)
  CHARACTER(*), INTENT(in) :: corner
  !! Corner for legend
  CHARACTER(*), DIMENSION(:, :), INTENT(in) :: series
  !! Data series in rows
  !! [name,textColor,lineStyle,lineColor,markStyle,markColor,boxColor]
  REAL(wp), DIMENSION(:), INTENT(in), OPTIONAL :: lineWidths
  !! Line widths for the plots
  REAL(wp), DIMENSION(:), INTENT(in), OPTIONAL :: markScales
  !! Marker sizes for the plots
  INTEGER, DIMENSION(:), INTENT(in), OPTIONAL :: markCounts
  !! Marker counts for the plots
  INTEGER, INTENT(in), OPTIONAL :: ncol
  !! Number of columns

  REAL(pp) :: width, height, xoff, yoff
  REAL(pp) :: plotWidth
  INTEGER :: opt, cornerl
  INTEGER :: bg_color, bb_color, bb_style, lncol, lnrow
  INTEGER, DIMENSION(SIZE(series, 1)) :: opts
  REAL(pp), DIMENSION(SIZE(series, 1)) :: lwidths, mscales
  INTEGER, DIMENSION(SIZE(series, 1)) :: mcounts, text_colors
  REAL(pp) :: text_offset, text_scale, text_spacing, text_justification
  INTEGER, DIMENSION(SIZE(series, 1)) :: box_colors, box_patterns
  REAL(pp), DIMENSION(SIZE(series, 1)) :: box_scales, box_line_widths
  INTEGER, DIMENSION(SIZE(series, 1)) :: line_colors, line_styles
  INTEGER, DIMENSION(SIZE(series, 1)) :: mark_colors
  CHARACTER(64), DIMENSION(SIZE(series, 1)) :: mark_styles
  INTEGER :: k

  CALL doLegendBox()

  opts = 0
  DO k = 1, SIZE(series, 1)
    IF (series(k, 3) /= '') opts(k) = IOR(opts(k), PL_LEGEND_LINE)
    IF (series(k, 5) /= '') opts(k) = IOR(opts(k), PL_LEGEND_SYMBOL)
    IF (series(k, 7) /= '') opts(k) = IOR(opts(k), PL_LEGEND_COLOR_BOX)
  END DO

  CALL doText()
  CALL doBoxes()
  CALL doLines()
  CALL doMarkers()

  CALL pllegend(width, height, opt, cornerl, xoff, yoff, plotWidth, &
    & bg_color, bb_color, bb_style, &
    & lnrow, lncol, opts, text_offset, &
    & text_scale, text_spacing, text_justification,&
    & text_colors, series(:, 1), &
    & box_colors, box_patterns, box_scales, box_line_widths, &
    & line_colors, line_styles, lwidths, &
    & mark_colors, mscales, mcounts, mark_styles)

CONTAINS

  SUBROUTINE doLegendBox
    opt = PL_LEGEND_BACKGROUND + PL_LEGEND_BOUNDING_BOX
    cornerl = getCorner(corner)
    xoff = 0.0_PP
    yoff = 0.0_PP
    plotWidth = 0.05_PP
    bg_color = 0
    bb_color = 1
    bb_style = getLineStyleCode('-')

    lncol = 1
    IF (PRESENT(ncol)) lncol = ncol
    lnrow = SIZE(series, 1) / lncol
  END SUBROUTINE doLegendBox

  SUBROUTINE doText
    text_offset = 0.3_PP
    text_scale = fontScale
    text_spacing = 3.0_PP
    text_justification = 0.0_PP

    DO k = 1, SIZE(series, 1)
      text_colors = getColorCode(series(k, 2))
    END DO
  END SUBROUTINE doText

  SUBROUTINE doBoxes
    DO k = 1, SIZE(series, 1)
      box_colors(k) = getColorCode(series(k, 7))
    END DO
    box_patterns = 0
    box_scales = 0.5_PP
    box_line_widths = 0.0_PP
  END SUBROUTINE doBoxes

  SUBROUTINE doLines
    lwidths = 1.0_PP
    IF (PRESENT(lineWidths)) lwidths = REAL(lineWidths, pp)

    DO k = 1, SIZE(series, 1)
      line_colors(k) = getColorCode(series(k, 4))
      line_styles(k) = getLineStyleCode(series(k, 3))
    END DO
  END SUBROUTINE doLines

  SUBROUTINE doMarkers
    mcounts = 2
    IF (PRESENT(markCounts)) mcounts = markCounts
    mscales = 1.0_PP
    IF (PRESENT(markScales)) mscales = REAL(markScales, pp)

    DO k = 1, SIZE(series, 1)
      mark_colors(k) = getColorCode(series(k, 6))
      mark_styles(k) = getSymbolCode(series(k, 5))
    END DO
  END SUBROUTINE doMarkers

  FUNCTION getCorner(text) RESULT(code)
    CHARACTER(*), INTENT(in) :: text
    INTEGER :: code

    code = PL_POSITION_INSIDE
    IF (startsWith(text, 'upper')) code = code + PL_POSITION_TOP
    IF (startsWith(text, 'lower')) code = code + PL_POSITION_BOTTOM
    IF (endsWith(text, 'right')) code = code + PL_POSITION_RIGHT
    IF (endsWith(text, 'left')) code = code + PL_POSITION_LEFT
  END FUNCTION getCorner

END SUBROUTINE legend

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Plotting Routines

!! Create a histogram
SUBROUTINE hist(d, N, db, relWidth, fillColor, fillPattern, &
  & lineColor, lineWidth)
  REAL(wp), DIMENSION(:), INTENT(in) :: d
  !! Data for binning
  INTEGER, INTENT(in), OPTIONAL :: N
  !! Number of bins
  REAL(wp), DIMENSION(2), INTENT(in), OPTIONAL :: db
  !! Boundaries of bin range
  REAL(wp), INTENT(in), OPTIONAL :: relWidth
  !! Relative width of bars (default 0.8)
  CHARACTER(*), INTENT(in), OPTIONAL :: fillColor
  !! Color of bar fills
  CHARACTER(*), INTENT(in), OPTIONAL :: fillPattern
  !! Pattern of bar fills
  CHARACTER(*), INTENT(in), OPTIONAL :: lineColor
  !! Color of lines around bars
  REAL(wp), OPTIONAL :: lineWidth
  !! Width of lines around bars

  REAL(wp), DIMENSION(:, :), ALLOCATABLE :: h
  REAL(wp), DIMENSION(2) :: dbl
  INTEGER :: Nl

  REAL(wp) :: relWidthl
  REAL(wp) :: lineWidthl

  Nl = 20
  IF (PRESENT(N)) Nl = N

  IF (PRESENT(db)) THEN
    dbl = db
  ELSE
    dbl = mixval(d) + [-1.0_WP, 1.0_WP] * EPSILON(1.0_WP)
  END IF

  h = binData(d, Nl, dbl, normalize=3)

  relWidthl = 1.0_WP
  IF (PRESENT(relWidth)) relWidthl = relWidth
  lineWidthl = 0.5_WP
  IF (PRESENT(lineWidth)) lineWidthl = lineWidth

  IF (PRESENT(lineColor)) THEN
    IF (PRESENT(fillColor)) THEN
      IF (PRESENT(fillPattern)) THEN
        CALL bar(h(:, 1), h(:, 2), relWidth=relWidthl, &
          & lineColor=lineColor, &
          & lineWidth=lineWidthl, &
          & fillColor=fillColor, fillPattern=fillPattern)
      ELSE
        CALL bar(h(:, 1), h(:, 2), relWidth=relWidthl, &
          & lineColor=lineColor, &
          & lineWidth=lineWidthl, &
          & fillColor=fillColor)
      END IF
    ELSE
      IF (PRESENT(fillPattern)) THEN
        CALL bar(h(:, 1), h(:, 2), h(:, 2), &
          & relWidth=relWidthl, lineColor=lineColor, &
          & lineWidth=lineWidthl, &
          & fillPattern=fillPattern)
      ELSE
        CALL bar(h(:, 1), h(:, 2), h(:, 2), &
          & relWidth=relWidthl, lineColor=lineColor, &
          & lineWidth=lineWidthl)
      END IF
    END IF
  ELSE
    IF (PRESENT(fillColor)) THEN
      IF (PRESENT(fillPattern)) THEN
        CALL bar(h(:, 1), h(:, 2), relWidth=relWidthl, &
          & lineWidth=lineWidthl, &
          & fillColor=fillColor, fillPattern=fillPattern)
      ELSE
        CALL bar(h(:, 1), h(:, 2), relWidth=relWidthl, &
          & lineWidth=lineWidthl, &
          & fillColor=fillColor)
      END IF
    ELSE
      IF (PRESENT(fillPattern)) THEN
        CALL bar(h(:, 1), h(:, 2), h(:, 2), relWidth=relWidthl, &
          & lineWidth=lineWidthl, &
          & fillPattern=fillPattern)
      ELSE
        CALL bar(h(:, 1), h(:, 2), h(:, 2), relWidth=relWidthl, &
          & lineWidth=lineWidthl)
      END IF
    END IF
  END IF

  CALL resetPen()
END SUBROUTINE hist

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Create scatter plot of data
SUBROUTINE scatter(x, y, c, s, markColor, markStyle, markSize)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! x-coordinates of data
  REAL(wp), DIMENSION(:), INTENT(in) :: y
  !! y-coordinates of data
  REAL(wp), DIMENSION(:), INTENT(in), OPTIONAL :: c
  !! Data for smooth coloring
  REAL(wp), DIMENSION(:), INTENT(in), OPTIONAL :: s
  !! Data for marker scaling
  CHARACTER(*), INTENT(in), OPTIONAL :: markColor
  !! Color of markers; overridden by z
  CHARACTER(*), INTENT(in), OPTIONAL :: markStyle
  !! Style of markers
  REAL(wp), INTENT(in), OPTIONAL :: markSize
  !! Size of markers

  REAL(pp), DIMENSION(:), ALLOCATABLE :: xl, yl
  REAL(pp), DIMENSION(:), ALLOCATABLE :: cb
  CHARACTER(32) :: code
  INTEGER :: k

  xl = localize(x)
  yl = localize(y)

  IF (PRESENT(markColor)) CALL setColor(markColor)
  code = getSymbolCode('')
  IF (PRESENT(markStyle)) code = getSymbolCode(markStyle)
  IF (PRESENT(markSize)) CALL plschr(0.0_PP, REAL(markSize, pp))
  IF (PRESENT(markSize)) CALL plssym(0.0_PP, REAL(markSize, pp))

  IF (PRESENT(c)) cb = REAL(mixval(c), pp)
  DO k = 1, SIZE(x)
    IF (PRESENT(c)) CALL plcol1(REAL((c(k) - cb(1)) / (cb(2) - cb(1)), pp))
    IF (PRESENT(s)) CALL plschr(0.0_PP, REAL(s(k), pp))
    IF (PRESENT(s)) CALL plssym(0.0_PP, REAL(s(k), pp))
    CALL plptex(xl(k), yl(k), 0.0_PP, 0.0_PP, 0.5_PP, code)
  END DO
  CALL resetPen()
END SUBROUTINE scatter

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot data using lines and or markers
SUBROUTINE plot(x, y, lineColor, lineStyle, lineWidth, &
  & markColor, markStyle, markSize)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! x-data for plot
  REAL(wp), DIMENSION(:), INTENT(in) :: y
  !! y-data for plot
  CHARACTER(*), INTENT(in), OPTIONAL :: lineColor
  !! Color of line
  CHARACTER(*), INTENT(in), OPTIONAL :: lineStyle
  !! Style of line; '' for no line
  REAL(wp), INTENT(in), OPTIONAL :: lineWidth
  !! Width of line
  CHARACTER(*), INTENT(in), OPTIONAL :: markColor
  !! Color of markers, if any
  CHARACTER(*), INTENT(in), OPTIONAL :: markStyle
  !! Style of markers; '' or absent for none
  REAL(wp), INTENT(in), OPTIONAL :: markSize
  !! Size of markers, if any

  REAL(pp), DIMENSION(:), ALLOCATABLE :: xl, yl
  CHARACTER(32) :: code
  INTEGER :: k

  xl = localize(x)
  yl = localize(y)

  IF (PRESENT(lineColor)) CALL setColor(lineColor)
  IF (PRESENT(lineWidth)) CALL setLineWidth(lineWidth)
  IF (PRESENT(lineStyle)) THEN
    CALL setLineStyle(lineStyle)
    IF (lineStyle /= '') CALL plline(xl, yl)
  ELSE
    CALL plline(xl, yl)
  END IF
  CALL resetPen()

  IF (PRESENT(markColor)) CALL setColor(markColor)
  IF (PRESENT(markSize)) CALL plssym(0.0_PP, REAL(markSize, pp))
  IF (PRESENT(markStyle)) THEN
    code = getSymbolCode(markStyle)
    IF (markStyle /= '') THEN
      DO k = 1, SIZE(x)
        CALL plptex(xl(k), yl(k), 0.0_PP, 0.0_PP, 0.5_PP, code)
      END DO
    END IF
  END IF
  CALL resetPen()
END SUBROUTINE plot

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot data using lines and or markers
SUBROUTINE plot3(x, y, z, lineColor, lineStyle, lineWidth,&
  & markColor, markStyle, markSize)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! x-data for plot
  REAL(wp), DIMENSION(:), INTENT(in) :: y
  !! y-data for plot
  REAL(wp), DIMENSION(:), INTENT(in) :: z
  !! z-data for plot
  CHARACTER(*), INTENT(in), OPTIONAL :: lineColor
  !! Color of line
  CHARACTER(*), INTENT(in), OPTIONAL :: lineStyle
  !! Style of line; '' for no line
  REAL(wp), INTENT(in), OPTIONAL :: lineWidth
  !! Width of line
  CHARACTER(*), INTENT(in), OPTIONAL :: markColor
  !! Color of markers, if any
  CHARACTER(*), INTENT(in), OPTIONAL :: markStyle
  !! Style of markers; '' or absent for none
  REAL(wp), INTENT(in), OPTIONAL :: markSize
  !! Size of markers, if any

  REAL(pp), DIMENSION(:), ALLOCATABLE :: xl, yl, zl
  REAL(pp) :: dx, dy, dz, sx, sy, sz
  CHARACTER(32) :: code
  INTEGER :: k

  xl = localize(x)
  yl = localize(y)
  zl = localize(z)

  IF (PRESENT(lineColor)) CALL setColor(lineColor)
  IF (PRESENT(lineWidth)) CALL setLineWidth(lineWidth)
  IF (PRESENT(lineStyle)) THEN
    CALL setLineStyle(lineStyle)
    IF (lineStyle /= '') CALL plline(xl, yl)
  ELSE
    CALL plline3(xl, yl, zl)
  END IF
  CALL resetPen()

  IF (PRESENT(markColor)) CALL setColor(markColor)
  IF (PRESENT(markSize)) CALL plssym(0.0_PP, REAL(markSize, pp))
  IF (PRESENT(markStyle)) THEN
    code = getSymbolCode(markStyle)
    IF (markStyle /= '') THEN
      dx = 1.0_PP
      dy = 0.0_PP
      dz = 0.0_PP
      sx = 0.0_PP
      sy = 0.0_PP
      sz = 0.0_PP
      DO k = 1, SIZE(x)
        CALL plptex3(xl(k), yl(k), zl(k), dx, dy, dz, sx, sy, &
          & sz, 0.5_PP, code)
      END DO
    END IF
  END IF
  CALL resetPen()
END SUBROUTINE plot3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot contour lines
SUBROUTINE contour(x, y, z, N, lineColor, lineStyle, lineWidth)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! x-coordinates of data
  REAL(wp), DIMENSION(:), INTENT(in) :: y
  !! y-coordinates of data
  REAL(wp), DIMENSION(:, :), INTENT(in) :: z
  !! Data for contouring
  INTEGER, INTENT(in), OPTIONAL :: N
  !! Number of levels to use in contour
  CHARACTER(*), INTENT(in), OPTIONAL :: lineColor
  !! Color of contour lines
  CHARACTER(*), INTENT(in), OPTIONAL :: lineStyle
  !! Style of contour lines
  REAL(wp), OPTIONAL :: lineWidth
  !! Width of contour lines

  REAL(pp), DIMENSION(:), ALLOCATABLE :: xl, yl
  REAL(pp), DIMENSION(:, :), ALLOCATABLE :: zl

  REAL(pp), DIMENSION(:), ALLOCATABLE :: edge
  INTEGER :: Nl, k

  xl = localize(x)
  yl = localize(y)
  zl = localize(z)
  Nl = 20
  IF (PRESENT(N)) Nl = N
  edge = [(REAL(k - 1, pp) / REAL(Nl - 1, pp) * (MAXVAL(zl) - MINVAL(zl)) &
    & + MINVAL(zl), k=1, Nl)]

  IF (PRESENT(lineColor)) CALL setColor(lineColor)
  IF (PRESENT(lineStyle)) CALL setLineStyle(lineStyle)
  IF (PRESENT(lineWidth)) CALL setLineWidth(lineWidth)

  CALL plcont(zl, 1, SIZE(xl), 1, SIZE(yl), edge, xl, yl)
  CALL resetPen()
END SUBROUTINE contour

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot a 3d surface
SUBROUTINE surface(x, y, z, N, lineStyle)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! x-coordinates of data
  REAL(wp), DIMENSION(:), INTENT(in) :: y
  !! y-coordinates of data
  REAL(wp), DIMENSION(:, :), INTENT(in) :: z
  !! Data for contouring
  INTEGER, INTENT(in), OPTIONAL :: N
  !! Number of levels to use in surface colors
  CHARACTER(*), INTENT(in), OPTIONAL :: lineStyle
  !! Style for xy lines ( '-' = on, '' = off )

  REAL(pp), DIMENSION(:), ALLOCATABLE :: xl, yl
  REAL(pp), DIMENSION(:, :), ALLOCATABLE :: zl

  REAL(pp), DIMENSION(:), ALLOCATABLE :: edge
  INTEGER :: Nl, opt

  opt = MAG_COLOR

  xl = localize(x)
  yl = localize(y)
  zl = localize(z)
  Nl = 20
  IF (PRESENT(N)) THEN
    Nl = N
    opt = IOR(opt, SURF_CONT)
  END IF
  edge = localize(linspace(MINVAL(z), MAXVAL(z), Nl))

  IF (PRESENT(lineStyle)) THEN
    SELECT CASE (lineStyle)
    CASE ('')
      opt = opt
    CASE ('-')
      opt = IOR(opt, FACETED)
    END SELECT
  END IF

  CALL plsurf3d(xl, yl, zl, opt, edge)
  CALL resetPen()
END SUBROUTINE surface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot a 3d wireframe
SUBROUTINE wireframe(x, y, z, lineColor)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! x-coordinates of data
  REAL(wp), DIMENSION(:), INTENT(in) :: y
  !! y-coordinates of data
  REAL(wp), DIMENSION(:, :), INTENT(in) :: z
  !! Data for contouring
  CHARACTER(*), INTENT(in), OPTIONAL :: lineColor
  !! Color of contour lines

  REAL(pp), DIMENSION(:), ALLOCATABLE :: xl, yl
  REAL(pp), DIMENSION(:, :), ALLOCATABLE :: zl

  xl = localize(x)
  yl = localize(y)
  zl = localize(z)

  IF (PRESENT(lineColor)) THEN
    CALL setColor(lineColor)
    CALL plot3d(xl, yl, zl, DRAW_LINEXY, .FALSE.)
  ELSE
    CALL plot3d(xl, yl, zl, IOR(DRAW_LINEXY, MAG_COLOR), .FALSE.)
  END IF

  CALL resetPen()
END SUBROUTINE wireframe

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot filled contours
SUBROUTINE contourf(x, y, z, N)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! x-coordinates of data
  REAL(wp), DIMENSION(:), INTENT(in) :: y
  !! y-coordinates of data
  REAL(wp), DIMENSION(:, :), INTENT(in) :: z
  !! Data for contouring
  INTEGER, INTENT(in), OPTIONAL :: N
  !! Number of levels to use in contour

  REAL(pp), DIMENSION(:), ALLOCATABLE :: xl, yl
  REAL(pp), DIMENSION(:, :), ALLOCATABLE :: zl

  REAL(pp), DIMENSION(:), ALLOCATABLE :: edge

  REAL(pp) :: fill_width
  REAL(pp) :: cont_width
  INTEGER :: cont_color
  INTEGER :: Nl

  xl = localize(x)
  yl = localize(y)
  zl = localize(z)
  Nl = 20
  IF (PRESENT(N)) Nl = N

  edge = localize(linspace(MINVAL(z), MAXVAL(z), Nl))

  fill_width = -1.0_PP
  cont_width = -1.0_PP
  cont_color = -1

  CALL plshades(zl, MINVAL(xl), MAXVAL(xl), MINVAL(yl), MAXVAL(yl), &
          & edge, fill_width, cont_color, cont_width, .TRUE.)
  CALL resetPen()
END SUBROUTINE contourf

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot vectors
SUBROUTINE quiver(x, y, u, v, s, c, scaling, lineColor, lineStyle, lineWidth)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! x-positions of vectors
  REAL(wp), DIMENSION(:), INTENT(in) :: y
  !! y-positions of vectors
  REAL(wp), DIMENSION(:, :), INTENT(in) :: u
  !! u-components of vectors
  REAL(wp), DIMENSION(:, :), INTENT(in) :: v
  !! v-components of vectors
  REAL(wp), DIMENSION(:, :), INTENT(in), OPTIONAL :: s
  !! Scale of vectors
  REAL(wp), DIMENSION(:, :), INTENT(in), OPTIONAL :: c
  !! Color values for vectors
  REAL(wp), INTENT(in), OPTIONAL :: scaling
  !! Scaling of vectors
  !! < 0 = Automatic, then scaled
  !!   0 = Automatic
  !! > 0 = Directly scaled
  CHARACTER(*), INTENT(in), OPTIONAL :: lineColor
  !! Color of vectors
  CHARACTER(*), INTENT(in), OPTIONAL :: lineStyle
  !! Style of vectors' lines
  REAL(wp), OPTIONAL :: lineWidth
  !! Width of vectors' lines

  REAL(pp), DIMENSION(:), ALLOCATABLE :: xl, yl
  REAL(pp), DIMENSION(:, :), ALLOCATABLE :: ul, vl, sl
  REAL(pp), DIMENSION(2) :: xb, yb, sb, cb, d
  REAL(pp) :: scalingl, scl, mag, clr
  INTEGER :: i, j

  xl = localize(x)
  yl = localize(y)
  ul = localize(u)
  vl = localize(v)

  d = REAL([x(2) - x(1), y(2) - y(1)], pp)

  xb = REAL(mixval(x), pp)
  yb = REAL(mixval(y), pp)
  IF (PRESENT(s)) THEN
    sl = localize(s)
    sl = sl / MAXVAL(sl)
  ELSE
    sl = localize(u**2 + v**2)
    sl = SQRT(sl)
    sl = sl / MAXVAL(sl)
  END IF
  sb = [MINVAL(sl), MAXVAL(sl)]
  cb = 0.0_WP
  IF (PRESENT(c)) cb = REAL([MINVAL(c), MAXVAL(c)], pp)

  scalingl = 1.0_PP
  IF (PRESENT(scaling)) scalingl = REAL(scaling, pp)

  IF (PRESENT(lineColor)) CALL setColor(lineColor)
  IF (PRESENT(lineStyle)) CALL setLineStyle(lineStyle)
  IF (PRESENT(lineWidth)) CALL setLineWidth(lineWidth)

  DO i = 1, SIZE(u, 1)
    DO j = 1, SIZE(u, 2)
      mag = NORM2([ul(i, j), vl(i, j)])
      scl = scalingl * NORM2(d) * sl(i, j)
      IF (ABS(scl) < 1.0E-5_WP) CYCLE
      IF (PRESENT(c)) THEN
        clr = REAL((c(i, j) - cb(1)) / (cb(2) - cb(1)), pp)
        clr = MAX(clr, 0.0_PP)
        clr = MIN(clr, 1.0_PP)
        CALL plcol1(clr)
      END IF
    CALL plvect(ul(i:i, j:j) / mag, vl(i:i, j:j) / mag, scl, xl(i:i), yl(j:j))
    END DO
  END DO

  CALL resetPen()
END SUBROUTINE quiver

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Create a bar graph
SUBROUTINE bar(x, y, c, relWidth, fillColor, fillPattern,&
  & lineColor, lineWidth)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! x-positions of the bars' centers
  REAL(wp), DIMENSION(:), INTENT(in) :: y
  !! y-positions of the bars' tops
  REAL(wp), DIMENSION(:), INTENT(in), OPTIONAL :: c
  !! Color scale for bars
  REAL(wp), INTENT(in), OPTIONAL :: relWidth
  !! Relative width of bars (default 0.8)
  CHARACTER(*), INTENT(in), OPTIONAL :: fillColor
  !! Color of bar fills
  CHARACTER(*), INTENT(in), OPTIONAL :: fillPattern
  !! Pattern of bar fills
  CHARACTER(*), INTENT(in), OPTIONAL :: lineColor
  !! Color of lines around bars
  REAL(wp), OPTIONAL :: lineWidth
  !! Width of lines around bars

  REAL(pp), DIMENSION(4) :: xl, yl
  REAL(pp), DIMENSION(2) :: cb
  REAL(pp) :: dx, dxs
  INTEGER :: k

  cb = 0.0_WP
  IF (PRESENT(c)) cb = REAL(mixval(c), pp)
  dxs = 0.8_PP
  IF (PRESENT(relWidth)) dxs = REAL(relWidth, pp)
  IF (SIZE(x) > 1) THEN
    dx = dxs * REAL(x(2) - x(1), pp) / 2.0_PP
  ELSE
    dx = dxs
  END IF

  IF (PRESENT(lineWidth)) CALL setLineWidth(lineWidth)

  DO k = 1, SIZE(x)
    xl = REAL([x(k) - dx, x(k) - dx, x(k) + dx, x(k) + dx], pp)
    yl = REAL([0.0_WP, y(k), y(k), 0.0_WP], pp)

    IF (PRESENT(fillColor)) CALL setColor(fillColor)
    IF (PRESENT(fillPattern)) CALL setFillPattern(fillPattern)
    IF (PRESENT(c)) CALL plcol1(REAL((c(k) - cb(1)) / (cb(2) - cb(1)), pp))
    CALL plfill(xl, yl)

    IF (PRESENT(lineColor)) CALL setColor(lineColor)
    CALL plline(xl, yl)
  END DO
  CALL resetPen()
END SUBROUTINE bar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Create a horizontal bar graph
SUBROUTINE barh(y, x, c, relWidth, fillColor, fillPattern, &
  & lineColor, lineWidth)
  REAL(wp), DIMENSION(:), INTENT(in) :: y
  !! y-positions of the bars' centers
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! x-positions of the bars' tops
  REAL(wp), DIMENSION(:), INTENT(in), OPTIONAL :: c
  !! Color scale for bars
  REAL(wp), INTENT(in), OPTIONAL :: relWidth
  !! Relative width of bars
  CHARACTER(*), INTENT(in), OPTIONAL :: fillColor
  !! Color of bar fills
  CHARACTER(*), INTENT(in), OPTIONAL :: fillPattern
  !! Pattern of bar fills
  CHARACTER(*), INTENT(in), OPTIONAL :: lineColor
  !! Color of lines around bars
  REAL(wp), OPTIONAL :: lineWidth
  !! Width of lines around bars

  REAL(pp), DIMENSION(4) :: xl, yl
  REAL(pp), DIMENSION(2) :: cb
  REAL(pp) :: dy, dys
  INTEGER :: k

  cb = 0.0_WP
  IF (PRESENT(c)) cb = REAL(mixval(c), pp)
  dys = 0.8_PP
  IF (PRESENT(relWidth)) dys = REAL(relWidth, pp)
  dy = dys * REAL(y(2) - y(1), pp) / 2.0_PP

  IF (PRESENT(lineWidth)) CALL setLineWidth(lineWidth)

  DO k = 1, SIZE(x)
    yl = REAL([y(k) - dy, y(k) - dy, y(k) + dy, y(k) + dy], pp)
    xl = REAL([0.0_WP, x(k), x(k), 0.0_WP], pp)

    IF (PRESENT(fillColor)) CALL setColor(fillColor)
    IF (PRESENT(fillPattern)) CALL setFillPattern(fillPattern)
    IF (PRESENT(c)) CALL plcol1(REAL((c(k) - cb(1)) / (cb(2) - cb(1)), pp))
    CALL plfill(xl, yl)

    IF (PRESENT(lineColor)) CALL setColor(lineColor)
    CALL plline(xl, yl)
  END DO
  CALL resetPen()
END SUBROUTINE barh

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Fill space between two lines
!!
!! TODO: describe the arguments
SUBROUTINE fillBetween(x, y1, y0, fillColor, fillPattern, lineWidth)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  REAL(wp), DIMENSION(:), INTENT(in) :: y1
  REAL(wp), DIMENSION(:), INTENT(in), OPTIONAL :: y0
  CHARACTER(*), INTENT(in), OPTIONAL :: fillColor
  CHARACTER(*), INTENT(in), OPTIONAL :: fillPattern
  REAL(wp), INTENT(in), OPTIONAL :: lineWidth

  REAL(pp), DIMENSION(:), ALLOCATABLE :: xl, y1l, y0l
  INTEGER :: N

  N = SIZE(x)

  xl = localize(x)
  y1l = localize(y1)
  IF (PRESENT(y0)) THEN
    y0l = localize(y0)
  ELSE
    ALLOCATE (y0l(N))
    y0l = 0.0_PP
  END IF

  IF (PRESENT(fillColor)) CALL setColor(fillColor)
  IF (PRESENT(fillPattern)) CALL setFillPattern(fillPattern)
  IF (PRESENT(lineWidth)) CALL setLineWidth(lineWidth)
  CALL plfill([xl(1:N:1), xl(N:1:-1)], [y1l(1:N:1), y0l(N:1:-1)])
  CALL resetPen()
END SUBROUTINE fillBetween

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Fill space between two lines
!!
!! TODO: describe the arguments
SUBROUTINE fillBetweenx(y, x1, x0, fillColor, fillPattern, lineWidth)
  REAL(wp), DIMENSION(:), INTENT(in) :: y
  REAL(wp), DIMENSION(:), INTENT(in) :: x1
  REAL(wp), DIMENSION(:), INTENT(in), OPTIONAL :: x0
  CHARACTER(*), INTENT(in), OPTIONAL :: fillColor
  CHARACTER(*), INTENT(in), OPTIONAL :: fillPattern
  REAL(wp), INTENT(in), OPTIONAL :: lineWidth

  REAL(pp), DIMENSION(:), ALLOCATABLE :: yl, x1l, x0l
  INTEGER :: N

  N = SIZE(y)

  yl = localize(y)
  x1l = localize(x1)
  IF (PRESENT(x0)) THEN
    x0l = localize(x0)
  ELSE
    ALLOCATE (x0l(N))
    x0l = 0.0_PP
  END IF

  IF (PRESENT(fillColor)) CALL setColor(fillColor)
  IF (PRESENT(fillPattern)) CALL setFillPattern(fillPattern)
  IF (PRESENT(lineWidth)) CALL setLineWidth(lineWidth)
  CALL plfill([x1l(1:N:1), x0l(N:1:-1)], [yl(1:N:1), yl(N:1:-1)])
  CALL resetPen()
END SUBROUTINE fillBetweenx

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot error bars for a set of data points
SUBROUTINE errorbar(x, y, xerr, yerr, lineColor, lineStyle, lineWidth)
  REAL(wp), DIMENSION(:), INTENT(in) :: x
  !! x-data for plot
  REAL(wp), DIMENSION(:), INTENT(in) :: y
  !! y-data for plot
  REAL(wp), DIMENSION(:), INTENT(in), OPTIONAL :: xerr
  !! x-data error for plot
  REAL(wp), DIMENSION(:), INTENT(in), OPTIONAL :: yerr
  !! y-data error for plot
  CHARACTER(*), INTENT(in), OPTIONAL :: lineColor
  !! Color of line
  CHARACTER(*), INTENT(in), OPTIONAL :: lineStyle
  !! Style of line; '' for no line
  REAL(wp), INTENT(in), OPTIONAL :: lineWidth
  !! Width of line

  REAL(pp), DIMENSION(:), ALLOCATABLE :: xl, yl
  REAL(pp), DIMENSION(:), ALLOCATABLE :: xll, xlh
  REAL(pp), DIMENSION(:), ALLOCATABLE :: yll, ylh

  xl = localize(x)
  yl = localize(y)

  IF (PRESENT(lineColor)) CALL setColor(lineColor)
  IF (PRESENT(lineWidth)) CALL setLineWidth(lineWidth)
  IF (PRESENT(lineStyle)) CALL setLineStyle(lineStyle)

  IF (PRESENT(xerr)) THEN
    xll = localize(x - xerr)
    xlh = localize(x + xerr)
    CALL plerrx(xll, xlh, yl)
  END IF

  IF (PRESENT(yerr)) THEN
    yll = localize(y - yerr)
    ylh = localize(y + yerr)
    CALL plerry(xl, yll, ylh)
  END IF

  CALL resetPen()
END SUBROUTINE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Drawing Pen Routines
!! Reset pen to default state
SUBROUTINE resetPen

  CALL setColor('')
  CALL setLineStyle('')
  CALL setLineWidth(0.5_WP)
  CALL setFillPattern('')
  CALL plschr(0.0_PP, REAL(fontScale, pp))
  CALL plssym(0.0_PP, REAL(fontScale, pp))
END SUBROUTINE resetPen

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE setLineWidth(lineWidth)
  REAL(wp), INTENT(in) :: lineWidth

  CALL plwidth(REAL(lineWidth, pp))
END SUBROUTINE setLineWidth

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the current pen line style
SUBROUTINE setLineStyle(style)
  CHARACTER(*), INTENT(in) :: style !! Style to set

  CALL pllsty(getLineStyleCode(style))
END SUBROUTINE setLineStyle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Return the code for a line style
FUNCTION getLineStyleCode(style) RESULT(code)
  CHARACTER(*), INTENT(in) :: style !! Style desired
  INTEGER :: code

  SELECT CASE (style)
  CASE ('-')
    code = 1
  CASE (':')
    code = 2
  CASE ('--')
    code = 3
  CASE default
    code = 1
  END SELECT
END FUNCTION getLineStyleCode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Return the code for a symbol style
FUNCTION getSymbolCode(style) RESULT(code)
  CHARACTER(*), INTENT(in) :: style !! Style desired
  CHARACTER(32) :: code

  SELECT CASE (style)
  CASE (PS_PLUS)
    code = CODE_PS_PLUS
  CASE (PS_CROSS)
    code = CODE_PS_CROSS
  CASE (PS_ASTERIC)
    code = CODE_PS_ASTERIC
  CASE (PS_DOT)
    code = CODE_PS_DOT
  CASE (PS_SQUARE)
    code = CODE_PS_SQUARE
  CASE (PS_STAR)
    code = CODE_PS_STAR
  CASE (PS_H_CIRCLE)
    code = CODE_PS_H_CIRCLE
  CASE (PS_H_SQUARE)
    code = CODE_PS_H_SQUARE
  CASE (PS_H_TRIAG_U)
    code = CODE_PS_H_TRIAG_U
  CASE (PS_H_DIAMOND)
    code = CODE_PS_H_DIAMOND
  CASE (PS_TRIAG_U)
    code = CODE_PS_TRIAG_U
  CASE (PS_TRIAG_L)
    code = CODE_PS_TRIAG_L
  CASE (PS_TRIAG_D)
    code = CODE_PS_TRIAG_D
  CASE (PS_TRIAG_R)
    code = CODE_PS_TRIAG_R
  CASE default
    code = '#(143)'
  END SELECT
END FUNCTION getSymbolCode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE setFillPattern(style)
  CHARACTER(*), INTENT(in) :: style

  CALL plpsty(getFillCode(style))
END SUBROUTINE setFillPattern

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION getFillCode(style) RESULT(code)
  CHARACTER(*), INTENT(in) :: style
  INTEGER :: code

  SELECT CASE (style)
  CASE ('')
    code = 0
  CASE ('-')
    code = 1
  CASE ('/')
    code = 3
  CASE ('|')
    code = 2
  CASE ('\')
    code = 4
  CASE ('#')
    code = 7
  CASE ('x')
    code = 8
  CASE default
    code = 0
  END SELECT
END FUNCTION getFillCode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the current pen color
SUBROUTINE setColor(color)
  CHARACTER(*), INTENT(in) :: color !! Name of color to set

  INTEGER :: ios
  REAL(pp) :: v

  READ (color, *, iostat=ios) v
  IF (ios == 0) THEN
    CALL plcol1(v)
  ELSE
    CALL plcol0(getColorCode(color))
  END IF
END SUBROUTINE setColor

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION getColorCode(color) RESULT(code)
  CHARACTER(*), INTENT(in) :: color
  INTEGER :: code

  SELECT CASE (color)
  CASE ('w', 'white')
    IF (blackOnWhite) THEN
      code = 1
    ELSE
      code = 2
    END IF
  CASE ('k', 'black')
    IF (blackOnWhite) THEN
      code = 2
    ELSE
      code = 1
    END IF
  CASE ('r', 'red')
    code = 3
  CASE ('g', 'green')
    code = 4
  CASE ('b', 'blue')
    code = 5
  CASE ('c', 'cyan')
    code = 6
  CASE ('m', 'magenta')
    code = 7
  CASE ('y', 'yellow')
    code = 8
  CASE ('fg')
    code = 2
  CASE ('bg')
    code = 1
  CASE default
    code = 2
  END SELECT

  code = code - 1
END FUNCTION getColorCode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Library Status Routines

!! Setup PlPlot library, optionally overriding defaults
SUBROUTINE setup(device, fileName, fontScaling, whiteOnBlack, &
  & transparent, colormap, figSize, isFileFamily)
  CHARACTER(*), INTENT(in), OPTIONAL :: device
  !! Output device to use
  !!
  !! * qtwidget
  !! * svgqt
  !! * pngqt
  CHARACTER(*), INTENT(in), OPTIONAL :: fileName
  !! Name of file(s) to write to
  !!
  !! The text `%n` will be replaced with the figure number
  REAL(wp), INTENT(in), OPTIONAL :: fontScaling
  !! Font scaling relative to default value
  LOGICAL, INTENT(in), OPTIONAL :: whiteOnBlack
  !! Default foreground and background colors
  LOGICAL, INTENT(in), OPTIONAL :: transparent
  !! Transparent background
  CHARACTER(*), INTENT(in), OPTIONAL :: colormap
  !! Colormap to use
  INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: figSize
  !! Size of figures to produce in pixels
  LOGICAL, OPTIONAL, INTENT(in) :: isFileFamily

  CHARACTER(64) :: bufx, bufy
  INTEGER :: ios

  IF (PRESENT(device)) THEN
    CALL plsdev(device)
  ELSE
    CALL plsdev(default_dev)
  END IF
  !!
  !! changed by vikas sharma,
  IF (PRESENT(isFileFamily)) THEN
    IF (isFileFamily) THEN
      CALL plsfam(1, 1, 100)
    END IF
  END IF
  !!
  IF (PRESENT(fileName)) THEN
    CALL plsfnam(fileName)
  ELSE
    CALL plsfnam('out')
  END IF

  IF (PRESENT(whiteOnBlack)) blackOnWhite = .NOT. whiteOnBlack

  IF (PRESENT(transparent)) transparentBackground = transparent

  CALL setIndexedColors()

  IF (PRESENT(colormap)) THEN
    CALL setColormap(colormap)
  ELSE
    CALL setColormap('CoolWarm')
  END IF

  CALL plfontld(0)
  IF (PRESENT(fontScaling)) fontScale = REAL(fontScaling, pp)

  IF (PRESENT(figSize)) THEN
    WRITE (bufx, *) figSize(1)
    WRITE (bufy, *) figSize(2)
    ios = plsetopt('geometry', TRIM(ADJUSTL(bufx))//'x'//TRIM(ADJUSTL(bufy)))
  ELSE
    ios = plsetopt('geometry', '640x480')
  END IF

  CALL plinit()

  CALL resetPen()
END SUBROUTINE setup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Show the plots end finialize the PlPlot library
SUBROUTINE show
  IF (.NOT. didShow) THEN
    CALL plend()
    didShow = .TRUE.
  END IF
END SUBROUTINE show

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Color Map Routines
!! Setup the indexed colors
SUBROUTINE setIndexedColors
  INTEGER, DIMENSION(8, 3) :: rgb
  REAL(pp), DIMENSION(8) :: a

  rgb(getColorCode('w') + 1, :) = [255, 255, 255] ! White
  rgb(getColorCode('k') + 1, :) = [0, 0, 0] ! Black
  rgb(getColorCode('r') + 1, :) = [255, 0, 0] ! Red
  rgb(getColorCode('g') + 1, :) = [0, 255, 0] ! Green
  rgb(getColorCode('b') + 1, :) = [0, 0, 255] ! Blue
  rgb(getColorCode('c') + 1, :) = [0, 255, 255] ! Cyan
  rgb(getColorCode('m') + 1, :) = [255, 0, 255] ! Magenta
  rgb(getColorCode('y') + 1, :) = [255, 255, 0] ! Yellow

  a = 1.0_pp
  IF (transparentBackground) a(1) = 0.0_pp

  CALL plscmap0a(rgb(:, 1), rgb(:, 2), rgb(:, 3), a)
END SUBROUTINE setIndexedColors

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the continuous colormap
SUBROUTINE setColormap(colormap)
  CHARACTER(*), INTENT(in) :: colormap !! Name of colormap to use

  REAL(pp), DIMENSION(:), ALLOCATABLE :: i, h, s, v

  SELECT CASE (colormap)
  CASE ('CoolWarm')
    h = [240.0, 195.0, 45.0, 0.0]

    s = [0.60, 0.95, 0.95, 0.60]
    v = [0.80, 0.30, 0.30, 0.80]
    i = [0.00, 0.50, 0.50, 1.00]

    CALL plscmap1n(256)
    CALL plscmap1l(.FALSE., i, h, s, v)
  CASE ('Gray')
    CALL plspal1('cmap1_gray.pal', .TRUE.)
  CASE ('BlueYellow')
    CALL plspal1('cmap1_blue_yellow.pal', .TRUE.)
  CASE ('BlueRed')
    CALL plspal1('cmap1_blue_red.pal', .TRUE.)
  CASE ('Radar')
    CALL plspal1('cmap1_radar.pal', .TRUE.)
  CASE ('HighFreq')
    CALL plspal1('cmap1_highfreq.pal', .TRUE.)
  CASE ('LowFreq')
    CALL plspal1('cmap1_lowfreq.pal', .TRUE.)
  END SELECT
END SUBROUTINE setColormap

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE EasyPlplot
