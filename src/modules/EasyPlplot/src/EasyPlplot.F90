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
module EasyPlplot
use GlobalData, ONLY: wp => DFP, I4B, LGT

#ifdef USE_PLPLOT
use plplot
#endif

use EasyPlplot_Utilities

implicit none
private

#ifdef USE_PLPLOT
integer( I4B ), parameter :: pp = plflt
#else
integer( I4B ), parameter :: pp = wp
#endif

character(*), parameter :: default_dev = 'qtwidget'
!! Default output device

CHARACTER( LEN = * ), PARAMETER, PUBLIC :: PS_PLUS="+"
CHARACTER( LEN = * ), PARAMETER, PUBLIC :: PS_CROSS="x"
CHARACTER( LEN = * ), PARAMETER, PUBLIC :: PS_ASTERIC="*"
CHARACTER( LEN = * ), PARAMETER, PUBLIC :: PS_DOT="."
CHARACTER( LEN = * ), PARAMETER, PUBLIC :: PS_SQUARE="s"
CHARACTER( LEN = * ), PARAMETER, PUBLIC :: PS_STAR="star"
CHARACTER( LEN = * ), PARAMETER, PUBLIC :: PS_H_CIRCLE="h_circle"
CHARACTER( LEN = * ), PARAMETER, PUBLIC :: PS_H_SQUARE="h_square"
CHARACTER( LEN = * ), PARAMETER, PUBLIC :: PS_H_TRIAG_U="h_triag_u"
CHARACTER( LEN = * ), PARAMETER, PUBLIC :: PS_H_DIAMOND="h_diamond"
CHARACTER( LEN = * ), PARAMETER, PUBLIC :: PS_TRIAG_U="triag_u"
CHARACTER( LEN = * ), PARAMETER, PUBLIC :: PS_TRIAG_L="triag_l"
CHARACTER( LEN = * ), PARAMETER, PUBLIC :: PS_TRIAG_D="triag_d"
CHARACTER( LEN = * ), PARAMETER, PUBLIC :: PS_TRIAG_R="triag_r"
CHARACTER( LEN = * ), PARAMETER :: CODE_PS_PLUS="#(140)"
CHARACTER( LEN = * ), PARAMETER :: CODE_PS_CROSS="#(141)"
CHARACTER( LEN = * ), PARAMETER :: CODE_PS_ASTERIC="#(142)"
CHARACTER( LEN = * ), PARAMETER :: CODE_PS_DOT="#(143)"
CHARACTER( LEN = * ), PARAMETER :: CODE_PS_SQUARE="#(144)"
CHARACTER( LEN = * ), PARAMETER :: CODE_PS_STAR="#(145)"
CHARACTER( LEN = * ), PARAMETER :: CODE_PS_H_CIRCLE="#(840)" !135
CHARACTER( LEN = * ), PARAMETER :: CODE_PS_H_SQUARE="#(841)"
CHARACTER( LEN = * ), PARAMETER :: CODE_PS_H_TRIAG_U="#(842)"
CHARACTER( LEN = * ), PARAMETER :: CODE_PS_H_DIAMOND="#(843)"
CHARACTER( LEN = * ), PARAMETER :: CODE_PS_TRIAG_U="#(852)"
CHARACTER( LEN = * ), PARAMETER :: CODE_PS_TRIAG_L="#(853)"
CHARACTER( LEN = * ), PARAMETER :: CODE_PS_TRIAG_D="#(854)"
CHARACTER( LEN = * ), PARAMETER :: CODE_PS_TRIAG_R="#(855)"


! Library State
logical( LGT ) :: didShow = .false.
!! Flag for library display status
real(pp) :: fontScale = 1.0_pp
!! Font scale factor to resetPen
logical( LGT ) :: blackOnWhite = .true.
!! Reverse black and white
logical( LGT ) :: transparentBackground = .false.
!! Transparent background

! Interfaces
interface localize
  module procedure localize_1
  module procedure localize_2
end interface

! Exports
public :: setup
! interface created
public :: show
! interface created
public :: figure
! interface created
public :: subplot
! interface created
public :: xylim, xlim, ylim, xyzlim !
! interface created
public :: labels, xlabel, ylabel, title
! interface created
public :: ticks, xticks, yticks, box
! interface created
public :: legend
! interface created

! public :: mixval, linspace
public :: binData
!!
public :: plot
! interface created
public :: plot3
! interface created
public :: scatter
! interface created
public :: errorbar
! interface created
!!
public :: contour
! interface created
public :: contourf
! interface created
public :: colorbar
! interface created
public :: colorbar2
! interface created
public :: bar
! interface created
public :: barh
! interface created
public :: hist
! interface created
public :: fillBetween
! interface created
public :: fillBetweenx
! interface created
public :: quiver
! interface created
public :: surface
! interface created
public :: wireframe
! interface created

contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Count data in each bin
function binData(d, N, db, normalize) result(o)
  real(wp), dimension(:), intent(in) :: d
  !! Data for binning
  integer, intent(in), optional :: N
  !! Number of bins
  real(wp), dimension(2), intent(in), optional :: db
  !! Boundaries of bin range
  integer, intent(in), optional :: normalize
  !! Normalization type (1=sum, 2=bin size, 3=maxval)
  real(wp), dimension(:, :), allocatable :: o

  real(wp), dimension(:), allocatable :: b
  integer :: Nl, k

  Nl = 10
  if (present(N)) Nl = N

  if (present(db)) then
    b = linspace(db(1), db(2), Nl + 1)
  else
    b = linspace(minval(d) - epsilon(1.0_wp), &
      & maxval(d) + epsilon(1.0_wp), Nl + 1)
  end if

  allocate (o(Nl, 2))
  o(:, 1) = (b(1:Nl) + b(2:Nl + 1)) / 2.0_wp

  do k = 1, Nl
    o(k, 2) = real(count(d >= b(k) .and. d <= b(k + 1)), wp)
  end do

  if (present(normalize)) then
    select case (normalize)
    case (1)
      o(:, 2) = o(:, 2) / sum(o(:, 2))
    case (2)
      do k = 1, Nl
        o(k, 2) = o(k, 2) / (b(k + 1) - b(k))
      end do
    case (3)
      o(:, 2) = o(:, 2) / maxval(o(:, 2))
    end select
  end if
end function binData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

function localize_1(A) result(o)
  real(wp), dimension(:), intent(in) :: A
  real(pp), dimension(:), allocatable :: o
  integer :: N, k

  N = size(A)
  allocate (o(N))
  forall (k=1:N) o(k) = real(A(k), pp)
end function localize_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

function localize_2(A) result(o)
  real(wp), dimension(:, :), intent(in) :: A
  real(pp), dimension(:, :), allocatable :: o
  integer :: N, M, i, j

  N = size(A, 1)
  M = size(A, 2)
  allocate (o(N, M))
  forall (i=1:N, j=1:M) o(i, j) = real(A(i, j), pp)
end function localize_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Axes and Figure Routines
!! Create a new figure
subroutine figure
  logical, save :: isFirst = .true.

  if (.not. isFirst) then
    call pleop()
  else
    isFirst = .false.
  end if

  call plbop()
  call plssub(1, 1)
  call pladv(1)
  call resetPen()
end subroutine figure

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Create a set of axes on a figure
subroutine subplot(ny, nx, i, aspect, is3d)
  integer, intent(in) :: nx
  !! Number of subplot columns
  integer, intent(in) :: ny
  !! Number of subplot rows
  integer, intent(in) :: i
  !! Subplot to use
  real(wp), intent(in), optional :: aspect
  !! Aspect ratio of the axes
  logical, intent(in), optional :: is3d

  logical :: is3dl

  call plssub(nx, ny)
  call pladv(i)
  call resetPen()

  is3dl = .false.
  if (present(is3d)) is3dl = is3d

  if (is3dl) then
    call plvpor(0.0_pp, 1.0_pp, 0.0_pp, 1.0_pp)
  else
    if (present(aspect)) then
      call plvasp(real(aspect, pp))
    else
      call plvsta()
    end if
  end if

  call defaultLim()
end subroutine subplot

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine defaultLim
  real(pp), parameter :: eps = epsilon(1.0_pp)

  call plwind(-eps, eps, -eps, eps)
end subroutine defaultLim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the x and y ranges of the plot
subroutine xylim(xb, yb)
  real(wp), dimension(2), intent(in) :: xb
  !! x-range of plot
  real(wp), dimension(2), intent(in) :: yb
  !! y-range of plot

  real(pp), dimension(2) :: xbl, ybl

  xbl = localize(xb)
  ybl = localize(yb)

  call plwind(xbl(1), xbl(2), ybl(1), ybl(2))
end subroutine xylim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the limits of the x-axis
subroutine xlim(xl, xh)
  real(wp), intent(in) :: xl, xh

  real(pp) :: x1, x2, y1, y2

  call plgvpw(x1, x2, y1, y2)
  call plwind(real(xl, pp), real(xh, pp), y1, y2)
end subroutine xlim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the limits of the y-axis
subroutine ylim(yl, yh)
  real(wp), intent(in) :: yl, yh

  real(pp) :: x1, x2, y1, y2

  call plgvpw(x1, x2, y1, y2)
  call plwind(x1, x2, real(yl, pp), real(yh, pp))
end subroutine ylim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the limits for a 3d plot
subroutine xyzlim(xb, yb, zb, altitude, azimuth, zoom)
  real(wp), dimension(2), intent(in) :: xb
  !! x-range of plot
  real(wp), dimension(2), intent(in) :: yb
  !! y-range of plot
  real(wp), dimension(2), intent(in) :: zb
  !! z-range of plot
  real(wp), intent(in), optional :: altitude
  !! Altitude angle of plot in degrees
  real(wp), intent(in), optional :: azimuth
  !! Azimuth angle of plot in degrees
  real(wp), intent(in), optional :: zoom
  !! Zoom ratio (default 1.0)

  real(pp) :: al, az, zm

  al = 45.0_pp
  if (present(altitude)) al = real(altitude, pp)
  az = 60.0_pp
  if (present(azimuth)) az = real(azimuth, pp)
  zm = 1.0_pp
  if (present(zoom)) zm = real(zoom, pp)

  call plwind(-1.0_pp, 1.0_pp, -1.0_pp, 1.5_pp)
  call plw3d(zm, zm, 1.2_pp * zm, &
  & real(xb(1), pp), real(xb(2), pp), &
  & real(yb(1), pp), real(yb(2), pp), &
  & real(zb(1), pp), real(zb(2), pp), al, az)
end subroutine xyzlim

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the ticks for the axes
subroutine ticks(dx, dy, logx, logy, color, lineWidth)
  real(wp), intent(in), optional :: dx
  !! Spacing between ticks on x-axis
  real(wp), intent(in), optional :: dy
  !! Spacing between ticks on y-axis
  logical, intent(in), optional :: logx
  !! Flag for log-ticks and labels on x-axis
  logical, intent(in), optional :: logy
  !! Flag for log-ticks and labels on y-axis
  character(*), intent(in), optional :: color
  !! Color code for ticks, box, and labels
  real(wp), optional :: linewidth
  !! Line width for ticks and box

  real(pp) :: dxl, dyl
  character(10) :: xopts, yopts

  dxl = 0.0_pp
  if (present(dx)) dxl = real(dx, pp)

  dyl = 0.0_pp
  if (present(dy)) dyl = real(dy, pp)

  xopts = 'bcnst'
  if (present(logx)) then
    if (logx) xopts = 'bcnstl'
  end if

  yopts = 'bcnstv'
  if (present(logy)) then
    if (logy) yopts = 'bcnstvl'
  end if

  call resetPen()

  if (present(color)) call setColor(color)
  if (present(lineWidth)) call setLineWidth(lineWidth)

  call plbox(xopts, dxl, 0, yopts, dyl, 0)
  call resetPen()
end subroutine ticks

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set x,y and plot labels
subroutine box(xLabel, yLabel, zLabel, color)
  character(*), intent(in) :: xLabel
  !! Label for x-axis
  character(*), intent(in) :: yLabel
  !! Label for x-axis
  character(*), intent(in) :: zLabel
  !! Label for z-axis
  character(*), intent(in), optional :: color
  !! Color of labels

  if (present(color)) call setColor(color)
  call plbox3('bnstu', xLabel, 0.0_pp, 0, 'bnstu', yLabel, &
    & 0.0_pp, 0, 'bnstu', zLabel, 0.0_pp, 0)
  call resetPen()
end subroutine box

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the ticks for the x-axis
subroutine xticks(d, logScale, primary, secondary, color, lineWidth)
  real(wp), intent(in), optional :: d
  !! Spacing between ticks
  logical, intent(in), optional :: logScale
  !! Flag for log-ticks and labels
  logical, intent(in), optional :: primary
  !! Draw primary axis
  logical, intent(in), optional :: secondary
  !! Draw secondary axis
  character(*), intent(in), optional :: color
  !! Color code for ticks, box, and labels
  real(wp), optional :: linewidth
  !! Line width for ticks and box
  real(pp) :: dxl, dyl
! interface created
  character(10) :: xopts, yopts

  dxl = 0.0_pp
  dyl = 0.0_pp
  if (present(d)) dxl = real(d, pp)

  xopts = 'nst'

  if (present(primary)) then
    if (primary) xopts = trim(xopts)//'b'
  else
    xopts = trim(xopts)//'b'
  end if

  if (present(secondary)) then
    if (secondary) xopts = trim(xopts)//'c'
  else
    xopts = trim(xopts)//'c'
  end if

  if (present(logScale)) then
    if (logScale) xopts = trim(xopts)//'l'
  end if
  yopts = ''

  if (present(color)) call setColor(color)
  if (present(lineWidth)) call setLineWidth(lineWidth)
  call plbox(xopts, dxl, 0, yopts, dyl, 0)
  call resetPen()
end subroutine xticks

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the ticks for the y-axis
subroutine yticks(d, logScale, primary, secondary, color, lineWidth)
  real(wp), intent(in), optional :: d
  !! Spacing between ticks
  logical, intent(in), optional :: logScale
  !! Flag for log-ticks and labels
  logical, intent(in), optional :: primary
  !! Draw primary axis
  logical, intent(in), optional :: secondary
  !! Draw secondary axis
  character(*), intent(in), optional :: color
  !! Color code for ticks, box, and labels
  real(wp), optional :: linewidth
  !! Line width for ticks and box
  real(pp) :: dxl, dyl
  character(10) :: xopts, yopts

  dxl = 0.0_pp
  dyl = 0.0_pp
  if (present(d)) dyl = real(d, pp)

  yopts = 'nst'

  if (present(primary)) then
    if (primary) yopts = trim(xopts)//'b'
  else
    yopts = trim(yopts)//'b'
  end if

  if (present(secondary)) then
    if (secondary) yopts = trim(yopts)//'c'
  else
    yopts = trim(yopts)//'c'
  end if

  if (present(logScale)) then
    if (logScale) yopts = trim(yopts)//'l'
  end if
  xopts = ''

  if (present(color)) call setColor(color)
  if (present(lineWidth)) call setLineWidth(lineWidth)
  call plbox(xopts, dxl, 0, yopts, dyl, 0)
  call resetPen()
end subroutine yticks

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set x,y and plot labels
subroutine labels(xLabel, yLabel, plotLabel, color)
  character(*), intent(in) :: xLabel
  !! Label for x-axis
  character(*), intent(in) :: yLabel
  !! Label for y-axis
  character(*), intent(in) :: plotLabel
  !! Label entire plot
  character(*), intent(in), optional :: color
  !! Color of labels

  if (present(color)) call setColor(color)
  call pllab(xLabel, yLabel, plotLabel)
  call resetPen()
end subroutine labels

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set x-label
subroutine xlabel(label, color)
  character(*), intent(in) :: label
  !! Label for axis
  character(*), intent(in), optional :: color
  !! Color of labels

  if (present(color)) call setColor(color)
  call plmtex('b', 3.0_pp, 0.5_pp, 0.5_pp, label)
  call resetPen()
end subroutine xlabel

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set y-label
subroutine ylabel(label, color)
  character(*), intent(in) :: label
  !! Label for axis
  character(*), intent(in), optional :: color
  !! Color of labels

  if (present(color)) call setColor(color)
  call plmtex('l', 5.0_pp, 0.5_pp, 0.5_pp, label)
  call resetPen()
end subroutine ylabel

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set plot title
subroutine title(label, color)
  character(*), intent(in) :: label
  !! Label for plot
  character(*), intent(in), optional :: color
  !! Color of labels

  if (present(color)) call setColor(color)
  call plmtex('t', 1.5_pp, 0.5_pp, 0.5_pp, label)
  call resetPen()
end subroutine title

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Add a colorbar to the top of the plot
subroutine colorbar(z, N, leftLabel, rightLabel)
  real(wp), dimension(:, :), intent(in) :: z
  !! Data used for levels computation
  integer, intent(in) :: N
  !! Number of levels to compute
  character(*), intent(in), optional :: leftLabel
  !! Label for left side of colorbar
  character(*), intent(in), optional :: rightLabel
  !! Label for right side of colorbar

  real(pp), dimension(:, :), allocatable :: values
  character(64), dimension(2) :: labels

  real(pp) :: fill_width
  real(pp) :: cont_width
  integer :: cont_color
  real(pp) :: colorbar_width
  real(pp) :: colorbar_height
  integer :: k

  values = reshape( &
    & real([(real(k - 1, wp) / real(N - 1, wp) * (maxval(z) - minval(z)) + &
    & minval(z), k=1, N)], pp), &
    & [N, 1])

  fill_width = 2.0_pp
  cont_width = 0.0_pp
  cont_color = 1
  labels = ''
  if (present(leftLabel)) labels(1) = leftLabel
  if (present(rightLabel)) labels(2) = rightLabel

  call plcolorbar(colorbar_width, colorbar_height,&
    & ior(PL_COLORBAR_GRADIENT, PL_COLORBAR_SHADE_LABEL), &
    & PL_POSITION_TOP,&
    & 0.0_pp, 0.01_pp, 0.75_pp, 0.05_pp,&
    & 0, 1, 1, 0.0_pp, 0.0_pp, &
    & cont_color, cont_width, &
    & [PL_COLORBAR_LABEL_LEFT, PL_COLORBAR_LABEL_RIGHT], labels, &
    & ['bcvmt'], [0.0_pp], [0], [size(values)], values)
end subroutine colorbar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Add a colorbar to the top of the plot
subroutine colorbar2(z, N, leftLabel, rightLabel)
  real(wp), dimension(:, :), intent(in) :: z
  !! Data used for levels computation
  integer, intent(in) :: N
  !! Number of levels to compute
  character(*), intent(in), optional :: leftLabel
  !! Label for left side of colorbar
  character(*), intent(in), optional :: rightLabel
  !! Label for right side of colorbar

  real(pp), dimension(:, :), allocatable :: values
  character(64), dimension(2) :: labels

  real(pp) :: fill_width
  real(pp) :: cont_width
  integer :: cont_color
  real(pp) :: colorbar_width
  real(pp) :: colorbar_height
  integer :: k

  values = reshape( &
    & real([(real(k - 1, wp) / real(N - 1, wp) * (maxval(z) - minval(z)) + &
    & minval(z), k=1, N)], pp), &
    & [N, 1])

  fill_width = 2.0_pp
  cont_width = 0.0_pp
  cont_color = 1
  labels = ''
  if (present(leftLabel)) labels(1) = leftLabel
  if (present(rightLabel)) labels(2) = rightLabel

  call plcolorbar(colorbar_width, colorbar_height,&
    & ior(PL_COLORBAR_GRADIENT, PL_COLORBAR_SHADE_LABEL), PL_POSITION_RIGHT,&
    & 0.01_pp, 0.0_pp, 0.05_pp, 0.75_pp,&
    & 0, 1, 1, 0.0_pp, 0.0_pp, &
    & cont_color, cont_width, &
    & [PL_COLORBAR_LABEL_BOTTOM, PL_COLORBAR_LABEL_TOP], labels, &
    & ['bcvmt'], [0.0_pp], [0], [size(values)], values)
end subroutine colorbar2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Create legend for plot data
!!
!! FIXME: Text sizing should be modifiable
subroutine legend(corner, series, lineWidths, markScales, markCounts, ncol)
  character(*), intent(in) :: corner
  !! Corner for legend
  character(*), dimension(:, :), intent(in) :: series
  !! Data series in rows
  !! [name,textColor,lineStyle,lineColor,markStyle,markColor,boxColor]
  real(wp), dimension(:), intent(in), optional :: lineWidths
  !! Line widths for the plots
  real(wp), dimension(:), intent(in), optional :: markScales
  !! Marker sizes for the plots
  integer, dimension(:), intent(in), optional :: markCounts
  !! Marker counts for the plots
  integer, intent(in), optional :: ncol
  !! Number of columns

  real(pp) :: width, height, xoff, yoff
  real(pp) :: plotWidth
  integer :: opt, cornerl
  integer :: bg_color, bb_color, bb_style, lncol, lnrow
  integer, dimension(size(series, 1)) :: opts
  real(pp), dimension(size(series, 1)) :: lwidths, mscales
  integer, dimension(size(series, 1)) :: mcounts, text_colors
  real(pp) :: text_offset, text_scale, text_spacing, text_justification
  integer, dimension(size(series, 1)) :: box_colors, box_patterns
  real(pp), dimension(size(series, 1)) :: box_scales, box_line_widths
  integer, dimension(size(series, 1)) :: line_colors, line_styles
  integer, dimension(size(series, 1)) :: mark_colors
  character(64), dimension(size(series, 1)) :: mark_styles
  integer :: k

  call doLegendBox()

  opts = 0
  do k = 1, size(series, 1)
    if (series(k, 3) /= '') opts(k) = ior(opts(k), PL_LEGEND_LINE)
    if (series(k, 5) /= '') opts(k) = ior(opts(k), PL_LEGEND_SYMBOL)
    if (series(k, 7) /= '') opts(k) = ior(opts(k), PL_LEGEND_COLOR_BOX)
  end do

  call doText()
  call doBoxes()
  call doLines()
  call doMarkers()

  call pllegend(width, height, opt, cornerl, xoff, yoff, plotWidth, &
    & bg_color, bb_color, bb_style, &
    & lnrow, lncol, opts, text_offset, &
    & text_scale, text_spacing, text_justification,&
    & text_colors, series(:, 1), &
    & box_colors, box_patterns, box_scales, box_line_widths, &
    & line_colors, line_styles, lwidths, &
    & mark_colors, mscales, mcounts, mark_styles)

  contains

  subroutine doLegendBox
    opt = PL_LEGEND_BACKGROUND + PL_LEGEND_BOUNDING_BOX
    cornerl = getCorner(corner)
    xoff = 0.0_pp
    yoff = 0.0_pp
    plotWidth = 0.05_pp
    bg_color = 0
    bb_color = 1
    bb_style = getLineStyleCode('-')

    lncol = 1
    if (present(ncol)) lncol = ncol
    lnrow = size(series, 1) / lncol
  end subroutine doLegendBox

  subroutine doText
    text_offset = 0.3_pp
    text_scale = fontScale
    text_spacing = 3.0_pp
    text_justification = 0.0_pp

    do k = 1, size(series, 1)
      text_colors = getColorCode(series(k, 2))
    end do
  end subroutine doText

  subroutine doBoxes
    do k = 1, size(series, 1)
      box_colors(k) = getColorCode(series(k, 7))
    end do
    box_patterns = 0
    box_scales = 0.5_pp
    box_line_widths = 0.0_pp
  end subroutine doBoxes

  subroutine doLines
    lwidths = 1.0_pp
    if (present(lineWidths)) lwidths = real(lineWidths, pp)

    do k = 1, size(series, 1)
      line_colors(k) = getColorCode(series(k, 4))
      line_styles(k) = getLineStyleCode(series(k, 3))
    end do
  end subroutine doLines

  subroutine doMarkers
    mcounts = 2
    if (present(markCounts)) mcounts = markCounts
    mscales = 1.0_pp
    if (present(markScales)) mscales = real(markScales, pp)

    do k = 1, size(series, 1)
      mark_colors(k) = getColorCode(series(k, 6))
      mark_styles(k) = getSymbolCode(series(k, 5))
    end do
  end subroutine doMarkers

  function getCorner(text) result(code)
    character(*), intent(in) :: text
    integer :: code

    code = PL_POSITION_INSIDE
    if (startsWith(text, 'upper')) code = code + PL_POSITION_TOP
    if (startsWith(text, 'lower')) code = code + PL_POSITION_BOTTOM
    if (endsWith(text, 'right')) code = code + PL_POSITION_RIGHT
    if (endsWith(text, 'left')) code = code + PL_POSITION_LEFT
  end function getCorner

end subroutine legend

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Plotting Routines

!! Create a histogram
subroutine hist(d, N, db, relWidth, fillColor, fillPattern, &
  & lineColor, lineWidth)
  real(wp), dimension(:), intent(in) :: d
  !! Data for binning
  integer, intent(in), optional :: N
  !! Number of bins
  real(wp), dimension(2), intent(in), optional :: db
  !! Boundaries of bin range
  real(wp), intent(in), optional :: relWidth
  !! Relative width of bars (default 0.8)
  character(*), intent(in), optional :: fillColor
  !! Color of bar fills
  character(*), intent(in), optional :: fillPattern
  !! Pattern of bar fills
  character(*), intent(in), optional :: lineColor
  !! Color of lines around bars
  real(wp), optional :: lineWidth
  !! Width of lines around bars

  real(wp), dimension(:, :), allocatable :: h
  real(wp), dimension(2) :: dbl
  integer :: Nl

  real(wp) :: relWidthl
  real(wp) :: lineWidthl

  Nl = 20
  if (present(N)) Nl = N

  if (present(db)) then
    dbl = db
  else
    dbl = mixval(d) + [-1.0_wp, 1.0_wp] * epsilon(1.0_wp)
  end if

  h = binData(d, Nl, dbl, normalize=3)

  relWidthl = 1.0_wp
  if (present(relWidth)) relWidthl = relWidth
  lineWidthl = 0.5_wp
  if (present(lineWidth)) lineWidthl = lineWidth

  if (present(lineColor)) then
    if (present(fillColor)) then
      if (present(fillPattern)) then
        call bar(h(:, 1), h(:, 2), relWidth=relWidthl, &
          & lineColor=lineColor, &
          & lineWidth=lineWidthl, &
          & fillColor=fillColor, fillPattern=fillPattern)
      else
        call bar(h(:, 1), h(:, 2), relWidth=relWidthl, &
          & lineColor=lineColor, &
          & lineWidth=lineWidthl, &
          & fillColor=fillColor)
      end if
    else
      if (present(fillPattern)) then
        call bar(h(:, 1), h(:, 2), h(:, 2), &
          & relWidth=relWidthl, lineColor=lineColor, &
          & lineWidth=lineWidthl, &
          & fillPattern=fillPattern)
      else
          call bar(h(:, 1), h(:, 2), h(:, 2), &
            & relWidth=relWidthl, lineColor=lineColor, &
            & lineWidth=lineWidthl)
      end if
    end if
  else
    if (present(fillColor)) then
      if (present(fillPattern)) then
        call bar(h(:, 1), h(:, 2), relWidth=relWidthl, &
          & lineWidth=lineWidthl, &
          & fillColor=fillColor, fillPattern=fillPattern)
      else
        call bar(h(:, 1), h(:, 2), relWidth=relWidthl, &
          & lineWidth=lineWidthl, &
          & fillColor=fillColor)
      end if
    else
      if (present(fillPattern)) then
        call bar(h(:, 1), h(:, 2), h(:, 2), relWidth=relWidthl, &
          & lineWidth=lineWidthl, &
          & fillPattern=fillPattern)
      else
        call bar(h(:, 1), h(:, 2), h(:, 2), relWidth=relWidthl, &
          & lineWidth=lineWidthl)
      end if
    end if
  end if

  call resetPen()
end subroutine hist

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Create scatter plot of data
subroutine scatter(x, y, c, s, markColor, markStyle, markSize)
  real(wp), dimension(:), intent(in) :: x
  !! x-coordinates of data
  real(wp), dimension(:), intent(in) :: y
  !! y-coordinates of data
  real(wp), dimension(:), intent(in), optional :: c
  !! Data for smooth coloring
  real(wp), dimension(:), intent(in), optional :: s
  !! Data for marker scaling
  character(*), intent(in), optional :: markColor
  !! Color of markers; overridden by z
  character(*), intent(in), optional :: markStyle
  !! Style of markers
  real(wp), intent(in), optional :: markSize
  !! Size of markers

  real(pp), dimension(:), allocatable :: xl, yl
  real(pp), dimension(:), allocatable :: cb
  character(32) :: code
  integer :: k

  xl = localize(x)
  yl = localize(y)

  if (present(markColor)) call setColor(markColor)
  code = getSymbolCode('')
  if (present(markStyle)) code = getSymbolCode(markStyle)
  if (present(markSize)) call plschr(0.0_pp, real(markSize, pp))
  if (present(markSize)) call plssym(0.0_pp, real(markSize, pp))

  if (present(c)) cb = real(mixval(c), pp)
  do k = 1, size(x)
    if (present(c)) call plcol1(real((c(k) - cb(1)) / (cb(2) - cb(1)), pp))
    if (present(s)) call plschr(0.0_pp, real(s(k), pp))
    if (present(s)) call plssym(0.0_pp, real(s(k), pp))
    call plptex(xl(k), yl(k), 0.0_pp, 0.0_pp, 0.5_pp, code)
  end do
  call resetPen()
end subroutine scatter

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot data using lines and or markers
subroutine plot(x, y, lineColor, lineStyle, lineWidth, &
  & markColor, markStyle, markSize)
  real(wp), dimension(:), intent(in) :: x
  !! x-data for plot
  real(wp), dimension(:), intent(in) :: y
  !! y-data for plot
  character(*), intent(in), optional :: lineColor
  !! Color of line
  character(*), intent(in), optional :: lineStyle
  !! Style of line; '' for no line
  real(wp), intent(in), optional :: lineWidth
  !! Width of line
  character(*), intent(in), optional :: markColor
  !! Color of markers, if any
  character(*), intent(in), optional :: markStyle
  !! Style of markers; '' or absent for none
  real(wp), intent(in), optional :: markSize
  !! Size of markers, if any

  real(pp), dimension(:), allocatable :: xl, yl
  character(32) :: code
  integer :: k

  xl = localize(x)
  yl = localize(y)

  if (present(lineColor)) call setColor(lineColor)
  if (present(lineWidth)) call setLineWidth(lineWidth)
  if (present(lineStyle)) then
    call setLineStyle(lineStyle)
    if (lineStyle /= '') call plline(xl, yl)
  else
    call plline(xl, yl)
  end if
  call resetPen()

  if (present(markColor)) call setColor(markColor)
  if (present(markSize)) call plssym(0.0_pp, real(markSize, pp))
  if (present(markStyle)) then
    code = getSymbolCode(markStyle)
    if (markStyle /= '') then
      do k = 1, size(x)
        call plptex(xl(k), yl(k), 0.0_pp, 0.0_pp, 0.5_pp, code)
      end do
    end if
  end if
  call resetPen()
end subroutine plot

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot data using lines and or markers
subroutine plot3(x, y, z, lineColor, lineStyle, lineWidth,&
  & markColor, markStyle, markSize)
  real(wp), dimension(:), intent(in) :: x
  !! x-data for plot
  real(wp), dimension(:), intent(in) :: y
  !! y-data for plot
  real(wp), dimension(:), intent(in) :: z
  !! z-data for plot
  character(*), intent(in), optional :: lineColor
  !! Color of line
  character(*), intent(in), optional :: lineStyle
  !! Style of line; '' for no line
  real(wp), intent(in), optional :: lineWidth
  !! Width of line
  character(*), intent(in), optional :: markColor
  !! Color of markers, if any
  character(*), intent(in), optional :: markStyle
  !! Style of markers; '' or absent for none
  real(wp), intent(in), optional :: markSize
  !! Size of markers, if any

  real(pp), dimension(:), allocatable :: xl, yl, zl
  real(pp) :: dx, dy, dz, sx, sy, sz
  character(32) :: code
  integer :: k

  xl = localize(x)
  yl = localize(y)
  zl = localize(z)

  if (present(lineColor)) call setColor(lineColor)
  if (present(lineWidth)) call setLineWidth(lineWidth)
  if (present(lineStyle)) then
    call setLineStyle(lineStyle)
    if (lineStyle /= '') call plline(xl, yl)
  else
    call plline3(xl, yl, zl)
  end if
  call resetPen()

  if (present(markColor)) call setColor(markColor)
  if (present(markSize)) call plssym(0.0_pp, real(markSize, pp))
  if (present(markStyle)) then
    code = getSymbolCode(markStyle)
    if (markStyle /= '') then
      dx = 1.0_pp
      dy = 0.0_pp
      dz = 0.0_pp
      sx = 0.0_pp
      sy = 0.0_pp
      sz = 0.0_pp
      do k = 1, size(x)
        call plptex3(xl(k), yl(k), zl(k), dx, dy, dz, sx, sy, &
          & sz, 0.5_pp, code)
      end do
    end if
  end if
  call resetPen()
end subroutine plot3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot contour lines
subroutine contour(x, y, z, N, lineColor, lineStyle, lineWidth)
  real(wp), dimension(:), intent(in) :: x
  !! x-coordinates of data
  real(wp), dimension(:), intent(in) :: y
  !! y-coordinates of data
  real(wp), dimension(:, :), intent(in) :: z
  !! Data for contouring
  integer, intent(in), optional :: N
  !! Number of levels to use in contour
  character(*), intent(in), optional :: lineColor
  !! Color of contour lines
  character(*), intent(in), optional :: lineStyle
  !! Style of contour lines
  real(wp), optional :: lineWidth
  !! Width of contour lines

  real(pp), dimension(:), allocatable :: xl, yl
  real(pp), dimension(:, :), allocatable :: zl

  real(pp), dimension(:), allocatable :: edge
  integer :: Nl, k

  xl = localize(x)
  yl = localize(y)
  zl = localize(z)
  Nl = 20
  if (present(N)) Nl = N
  edge = [(real(k - 1, pp)/real(Nl - 1, pp)*(maxval(zl) - minval(zl)) &
    & + minval(zl), k=1, Nl)]

  if (present(lineColor)) call setColor(lineColor)
  if (present(lineStyle)) call setLineStyle(lineStyle)
  if (present(lineWidth)) call setLineWidth(lineWidth)

  call plcont(zl, 1, size(xl), 1, size(yl), edge, xl, yl)
  call resetPen()
end subroutine contour

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot a 3d surface
subroutine surface(x, y, z, N, lineStyle)
  real(wp), dimension(:), intent(in) :: x
  !! x-coordinates of data
  real(wp), dimension(:), intent(in) :: y
  !! y-coordinates of data
  real(wp), dimension(:, :), intent(in) :: z
  !! Data for contouring
  integer, intent(in), optional :: N
  !! Number of levels to use in surface colors
  character(*), intent(in), optional :: lineStyle
  !! Style for xy lines ( '-' = on, '' = off )

  real(pp), dimension(:), allocatable :: xl, yl
  real(pp), dimension(:, :), allocatable :: zl

  real(pp), dimension(:), allocatable :: edge
  integer :: Nl, opt

  opt = MAG_COLOR

  xl = localize(x)
  yl = localize(y)
  zl = localize(z)
  Nl = 20
  if (present(N)) then
    Nl = N
    opt = ior(opt, SURF_CONT)
  end if
  edge = localize(linspace(minval(z), maxval(z), Nl))

  if (present(lineStyle)) then
    select case (lineStyle)
    case ('')
      opt = opt
    case ('-')
      opt = ior(opt, FACETED)
    end select
  end if

  call plsurf3d(xl, yl, zl, opt, edge)
  call resetPen()
end subroutine surface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot a 3d wireframe
subroutine wireframe(x, y, z, lineColor)
  real(wp), dimension(:), intent(in) :: x
  !! x-coordinates of data
  real(wp), dimension(:), intent(in) :: y
  !! y-coordinates of data
  real(wp), dimension(:, :), intent(in) :: z
  !! Data for contouring
  character(*), intent(in), optional :: lineColor
  !! Color of contour lines

  real(pp), dimension(:), allocatable :: xl, yl
  real(pp), dimension(:, :), allocatable :: zl

  xl = localize(x)
  yl = localize(y)
  zl = localize(z)

  if (present(lineColor)) then
    call setColor(lineColor)
    call plot3d(xl, yl, zl, DRAW_LINEXY, .false.)
  else
    call plot3d(xl, yl, zl, ior(DRAW_LINEXY, MAG_COLOR), .false.)
  end if

  call resetPen()
end subroutine wireframe

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot filled contours
subroutine contourf(x, y, z, N)
  real(wp), dimension(:), intent(in) :: x
  !! x-coordinates of data
  real(wp), dimension(:), intent(in) :: y
  !! y-coordinates of data
  real(wp), dimension(:, :), intent(in) :: z
  !! Data for contouring
  integer, intent(in), optional :: N
  !! Number of levels to use in contour

  real(pp), dimension(:), allocatable :: xl, yl
  real(pp), dimension(:, :), allocatable :: zl

  real(pp), dimension(:), allocatable :: edge

  real(pp) :: fill_width
  real(pp) :: cont_width
  integer :: cont_color
  integer :: Nl

  xl = localize(x)
  yl = localize(y)
  zl = localize(z)
  Nl = 20
  if (present(N)) Nl = N

  edge = localize(linspace(minval(z), maxval(z), Nl))

  fill_width = -1.0_pp
  cont_width = -1.0_pp
  cont_color = -1

  call plshades(zl, minval(xl), maxval(xl), minval(yl), maxval(yl), &
          & edge, fill_width, cont_color, cont_width, .true.)
  call resetPen()
end subroutine contourf

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot vectors
subroutine quiver(x, y, u, v, s, c, scaling, lineColor, lineStyle, lineWidth)
  real(wp), dimension(:), intent(in) :: x
  !! x-positions of vectors
  real(wp), dimension(:), intent(in) :: y
  !! y-positions of vectors
  real(wp), dimension(:, :), intent(in) :: u
  !! u-components of vectors
  real(wp), dimension(:, :), intent(in) :: v
  !! v-components of vectors
  real(wp), dimension(:, :), intent(in), optional :: s
  !! Scale of vectors
  real(wp), dimension(:, :), intent(in), optional :: c
  !! Color values for vectors
  real(wp), intent(in), optional :: scaling
  !! Scaling of vectors
  !! < 0 = Automatic, then scaled
  !!   0 = Automatic
  !! > 0 = Directly scaled
  character(*), intent(in), optional :: lineColor
  !! Color of vectors
  character(*), intent(in), optional :: lineStyle
  !! Style of vectors' lines
  real(wp), optional :: lineWidth
  !! Width of vectors' lines

  real(pp), dimension(:), allocatable :: xl, yl
  real(pp), dimension(:, :), allocatable :: ul, vl, sl
  real(pp), dimension(2) :: xb, yb, sb, cb, d
  real(pp) :: scalingl, scl, mag, clr
  integer :: i, j

  xl = localize(x)
  yl = localize(y)
  ul = localize(u)
  vl = localize(v)

  d = real([x(2) - x(1), y(2) - y(1)], pp)

  xb = real(mixval(x), pp)
  yb = real(mixval(y), pp)
  if (present(s)) then
    sl = localize(s)
    sl = sl / maxval(sl)
  else
    sl = localize(u**2 + v**2)
    sl = sqrt(sl)
    sl = sl / maxval(sl)
  end if
  sb = [minval(sl), maxval(sl)]
  cb = 0.0_wp
  if (present(c)) cb = real([minval(c), maxval(c)], pp)

  scalingl = 1.0_pp
  if (present(scaling)) scalingl = real(scaling, pp)

  if (present(lineColor)) call setColor(lineColor)
  if (present(lineStyle)) call setLineStyle(lineStyle)
  if (present(lineWidth)) call setLineWidth(lineWidth)

  do i = 1, size(u, 1)
    do j = 1, size(u, 2)
      mag = norm2([ul(i, j), vl(i, j)])
      scl = scalingl * norm2(d) * sl(i, j)
      if (abs(scl) < 1.0E-5_wp) cycle
      if (present(c)) then
        clr = real((c(i, j) - cb(1)) / (cb(2) - cb(1)), pp)
        clr = max(clr, 0.0_pp)
        clr = min(clr, 1.0_pp)
        call plcol1(clr)
      end if
    call plvect(ul(i:i, j:j) / mag, vl(i:i, j:j) / mag, scl, xl(i:i), yl(j:j))
    end do
  end do

  call resetPen()
end subroutine quiver

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Create a bar graph
subroutine bar(x, y, c, relWidth, fillColor, fillPattern,&
  & lineColor, lineWidth)
  real(wp), dimension(:), intent(in) :: x
  !! x-positions of the bars' centers
  real(wp), dimension(:), intent(in) :: y
  !! y-positions of the bars' tops
  real(wp), dimension(:), intent(in), optional :: c
  !! Color scale for bars
  real(wp), intent(in), optional :: relWidth
  !! Relative width of bars (default 0.8)
  character(*), intent(in), optional :: fillColor
  !! Color of bar fills
  character(*), intent(in), optional :: fillPattern
  !! Pattern of bar fills
  character(*), intent(in), optional :: lineColor
  !! Color of lines around bars
  real(wp), optional :: lineWidth
  !! Width of lines around bars

  real(pp), dimension(4) :: xl, yl
  real(pp), dimension(2) :: cb
  real(pp) :: dx, dxs
  integer :: k

  cb = 0.0_wp
  if (present(c)) cb = real(mixval(c), pp)
  dxs = 0.8_pp
  if (present(relWidth)) dxs = real(relWidth, pp)
  if (size(x) > 1) then
    dx = dxs * real(x(2) - x(1), pp) / 2.0_pp
  else
    dx = dxs
  end if

  if (present(lineWidth)) call setLineWidth(lineWidth)

  do k = 1, size(x)
    xl = real([x(k) - dx, x(k) - dx, x(k) + dx, x(k) + dx], pp)
    yl = real([0.0_wp, y(k), y(k), 0.0_wp], pp)

    if (present(fillColor)) call setColor(fillColor)
    if (present(fillPattern)) call setFillPattern(fillPattern)
    if (present(c)) call plcol1(real((c(k) - cb(1)) / (cb(2) - cb(1)), pp))
    call plfill(xl, yl)

    if (present(lineColor)) call setColor(lineColor)
    call plline(xl, yl)
  end do
  call resetPen()
end subroutine bar

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Create a horizontal bar graph
subroutine barh(y, x, c, relWidth, fillColor, fillPattern, &
  & lineColor, lineWidth)
  real(wp), dimension(:), intent(in) :: y
  !! y-positions of the bars' centers
  real(wp), dimension(:), intent(in) :: x
  !! x-positions of the bars' tops
  real(wp), dimension(:), intent(in), optional :: c
  !! Color scale for bars
  real(wp), intent(in), optional :: relWidth
  !! Relative width of bars
  character(*), intent(in), optional :: fillColor
  !! Color of bar fills
  character(*), intent(in), optional :: fillPattern
  !! Pattern of bar fills
  character(*), intent(in), optional :: lineColor
  !! Color of lines around bars
  real(wp), optional :: lineWidth
  !! Width of lines around bars

  real(pp), dimension(4) :: xl, yl
  real(pp), dimension(2) :: cb
  real(pp) :: dy, dys
  integer :: k

  cb = 0.0_wp
  if (present(c)) cb = real(mixval(c), pp)
  dys = 0.8_pp
  if (present(relWidth)) dys = real(relWidth, pp)
  dy = dys * real(y(2) - y(1), pp) / 2.0_pp

  if (present(lineWidth)) call setLineWidth(lineWidth)

  do k = 1, size(x)
    yl = real([y(k) - dy, y(k) - dy, y(k) + dy, y(k) + dy], pp)
    xl = real([0.0_wp, x(k), x(k), 0.0_wp], pp)

    if (present(fillColor)) call setColor(fillColor)
    if (present(fillPattern)) call setFillPattern(fillPattern)
    if (present(c)) call plcol1(real((c(k) - cb(1)) / (cb(2) - cb(1)), pp))
    call plfill(xl, yl)

    if (present(lineColor)) call setColor(lineColor)
    call plline(xl, yl)
  end do
  call resetPen()
end subroutine barh

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Fill space between two lines
!!
!! TODO: describe the arguments
subroutine fillBetween(x, y1, y0, fillColor, fillPattern, lineWidth)
  real(wp), dimension(:), intent(in) :: x
  real(wp), dimension(:), intent(in) :: y1
  real(wp), dimension(:), intent(in), optional :: y0
  character(*), intent(in), optional :: fillColor
  character(*), intent(in), optional :: fillPattern
  real(wp), intent(in), optional :: lineWidth

  real(pp), dimension(:), allocatable :: xl, y1l, y0l
  integer :: N

  N = size(x)

  xl = localize(x)
  y1l = localize(y1)
  if (present(y0)) then
    y0l = localize(y0)
  else
    allocate (y0l(N))
    y0l = 0.0_pp
  end if

  if (present(fillColor)) call setColor(fillColor)
  if (present(fillPattern)) call setFillPattern(fillPattern)
  if (present(lineWidth)) call setLineWidth(lineWidth)
  call plfill([xl(1:N:1), xl(N:1:-1)], [y1l(1:N:1), y0l(N:1:-1)])
  call resetPen()
end subroutine fillBetween

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Fill space between two lines
!!
!! TODO: describe the arguments
subroutine fillBetweenx(y, x1, x0, fillColor, fillPattern, lineWidth)
  real(wp), dimension(:), intent(in) :: y
  real(wp), dimension(:), intent(in) :: x1
  real(wp), dimension(:), intent(in), optional :: x0
  character(*), intent(in), optional :: fillColor
  character(*), intent(in), optional :: fillPattern
  real(wp), intent(in), optional :: lineWidth

  real(pp), dimension(:), allocatable :: yl, x1l, x0l
  integer :: N

  N = size(y)

  yl = localize(y)
  x1l = localize(x1)
  if (present(x0)) then
    x0l = localize(x0)
  else
    allocate (x0l(N))
    x0l = 0.0_pp
  end if

  if (present(fillColor)) call setColor(fillColor)
  if (present(fillPattern)) call setFillPattern(fillPattern)
  if (present(lineWidth)) call setLineWidth(lineWidth)
  call plfill([x1l(1:N:1), x0l(N:1:-1)], [yl(1:N:1), yl(N:1:-1)])
  call resetPen()
end subroutine fillBetweenx

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Plot error bars for a set of data points
subroutine errorbar(x, y, xerr, yerr, lineColor, lineStyle, lineWidth)
  real(wp), dimension(:), intent(in) :: x
  !! x-data for plot
  real(wp), dimension(:), intent(in) :: y
  !! y-data for plot
  real(wp), dimension(:), intent(in), optional :: xerr
  !! x-data error for plot
  real(wp), dimension(:), intent(in), optional :: yerr
  !! y-data error for plot
  character(*), intent(in), optional :: lineColor
  !! Color of line
  character(*), intent(in), optional :: lineStyle
  !! Style of line; '' for no line
  real(wp), intent(in), optional :: lineWidth
  !! Width of line

  real(pp), dimension(:), allocatable :: xl, yl
  real(pp), dimension(:), allocatable :: xll, xlh
  real(pp), dimension(:), allocatable :: yll, ylh

  xl = localize(x)
  yl = localize(y)

  if (present(lineColor)) call setColor(lineColor)
  if (present(lineWidth)) call setLineWidth(lineWidth)
  if (present(lineStyle)) call setLineStyle(lineStyle)

  if (present(xerr)) then
    xll = localize(x - xerr)
    xlh = localize(x + xerr)
    call plerrx(xll, xlh, yl)
  end if

  if (present(yerr)) then
    yll = localize(y - yerr)
    ylh = localize(y + yerr)
    call plerry(xl, yll, ylh)
  end if

  call resetPen()
end subroutine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Drawing Pen Routines
!! Reset pen to default state
subroutine resetPen

  call setColor('')
  call setLineStyle('')
  call setLineWidth(0.5_wp)
  call setFillPattern('')
  call plschr(0.0_pp, real(fontScale, pp))
  call plssym(0.0_pp, real(fontScale, pp))
end subroutine resetPen

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine setLineWidth(lineWidth)
  real(wp), intent(in) :: lineWidth

  call plwidth(real(lineWidth, pp))
end subroutine setLineWidth

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the current pen line style
subroutine setLineStyle(style)
  character(*), intent(in) :: style   !! Style to set

  call pllsty(getLineStyleCode(style))
end subroutine setLineStyle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Return the code for a line style
function getLineStyleCode(style) result(code)
  character(*), intent(in) :: style   !! Style desired
  integer :: code

  select case (style)
  case ('-')
    code = 1
  case (':')
    code = 2
  case ('--')
    code = 3
  case default
    code = 1
  end select
end function getLineStyleCode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Return the code for a symbol style
function getSymbolCode(style) result(code)
  character(*), intent(in) :: style   !! Style desired
  character(32) :: code

  select case (style)
  case (PS_PLUS)
    code = CODE_PS_PLUS
  case (PS_CROSS)
    code = CODE_PS_CROSS
  case (PS_ASTERIC)
    code = CODE_PS_ASTERIC
  case (PS_DOT)
    code = CODE_PS_DOT
  case (PS_SQUARE)
    code = CODE_PS_SQUARE
  case (PS_STAR)
    code = CODE_PS_STAR
  case (PS_H_CIRCLE)
    code = CODE_PS_H_CIRCLE
  case (PS_H_SQUARE)
    code = CODE_PS_H_SQUARE
  case (PS_H_TRIAG_U)
    code = CODE_PS_H_TRIAG_U
  case (PS_H_DIAMOND)
    code = CODE_PS_H_DIAMOND
  case (PS_TRIAG_U)
    code = CODE_PS_TRIAG_U
  case (PS_TRIAG_L)
    code = CODE_PS_TRIAG_L
  case (PS_TRIAG_D)
    code = CODE_PS_TRIAG_D
  case (PS_TRIAG_R)
    code = CODE_PS_TRIAG_R
  case default
    code = '#(143)'
  end select
end function getSymbolCode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine setFillPattern(style)
  character(*), intent(in) :: style

  call plpsty(getFillCode(style))
end subroutine setFillPattern

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

function getFillCode(style) result(code)
  character(*), intent(in) :: style
  integer :: code

  select case (style)
  case ('')
    code = 0
  case ('-')
    code = 1
  case ('/')
    code = 3
  case ('|')
    code = 2
  case ('\')
    code = 4
  case ('#')
    code = 7
  case ('x')
    code = 8
  case default
    code = 0
  end select
end function getFillCode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the current pen color
subroutine setColor(color)
  character(*), intent(in) :: color   !! Name of color to set

  integer :: ios
  real(pp) :: v

  read (color, *, iostat=ios) v
  if (ios == 0) then
    call plcol1(v)
  else
    call plcol0(getColorCode(color))
  end if
end subroutine setColor

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

function getColorCode(color) result(code)
  character(*), intent(in) :: color
  integer :: code

  select case (color)
  case ('w', 'white')
    if (blackOnWhite) then
      code = 1
    else
      code = 2
    end if
  case ('k', 'black')
    if (blackOnWhite) then
      code = 2
    else
      code = 1
    end if
  case ('r', 'red')
    code = 3
  case ('g', 'green')
    code = 4
  case ('b', 'blue')
    code = 5
  case ('c', 'cyan')
    code = 6
  case ('m', 'magenta')
    code = 7
  case ('y', 'yellow')
    code = 8
  case ('fg')
    code = 2
  case ('bg')
    code = 1
  case default
    code = 2
  end select

  code = code - 1
end function getColorCode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Library Status Routines

!! Setup PlPlot library, optionally overriding defaults
subroutine setup(device, fileName, fontScaling, whiteOnBlack, &
  & transparent, colormap, figSize, isFileFamily)
  character(*), intent(in), optional :: device
  !! Output device to use
  !!
  !! * qtwidget
  !! * svgqt
  !! * pngqt
  character(*), intent(in), optional :: fileName
  !! Name of file(s) to write to
  !!
  !! The text `%n` will be replaced with the figure number
  real(wp), intent(in), optional :: fontScaling
  !! Font scaling relative to default value
  logical, intent(in), optional :: whiteOnBlack
  !! Default foreground and background colors
  logical, intent(in), optional :: transparent
  !! Transparent background
  character(*), intent(in), optional :: colormap
  !! Colormap to use
  integer, dimension(2), intent(in), optional :: figSize
  !! Size of figures to produce in pixels
  logical, optional, intent( in ) :: isFileFamily

  character(64) :: bufx, bufy
  integer :: ios

  if (present(device)) then
    call plsdev(device)
  else
    call plsdev(default_dev)
  end if
  !!
  !! changed by vikas sharma,
  IF( present( isFileFamily ) ) THEN
    IF( isFileFamily ) THEN
      call plsfam(1, 1, 100)
    END IF
  END IF
  !!
  if (present(fileName)) then
    call plsfnam(fileName)
  else
    call plsfnam('out')
  end if

  if (present(whiteOnBlack)) blackOnWhite = .not. whiteOnBlack

  if (present(transparent)) transparentBackground = transparent

  call setIndexedColors()

  if (present(colormap)) then
    call setColormap(colormap)
  else
    call setColormap('CoolWarm')
  end if

  call plfontld(0)
  if (present(fontScaling)) fontScale = real(fontScaling, pp)

  if (present(figSize)) then
    write (bufx, *) figSize(1)
    write (bufy, *) figSize(2)
    ios = plsetopt('geometry', trim(adjustl(bufx))//'x'//trim(adjustl(bufy)))
  else
    ios = plsetopt('geometry', '640x480')
  end if

  call plinit()

  call resetPen()
end subroutine setup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Show the plots end finialize the PlPlot library
subroutine show
  if (.not. didShow) then
    call plend()
    didShow = .true.
  end if
end subroutine show

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Color Map Routines
!! Setup the indexed colors
subroutine setIndexedColors
  integer, dimension(8, 3) :: rgb
  real(plflt), dimension(8) :: a

  rgb(getColorCode('w') + 1, :) = [255, 255, 255] ! White
  rgb(getColorCode('k') + 1, :) = [0, 0, 0]       ! Black
  rgb(getColorCode('r') + 1, :) = [255, 0, 0]     ! Red
  rgb(getColorCode('g') + 1, :) = [0, 255, 0]     ! Green
  rgb(getColorCode('b') + 1, :) = [0, 0, 255]     ! Blue
  rgb(getColorCode('c') + 1, :) = [0, 255, 255]   ! Cyan
  rgb(getColorCode('m') + 1, :) = [255, 0, 255]   ! Magenta
  rgb(getColorCode('y') + 1, :) = [255, 255, 0]   ! Yellow

  a = 1.0_plflt
  if (transparentBackground) a(1) = 0.0_wp

  call plscmap0a(rgb(:, 1), rgb(:, 2), rgb(:, 3), a)
end subroutine setIndexedColors

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the continuous colormap
subroutine setColormap(colormap)
  character(*), intent(in) :: colormap    !! Name of colormap to use

  real(pp), dimension(:), allocatable :: i, h, s, v

  select case (colormap)
  case ('CoolWarm')
    h = [240.0, 195.0, 45.0, 0.0]

    s = [0.60, 0.95, 0.95, 0.60]
    v = [0.80, 0.30, 0.30, 0.80]
    i = [0.00, 0.50, 0.50, 1.00]

    call plscmap1n(256)
    call plscmap1l(.false., i, h, s, v)
  case ('Gray')
    call plspal1('cmap1_gray.pal', .true.)
  case ('BlueYellow')
    call plspal1('cmap1_blue_yellow.pal', .true.)
  case ('BlueRed')
    call plspal1('cmap1_blue_red.pal', .true.)
  case ('Radar')
    call plspal1('cmap1_radar.pal', .true.)
  case ('HighFreq')
    call plspal1('cmap1_highfreq.pal', .true.)
  case ('LowFreq')
    call plspal1('cmap1_lowfreq.pal', .true.)
  end select
end subroutine setColormap

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end module EasyPlplot
