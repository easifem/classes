!> A collection of example plots
PROGRAM examples
USE easy_plplot_m
IMPLICIT NONE
REAL(wp), PARAMETER :: pi = ACOS(-1.0D0)

    call setup(device='svg', fileName='media/example-%n.svg', figSize=[600, 500], transparent=.true.)

CALL doPlot()
CALL doScatter()
CALL doContour()
CALL doLegend()
CALL doQuiver()
CALL doBar()
CALL doFillBetween()
CALL doHist()
CALL doSurface()
CALL doError()
CALL doLogPlot()

CALL show()

CONTAINS

!> ![plot](../|media|/example-1.svg)
SUBROUTINE doPlot
  INTEGER, PARAMETER :: N = 20
  REAL(wp), DIMENSION(N) :: x, y

  x = linspace(0.0_WP, 1.0_WP, N)
  y = x**2 - 1.0_WP

  CALL figure()
  CALL subplot(1, 1, 1)
  CALL xylim(mixval(x), mixval(y))

  CALL plot(x, y, lineColor='red', lineWidth=2.0_WP, &
          & markStyle='.', markColor='cyan', markSize=2.0_WP)

CALL plot(x, -1.0_WP - y, lineColor='blue', lineStyle=':', lineWidth=2.0_WP, &
                      & markStyle='+', markColor='green', markSize=1.0_WP)

!~                 call ticks()
  CALL xticks(primary=.TRUE., secondary=.FALSE.)
  CALL yticks(primary=.TRUE., secondary=.FALSE.)
!~                 call labels('x','y','f(x)=x#u2#d-1; g(x)=-x#u2#d')
  CALL xlabel('x')
  CALL ylabel('y')
  CALL title('f(x)=x#u2#d-1; g(x)=-x#u2#d')
END SUBROUTINE doPlot

!> ![scatter](../|media|/example-2.svg)
SUBROUTINE doScatter
  INTEGER, PARAMETER :: N = 100
  REAL(wp), DIMENSION(N) :: x, y, z

  CALL RANDOM_NUMBER(x)
  CALL RANDOM_NUMBER(y)
  z = SQRT(x**2 + y**2)

  CALL figure()

  CALL subplot(2, 2, 1)
  CALL xylim([0.0_WP, 1.0_WP], [0.0_WP, 1.0_WP])
  CALL scatter(x, y)
  CALL ticks()
  CALL labels('x', 'y', '')

  CALL subplot(2, 2, 2)
  CALL xylim([0.0_WP, 1.0_WP], [0.0_WP, 1.0_WP])
  CALL scatter(x, y, c=z)
  CALL ticks()
  CALL labels('x', 'y', '')

  CALL subplot(2, 2, 3)
  CALL xylim([0.0_WP, 1.0_WP], [0.0_WP, 1.0_WP])
  CALL scatter(x, y, s=(4.0_WP * z + 1.0_WP), markColor='blue')
  CALL ticks()
  CALL labels('x', 'y', '')

  CALL subplot(2, 2, 4)
  CALL xylim([0.0_WP, 1.0_WP], [0.0_WP, 1.0_WP])
  CALL scatter(x, y, c=z, s=(4.0_WP * z + 1.0_WP))
  CALL ticks()
  CALL labels('x', 'y', '')
END SUBROUTINE doScatter

!> ![contour](../|media|/example-3.svg)
SUBROUTINE doContour
  INTEGER, PARAMETER :: N = 50
  REAL(wp), DIMENSION(N) :: x, y
  REAL(wp), DIMENSION(N, N) :: z
  INTEGER :: i, j

  x = linspace(-10.0_WP, 10.0_WP, N)
  y = linspace(-10.0_WP, 10.0_WP, N)
  DO CONCURRENT(i=1:N, j=1:N)
    z(i, j) = SIN(SQRT(x(i)**2 + y(j)**2)) / SQRT(x(i)**2 + y(j)**2)
  END DO

  CALL figure()

  CALL subplot(1, 1, 1, aspect=1.0_WP)
  CALL xylim(mixval(x), mixval(y))
  CALL contourf(x, y, z, 10)
  CALL contour(x, y, z, 10)
  CALL colorbar(z, 5)
  CALL ticks()
  CALL labels('x', 'y', '')
END SUBROUTINE doContour

!> ![legend](../|media|/example-4.svg)
SUBROUTINE doLegend
  INTEGER, PARAMETER :: N = 20
  REAL(wp), DIMENSION(N) :: x, y
  CHARACTER(32), DIMENSION(3, 7) :: series

  x = linspace(0.0_WP, 1.0_WP, N)
  y = x**2 - 1.0_WP

  CALL figure()
  CALL subplot(1, 1, 1)
  CALL xylim(mixval(x), mixval(y))

  CALL plot(x, y, lineColor='red', lineWidth=2.0_WP, &
          & markStyle='.', markColor='cyan', markSize=2.0_WP)

CALL plot(x, -1.0_WP - y, lineColor='blue', lineStyle=':', lineWidth=2.0_WP, &
                      & markStyle='+', markColor='green', markSize=1.0_WP)

  ! [name,textColor,lineStyle,lineColor,markStyle,markColor]
 series(1, :) = [CHARACTER(32) :: 'f(x)=x#u2#d-1', '', '-', 'r', '.', 'c', '']
  series(2, :) = [CHARACTER(32) :: 'g(x)=-x#u2#d', '', ':', 'b', '+', 'g', '']
  series(3, :) = [CHARACTER(32) :: 'Box', '', '', '', '', '', 'r']

  CALL legend('center left', series)
  CALL ticks()
  CALL labels('x', 'y', '')
END SUBROUTINE doLegend

!> ![quiver](../|media|/example-5.svg)
SUBROUTINE doQuiver
  INTEGER, PARAMETER :: N = 20
  REAL(wp), DIMENSION(N) :: x, y
  REAL(wp), DIMENSION(N, N) :: u, v, m
  INTEGER :: i, j

  x = linspace(-10.0_WP, 10.0_WP, N)
  y = linspace(-10.0_WP, 10.0_WP, N)
  DO CONCURRENT(i=1:N, j=1:N)
    u(i, j) = -y(j)
    v(i, j) = x(i)
    m(i, j) = SQRT(u(i, j)**2 + v(i, j)**2)
  END DO

  CALL figure()

  CALL subplot(1, 1, 1, aspect=1.0_WP)
  CALL xylim(mixval(x), mixval(y))
  CALL quiver(x, y, u, v, c=m, s=m, scaling=2.0_WP, lineWidth=2.0_WP)
  CALL colorbar(m, 10)
  CALL ticks()
  CALL labels('x', 'y', '')
END SUBROUTINE doQuiver

!> ![bar](../|media|/example-6.svg)
SUBROUTINE doBar
  INTEGER, PARAMETER :: N = 21
  REAL(wp), DIMENSION(N) :: x, y

  x = linspace(-PI, PI, N)
  y = EXP(-x**2)

  CALL figure()

  CALL subplot(1, 2, 1)
  CALL xylim(mixval(x) + [-0.1_WP, 0.1_WP], mixval(y) + [0.0_WP, 0.1_WP])
  CALL bar(x, y, c=y, relWidth=1.0_WP)
  CALL ticks()
  CALL labels('x', 'y', '')

  CALL subplot(1, 2, 2)
  CALL xylim(mixval(y) + [0.0_WP, 0.1_WP], mixval(x) + [-0.1_WP, 0.1_WP])
  CALL barh(x, y, fillColor='r', relWidth=1.0_WP)
  CALL ticks()
  CALL labels('x', 'y', '')
END SUBROUTINE doBar

!> ![fillBetween](../|media|/example-7.svg)
SUBROUTINE doFillBetween
  INTEGER, PARAMETER :: N = 51
  REAL(wp), DIMENSION(N) :: x, y1, y2

  x = linspace(-3.0_WP, 3.0_WP, N)
  y1 = x**2 - 1.0_WP
  y2 = x**3 - 1.0_WP

  CALL figure()
  CALL subplot(1, 1, 1)
  CALL xylim(mixval(x), mixval([y1, y2]))
 CALL fillBetween(x, y1, y2, fillColor='c', fillPattern='#', lineWidth=2.0_WP)
  CALL plot(x, y1, lineColor='k', lineWidth=3.0_WP)
  CALL plot(x, y2, lineColor='k', lineWidth=3.0_WP)
  CALL ticks(color='b', lineWidth=3.0_WP)
  CALL labels('x', 'y', 'f(x)=x#u2#d-1', color='r')
END SUBROUTINE doFillBetween

!> ![hist](../|media|/example-8.svg)
SUBROUTINE doHist
  INTEGER, PARAMETER :: N = 10000
  REAL(wp), DIMENSION(N, 12) :: r
  REAL(wp), DIMENSION(N) :: x
  REAL(wp), DIMENSION(:, :), ALLOCATABLE :: h

  CALL RANDOM_NUMBER(r)
  x = SUM(r, 2) - 6.0_WP
  CALL figure()

  CALL subplot(1, 2, 1)
  CALL xylim(mixval(x), [0.0_WP, 1.05_WP])
  CALL hist(x, 20)
  CALL ticks()

  h = binData(x, 20, normalize=2)
  CALL subplot(1, 2, 2)
  CALL xylim(mixval(h(:, 1)), [0.0_WP, 1.05_WP * MAXVAL(h(:, 2))])
  CALL bar(h(:, 1), h(:, 2), c=h(:, 2), relWidth=1.0_WP)
  CALL ticks()
END SUBROUTINE doHist

!> ![surface](../|media|/example-9.svg)
SUBROUTINE doSurface
  INTEGER, PARAMETER :: N = 24
  REAL(wp), DIMENSION(N) :: x, y
  REAL(wp), DIMENSION(N, N) :: z
  INTEGER :: i, j

  x = linspace(-10.0_WP, 10.0_WP, N)
  y = linspace(-10.0_WP, 10.0_WP, N)
  DO CONCURRENT(i=1:N, j=1:N)
    z(i, j) = SIN(SQRT(x(i)**2 + y(j)**2)) / SQRT(x(i)**2 + y(j)**2)
  END DO

  CALL figure()

  CALL subplot(1, 1, 1, is3d=.TRUE.)
  CALL xyzlim(mixval(x), mixval(y), mixval(z), zoom=1.1_WP)
  CALL surface(x, y, z, 11)
!~                 call wireframe(x,y,z,lineColor='k')
  CALL box('x', 'y', 'z')

END SUBROUTINE doSurface

!> ![error](../|media|/example-10.svg)
SUBROUTINE doError

  INTEGER, PARAMETER :: N = 25
  REAL(wp), DIMENSION(N) :: x, y, xe, ye
  REAL(wp), DIMENSION(12) :: r
  REAL(wp) :: m, s
  INTEGER :: k

  x = linspace(0.0_WP, 5.0_WP, N)
  y = 1.0_WP / (x**2 + 1.0_WP)

  DO k = 1, N
    CALL RANDOM_NUMBER(r)
    r = 0.25_WP * (2.0_WP * r - 1.0_WP)
    m = SUM(r) / REAL(SIZE(r))
    s = SQRT(SUM((r - m)**2) / REAL(SIZE(r) - 1))
    x(k) = x(k) + m
    xe(k) = s

    CALL RANDOM_NUMBER(r)
    r = 0.15_WP * (2.0_WP * r - 1.0_WP)
    m = SUM(r) / REAL(SIZE(r))
    s = SQRT(SUM((r - m)**2) / REAL(SIZE(r) - 1))
    y(k) = y(k) + m
    ye(k) = s
  END DO

  CALL figure()
  CALL subplot(1, 1, 1)
        call xylim(mixval([x - xe, x + xe]) + [-0.5_wp, 0.5_wp], mixval([y - ye, y + ye]) + [-0.2_wp, 0.2_wp])
  CALL errorbar(x, y, xerr=xe, yerr=ye, lineColor='b', lineWidth=1.0_WP)
  CALL plot(x, y, lineStyle='', markStyle='s', markColor='r', markSize=1.5_WP)
  CALL ticks()
  CALL labels('x', 'y', '')
END SUBROUTINE doError

!> ![logPlot](../|media|/example-11.svg)
SUBROUTINE doLogPlot

  INTEGER, PARAMETER :: N = 25
  REAL(wp), DIMENSION(N) :: x, y, yl

  x = linspace(0.0_WP, 5.0_WP, N)
  y = EXP(-x**2)
  yl = LOG10(y)

  CALL figure()
  CALL subplot(1, 1, 1)
  CALL xylim(mixval(x), mixval(yl))
  CALL plot(x, yl, lineColor='r', lineWidth=2.0_WP)
  CALL ticks(logy=.TRUE.)
  CALL labels('x [linear]', 'y [log]', 'exp(-x#u2#d)')
END SUBROUTINE doLogPlot

END PROGRAM examples
