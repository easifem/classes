!> Create a series of plots for an animation
program animate
    use utilities_m
    use easy_plplot_m
    implicit none

    real(wp), dimension(:), allocatable :: x, y, t
    integer :: N, M, i, k
    real(wp), parameter :: pi = acos(-1.0d0)

    N = 100
    M = 50

    x = linspace(0.0_wp, PI, N)
    t = linspace(0.0_wp, 10.0_wp, M)

    call setup(device="wingcc")
    do k = 1, M
        y = [(f(x(i), t(k)), i=1, N)]
        call figure()
        call subplot(1, 1, 1)
        call xylim(mixval(x), [-1.1_wp, 1.1_wp])
        call plot(x, y, lineColor='b', lineWidth=2.0_wp)
        call ticks()
        call labels('x', 'y', '')
    end do
    call show()

contains

    pure function f(x, t) result(o)
        real(wp), intent(in) :: x, t
        real(wp) :: o

        o = sin(t)*cos(x + t)
    end function f

end program animate
