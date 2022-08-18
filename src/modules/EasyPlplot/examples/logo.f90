!> Create the project logo
program logo
    use utilities_m
    use easy_plplot_m
    implicit none
    real(wp), parameter :: pi = acos(-1.0d0)

    call setup(device='svg', fileName='media/logo-%n.svg', figSize=[600, 500])
    call makeLogo
    call show()

contains

    !> ![logo](../|media|/logo-1.svg)
    subroutine makeLogo
        real(wp), dimension(:), allocatable :: x, y1, y2, y3

        x = linspace(0.0_wp, 1.0_wp, 100)
        y1 = x**2 - 1.0_wp
        y2 = 2.0_wp*x - 1.0_wp
        y3 = x
        y3 = cos(2.0_wp*PI*x)

        call figure()
        call subplot(1, 1, 1)
        call xylim(mixval(x), mixval([y1, y2, y3])*1.1_wp)

        call plot(x, y1, lineColor='b', lineWidth=1.2_wp)
        call plot(x, y2, lineColor='r', lineWidth=1.2_wp)
        call plot(x, y3, lineColor='c', lineWidth=1.2_wp)

        call ticks(lineWidth=1.2_wp)
        call labels('', '', '')
    end subroutine makeLogo

end program logo
