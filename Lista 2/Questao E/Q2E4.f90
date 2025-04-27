program QuestaoE
    implicit none

    real, dimension(200)::x
    integer::i

    x(1)=3.0
    do i =2,200

        x(i)=n(x(i-1))

        write(*,*)i,x(i)

    end do
    contains

    function n(x)
        implicit none

        real::x,n

        n = x-(f(x)*flinha(x))/(flinha(x)*flinha(x)-fdlinha(x)*f(x))

        return

    end function

    function f(x)
        implicit none
        real::f,x

        f = x*x*x*x-9*x*x*x+9*x*x-27

        return
    end function

    function flinha(x)
        implicit none
        real::flinha,x

        flinha =4*x*x*x-27*x*x+18*x

    end function

    function fdlinha(x)
        implicit none
        real::fdlinha,x

        fdlinha=12*x*x-54*x+18

    end function
end program

