


program QuestaoE
    implicit none

    real, dimension(100)::x
    integer::i

    x(1)=-1.0
    do i =2,100

        x(i)=n(x(i-1))

        if (abs(x(i))<0.0001) then

        write(*,*)i,x(i)
        exit

        end if

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

        f = exp(x)-x-1

        return
    end function

    function flinha(x)
        implicit none
        real::flinha,x

        flinha =exp(x)-1

    end function

    function fdlinha(x)
        implicit none
        real::fdlinha,x

        fdlinha=exp(x)

    end function
end program
