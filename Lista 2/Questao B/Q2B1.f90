

program QuestaoA
    implicit none

    real, dimension(100)::x
    integer::i

    x(1)=-0.1
    do i =2,100

        x(i)=n(x(i-1))
        if (abs(x(i)-(-4.08773199E-02))<0.00001) then


            write(*,*)i,x(i)
            exit

        end if

    end do


    x(1)=0.96
    do i =2,100
        x(i)=n(x(i-1))
        if (abs(x(i)-(0.962398410))<0.00001) then

            write(*,*)i,x(i)
            exit

        end if

    end do

    contains

    function n(x)
        implicit none

        real::x,n

        n = x-f(x)/flinha(x)

        return

    end function

    function f(x)
        implicit none
        real::f,x

        f = 230*x*x*x*x+18*x*x*x+9*x*x-221*x-9

        return
    end function

    function flinha(x)
        implicit none
        real::flinha,x

        flinha = 4*230*x*x*x+3*18*x*x+18*x-221

    end function
end program
