program QE
    implicit none
    real, dimension(:), allocatable:: x
    integer::i

    allocate(x(0:100))



    x(0) = 1

    do i=1,100

        write(*,*) i, h(x(i-1))
        x(i) = h(x(i-1))

    end do


    contains

    function f(x)
        implicit none
        real:: x,f

        f = (2-exp(x)+x*x)/3

        return

    end function

    function g(x)
        implicit none
        real:: x,g

        g = 5/(x*X)+2

        return

    end function

    function h(x)
        implicit none
        real:: x,h

        h = 6**(-x)

        return

    end function
end program

