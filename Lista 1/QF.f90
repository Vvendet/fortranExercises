program QF
    implicit none
    real, dimension(:), allocatable:: x
    integer::i

    allocate(x(0:100))



    x(0) = 0.3

    do i=1,100

        write(*,*) i, f(x(i-1))
        x(i) = f(x(i-1))

    end do


    contains

    function f(x)
        implicit none
        real:: x,f

        f = 2*x-3*x*x

        return

    end function

end program


