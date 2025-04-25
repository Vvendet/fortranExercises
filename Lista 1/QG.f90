program QG
    implicit none
    real, dimension(:), allocatable:: x
    integer::i

    allocate(x(0:100))

    x(0)=1
    x(1)= 2

    do i=2,100
        x(i)= a(x(i-1),x(i-2))

        write(*,*) i, x(i)

    end do


    contains

    function a(x1,x2)
        implicit none
        real::a,x1,x2

        a = 7*x1-6*x2


    end function

    function b(x1,x2)
        implicit none
        real::b,x1,x2

        b = 6*x1-9*x2


    end function

    function c(x1,x2)
        implicit none
        real::c,x1,x2

        c = 2*x1-4*x2


    end function
end program
