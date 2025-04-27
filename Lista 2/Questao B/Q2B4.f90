program QuestaoB
    implicit none
    integer::i
    double precision,dimension(100)::x

    x(1)=14



    do i=2,100

        x(i)=f(x(i-1))
        write(*,*)i,x(i)

    end do



    contains

    function f(x)
        implicit none
        double precision::f,x
        real(8), parameter :: pi=3.14159274D0

        f=log10(x)+ 3*cube_root(x)-pi-(log10(x)+ 3*cube_root(x)-pi)/(log10(exp(1.0)/x+1/cube_root(x*x)))

    end function

    REAL FUNCTION cube_root(x)
        IMPLICIT NONE
        !
        ! Function to calculate the cube root of a positive real number
        !
        ! Dummy argument declaration
        double precision, INTENT(IN) :: x
        !
        ! Local variable declaration
        REAL :: log_x
        !
        ! Calculate cube root by using logs
        log_x = LOG(x)
        cube_root = EXP(log_x/3.0)
    !
    END FUNCTION cube_root


end program
