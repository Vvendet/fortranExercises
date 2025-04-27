
program QuestaoB !!Encontrar tres raizes do polinomio x^3-x-1/5
    implicit none

    double precision, dimension(100)::c,x
    integer::i,j


    do j =1,50

        write(*,*)j
        x(1)=(42.98+0.14*j)/49.0
        write(*,*)"CHUTE INICIAL:"
        write(*,*)x(1)
        write(*,*)"!!!!!!!!!"


        do i =2,100 !iteracao no metodo da newton raphson
            x(i)=f(x(i-1))
            write(*,*)i, x(i)
        end do



    end do


    contains


    function f(x)
        implicit none
        double precision::f,x
        real(8), parameter :: pi=3.14159274D0

        f=cube_root(1/5.0+x)

    end function

    function h(x) !a funcao
        implicit none
        double precision::h,x
        real(8), parameter :: pi=3.14159274D0

        h = x*x*x-x-0.2d0

        return
    end function

    REAL FUNCTION cube_root(x) !funcao de apoio para tirar raiz cubica
        IMPLICIT NONE
        !
        ! Function to calculate the cube root of a positive real number
        !
        ! Dummy argument declaration
        double precision, INTENT(IN) :: x
        !
        ! Local variable declaration
        double precision :: log_x
        !
        ! Calculate cube root by using logs
        log_x = LOG(x)
        cube_root = EXP(log_x/3.0)
    !
    END FUNCTION cube_root


end program
