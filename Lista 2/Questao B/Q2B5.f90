



program QuestaoB
    implicit none

    double precision, dimension(100)::x,c
    double precision::a,b
    integer::i


    a=0.5d0
    b=1.5d0

    x(1)=1.7
    x(2)=h(x(1))

    do i =3,100 !iteracao no metodo da secante
        x(i)=g(x(i-1),x(i-2))
        if (abs(x(i)-x(i-1))<0.000001) then
            write(*,*)"Iteracoes para convergir no metodo da secante:",i, x(i)
            exit
        end if
    end do

    do i =1,100 !iteracao no metodo da bisssecao

        c(i)=(a+b)/2
        if (h(c(i))<0) then
            a=c(i)
        else if (h(c(i))>0) then
            b=c(i)
        end if

        if (abs(c(i)-c(i-1))<0.000001) then
            write(*,*)"Iteracoes para convergir no metodo da bissecao:",i,c(i)
            exit
        end if

    end do



    contains

    function f(x) !funcao metodo newton raphson
        implicit none
        double precision::f,x
        real(8), parameter :: pi=3.14159274D0

        f = (log10(x)+3*cube_root(x)-pi)-(log10(x)+3*cube_root(x)-pi)/((cube_root(x*x)+log(10.d0)*x)/(log(10.d0)*cube_root(x*x*x*x*x)))

        return
    end function

    function g(x,y) !funcao metodo da secante
        implicit none
        double precision::g,x,y
        real(8), parameter :: pi=3.14159274D0

        g = x-((log10(x)+3*cube_root(x)-pi)*(x-y))/(log10(x)+3*cube_root(x)-pi-(log10(y)+3*cube_root(y)-pi))

        return
    end function

    function h(x) !a funcao
        implicit none
        double precision::h,x
        real(8), parameter :: pi=3.14159274D0

        h = (log10(x)+3*cube_root(x)-pi)

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
