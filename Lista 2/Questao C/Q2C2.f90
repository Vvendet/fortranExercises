program QuestaoB !!Encontrar tres raizes do polinomio x^3-x-1/5
    implicit none

    double precision, dimension(100)::c,x
    integer::i,j


    do j =1,50

        x(1)=(2.5*j-51.5)/49.0
        write(*,*)
        do i =2,100 !iteracao no metodo da newton raphson
            x(i)=f(x(i-1))
            if (abs(x(i)-(-0.87888565063476576))<0.000001) then
                write(*,*)"Solucao:",i, x(i)
                exit
            else if (abs(x(i)-(-0.10000076293945313))<0.000001) then
                write(*,*)"Solucao:",i, x(i)
                exit
            else if (abs(x(i)-(1.0880332946777345))<0.000001) then
                write(*,*)"Solucao:",i, x(i)
                exit
            end if
        end do



    end do


    contains


    function f(x)
        implicit none
        double precision::f,x
        real(8), parameter :: pi=3.14159274D0

        f=x-(x*x*x-x-0.2d0)/(3*x*x-1)

    end function

    function h(x) !a funcao
        implicit none
        double precision::h,x
        real(8), parameter :: pi=3.14159274D0

        h = x*x*x-x-0.2d0

        return
    end function


end program
