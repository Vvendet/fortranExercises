




program QuestaoB !!Encontrar tres raizes do polinomio x^3-x-1/5
    implicit none

    double precision, dimension(100)::c
    double precision::a1,b1,a2,b2,a3,b3
    integer::i


    a1=-1.2d0
    b1=-0.8d0

    a2=-0.3d0
    b2=-0.1d0

    a3=1.05d0
    b3=1.15d0



    do i =1,100 !iteracao no metodo da bisssecao

        c(i)=(a1+b1)/2
        if (h(c(i))<0) then
            a1=c(i)
        else if (h(c(i))>0) then
            b1=c(i)
        end if

        if (abs(c(i)-c(i-1))<0.000001) then
            write(*,*)"Primeira solucao:",i,c(i)
            exit
        end if

    end do

    do i =1,100 !iteracao no metodo da bisssecao

        c(i)=(a2+b2)/2
        if (h(c(i))<0) then
            a2=c(i)
        else if (h(c(i))>0) then
            b2=c(i)
        end if

        if (abs(c(i)-c(i-1))<0.000001) then
            write(*,*)"Segunda solucao:",i,c(i)
            exit
        end if

    end do

    do i =1,100 !iteracao no metodo da bisssecao

        c(i)=(a3+b3)/2
        if (h(c(i))<0) then
            a3=c(i)
        else if (h(c(i))>0) then
            b3=c(i)
        end if

        if (abs(c(i)-c(i-1))<0.000001) then
            write(*,*)"Terceira solucao:",i,c(i)
            exit
        end if

    end do



    contains

    function h(x) !a funcao
        implicit none
        double precision::h,x
        real(8), parameter :: pi=3.14159274D0

        h = x*x*x-x-0.2d0

        return
    end function


end program
