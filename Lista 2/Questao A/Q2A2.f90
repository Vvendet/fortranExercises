
program QuestaoA
    implicit none

    real, dimension(100)::x
    integer::i,escolha
    real:: a,b,c

    write(*,*)"Escreva 1, para newton raphson"
    write(*,*)"Escreva 2, para bissecao"
    write(*,*)"escreva 3, para ponto fixo"
    read(*,*)escolha
    if (escolha==2) then
        write(*,*) "Escolha os valores a e b do intervalo, respectivamente:"
        read(*,*)a
        read(*,*)b
    else
        write(*,*)"Escolha o chute inicial"
        read(*,*)x(1)
    end if

    open(unit=777,file='saida.dat',status='unknown')
    do i =2,100
        if (escolha == 1) then
            x(i)=n(x(i-1))
            write(777,*)i,x(i)

        else if (escolha==2) then
            x(i)=(a+b)/2
            c = f(x(i))

            if (c>0) then
                b=x(i)

            else if (c<0) then
                a=x(i)

            end if

            write(777,*)i, x(i)


        else if (escolha==3) then
            x(i)=p(x(i-1))
            write(777,*)i,x(i)

        end if
    end do
    close(unit=777)



    contains

    function n(x)
        implicit none

        real::x,n

        n = (x*x*x-7*x*x+14*x-6)-(x*x*x-7*x*x+14*x-6)/(3*x*x-14*x+14)

        return

    end function

    function p(x)
        implicit none

        real::p,x

        p = (6+7*x*x-14*x)**(1/3)


    end function

    function f(x)
        implicit none
        real::f,x

        f = x*x*x-7*x*x+14*x+14

        return
    end function




end program
