

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

        n = (230*x*x*x*x+18*x*x*x+9*x*x-221*x-9)-(230*x*x*x*x+18*x*x*x+9*x*x-221*x-9)/(4*230*x*x*x+3*18*x*x+2*9*x-221)

        return

    end function

    function p(x)
        implicit none

        real::p,x

        p = sqrt(sqrt((9+221*x-9*x*x+18*x*x*x)/230))


    end function

    function f(x)
        implicit none
        real::f,x

        f = 230*x*x*x*x+18*x*x*x+9*x*x-221*x-9

        return
    end function
end program
