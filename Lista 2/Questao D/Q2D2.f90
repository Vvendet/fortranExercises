program QuestaoD
    implicit none
    real,dimension(3)::a,b
    real,dimension(100)::x
    integer::i,j


    a(1)=-2.5
    a(2)=-0.8

    b(1)=-1.5
    b(2)=0.4

    do j=1,2

        write(*,*)"ITERACAO METODO POSICAO FALSA PARA SOLUCAO"
        write(*,*)j
        write(*,*)"#######"
        do i=1,100

            x(i)=g(a(j),b(j))

            if (f(x(i))*f(a(j))<0) then

                a(j)=x(i)

            else if (f(x(i))*f(b(j))<0) then

                b(j)=x(i)

            end if

            write(*,*)i,x(i)




        end do

    end do
    contains

    function g(a,b)
        implicit none
        real::g,a,b

        g=(a*f(b)-b*f(a))/(f(b)-f(a))

    end function

    function f(x)
        implicit none
        real::f,x

        f=sin(x)-x/2.0

        return

    end function
end program

