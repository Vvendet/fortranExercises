program QuestaoD
    implicit none
    real::a,b
    real,dimension(100)::x
    integer::i


    a=-2.5
    b=-1.5


    write(*,*)"Solucionando com metodo da"
    write(*,*)"POSICAO FALSA"
    do i=1,100

        x(i)=g(a,b)

        if (f(x(i))*f(a)<0) then

            a=x(i)

        else if (f(x(i))*f(b)<0) then

            b=x(i)

        end if

        if (abs(x(i)-(-1.895494))<0.00001) then
            write(*,*)i,x(i)
            exit

        end if
    end do

    write(*,*)"Solucionando com metodo da"
    write(*,*)"NEWTON RAPHSON"
    do i=2,100

        x(i)=h(x(i-1))

        if (abs(x(i)-(-1.895494))<0.00001) then
            write(*,*)i,x(i)
            exit

        end if
    end do

    write(*,*)"Solucionando com metodo da"
    write(*,*)"SECANTE"
    do i=3,100

        x(i)=s(x(i-1),x(i-2))

        if (abs(x(i)-(-1.895494))<0.00001) then
            write(*,*)i,x(i)
            exit

        end if
    end do






    contains

    function g(a,b)
        implicit none
        real::g,a,b

        g=(a*f(b)-b*f(a))/(f(b)-f(a))

    end function

    function h(x)
        implicit none
        real::h,x

        h=x-f(x)/flinha(x)

    end function

    function s(x,y)
        implicit none
        real::s,x,y

        s=x-(f(x)*(x-y))/(f(x)-f(y))
    end function

    function f(x)
        implicit none
        real::f,x

        f=sin(x)-x/2.0

        return

    end function

    function flinha(x)
        implicit none
        real::flinha,x

        flinha=cos(x)-1/2.0

        return

    end function




end program
