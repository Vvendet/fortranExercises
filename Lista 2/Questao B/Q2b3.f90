


program QuestaoB
    implicit none

    real, dimension(100)::x
    integer::i

    x(1)=1.7

    open(unit=777,file='saida.dat',status='unknown')
    do i =2,100
        x(i)=f(x(i-1))
        write(777,*)i,x(i)
    end do
    close(unit=777)



    contains

    function f(x)
        implicit none
        real::f,x

        f = 2*x-(1/sqrt(sqrt(sqrt(3.0))))*x*x

        return
    end function
end program
