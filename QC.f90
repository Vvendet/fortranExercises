
program QC
    implicit none
    real::t
    double precision, dimension(:), allocatable:: x,y
    integer::i,j

    allocate(x(0:100))
    allocate(y(0:100))

    open(unit=777, file='QC.dat', status='unknown') ! Abrir arquivo

    do j = 1,200 !iterar sobre os chutes iniciais
        t = j
        x(0)=cos(t)

        do i=1,100 !iterar sobre a fun��o g(x) at� encontrar um ponto pr�ximo ao ponto fixo

            x(i) = f(x(i-1))
            if (abs(x(i)-0.73908513)<0.00000003) then !verificar se a aproxima��o est� a 6 casas decimais
                write(777,*)  abs(x(0)-0.73908513), i !escrever no arquivo a itera��o e a dist�ncia do valor ao ponto fixo
                exit !como j� tem boa aproxima��o, terminar a itera��o
            end if

        end do
    end do




    close(unit=777) !fechar arquivo


    contains

    function f(x)
        implicit none
        double precision:: x,f

        f = cos(x)

        return

    end function

        function g(x) !Esta fun��o n�o tem ponto fixo assintoticamente est�vel.
            !O ponto fixo � x = 50
            !se x < 50, a fun��o explode para -inf
            !se x > 50, a fun��o explode para +inf
            !Apenas se x=50 a fun��o converge.
        implicit none
        double precision:: x,g

        g = (4*x**3-420*x*x+10600*x-40000)/(3*x*x-280*x+5300)

        return

    end function
end program
