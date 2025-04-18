
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

        do i=1,100 !iterar sobre a função g(x) até encontrar um ponto próximo ao ponto fixo

            x(i) = f(x(i-1))
            if (abs(x(i)-0.73908513)<0.00000003) then !verificar se a aproximação está a 6 casas decimais
                write(777,*)  abs(x(0)-0.73908513), i !escrever no arquivo a iteração e a distância do valor ao ponto fixo
                exit !como já tem boa aproximação, terminar a iteração
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

        function g(x) !Esta função não tem ponto fixo assintoticamente estável.
            !O ponto fixo é x = 50
            !se x < 50, a função explode para -inf
            !se x > 50, a função explode para +inf
            !Apenas se x=50 a função converge.
        implicit none
        double precision:: x,g

        g = (4*x**3-420*x*x+10600*x-40000)/(3*x*x-280*x+5300)

        return

    end function
end program
