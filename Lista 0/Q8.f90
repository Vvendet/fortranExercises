program Q8 !Sequencia de fibonacci
    implicit none
    real, dimension(:), allocatable:: x
    integer:: N,i

    write(*,*)'Escreva quantos numeros de fibonaccia voce quer:'
    read(*,*)N

    allocate(x(N))

    x(1)=1
    x(2)=1

    do i=3,N !!calcular os elementos da sequencia fibonacci
        x(i)=x(i-1)+x(i-2)
    end do

    open(unit=777,file='fibonacci.dat',status='unknown') !Abrindo arquivo onde será colocado os valores

    do i=1,N
        write(777,*)x(i),INT(f(x(i))) !Escrevendo os valores no arquivo
    end do

    close(unit=777)

    deallocate(x)

    contains

    function f(x) !Funcao para calcular as raizes dos numeros de fibonacci
        implicit none
        real:: f,x

        f = sqrt(x(i))+ x(i)** (0.3333333333333333333)


        return
    end function

end program
