program Q6 !multiplicacao de matrizes quadradas

    implicit none

    integer::i,j,k,l, N, flop
    real::x,y, soma
    real(8), parameter :: pi=3.14159274D0
    real, dimension(:,:), allocatable::A,B,C

    write(*,*)'Escreva o tamanho das matrizes:'
    read(*,*)N !definir tamanho das matrizes

    allocate(A(N,N))
    allocate(B(N,N))
    allocate(C(N,N))
    flop = 0

    do i=1,N !gerar as matrizes para serem multiplicadas
        do j=1,N
            call random_number(x)
            A(i,j)=cos(x*pi)*5
            call random_number(y)
            B(i,j)=cos(y*pi)*5
        end do
    end do


    !realizar multiplicacao AB
    do i=1,N !iterar em cada linha

        do k=1,N !iterar em cada coluna de B
            soma = 0
            do j=1,N  !iterar em cada coluna de A
                soma = soma+A(i,j)*B(j,k)

            end do
            flop = flop + 1
            C(i,k) = soma
        end do

    end do

    do i=1,N
        write(*,*) A(i,:)
    end do
    write(*,*) ''
    do i=1,N
        write(*,*) B(i,:)

    end do
    write(*,*) ''

    do i=1,N
        write(*,*) C(i,:)
    end do

    write(*,*) flop, N

    deallocate(A)
    deallocate(B)
    deallocate(C)
end program
