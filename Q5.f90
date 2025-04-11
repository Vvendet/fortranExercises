program Q5 !matriz transposta
    implicit none
    integer:: N
    real:: x
    real, dimension(:,:), allocatable::A,AT
    integer::i,j



    write(*,*) 'Digite o tamanho da sua matriz quadrada:'
    read(*,*) N !definir tamanho da matriz

    allocate(A(N,N))
    allocate(AT(N,N))

    do i=1,N !gerar a matriz aleatoriamente
        do j=1,N
            call random_number(x)
            A(i,j)=cos(x)*100
        end do
    end do

    do i=1,N
        write(*,*) A(i,:)
    end do

    !calcular a transposta == linhas <-> colunas

    do i=1,N
        do j=1,N
            AT(i,j)=A(j,i)
        end do
    end do

    write(*,*) 'Sua matriz transposta e:'
    do i=1,N

        write(*,*) AT(i,:)
    end do

    deallocate(A)
    deallocate(AT)

end program
