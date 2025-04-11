program Q2 !asseno do intervalo anteriormente calculado
    implicit none
    integer:: i
    real, dimension(25)::x, y,z


    open(unit=666,file='saida.dat',status='old') !abrir arquivo a ser lido
    do i=1,25 !leitura do arquivo
        read(666,*) x(i), y(i)
    end do
    close(unit=666) !fechar arquivo

    do i=1,25 !calcular aseno
        z(i)= asin(y(i))
    end do

    open(unit=777, file='saida.dat', status='old') !abrir arquivo para reescrever
    do i=1,25
        write(777,*) x(i), y(i), z(i) !escrever novos valores
    end do
    close(unit=777) !fechar

end program

