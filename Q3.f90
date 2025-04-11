
program Q3 !ordenacao de valores do seno
    implicit none
    integer:: i,j
    real:: menor
    real, dimension(25):: x,y,z, sorted, y1

    open(unit=666,file='saida.dat',status='old')

    do i=1,25
        read(666,*) x(i), y(i), z(i) !ler os valores obtidos em saida.dat
    end do !os valores em y(i) sao os valores do seno
    y1 = y

    close(unit=666)

    menor = 999.0
    do j=1,25 !preencher os valores em sorted

        do i=1,25 !eliminar o menor numero
            if(menor==y1(i))then
                y1(i)= 999.0
                menor = 999.0
            end if
        end do

        do i=1,25 !percorrer y
            if (y1(i)<menor) then !comparar quem é menor
                menor = y1(i)
            end if

        end do
        sorted(j)=menor
    end do


    open(unit=777, file='saida_ordenada.dat', status='unknown') !abrir novo arquivo
    do i=1,25 !escrever no novo arquivo
        write(777,*) i, sorted(i)
    end do
    close(unit=777)


end program
