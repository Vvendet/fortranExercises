
program Q4 !simplesmente gerar numero aleatoriamente de -L a L
    implicit none
    real:: x(5)
    real(8), parameter :: pi=3.14159274D0
    integer::i

    call random_number(x) !gerar um numero aleatório

    do i=1,5
        write(*,*) cos(x(i)*pi)*10 !fazer variar de [-10,10]
    end do
end program
