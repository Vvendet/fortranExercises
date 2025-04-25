
Program Q1

    implicit none 
    
    real:: x,y, p
    real(8), parameter :: pi=3.14159274D0
    integer::i
    
    p= 2*pi/25
    
    open(unit=777, file='saida.dat', status='unknown')
    
    do i=0,25
    
        write(777,*) p*i, sin(p*i)
    
    end do 
    
    close(unit=777)
    
End Program Q1