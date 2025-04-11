program Q7 !formula de bhaskara
    implicit none
    real:: a,b,c,delta

    write(*,*) 'escreva os coeficientes de sua equa��o quadrada:'
    read(*,*)a
    read(*,*)b
    read(*,*)c

    delta = b*b-4*a*c

    if (delta>0) then
        write(*,*)'Existem duas ra�zes reais distintas.'
        write(*,*)(-b+delta**(0.5))/2*a, (-b-delta**(0.5))/2*a

    else if (delta==0) then
        write(*,*)'Existem uma ra�z real de multiplicidade 2.'
        write(*,*)-b/2*a

    else
        write(*,*)'Existem duas ra�zes complexas.'
        write(*,*)-b/2*a,'+','i',(delta*(-1))**(0.5)/2*a, -b/2*a,'-','i',(delta*(-1))**(0.5)/2*a
    end if
end program
