program sistemas_lineares

  implicit none

  double precision, dimension (:,:),allocatable::a
  double precision, dimension (:),allocatable::b,x,y
  integer::N,i,j

  write(*,*)"Escreva o tamanho da matriz A"
  read(*,*)N

  allocate(a(N,N))
  allocate(x(N))
  allocate(b(N))
  allocate(y(N))

 


  write(*,*)"Vamos preencher a matriz A"
  do i=1,N
     write(*,*)"Escreva os valores da linha", i
     do j=1,N
        read(*,*)a(i,j)
     end do

  end do
  write(*,*)"Vamos preencher a matriz B"
  do i=1,N
     write(*,*)"Escreva os valores da linha", i
     read(*,*) b(i)

  end do

  write(*,*)"Agora, os chutes iniciais"
  do i=1,N
     write(*,*)"Escreva os valores da linha", i
     read(*,*)x(i)
  end do


  call jacobi(N,a,b,y,x)

  write(*,*)x

  deallocate(a)
  deallocate(x)
  deallocate(b)
  deallocate(y)





contains

  subroutine seidel(N,a,b,x)
    implicit none

    double precision:: soma
    double precision, dimension (N,N):: a
    double precision, dimension (N):: x,b
    integer::i,j,k,N

    
    do k = 1,300

       do i = 1, n !iterar sobre as linhas

          soma = 0
       
          do j = 1,n!iterar sobre colunas

             if (i.ne. j) then !isolar tudo

                soma = soma + a(i,j)*x(j)
            
             end if
          
          
          end do

          x(i) = (b(i)-soma)/a(i,i) 
       
       end do
       write(*,*)x
    end do

  end subroutine seidel

  subroutine jacobi(N,a,b,y,x)

    implicit none

    double precision:: soma
    double precision, dimension (N,N):: a
    double precision, dimension (N):: x,b,y
    integer::i,j,k,N

    
    do k = 1,300
       x = y
       do i = 1, n !iterar sobre as linhas

          soma = 0
       
          do j = 1,n!iterar sobre colunas

             if (i.ne. j) then !isolar tudo

                soma = soma + a(i,j)*x(j)
            
             end if
          
          
          end do

          y(i) = (b(i)-soma)/a(i,i) 
       
       end do
       write(*,*)x
    end do

  end subroutine jacobi

  subroutine sor

  end subroutine sor
  
  
end program sistemas_lineares
