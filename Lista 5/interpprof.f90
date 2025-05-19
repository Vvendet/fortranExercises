program ajuste_interp

  implicit none


  double precision, dimension (:,:), allocatable :: a, atransp, k
  double precision, dimension (:), allocatable :: t, y, b, x
  double precision :: soma
  integer :: i, j, l, N, GRAU

  GRAU = 2
  N = 7

  allocate(a(N+1,GRAU+1))
  allocate(atransp(GRAU+1,N+1))
  allocate(k(GRAU+1,GRAU+1))
  allocate(t(N+1))
  allocate(y(N+1))
  allocate(b(GRAU+1))
  allocate(x(GRAU+1))

  t(1) = 0.03d0
    t(2) = 0.42d0
    t(3) = 0.76d0
    t(4) = 0.68d0
    t(5) = 0.93d0
    t(6) = 1.21d0
    t(7)=1.54d0
    t(8)=1.41d0

    y(1) = 6.45d0
    y(2) = 6.45d0
    y(3) = 8.76d0
    y(4) = 10.06d0 !4.097d0
    y(5) = 13.63d0
    y(6) = 27.37d0
    y(7) = 31.67d0
    y(8) = 41.29d0

  !construir a matriz A
  do i = 1, N+1
     do j=1,GRAU+1
        a(i,j) = t(i)**(j-1)
     end do
  end do

  !construir a transposta de A
  do i = 1, GRAU+1
     do j = 1, N+1
        atransp(i,j) = a(j,i)
     end do
  end do

  !construir a matriz atransp*a = K (de Gram)
  do i = 1, GRAU+1
     do j = 1, GRAU+1
        soma = 0.d0
        do l = 1, N+1
           soma = soma + atransp(i,l)*a(l,j)
        end do
        k(i,j) = soma
     end do
  end do

  !montar atransp*b
    do i=1,GRAU+1
        soma = 0.d0
        do j=1,N+1
            soma = soma+atransp(i,j)*y(j)

        end do
        b(i)=soma
    end do

  !Resolver o sistema regularizado
  call sistema(GRAU+1,k,x,b)

  !Resolver o sistema de Vandermonde para polinômio interpolador
  !call sistema(GRAU+1,a,x,y)

  write(*,*) x

  deallocate(a)
  deallocate(atransp)
  deallocate(k)
  deallocate(t)
  deallocate(y)
  deallocate(b)
  deallocate(x)

contains

  subroutine sistema(N, a, x, b)

    implicit none

    double precision, dimension (N,N) :: a
    double precision, dimension (N) :: x, b
    double precision :: soma
    integer :: i, j, k, N

    do k = 1, 10000
       do i=1,N !laço nas linhas
          soma = 0.d0
          do j=1,N !laço nas colunas
             if(j.ne.i) then !exluir coluna da posição diagonal
                soma = soma + a(i,j)*x(j)
             end if
          end do
          x(i) = (b(i) - soma)/a(i,i)
       end do
    end do

    return

  end subroutine sistema


end program ajuste_interp
