program lista4
  implicit none

  double precision,dimension(:),allocatable::x,y,z
  integer::n,i

  n=100

  allocate(x(n))
  allocate(y(n))
  allocate(z(n))
  x(1)=0.1d0
  y(1)=0.1d0
  z(1)=0.1d0

  do i=2,100

     x(i)=g1(y(i-1),z(i-1))
     y(i)=g2(x(i),z(i-1))
     z(i)=g3(x(i-1),y(i))

     write(*,*)i,x(i),y(i),z(i)
     
  end do


  

  deallocate(x)
  deallocate(y)
  deallocate(z)
contains

  function g1(x2,x3)
    implicit none

    double precision::x2,x3,g1

    g1 =1.d0/6.d0+cos(x2*x3) 

  end function g1


  function g2(x1,x3)
    implicit none
    double precision::x1,x3,g2

    g2 = sqrt(1.06d0+sin(x3)+x1*x1)/9.d0-0.1d0
    

  end function g2


  function g3(x1,x2)
    implicit none
    double precision::x1,x2,g3
    real(8), parameter :: pi=3.14159274D0

    g3=(3.d0-10.d0*pi)/60.d0-1.d0/20.d0*exp(x1*x2)
  end function g3
  
end program lista4
