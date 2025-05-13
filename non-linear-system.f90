program lista4
  implicit none

  double precision,dimension(:),allocatable::x,y,z
  integer::n,i

  n=100

  allocate(x(n))
  allocate(y(n))
  allocate(z(n))
  !x(1)=0.1d0
  !y(1)=0.1d0
  !z(1)=0.1d0

  x(1)=0.25d0
  y(1)=0.25d0

  do i=2,100
     if (y(i-1)<0) then
        x(i)=h12(y(i-1))
        
     else
        x(i)=h11(y(i-1))
        
     end if
     y(i)=h2(x(i-1),y(i-1))

     write(*,*)i,x(i),y(i)
     
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



  function h11(x2)
    implicit none

    double precision::x2,h11

    h11=x2/sqrt(5.d0)

  end function h11

  function h12(x2)
    implicit none

    double precision::x2,h12

    h12=-x2/sqrt(5.d0)
    
  end function h12

  function h2(x1,x2)
    implicit none
    double precision::x1,x2,h2

    h2=(sin(x1)+cos(x2))/4.d0
    

  end function h2
end program lista4
