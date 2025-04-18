program QB
    implicit none
    real, dimension(:), allocatable:: x,y,z,w,u
    integer::i

    allocate(x(0:100))
    allocate(z(0:100))
    allocate(w(0:100))
    allocate(y(0:100))
    allocate(u(0:100))
    x(0) = 1.5
    z(0) = 1.5
    u(0) = 1.5
    w(0) = 1.5
    y(0) = 1.5

    open(unit=111, file='i.dat', status='unknown')
    do i=1,100

        write(111,*) i, f(x(i-1))
        x(i) = f(x(i-1))

    end do
    close(unit=111)

    open(unit=222, file='ii.dat', status='unknown')
    do i=1,100

        write(222,*) i, g(w(i-1))
        w(i) = g(w(i-1))

    end do
    close(unit=222)

    open(unit=333, file='iii.dat', status='unknown')
    do i=1,100

        write(333,*) i, h(u(i-1))
        u(i) = h(u(i-1))

    end do
    close(unit=333)

    open(unit=444, file='iv.dat', status='unknown')
    do i=1,100

        write(444,*) i, p(y(i-1))
        y(i) = p(y(i-1))

    end do
    close(unit=444)

    open(unit=555, file='v.dat', status='unknown')
    do i=1,100

        write(555,*) i, q(z(i-1))
        z(i) = q(z(i-1))

    end do
    close(unit=555)




        contains

    function f(x) !Não convergiu

        implicit none
        real:: x,f

        f = x-x**3-4*x*x+10

        return
    end function

    function g(x) !Não convergiu

        implicit none
        real:: x,g

        g = sqrt(10/x-4*x)

        return
    end function

    function h(x) !Convergiu com 7 iterações

        implicit none
        real:: x,h

        h = sqrt(10/(4+x))

        return
    end function

    function p(x) !Convergiu com 4 iterações

        implicit none
        real:: x,p

        p = x - (x**3+4*x*x-10)/(3*x*x+8*x)

        return
    end function

    function q(x) !Convergiu com 21 iterações

        implicit none
        real:: x,q

        q = (sqrt(10-x**3))/2

        return
    end function

end program
