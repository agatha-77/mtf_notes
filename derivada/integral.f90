program integral
	implicit none
	integer, parameter :: FILE_NUM = 10

	integer :: n,i,j,m
	real :: soma_trapezio, integral_trapezio
	real :: h, soma_simpson1, soma_simpson2, integral_simpson
	real, allocatable :: x(:), y(:)

	90 format("INTEGRAL POR TRAPEZIO: ", f8.2)
	80 format("INTEGRAL POR SIMPSON: ", f8.2)

	open(FILE_NUM, file = "data/input.dat")
	call linenumber(n,FILE_NUM)
	allocate(x(n),y(n))

	do i=1,n
		read(FILE_NUM,*) x(i), y(i)
	end do
	close(FILE_NUM)

	h = (x(n) - x(1))/n

!************* CALCULO TRAPEZIO *************
	soma_trapezio = 0.0

	do i=2,n-1
		soma_trapezio = soma_trapezio + y(i)
	end do

	integral_trapezio = (y(1) + y(n) + 2*soma_trapezio )*h/2

!************* CALCULO SIMPSON *************
	soma_simpson1 = 0.0
	soma_simpson2 = 0.0

	do i = 1, (n/2 - 1)
		soma_simpson1 = soma_simpson1 + y(2*i)
	end do

	do i = 1, (n/2)
		soma_simpson2 = soma_simpson2 + y(2*i - 1)
	end do

	integral_simpson = (y(1) + soma_simpson1*2 + soma_simpson2*4 + y(n)) * (h/3)

	print 90, integral_trapezio
	print 80, integral_simpson


end program integral

subroutine linenumber (n , file_num)
	integer :: n , stat , file_num
	n = 0

	do
		read ( file_num ,* , iostat = stat )
		if ( stat /= 0) exit
		n=n +1
	end do

	rewind ( file_num )
end subroutine
