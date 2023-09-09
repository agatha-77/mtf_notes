program derivada_dados
	implicit none
	integer :: i,n
	real :: h
	real, allocatable :: x(:), y(:), der_y(:)

	90 format(2f8.2)

	open(10,file="dados_q.dat")
	call linenumber(n,10)
	allocate(x(n), y(n), der_y(n))

	do i = 1,n
		read(10,*) x(i), y(i)
	end do
	close(10)

	do i = 1,n-1
		h = x(i) - x(i+1)
		der_y(i) = (y(i) - y(i+1))/h
		write(*,90) x(i), der_y(i)
	end do

	der_y(n) = (y(n-1) - y(n))/(x(n-1) - x(n))
	write(*,90) x(n), der_y(n)

end program derivada_dados

subroutine linenumber (n , file_num )
	integer :: n , stat , file_num
	n = 0
	do
		read ( file_num ,* , iostat = stat )
		if ( stat /= 0) exit
		n=n +1
	end do
	rewind ( file_num )
end subroutine
