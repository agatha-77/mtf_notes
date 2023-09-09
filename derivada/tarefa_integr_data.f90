program integral_dados

	implicit none
	integer, parameter :: FILE_NUM = 10

	integer :: i, n
	real, allocatable :: time(:), vel(:)
	real :: soma, soma_trapezio, soma_simpson, h

	open(FILE_NUM,"data/input.dat")
	call linenumber(n,FILE_NUM)

	do i=1,n
		read(FILE_NUM,*) time(i), vel(i)
	end do
	close(FILE_NUM)

	soma = 0.0
	do i = 1, n
		soma = soma + y(i)
	end do	


end program integral_dados

subroutine linenumber (n , file_num )
	integer :: n, stat, file_num
	n = 0
	do
		read ( file_num ,* , iostat = stat )
		if ( stat /= 0) exit
		n=n +1
	end do
	rewind ( file_num )
end subroutine

subroutine integra_trapezio

end subroutine
