program matrix_gauss_solution
	real, allocatable :: matriz(:,:)
	real, allocatable :: resultado(:)
	real :: mult
	integer :: i,j,k,n

	open(10,FILE='matriz.dat')
	call linenumber(n)
	allocate (matriz(n,n+1))
	allocate (resultado(n))

	do i=1,n
		read(10,*) (matriz(i,j),j=1,N+1)
	end do

	write(*,*) "== A matriz original =="
	do i=1,n
		write(*,20) (matriz(i,j),j=1,N+1)
	end do
	close(10)

	! ########################
	! Eliminação gaussiana
	! ########################
	! -> Escalonamento:
	do i = 1, n-1
		do j = i + 1,n
			mult = matriz(j,i)/matriz(i,i)

			do k = i, n + 1
				matriz(j,k) = matriz(j,k) - mult* matriz(i,k)
			end do
		end do
	end do

	write(*,*) ""
	write(*,*) "== A matriz escalonada =="
	do i=1,n
		write(*,20) (matriz(i,j),j=1,N+1)
	end do

	do i = 1,n
		resultado(i) = matriz(i,i+1)/matriz(i,i)
	end do

	write(*,*) ""
	write(*,*) "A solução por método de Gauss é:"

	do i=1,n
		write(*,*)resultado(i)
	end do

	20 format(11f8.2)

end program matrix_gauss_solution

subroutine linenumber(n)
	integer :: n,stat
	n = 0

	do
		read(10,*,iostat=stat)
		if(stat /= 0) exit
		n=n+1
	end do
	rewind(10)

end subroutine
