program paraquedas
	real, allocatable :: matriz(:,:)
	real(8), allocatable :: resultado(:)
	real :: mult
	integer :: i,j,k,n

	open(10,FILE='dados/q4.dat')
	90 format(A3,F8.3)
	call linenumber(n)
	allocate (matriz(n,n+1))
	allocate (resultado(n))

	do i=1,n
		read(10,*) (matriz(i,j),j=1,N+1)
	end do

	write(*,*) "-----------------"
	write(*,*) "A matriz do sistema"
	do i=1,n
		write(*,20) (matriz(i,j),j=1,N+1)
	end do
	close(10)

	! ########################
	! Eliminacao gaussiana
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
	write(*,*) "-------------------"
	write(*,*) "A matriz escalonada"
	do i=1,n
		write(*,20) (matriz(i,j),j=1,N+1)
	end do

	do i = 1,n
		resultado(i) = matriz(i,i+1)/matriz(i,i)
	end do

	write(*,*) ""
	write(*,*) "A solucao, por metodo de Gauss, para o sistema e:"
	write(*,90) "T =",resultado(1)
	write(*,90) "R =",resultado(2)
	write(*,90) "a =",resultado(3)



	20 format(11f8.2)

end program paraquedas

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
