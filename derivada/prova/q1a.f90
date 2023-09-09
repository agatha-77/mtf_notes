program caminhao
	integer, parameter :: FILE_NUM = 10

	real, allocatable :: matriz(:,:)
	real, allocatable :: resultado(:)
	real :: mult
	integer :: i,j,k,n

	90 format(A,I1, F8.3)

	open(FILE_NUM,FILE='matrizq1.dat')
	call linenumber(FILE_NUM,n)
	allocate (matriz(n,n+1))
	allocate (resultado(n))

	do i=1,n
		read(FILE_NUM,*) (matriz(i,j),j=1,N+1)
	end do

	close(FILE_NUM)

	! -> Escalonamento:
	do i = 1, n-1
		do j = i + 1,n
			mult = matriz(j,i)/matriz(i,i)

			do k = i, n + 1
				matriz(j,k) = matriz(j,k) - mult* matriz(i,k)
			end do
		end do
	end do

	do i = 1,n
		resultado(i) = matriz(i,i+1)/matriz(i,i)
	end do

	print *, "A solucao por metodo de gauss e:"

	do i = 1,n
		print 90, "C_", i, resultado(i)
	end do

end program caminhao


subroutine linenumber(FILE_NUM,n)
	integer :: n,stat, FILE_NUM
	n = 0

	do
		read(FILE_NUM,*,iostat=stat)
		if(stat /= 0) exit
		n=n+1
	end do
	rewind(FILE_NUM)

end subroutine
