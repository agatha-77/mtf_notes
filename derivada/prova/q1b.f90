program matrix_lu_solution
	implicit none
	real :: mult
	real, allocatable :: mA(:,:), mL(:,:)
	real, allocatable :: b(:), x(:), y(:), c(:), s(:)
	integer :: i,j,k,n

	open(10,FILE="matrizq1.dat")
	call linenumber(n)
	allocate(mA(n,n+1), mL(n,n))
	allocate(b(n),x(n),y(n),c(n),s(n))

	do i=1,n
		read(10,*) (mA(i,j),j=1,n+1)
	end do
	close(10)

	do i = 1,n
		write(*,"(11f8.2)") (mA(i,j), j = 1, n+1)
	end do

	do i = 1,n
		b(i) = mA(i,n+1)
	end do

	mL = 0.0

	do i = 1, n-1
		do j= i+1, n
			mult = mA(j,i)/mA(i,i)
			mL(j,i) = mult
			do k = i,n
				mA(j,k) = mA(j,k) - mult * mA(i,k)
			end do
		end do
	end do

	do i = 1,n
		mL(i,i) = 1
	end do

	write(*,*) ""
	write(*,*) "---------> MATRIZ U"
	do i = 1,n
		write(*,"(11f8.2)") (mA(i,j),j=1,n)
	end do

	write(*,*) ""
	write(*,*) "---------> MATRIZ L"
	do i = 1,n
		write(*,"(11F8.2)") (mL(i,j),j=1,n)
	end do

	y = 0.0
	do i = 1,n
		c(i) = 0.0
		do j = 1, i-1
			c(i) = c(i) + y(j) * mL(i,j)
		end do
		y(i) = b(i) - c(i)
	end do

	x = 0.0
	do i = n,1,-1
		c(i) = 0.0
		do j = i+1,n
			c(i) = c(i) + mA(i,j)*s(j)
		end do
		s(i) = (y(i) - c(i))/mA(i,i)
	end do

	write(*,*)
	write(*,*) "OS VALORES DO CAMINHAO"
	do i = 1,n
		write(*,"(A5,11F8.1)")"C = ",s(i)
	end do

end program matrix_lu_solution

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
