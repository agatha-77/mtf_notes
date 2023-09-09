program tudo_de_uma_vez
	implicit none
	character(len=20) :: file1="dados/q1a.dat", file2="dados/q1b.dat"

	write(*,*)""
	write(*,*)"-----------------------------------------"
	write(*,*)"|RESOLVENDO O SISTEMA DO ARQUIVO q1a.dat|"
	write(*,*)"-----------------------------------------"
	call lu_solver(trim(file1), len(trim(file1)))

	write(*,*)""
	write(*,*)""
	write(*,*)"-----------------------------------------"
	write(*,*)"|RESOLVENDO O SISTEMA DO ARQUIVO q1b.dat|"
	write(*,*)"-----------------------------------------"
	call lu_solver(trim(file2), len(trim(file2)))


end program tudo_de_uma_vez


subroutine linenumber(n,file_num)
	integer :: n,stat, file_num
	n = 0

	do
		read(file_num,*,iostat=stat)
		if(stat /= 0) exit
		n=n+1
	end do
	rewind(file_num)

end subroutine

subroutine lu_solver(file_name, fname_length)
	implicit none
	real :: mult
	real, allocatable :: mA(:,:), mL(:,:)
	real, allocatable :: b(:), x(:), y(:), c(:), s(:)
	integer :: i,j,k,n, fname_length
	character(len=fname_length) :: file_name

	90 format(11f8.2)

	open(10,file=file_name)
	call linenumber(n,10)

	allocate(mA(n,n+1), mL(n,n))
	allocate(b(n),x(n),y(n),c(n),s(n))

	write(*,*)"-> Matriz original do arquivo:"
	do i = 1,n
		read(10,*) (mA(i,j), j=1, n+1)
		write(*,90) (mA(i,j), j=1, n+1)
		b(i) = mA(i,n+1)
	end do

	mL = 0.0

	! Diagonalizacao
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
	write(*,*) "********** Matriz U **********"
	do i = 1,n
		write(*,90) (mA(i,j),j=1,n)
	end do

	write(*,*) ""
	write(*,*) "********** Matriz L **********"
	do i = 1,n
		write(*,90) (mL(i,j),j=1,n)
	end do

	! Resolvendo para y
	y = 0.0
	do i = 1,n
		c(i) = 0.0
		do j =1, i-1
			c(i) = c(i) + y(j) * mL(i,j)
		end do
		y(i) = b(i) - c(i)
	end do

	write(*,*)
	write(*,*) "Os valores de Y"
	do i = 1,n
		write(*,"(A5,11F8.1)")"Y = ",y(i)
	end do

	! Resolvendo para x
	x = 0.0
	do i = n,1,-1
		c(i) = 0.0
		do j = i+1,n
			c(i) = c(i) + mA(i,j)*s(j)
		end do
		s(i) = (y(i) - c(i))/mA(i,i)
	end do

	write(*,*)
	write(*,*) "Os valores de X"
	do i = 1,n
		write(*,"(A5,11F8.1)")"X = ",s(i)
	end do

	close(10)

end subroutine
