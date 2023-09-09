program molas_4
	implicit none
	real, allocatable :: mA(:,:)
	real, allocatable :: x0(:), x(:), d(:), y(:)
	real :: c, erro, tol, soma1, soma2
	integer :: i,j,k,kMAX,n

	90 format(10f8.1)
	91 format(5f8.3,i5)
	92 format("x_"I1," =	" f6.2)

	open(10,file="dados/q3.dat")
	call linenumber(n,10)
	allocate(mA(n,n+1), x0(n), x(n), d(n), y(n))

	write(*,*) "-----------------------------"
	write(*,*) "A matriz original do sistema:"
	do i = 1 ,n
		read(10,*) (mA(i,j),j = 1, n+1)
		y(i) = mA(i, n + 1)
		write(*,90) (mA(i,j),j = 1, n+1)
	end do

	k = 0
	kMAX = 10

	tol = 0.001
	erro=1.0
	x0(1) = 0
	x0(2) = 0
	x0(3) = 0
	x0(4) = 5

	print*,""
	print*,"----------------------------------"
	print*,"Iniciando calculo por Gauss-Seidel"
	print*,"----------------------------------"
	print*,"x1	x2	x3	x4	erro	i"
	do while(k < kMAX)
		do i = 1, n
			soma1 = 0
			do j= i+1, n

				soma1 = soma1 + mA(i,j)*x0(j)
			end do

			soma2 = 0
			do j = 1,i-1
				soma2 = soma2 + mA(i,j)*x0(j)
			end do

			x(i) = (mA(i,n+1) - soma1 - soma2)/mA(i,i)
		end do

		do i = 1,n
			d(i) = abs(x(i) - x0(i))
			x0(i) = x(i)
		end do
		erro = maxval(d)
		k = k+1
		write(*,91)x,erro,k

	end do

	print*,""
	print*,"----------"
	print*,"Resultado:"
	do i = 1,n
		write(*,92)i,x(i)
	end do

	close(10)

end program molas_4


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
