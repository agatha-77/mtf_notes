program the_big_one

interface matrizes
	function escala_gauss(matriz,n)
		real, intent(in), allocatable :: matriz (:,:)
		integer, intent(in) :: n
		real, intent(out) :: matriz_gauss(size(matriz),size(matriz))
	end function
end interface

	real, allocatable :: matriz_dummy(:,:)
	integer :: n,i,j

	open(10,file="q1.dat")
	call linenumber(n)
	allocate(matriz_dummy(n,n+1))
	do i=1,n
		read(10,*) (matriz_dummy(i,j),j=1,n+1)
	end do

	matriz_dummy = escala_gauss(matriz_dummy)

	close(10)

end program the_big_one

function escala_gauss(matriz)
	real, allocatable :: matriz(:,:), escala_gauss
	real :: mult
	integer :: n,i,j,k

	do i = 1, n-1
		do j = i + 1,n
			mult = matriz(j,i)/matriz(i,i)

			do k = i, n + 1
				matriz(j,k) = matriz(j,k) - mult* matriz(i,k)
			end do
		end do
	end do
	escala_gauss = matriz
end function

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
