program matriz_jacobiana
	implicit none
	real :: x,y,h,f1,f2
	real, dimension(2,2) :: jacob
	integer :: i,j

	x = 1.0
	y = 5.0
	h = 0.001

	do i = 1,2
		do j = 1,2
			call handler(i,j)
		end do
	end do

	do i=1,2
		write(*,*) (jacob(i,j), j=1 ,2)
	end do

CONTAINS
subroutine handler(j1,j2)
	implicit none
	real :: der_x,der_y
	integer :: j1,j2
	external der_x,der_y

	if(j1 == 1) then
		if(j2 == 1) then
			jacob(j1,j2) = der_x(x,y,h,1)
		else if (j2 == 2) then
			jacob(j1,j2) = der_y(x,y,h,1)
		end if
	else if(j1 == 2) then
		if(j2 == 1) then
			jacob(j1,j2) = der_x(x,y,h,2)
		else if (j2 == 2) then
			jacob(j1,j2) = der_y(x,y,h,2)
		end if
	end if
end subroutine

end program matriz_jacobiana


real function der_x(x,y,h,choser)
	implicit none
	real :: x,y,h,f1,f2
	integer :: choser
	external f1,f2

	if(choser == 1) der_x = (f1(x,y) - f1(x-h,y))/h
	if(choser == 2) der_x = (f2(x,y) - f2(x-h,y))/h

end function

real function der_y(x,y,h,choser)
	implicit none
	real :: x,y,h,f1,f2
	integer :: choser
	external f1,f2

	if(choser == 1) der_y = (f1(x,y) - f1(x,y-h))/h
	if(choser == 2) der_y = (f2(x,y) - f2(x,y-h))/h

end function


real function f1(x,y)
	implicit none
	real :: x,y

	f1 = x + y -3
end function

real function f2(x,y)
	implicit none
	real :: x,y

	f2 = x**2 + y**2 - 9
end function
