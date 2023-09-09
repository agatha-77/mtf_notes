program derivative
implicit none
	real :: a,b,x,h,f,df,ddf, passo
	real :: der1, der2

	10 format(4ES14.3)

	a = -4.0
	b = 4.0
	x = a
	h = 0.01
	passo = 0.001

	do while(x<=b)
		df = der1(x,h)
		ddf = der2(x,h)
		write(*,10) x,f(x),df,ddf
		x = x + passo
	end do

end program derivative


!############# AS DERIVADA AQUI #########################
real function der1(x,h)
	implicit none
	real :: x,h,f
	external f

	der1 = (f(x) - f(x-h))/h

end function

real function der2(x,h)
	implicit none
	real :: x,h,f
	external f

	der2 = (f(x + h) - 2*f(x) + f(x-h))/(h**2)

end function

!########## A FUNÇÂO AQUI #################
real function f(x)
	implicit none
	real :: x

	f = exp(-x**2)
end function
