program derivative
implicit none
	real :: a,b,x,h,f,df,ddf, passo
	real :: der1, der2

	10 format(4F14.3)

	a = -4.0
	b = 4.0
	x = a
	h = 0.01
	passo = 0.1

	do while(x<=b)
		ddf = der2(x,h)
		write(*,10) x,f(x),ddf
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

!########## A FUNCAOO AQUI #################
real function f(x)
	implicit none
	real :: x

	f = sin(x)
end function
