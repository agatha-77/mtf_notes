program integral
	implicit none
	real(8) :: passo, soma, soma_simpson,x,a,b , n = 1000.0
	real(8) :: integral_intervalo,f, integral_simpson

	10 format(4F10.3)

	a = 0.0
	b = 3.0
	passo = (b-a)/n
	soma = 0.0
	soma_simpson = 0.0
	x = a

	write(*,*) ""
	write(*,*) "#########################################"
	write(*,*) "# Realizando calculo da integral por metodo de trapezio e simpson #"
	write(*,*) "#########################################"
	write(*,*) ""

	do while(x < b)
		write(*,10) x, f(x),soma, soma_simpson
		soma_simpson = soma_simpson + integral_simpson(x, x+passo)
		soma = integral_intervalo(x,x+passo) + soma
		x = x + passo
	end do

	write(*,*) ""
	write(*,*) "######################################"
	write(*,'(A,F12.8)')" # Integral por trapezio: ", soma
	write(*,'(A,F12.8)')" # Integral por simpson: ",soma_simpson

end program integral

real(8) function f(x)
	real(8) :: x

	f = cos(x)

end function

real(8) function integral_simpson(a,b)
	implicit none
	real(8) :: a,b,h,f
	external f

	integral_simpson = (f(a) + 4*f((a+b)/2) + f(b)) * (b-a)/6

end function

real(8) function integral_intervalo(a,b)
	implicit none
	real(8) :: a,b,h,f
	external f

	integral_intervalo = (f(a) + f(b))*(b-a)/2

end function
