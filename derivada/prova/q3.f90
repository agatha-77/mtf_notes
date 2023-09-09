program piramide
	implicit none
	real, parameter :: LIM_SUP = 150, LIM_INF = 0
	integer, parameter :: N = 6

	integer :: i
	real :: passo, soma, soma_simpson,x, h, b
	real :: A, integral_simpson, soma_simpson1, soma_simpson2, trabalho
	real :: trab_escravo, escravos

	10 format(I3, 4F10.3)
	20 format(A, F10.3)

	h = (LIM_SUP - LIM_INF)/N

	soma_simpson1 = 0.0
	soma_simpson2 = 0.0

	do i = 1,N
		x = x+h
		if (mod(i,2) == 0) then
			soma_simpson1 = soma_simpson1 + A(x)
		else
			soma_simpson2 = soma_simpson2 + A(x)
		end if
		print 10, i, x, A(x) , soma_simpson1, soma_simpson2
	end do

	integral_simpson = (A(LIM_INF) + soma_simpson1*2 + soma_simpson2*4 + A(LIM_SUP)) * (h/3)

	trabalho = 2014 * integral_simpson

	trab_escravo = 17.062 * (10**6)
	escravos = trab_escravo / trabalho

	print 20, "O trabalho: ", trabalho
	print 20, "Para tal, precisaremos da seguinte quantidade de trabalhadores: ", escravos

end program piramide

real function A(x)
	real :: x

	A = x * 9/(4*(200 - x)**2)

end function
