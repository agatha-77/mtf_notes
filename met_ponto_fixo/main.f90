program ponto_fixo
	implicit none
	real :: x, x0, tol, f_x, erro
	integer :: iter
	99 FORMAT("   n:", I2, "	x = ",F10.8,"	f = ",F10.8)

	print*, "Vamos calcular a raiz da função f(x) = x - tan(x/0.9)"
	print*, " "

	x = 0.25
	tol = 1d-6
	do while (ABS(x0 - x) > tol)
		x0 = x
		x = tanh(x/0.9)
		f_x = (x0 - x)

		if (iter < 100) then
			iter = iter + 1
			else
			exit
		end if
		write(*,99) iter, x0, f_x

		if(x0 > 1) then
			print*, "Não tem raiz no intervalo"
			exit
		end if
	end do

	print*, " "
	print*, "A raiz da função encontrada através do método de iteração é:", x0

end program ponto_fixo
