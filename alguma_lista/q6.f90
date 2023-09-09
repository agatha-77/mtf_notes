PROGRAM tempo_de_queda_secante
IMPLICIT NONE
REAL :: x, x0, erro

CALL sec(x)

END PROGRAM tempo_de_queda_secante


REAL FUNCTION f(x)
	REAL :: x, s0, v_0, g
	s0 = 5
	v_0 = 30
	g = - 9.81
	f = s0 + v_0*x + g*x**2/2
END FUNCTION f


SUBROUTINE sec(x)
	IMPLICIT NONE
	REAL :: f
	REAL :: x, x0, x1
	INTEGER :: K, Kmax
	K = 0
	x0 = 2
	Kmax = 10
	x = 6

	DO
		x1 = (x0 * f(x) - x * f(x0))/(f(x) - f(x0))
		IF (abs(f(x1)) <= 0.0001) EXIT
		x0 = x
		x = x1
		K = K + 1
		print * , "O proximo valor para x sera:", x1
		IF ( K > Kmax ) STOP
	END DO
	print*, "A raiz sera aproximadamente:", x1
END SUBROUTINE sec
!"=================================================="
