PROGRAM capacidade_calorifica_secante
	IMPLICIT NONE
	REAL :: x, x0, erro
	CALL sec(x)
END PROGRAM capacidade_calorifica_secante


REAL FUNCTION f(x)
	REAL :: x, t
	cp= 7.29 + 2.374d-3 * x + 2.67d-7*x*x
	f = cp - 10
END FUNCTION f

SUBROUTINE sec ( x )
	IMPLICIT NONE
	REAL :: f
	REAL :: x , x0 , x1
	INTEGER :: K , Kmax
	K = 0
	x0 = 1500
	Kmax = 20
	x = 300
	DO
		x1 = ( x0 * f ( x ) - x * f ( x0 ))/( f ( x ) - f ( x0 ))
		IF ( abs ( f ( x1 )) .le. 0.0001) EXIT
		x0 = x
		x = x1
		K = K + 1
		print * , "O novo x sera:", x1
		IF ( K > Kmax ) STOP
	END DO
	print *, "A raiz sera (aproximadamente):", x1
END SUBROUTINE sec
