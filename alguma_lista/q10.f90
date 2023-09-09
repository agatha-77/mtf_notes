PROGRAM instante_t_secante
IMPLICIT NONE
REAL :: x
CALL sec ( x )
END PROGRAM instante_t_secante


SUBROUTINE sec ( x )
IMPLICIT NONE
REAL :: f
REAL :: x , x0 , x1
INTEGER :: K , Kmax
K = 0
x0 = 0
Kmax = 5
x = 4

DO
x1 = ( x0 * f ( x ) - x * f ( x0 ))/( f ( x ) - f ( x0 ))
    IF ( abs ( f ( x1 )) .le. 0.001) EXIT
    x0 = x
    x = x1
    K = K + 1
    print * , "O novo x sera:", x1
    IF ( K > Kmax ) STOP
END DO

print *, "A raiz sera (aproximadamente):", x1
END SUBROUTINE sec






!============Funcaoo==========================
real function f(x)    ! A=x para facilitar.
    implicit none
    real:: x,pi
    pi=3.1415
    f = 80 + 90*cos(pi*x/3)
end function
