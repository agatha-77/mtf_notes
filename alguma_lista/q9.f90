PROGRAM angulo_positivo_secante
IMPLICIT NONE
REAL :: x
CALL sec ( x )
END PROGRAM angulo_positivo_secante


SUBROUTINE sec (x)
IMPLICIT NONE
REAL :: f
REAL :: x , x0 , x1
INTEGER :: K , Kmax
K = 0
x0 = 0
Kmax = 5
x = 0.125664

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



real function f(x)
    implicit none
    real:: x,h,d,pi,ff
    pi=3.1415
    h=300
    FF=0.8
    D=14
    f = (FF*pi*(h/cos(x))**2)/(0.5*pi*D*D*(1+sin(x)-0.5*cos(x)))-1200
end function
