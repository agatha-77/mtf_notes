program metodo_newton
  implicit none
  integer :: k, kmax
  real :: x, f, f_x, tolerancia

  x = 1.0
  tolerancia = 0.1
  kmax = 10 

  do k = 1, kmax

    f = exp(x) - tan(x)
    f_x = exp(x) - (1.0/cos(x))**2
    x = x - f/f_x

    if (abs(f) <= tolerancia) then
      write(*,*) "Raiz encontrada: x = ", x
      stop
    end if
  end do

  write(*,*) "Nao foi possivel encontrar a raiz dentro da tolerancia."
end program metodo_newton
