program posicao_falsa
    implicit none
    real :: tolerancia, erro, a, b, c, f, certo
    integer :: k, kmax, verificador

    write(*,*)"Estime o valor de a:"
    read(*,*)a
    write(*,*)
    write(*,*)"Estime o valor de b:"
    read(*,*)b
    write(*,*)"O intervalo digitado e: [",a,b,"]"

    tolerancia = 1e-3
    erro = 2d0
    k = 1
    kmax = 5
    certo = 0
    verificador = 0

    do while((erro > tolerancia) .and. (k <= kmax))

        if(f(a)*f(b) < 0) then
            c = ((a*f(b)) - (b*f(a)))/(f(b) - f(a))
            if(f(a)*f(c) < 0)then
                b = c
            else
                a = c
                certo = a
            end if
        else
            write(*,*)"Intervalo errado."
            verificador = 1
            exit
        end if
        erro = abs(f(c))
        k = k + 1
    end do

    if (verificador == 0) then
    print*, "Uma raiz nesse intervalo sera:", certo
    end if

end program posicao_falsa

!============Funcao==========================

real function f(x)
    implicit none
    real :: x

    f = 2*x**(3) + x**2 - 2
end function
