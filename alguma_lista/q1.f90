program bissecao
implicit none
    real:: tolerancia, erro, a, b, c, f, certo
    integer:: k, kmax, verificador

    write(*,*)"Chute o valor para a:"
    read(*,*)a
    write(*,*)
    write(*,*)"Chute um valor para b:"
    read(*,*)b
    write(*,*)"O intervalo é: [",a,b,"]"
    tolerancia = 1e-3
    erro = 2d0
    k = 1
    kmax = 5
    certo = 0
    verificador = 0

        do while((erro > tolerancia) .and. (k .le. kmax))
            if(f(a)*f(b) < 0) then
            c = (a+b)/2
                if(f(a)*f(c)<0)then
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
        print*, "Uma raiz nesse intervalo será:",certo
        end if

end program bissecao

real function f(x)
    implicit none
    real:: x

    f = x**(3) - 6*(x**2) - x + 30
end function
