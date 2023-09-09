! Autor: Alfredo Pacheco
! Para a disciplina de Métodos Numéricos Para a Física

program binario
	integer :: num_decimal,num_binario_int
	character(len=50) :: num_binario_char

	write(*,*) "Insira o número para conversão"

	read*, num_decimal

	if(num_decimal .EQ. 1) then
		write(*,*) num_decimal
		STOP
	end if

	if(mod(num_decimal,2) .EQ. 0) then
		num_binario_char = "0"
		num_decimal = num_decimal/2
	else
		num_binario_char = "1"
		num_decimal = (num_decimal - 1)/2
	end if

	do while(num_decimal .NE. 1)
		if(mod(num_decimal,2) .EQ. 0) then
			num_binario_char = trim(num_binario_char)//"0"
			num_decimal = num_decimal/2
		else
			num_binario_char = trim(num_binario_char)//"1"
			num_decimal = (num_decimal - 1)/2
		end if
	end do

	! Falta o último dígito a ser inserido na conversão
	num_binario_char = trim(num_binario_char)//"1"
	call reverse(num_binario_char,len_trim(num_binario_char)) ! É necessário reverter a string do binário

	! Cria uma variável numérica com o valor binário...
	read(num_binario_char,'(I10)') num_binario_int

	write(*,*) num_binario_char
!	write(*,*) num_binario_int

CONTAINS

subroutine reverse(string,str_length)
	integer :: str_length,i
	character(len=str_length) :: string
	character :: temp

	do i=1,str_length/2
		temp = string(i:i)
		string(i:i) = string(str_length + 1 - i:str_length + 1 -i)
		string(str_length + 1 - i:str_length + 1 - i) = temp
	end do

end subroutine

end program
