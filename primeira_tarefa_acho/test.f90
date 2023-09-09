program test
	character(len=2) :: a='a',b='b'
	character(len=12) :: other='b'
	integer :: i

	do i=1,10,1
		other = trim(other)//'a'
	end do

	write(*,*)other

end program
