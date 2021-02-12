module module3
	implicit none
	contains
	subroutine foo( nptrs, tag )
		integer, intent(in), optional :: nptrs
		character( * ), intent(in), optional :: tag

		if(present(tag)) then
			block
				integer :: nptrs
				nptrs = 4
				write( *, * ) "nptrs :: ", nptrs
			end block
		else
			write( *, * ) "nptrs :: ", nptrs
		end if
	end subroutine foo

end module module3