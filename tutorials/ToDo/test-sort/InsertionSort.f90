program main
  implicit none
  integer :: n, i, j, key, k
  integer, allocatable :: indx( : )

  write( *, * ) "enter the length"
  read( *, * ) n

  allocate( indx( n ) )
  do i = 1, n
    write( *, "(I4,A)", advance="no" ) i, " ::entry = "
    read( *, * ) indx( i )
  end do

  write( *, * ) "you have entered"
  write( *, * ) indx

  do j = 2, n
    key = indx( j )
    i = j -1;
    do
      if( i .gt. 0 .and. indx( i ) .gt. key ) then
        indx( i + 1 ) = indx( i )
        i = i - 1;
      else
        exit
      end if
    end do
    indx( i + 1 ) = key
  end do

  write( *, * ) "after sorting ::"
  write( *, * ) indx

end program main