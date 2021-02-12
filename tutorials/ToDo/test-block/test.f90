program main
  implicit none
  integer :: i
  write( *, * ) "enter ++ i"
  read( *, * ) i

select case( i )
case( 1 )
  block
    use module1
    call foo( )
  end block
case( 2 )
  block
    use module2
    call foo( )
  end block
end select

end program main