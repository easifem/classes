program main
  use module3
  implicit none

  call foo( nptrs = 4 )
  call foo( tag = "hello" )

end program main