module module1
  implicit none

  contains

  subroutine foo()
    write( *, * ) "i am foo() in module1"
  end subroutine foo
end module module1