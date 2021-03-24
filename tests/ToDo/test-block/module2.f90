module module2
  implicit none

  contains

  subroutine foo()
    write( *, * ) "i am foo() in module2"
  end subroutine foo
end module module2