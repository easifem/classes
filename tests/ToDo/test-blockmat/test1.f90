program main
  use globaldata
  use io
  use blockmat_class

  type( blockmat_ ) :: obj1, obj2
  real( dfp ) :: mat( 3, 3 )

  call random_number( mat )
  call displayarray( mat, "mat" )
  obj1 = mat
  call obj1 % display()
  obj2 = obj1
  call obj2 % display()

end program main