program main
  use indexvector_class

  type(indexvector_) :: obj

  call obj % pushstart(  )
  call obj % push( 1 )
  call obj % display( )
  call obj % push( [2,3,4,5,6] )
  call obj % display(  )
  call obj % push( 10 )
  call obj % display(  )
  call obj % pushend(  )
  call obj % display(  )
end program main
