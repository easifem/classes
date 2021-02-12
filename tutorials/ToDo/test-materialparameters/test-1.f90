! test key value constructor
program main
  use globaldata
  use keyvalue_class
  use string_class
  use materialparameter_class

  type( materialparameter_ ) :: obj
  type( keyvalue_ ) :: prop
  real( dfp ) :: mat2( 2, 2 )
  type( string_ ) :: str
  character( len = 100 ) :: chr
  real( dfp ) :: var
  logical( lgt ) :: boolvar


  call obj % append( keyvalue( "hello", 1 ) )
  call obj % append( keyvalue( "world", 2 ) )
  call obj % append( keyvalue( "!!!", [1.0_dfp]) )
  call obj % display( )


  boolvar = obj % param( 1 ) % Key .EQ. "hello"
  write( *, * ) boolvar

end program main
