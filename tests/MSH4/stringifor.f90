program main
  use stringifor
  use penf_stringify

  type( string ) :: str1, str2

  str1 = string( Str( 1, .true. ) )
  write( *, * ) str1

  str2 = str1 % Join( [string( "hello"), string("world")], sep = "|"  )

  write( *, * ) str2
  
end program main