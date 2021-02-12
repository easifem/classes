program main
  implicit none
  
  real, pointer :: ptr1, ptr2, ptr3

  allocate( ptr1 ); ptr1 = 3.0;

  ptr2 => ptr1

  ptr3 => ptr2

  nullify( ptr2 )

  write( *, * ) ptr1, ptr3
end program main
