! The transfer intrinsic is Fortran 95's version of a void pointer.
! This program shows how to use transfer to encode a user-defined type
! in a character array.
program main
  use easifem
  implicit none

  type(genericelement_), target :: elem
  character(len=1), dimension(:), allocatable :: enc
  integer :: length

  elem % nptrs = [1,2,3,4]
  elem % nsd = 2
  elem % xidimension =2
  elem % Mat_type = 2
  elem % elemtopology = 2

  call elem % display(  )

  ! Encode the data_t object in a character array
  length = size(transfer(elem, enc))
  allocate(enc(length))
  enc = transfer(elem, enc)
  print *, 'Encoded: ', enc

  ! Decode again as data_t
  call elem % deallocatedata()
  elem = transfer(enc, elem)
  call elem % display()

  ! Clean up
  deallocate(enc)
end program main 
