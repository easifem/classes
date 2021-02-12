! The transfer intrinsic is Fortran 95's version of a void pointer.
! This program shows how to use transfer to encode a user-defined type
! in a character array.
program transfer_ex
  implicit none

  ! A user-defined data type
  type :: data_t
     real :: x,y
  end type data_t

  ! Data to be encoded
  type(data_t), target :: d

  ! Encode data as an array of characters (one byte each)
  character(len=1), dimension(:), allocatable :: enc
  integer :: length

  ! Stash our data in d
  d%x = 9.d0; d%y=10.d0;
  print *, 'Data: ', d%x, d%y

  ! Encode the data_t object in a character array
  length = size(transfer(d, enc))
  allocate(enc(length))
  enc = transfer(d, enc)
  print *, 'Encoded: ', enc

  ! Decode again as data_t
  d = transfer(enc, d)
  print *, 'Decoded: ', d%x, d%y

  ! Clean up
  deallocate(enc)
end program transfer_ex
