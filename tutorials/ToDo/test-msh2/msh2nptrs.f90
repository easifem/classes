program main
  use easifem
  implicit none

  type( File_ ) :: inFile, outFile
  class( MSH2Elements_ ), pointer :: obj
  class( msh2physicalnames_ ), pointer :: names
  type( mesh_ ) :: omega
  integer :: i
  integer( i4b ), allocatable :: nptrs( : )
  logical( lgt ) :: ierr
  real( dfp ), allocatable :: XiJ( :, : )

  call inFile % OpenFileToRead( "./", "mesh", ".msh" )
  allocate( obj )

  allocate( names )
  call names % readfromfile( infile, ierr )
  call obj % writeElementsInDifferentFile( infile, names, "$Elements", "$EndElements" )

  call omega % initiate( element( ) )
  ! call obj % getelements( omega, infile, 2 )
  ! call obj % getelements( omega, infile, names, 2, 1 )
  call obj % getelements( omega, infile, names, 2, [1,2,3] )
  call omega % display( )
end program