program main
  use File_Class
  use msh2format_Class
  use GlobalData
  use string_Class
  use IO
  implicit none

  type( msh2format_ ) :: obj
  type( File_ ) :: aFile
  integer :: i
  integer, allocatable :: nptrs( : )
  logical( lgt ) :: ierr

  call aFile % OpenFileToRead( "./", "mesh", ".msh" )

  call obj % readFromFile( aFile, ierr )

  call obj % writetofile( aFile, "$Format", "$EndFormat" )

end program