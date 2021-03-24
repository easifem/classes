program main
  use File_Class
  use msh2PhysicalNames_Class
  use GlobalData
  use string_Class
  use IO
  implicit none

  type( msh2PhysicalNames_ ) :: obj
  type( File_ ) :: aFile
  integer :: i
  integer, allocatable :: nptrs( : )
  logical( lgt ) :: ierr 

  call aFile % OpenFileToRead( "./", "mesh", ".msh" )
  call obj % readFromFile( aFile, ierr )

  call obj % writeToFile( aFile, " ", " " )

  ! nptrs = obj .index. [ String( "Dirichlet" ), String( "Nitsche" ) ]
  ! write( *, * ) nptrs
  
  ! call obj % readFromFile( aFile )

  ! nptrs = obj .index. [ String( "Dirichlet" ), String( "Nitsche" ) ]
  ! write( *, * ) nptrs

end program