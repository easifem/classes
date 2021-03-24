program main
  use Utility, only: displayarray
  use GlobalData
  use string_Class
  use File_Class
  use MSH2Nodes_Class
  use node_class
  use IO
  use groupdof_class
  implicit none

  type( File_ ) :: inFile, outFile
  class( MSH2Nodes_ ), pointer :: obj
  integer :: i
  real( dfp ), allocatable :: XiJ( :, : )
  type( node_ ) :: nodes
  type( groupdof_ ) :: X

  call inFile % OpenFileToRead( "./", "mesh", ".msh" )

  allocate( obj )

  ! call obj % getNodes( inFile, "", "" )

  ! call obj % getNodes( XiJ, 2, inFile )
  ! call displayarray( XiJ, "XiJ" )

  ! call obj % getNodes( nodes, 2, inFile )
  ! call nodes % display( )

  call obj % getNodes( X, 2, inFile )
  call X % Display( )

end program