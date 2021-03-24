program main
  use GlobalData
  use string_Class
  use File_Class
  use MSH_Module
  use IO
  implicit none

  Class( mshEntities_ ), pointer :: entities
  type( File_ ) :: inFile, outFile
  Class( MSHNodes_ ), pointer :: nodes
  Class( MSHElements_ ), pointer :: elements

  integer :: i, iblock
  integer, allocatable :: nptrs( : )

  call inFile % OpenFileToRead( "./", "mesh", ".msh" )

  allocate( entities )
  call entities % readFromFile( inFile )

  allocate( nodes )
  call nodes % readFromFile( inFile )

  allocate( elements )
  call elements % readFromFile( inFile )

  do iblock = 1, elements % numEntityBlocks
    SELECT CASE( elements % elements( iblock  ) % dimEntity )
      CASE( 0 )
        entities % PointEntity( elements % elements( iblock ) % TagEntity ) % elements => elements % elements( iblock )
      CASE( 1 )
        entities % CurveEntity( elements % elements( iblock ) % TagEntity ) % elements => elements % elements( iblock )
      CASE( 2 )
        entities % SurfaceEntity( elements % elements( iblock ) % TagEntity ) % elements => elements % elements( iblock )
      CASE( 3 )
        entities % VolumeEntity( elements % elements( iblock ) % TagEntity ) % elements => elements % elements( iblock )
    END SELECT
  end do

  stop

  call outfile % openfiletowrite( "./", "out", ".txt" )
  call elements % writetofile( outfile, "", "" )
end program