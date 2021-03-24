program main
  use basetype
  use basemethod
  use mshType
  
  type( mshEntity_ ) :: obj
  type( mshEntity_ ), allocatable :: pointEntities( : )
  type( File_ ) :: aFile
  logical( LGT ) :: ierr
  integer( i4b ) :: tpoints, tcurves, tsurfaces, tvolumes, i
  
  call openFileToRead( aFile, "./", "mesh", ".msh" )
  call obj % GotoTag( aFile, ierr )
  read( aFile % UnitNo, * ) tpoints, tcurves, tsurfaces, tvolumes
  allocate( pointEntities( tpoints ) )
  do i = 1, tpoints
    call ReadPointEntity( pointEntities( i ), aFile, .false., ierr )
    write( *, * ) pointEntities( i ) % Uid, pointEntities( i ) % X, &
      & pointEntities( i ) % Y, pointEntities( i ) % Z, &
      & pointEntities( i ) % PhysicalTag
  end do
  end program main