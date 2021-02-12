program main
  use basetype
  use basemethod
  use StringiFor
  use mshType
  
  type( mshPhysicalNames_ ) :: obj
  type( File_ ) :: aFile
  logical( LGT ) :: ierr
  
  call openFileToRead( aFile, "./", "mesh", ".msh" )
  call obj % ReadFromFile( aFile, ierr )
  ! call display( obj, "obj" )

  ! block
  !   write( *, * ) "test:: size ", size( obj )
  !   write( *, * ) "test:: totalphysicalpoint ", TotalPhysicalPoints( obj )
  !   write( *, * ) "test:: totalphysicalCurve ", TotalPhysicalCurves( obj )
  !   write( *, * ) "test:: totalphysicalSurface ", TotalPhysicalSurfaces( obj )
  !   write( *, * ) "test:: totalphysicalVolume ", TotalPhysicalVolumes( obj )
  
  !   write( *, * ) "test:: getindex() ", getIndex( Obj, "bottom")
  !   write( *, * ) "test:: getindex() ", getIndex( Obj, "right")
  !   write( *, * ) "test:: getindex() ", getIndex( Obj, "top")
  !   write( *, * ) "test:: getindex() ", getIndex( Obj, "left")
  !   write( *, * ) "test:: getindex() ", getIndex( Obj, "dbc")
  !   write( *, * ) "test:: getindex() ", getIndex( Obj, "domain")
  !   write( *, * ) "test:: getindex() ", getIndex( Obj, "volume")
  
  !   write( *, * ) "test:: getindex() ", getIndex( Obj, &
  !     & [String("bottom"), String("right"), String("volume")])
  !   write( *, * ) "test:: getindex() ", getIndex( Obj, [1,0])
  !   write( *, * ) "test:: getindex() ", getIndex( Obj, [1,2])
  !   write( *, * ) "test:: getindex() ", getIndex( Obj, 1 )
  !   write( *, * ) "test:: getindex() ", getIndex( Obj, 2 )
  !   write( *, * ) "test:: getindex() ", getIndex( Obj, 3 )
  ! end block

  ! block
  !   type( string ), allocatable :: names( : )
  !   names = PhysicalPointNames( obj )
  !   write( *, * ) names
  !   names = PhysicalCurveNames( obj )
  !   write( *, * ) TRIM( names )
  ! end block

  ! block
  !   integer( i4b ), allocatable :: tag( : )
  !   tag = PhysicalPointTags( obj )
  !   write( *, * ) tag, size( tag )
  !   tag = PhysicalCurveTags( obj )
  !   write( *, * ) tag, size( tag )
  ! end block

end program main