SUBMODULE( vtkType ) Methods
  !! This submodule implements methods defined for [[vtk_]] class

USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_init
  INTEGER( I4B ) :: ierr
  CHARACTER( LEN = 1000 ) :: tempstr

  obj % extension = trim( extension )
  obj % fmt = trim( fmt )
  obj % meshTopology = trim( meshtopology )

  IF( present( nx1 ) ) obj % nx1 = nx1
  IF( present( nx2 ) ) obj % nx2 = nx2
  IF( present( ny1 ) ) obj % ny1 = ny1
  IF( present( ny2 ) ) obj % ny2 = ny2
  IF( present( nz1 ) ) obj % nz1 = nz1
  IF( present( nz2 ) ) obj % nz2 = nz2

  IF( size( indx ) .EQ. 1 ) THEN
    ! /TimeStep/
    obj % path = trim( path ) // "/VTK/TimeStep/"

    obj % filename = trim( filename ) // "_" // trim( int2str( indx( 1 ) ) )

  ELSE

    ! /Iteration/TimeStep/
    obj % path = trim( path ) // "/VTK/Iteration/TimeStep_" &
              & // trim( int2str( indx( 1 ) ) )

    obj % filename = trim( filename ) // "_" // trim( int2str( indx( 2 ) ) )
  END IF

  tempstr = trim( obj % path ) // trim( obj % filename ) // trim( extension )

  CALL executecommand( "mkdir -p "// trim( obj % path ), &
    &  "vtkType@Methods.f90>>vtk_init()" )

  ! now directories are ready and we initiate vtk_file object

  ierr = obj % afile % initialize( &
    & format = trim( fmt ), &
    & filename = trim( tempstr ), &
    & mesh_topology = trim( meshTopology ), &
    & nx1 = obj % nx1, nx2 = obj % nx2, &
    & ny1 = obj % nx1, ny2 = obj % ny2, &
    & nz1 = obj % nz1, nz2 = obj % nz2 )
END PROCEDURE vtk_init

!----------------------------------------------------------------------------
!                                                                   Finalize
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_final
  INTEGER( I4B ) :: ierr

  IF( Obj % inPiece ) THEN
    ierr = obj % afile % xml_writer % write_piece( )
    Obj % inPiece = .FALSE.
  END IF

  ierr = obj % afile % finalize( )
  obj % path = ""
  obj % filename = ""
  obj % extension = ""
  obj % fmt = ""
  obj % meshtopology = ""
  obj % nx1 = 0
  obj % nx2 = 0
  obj % ny1 = 0
  obj % ny2 = 0
  obj % nz1 = 0
  obj % nz2 = 0
END PROCEDURE vtk_final

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_display
  INTEGER( I4B ) :: I

  IF( PRESENT( unitno ) ) THEN
    I = unitno
  ELSE
    I = stdout
  END IF

  CALL Blanklines( UnitNo = I )
  IF( LEN_TRIM( msg ) .NE. 0 ) WRITE( I, "(A)" ) TRIM( msg )
  CALL equalline( UnitNo = I )
  WRITE( I, "(A)" ) "PATH :: "//TRIM( obj % path )
  WRITE( I, "(A)" ) "FILENAME :: "//TRIM( obj % filename )
  WRITE( I, "(A)" ) "EXTENSION :: "//TRIM( obj % extension )
  WRITE( I, "(A)" ) "FORMAT :: "//TRIM( obj % fmt )
  WRITE( I, "(A)" ) "MESH TOPOLOGY :: "//TRIM( obj % meshtopology )
  CALL equalline( UnitNo = I )
  CALL Blanklines( UnitNo = I )
END PROCEDURE vtk_display

!----------------------------------------------------------------------------
!                                                                 openPiece
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_start_write_geo
END PROCEDURE vtk_start_write_geo

!----------------------------------------------------------------------------
!                                                                 closePiece
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_stop_write_geo
  INTEGER( I4B ) :: ierr
  IF( obj % inPiece ) THEN
    ierr = obj % afile % xml_writer % write_piece( )
    obj % inPiece = .FALSE.
  END IF
END PROCEDURE vtk_stop_write_geo

!----------------------------------------------------------------------------
!                                                              WriteMeshData
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_write_mesh_data_1
  INTEGER( I4B ) :: ierr, nc, np, nsd, ii, n, elemType, a, b
  TYPE( IntVector_ ) :: intvec( 2 )
  REAL( DFP ), ALLOCATABLE :: x( : ), y( : ), z( : )
  INTEGER( I4B ), ALLOCATABLE :: nptrs( : )
  INTEGER( Int8 ), ALLOCATABLE :: vtk_type( : )
  INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : )
  INTEGER( I4B ), ALLOCATABLE :: vtkIndx( : )

  IF( .NOT. mdobj % isInitiated ) THEN
    CALL mdobj % initiate( meshobj = meshobj )
  END IF

  np = mdobj % tnodes
  nc = meshobj % SIZE( )

  IF( .NOT. obj % inPiece ) THEN
    ierr = obj % afile % xml_writer % write_piece( np = np, nc = nc )
    obj % inPiece = .TRUE.
  END IF

  ALLOCATE( x( np ), y( np ), z( np ) )
  nsd = SIZE( nodes, 1 )

  SELECT CASE( nsd )

  CASE( 1 )

    DO ii = 1, np
      x( ii ) = nodes( 1, mdobj % nptrs( ii ) )
      y( ii ) = 0.0_DFP
      z( ii ) = 0.0_DFP
    END DO

  CASE( 2 )

    DO ii = 1, np
      x( ii ) = nodes( 1, mdobj % nptrs( ii ) )
      y( ii ) = nodes( 2, mdobj % nptrs( ii ) )
      z( ii ) = 0.0_DFP
    END DO

  CASE( 3 )

    DO ii = 1, np
      x( ii ) = nodes( 1, mdobj % nptrs( ii ) )
      y( ii ) = nodes( 2, mdobj % nptrs( ii ) )
      z( ii ) = nodes( 3, mdobj % nptrs( ii ) )
    END DO

  END SELECT

  ierr = obj % afile % xml_writer % write_geo( &
    & np = np, nc = nc, x = x, y = y, z = z )

  n = 0
  DO ii = 1, nc
    nptrs = meshobj % elem( ii ) % ptr % getNptrs( )
    n = n + size(nptrs)
  END DO

  CALL Initiate( intvec( 1 ), n )
  CALL Initiate( intvec( 2 ), nc )
  ALLOCATE( vtk_type( nc ) )

  !! global
  nptrs = meshobj % elem( 1 ) % ptr % getNptrs( )
  !! local
  local_nptrs = mdobj % local_nptrs( nptrs )
  elemType = meshobj % elem( 1 ) % ptr % RefElem % Name
  CALL getVTKelementType( elemType = elemType, &
    & vtk_type = vtk_type( 1 ), Nptrs = vtkIndx )
  nptrs = local_nptrs( vtkIndx )
  intvec( 1 ) % val( 1 : size( nptrs ) ) = nptrs-1
  intvec( 2 ) % val( 1 ) = size( nptrs )

  a = 0; b = size( nptrs )

  DO ii = 2, nc
    !! global
    nptrs = meshobj % elem( ii ) % ptr % getNptrs( )
    !! local
    local_nptrs = mdobj % local_nptrs( nptrs )

    elemType = meshobj % elem( ii ) % ptr % RefElem % Name

    CALL getVTKelementType( elemType = elemType, &
      & vtk_type = vtk_type( ii ), Nptrs = vtkIndx )

    nptrs = local_nptrs( vtkIndx )

    a = b + 1; b = b + size( nptrs )

    intvec( 1 ) % val( a : b ) = nptrs - 1
    intvec( 2 ) % val( ii ) = &
      & intvec( 2 ) % val( ii - 1 ) + size( nptrs )
  END DO

  ierr = obj % afile % xml_writer % write_connectivity( &
    & nc = nc, connectivity = intvec( 1 ) % val, &
    & offset = intvec( 2 ) % val, &
    & cell_type = vtk_type )

  DEALLOCATE( x, y, z, nptrs, local_nptrs, vtkIndx, vtk_type )
  CALL DeallocateData( intvec( 1 ) )
  CALL DeallocateData( intvec( 2 ) )

END PROCEDURE vtk_write_mesh_data_1

!----------------------------------------------------------------------------
!                                                              WriteMeshData
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_write_mesh_data_2
  INTEGER( I4B ) :: ierr, nc, np, nsd, ii, n, elemType, a, b
  TYPE( IntVector_ ) :: intvec( 2 )
  INTEGER( I4B ), ALLOCATABLE :: nptrs( : )
  INTEGER( Int8 ), ALLOCATABLE :: vtk_type( : )
  INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : )
  INTEGER( I4B ), ALLOCATABLE :: vtkIndx( : )

  np = size( nodes, 2 )
  nc = meshobj % SIZE( )

  IF( .NOT. obj % inPiece ) THEN
    ierr = obj % afile % xml_writer % write_piece( np = np, nc = nc )
    obj % inPiece = .TRUE.
  END IF

  nsd = SIZE( nodes, 1 )

  IF( nsd .ne. 3 ) THEN
    CALL Display( "ERROR:: vtkType@Methods.f90")
    CALL Display( "        vtk_write_mesh_data_2()")
    CALL Display( "        size( nodes, 1) should be 3")
    STOP
  END IF

  ierr = obj % afile % xml_writer % write_geo( &
    & np = np, nc = nc, xyz = nodes )

  n = 0
  DO ii = 1, nc
    nptrs = meshobj % elem( ii ) % ptr % getNptrs( )
    n = n + size(nptrs)
  END DO

  CALL Initiate( intvec( 1 ), n )
  CALL Initiate( intvec( 2 ), nc )
  ALLOCATE( vtk_type( nc ) )


  local_nptrs = meshobj % elem( 1 ) % ptr % getNptrs( )
  elemType = meshobj % elem( 1 ) % ptr % RefElem % Name

  CALL getVTKelementType( elemType = elemType, &
    & vtk_type = vtk_type( 1 ), Nptrs = vtkIndx )

  nptrs = local_nptrs( vtkIndx )
  intvec( 1 ) % val( 1 : size( nptrs ) ) = nptrs-1
  intvec( 2 ) % val( 1 ) = size( nptrs )

  a = 0; b = size( nptrs )

  DO ii = 2, nc
    !! global
    local_nptrs = meshobj % elem( ii ) % ptr % getNptrs( )

    elemType = meshobj % elem( ii ) % ptr % RefElem % Name

    CALL getVTKelementType( elemType = elemType, &
      & vtk_type = vtk_type( ii ), Nptrs = vtkIndx )

    nptrs = local_nptrs( vtkIndx )

    a = b + 1; b = b + size( nptrs )

    intvec( 1 ) % val( a : b ) = nptrs - 1
    intvec( 2 ) % val( ii ) = &
      & intvec( 2 ) % val( ii - 1 ) + size( nptrs )
  END DO

  ierr = obj % afile % xml_writer % write_connectivity( &
    & nc = nc, connectivity = intvec( 1 ) % val, &
    & offset = intvec( 2 ) % val, &
    & cell_type = vtk_type )

  DEALLOCATE( nptrs, local_nptrs, vtkIndx, vtk_type )
  CALL DeallocateData( intvec( 1 ) )
  CALL DeallocateData( intvec( 2 ) )

END PROCEDURE vtk_write_mesh_data_2

!----------------------------------------------------------------------------
!                                                              WriteMeshData
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_write_mesh_data_3
  INTEGER( I4B ) :: ierr, nc, np, nsd, ii, n, elemType, a, b, imesh, jj
  TYPE( IntVector_ ) :: intvec( 2 )
  REAL( DFP ), ALLOCATABLE :: x( : ), y( : ), z( : )
  INTEGER( I4B ), ALLOCATABLE :: nptrs( : )
  INTEGER( Int8 ), ALLOCATABLE :: vtk_type( : )
  INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : )
  INTEGER( I4B ), ALLOCATABLE :: vtkIndx( : )

  np = SIZE(nodes, 2)
  nc = 0
  DO jj=1,SIZE(tag)
    ii=tag(jj)
    nc = nc + meshobj(ii)%ptr%SIZE()
  END DO

  IF( .NOT. obj % inPiece ) THEN
    ierr = obj%afile%xml_writer%write_piece( np = np, nc = nc )
    obj % inPiece = .TRUE.
  END IF

  ALLOCATE( x( np ), y( np ), z( np ) )
  nsd = SIZE( nodes, 1 )

  SELECT CASE( nsd )

  CASE( 1 )

    DO ii = 1, np
      x( ii ) = nodes( 1, local2global(ii) )
      y( ii ) = 0.0_DFP
      z( ii ) = 0.0_DFP
    END DO

  CASE( 2 )

    DO ii = 1, np
      x( ii ) = nodes( 1, local2global(ii) )
      y( ii ) = nodes( 2, local2global(ii) )
      z( ii ) = 0.0_DFP
    END DO

  CASE( 3 )

    DO ii = 1, np
      x( ii ) = nodes( 1, local2global(ii) )
      y( ii ) = nodes( 2, local2global(ii) )
      z( ii ) = nodes( 3, local2global(ii) )
    END DO

  END SELECT

  ierr = obj % afile % xml_writer % write_geo( &
    & np = np, nc = nc, x = x, y = y, z = z )

  n = 0
  DO jj=1,SIZE(tag)
    imesh=tag(jj)
    DO ii = 1, meshobj(imesh)%ptr%SIZE()
      nptrs = meshobj(imesh)%ptr%elem(ii)%ptr%getNptrs( )
      n = n + size(nptrs)
    END DO
  END DO

  CALL Initiate( intvec( 1 ), n )
  CALL Initiate( intvec( 2 ), nc )
  ALLOCATE( vtk_type( nc ) )

  ! a = 0; b = size( nptrs )
  a = 0; b=0; n = 0

  DO jj=1,SIZE(tag)
    imesh=tag(jj)
    DO ii = 1, meshobj(imesh)%ptr%size()
      n=n+1
      !! global
      nptrs = meshobj(imesh)%ptr%elem(ii)%ptr%getNptrs( )
      local_nptrs = map(nptrs)
      elemType = meshobj(imesh)%ptr%elem(ii)%ptr%RefElem%Name
      CALL getVTKelementType( elemType = elemType, &
        & vtk_type = vtk_type( n ), Nptrs = vtkIndx )
      nptrs=local_nptrs(vtkIndx)
      a = b + 1; b = b + size( nptrs )
      intvec( 1 ) % val( a : b ) = nptrs - 1
      IF( n .EQ. 1 ) THEN
        intvec(2)%val(1) = size(nptrs)
      ELSE
        intvec( 2 ) % val( n ) = &
          & intvec( 2 ) % val( n - 1 ) + size( nptrs )
      END IF
    END DO
  END DO

  ierr = obj % afile % xml_writer % write_connectivity( &
    & nc = nc, connectivity = intvec( 1 ) % val, &
    & offset = intvec( 2 ) % val, &
    & cell_type = vtk_type )

  DEALLOCATE( x, y, z, nptrs, local_nptrs, vtkIndx, vtk_type )
  CALL DeallocateData( intvec( 1 ) )
  CALL DeallocateData( intvec( 2 ) )

END PROCEDURE vtk_write_mesh_data_3

!----------------------------------------------------------------------------
!                                                               openNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_start_write_node_data
  INTEGER( I4B ) :: ierr
  ierr = obj % afile % xml_writer % write_dataArray( location="node", &
    & action="open")
END PROCEDURE vtk_start_write_node_data

!----------------------------------------------------------------------------
!                                                             closeNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_stop_write_node_data
  INTEGER( I4B ) :: ierr
  ierr = obj % afile % xml_writer % write_dataArray( location="node", &
    & action="close")
END PROCEDURE vtk_stop_write_node_data

!----------------------------------------------------------------------------
!                                                             WriteNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_write_node_data_1

  INTEGER( I4B ) :: iname
  INTEGER( I4B ) :: ttime
  INTEGER( I4B ) :: tspace
  INTEGER( I4B ) :: tsize
  INTEGER( I4B ) :: is
  INTEGER( I4B ) :: ie
  INTEGER( I4B ) :: i
  INTEGER( I4B ) :: itime

  INTEGER( I4B ), ALLOCATABLE :: dofs( : )
  REAL( DFP ), ALLOCATABLE :: vals( :, : )
  CHARACTER( LEN = 50 ) :: data_name

  ! main program
  iname = IndexOf( dofobj, name )
  IF( iname .EQ. 0 ) RETURN

  ! decide scalar or vector
  IF( dofobj % map( iname, 2 ) .EQ. -1 ) THEN
    tspace = 1
  ELSE IF( dofobj % map( iname, 2 ) .GT. 0 ) THEN
    tspace = dofobj % map( iname, 2 )
  END IF

  tsize = dofobj % map( iname+1, 5 ) - dofobj % map( iname, 5 )
    !! total number of DOFs for iname
  ALLOCATE( dofs( tsize ) )
    !! dofs contains the index ids of DOFs corresponding to iname

  DO i = 0, tsize-1
    dofs( i+1 ) = dofobj % map( iname, 5 ) + i
  END DO

  ! If iname has only spacecompo (i.e. timecompo = 1)
  IF( dofobj % map( iname, 3 ) .EQ. 1 ) THEN

    data_name = trim( name ) // trim(prefix)

    IF( tspace .EQ. 2 ) THEN
      CALL getarrayvalues( v = vals, val = x, obj = dofobj, &
        & dofno = dofs( 1:tspace ), force3D = .true. )
    ELSE
      CALL getarrayvalues( v = vals, val = x, obj = dofobj, &
        & dofno = dofs( 1:tspace ) )
    END IF

    i = obj % afile % xml_writer % write_dataArray( data_name, vals )

  ELSE IF ( dofobj % map( iname, 3 ) .GT. 1 ) THEN

    ttime = dofobj % map( iname, 3 )
    itime = 1

    DO itime = 1, ttime

      data_name = trim( name ) // trim( int2str( itime ) )
      is = ( itime  - 1 ) * tspace + 1
      ie = is + tspace - 1

      IF( tspace .EQ. 2 ) THEN
        CALL getarrayvalues( v = vals, val = x, obj = dofobj, &
          & dofno = dofs( is:ie ), force3D = .true. )
      ELSE
        CALL getarrayvalues( v = vals, val = x, obj = dofobj, &
          & dofno = dofs( is:ie ) )
      END IF

      i = obj % afile % xml_writer % write_dataArray( data_name, vals )

    END DO

  END IF

  IF( ALLOCATED(  vals ) ) DEALLOCATE( vals )
  IF( ALLOCATED(  dofs ) ) DEALLOCATE( dofs )

END PROCEDURE vtk_write_node_data_1

!----------------------------------------------------------------------------
!                                                              WriteNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_write_node_data_2
  CHARACTER( LEN = 1 ), ALLOCATABLE :: names( : )
  INTEGER( I4B ) :: ii, n

  names = .names. dofobj
  n = SIZE( names )

  IF( PRESENT(prefix) ) THEN
    DO ii = 1, n
      CALL obj % vtk_write_node_data_1( x = x, dofobj = dofobj, &
        & name = names( ii ), prefix='_'//trim(prefix) )
    END DO
  ELSE
    DO ii = 1, n
      CALL obj % vtk_write_node_data_1( x = x, dofobj = dofobj, &
        & name = names( ii ), prefix='' )
    END DO
  END IF

  IF( ALLOCATED( names ) ) DEALLOCATE( names )
END PROCEDURE vtk_write_node_data_2

!----------------------------------------------------------------------------
!                                                           openElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_start_write_elem_data
  INTEGER( I4B ) :: ierr
  ierr = obj % afile % xml_writer % write_dataArray( location="cell", &
    & action="open")
END PROCEDURE vtk_start_write_elem_data

!----------------------------------------------------------------------------
!                                                           closeElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_stop_write_elem_data
  INTEGER( I4B ) :: ierr
  ierr = obj % afile % xml_writer % write_dataArray( location="cell", &
    & action="close")
END PROCEDURE vtk_stop_write_elem_data

!----------------------------------------------------------------------------
!                                                              writeElemData
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_write_cell_data_1
  INTEGER( I4B ) :: ii
  ii=obj%afile%xml_writer%write_dataArray( name, val )
END PROCEDURE vtk_write_cell_data_1
!----------------------------------------------------------------------------
!                                                              writeElemData
!----------------------------------------------------------------------------

MODULE PROCEDURE vtk_write_cell_data_2
  INTEGER( I4B ) :: ii
  ii=obj%afile%xml_writer%write_dataArray( name, val )
END PROCEDURE vtk_write_cell_data_2

END SUBMODULE Methods