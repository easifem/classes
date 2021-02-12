!
! In this program we test MeshData_ class
!-------------------------------------------------------------------

PROGRAM MAIN
  USE EASIFEM

  TYPE( Gmsh_MSH_ ) :: MSH
  TYPE( Mesh_ ) :: Omega, ElasticMesh1, ElasticMesh2
  TYPE( MeshData_ ) :: MeshDataOmega
  INTEGER( I4B )  ::  NSD,  iNode,  tElements,  tNodes, iel, &
                      & DummyInt
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )

  NSD = 3
  MSH = Gmsh_MSH( "mesh1", ".msh", "./mesh1/", NSD )

  Omega = Mesh( ElementObj = Element( ) )
  Omega = PhysicalTag
  CALL MSH % getVolumeElements( Omega )

  ElasticMesh1 = Mesh( Element( ) )
  CALL MSH % getSurfaceElements( ElasticMesh1, [String( "Top" )] )
  
  CALL MeshDataOmega % Initiate( ElasticMesh1 )
  CALL MeshDataOmega % InitiateNodeToNodes( ElasticMesh1 )
  CALL MeshDataOmega % InitiateNodeToElements( ElasticMesh1 )
  CALL MeshDataOmega % InitiateElementToElements( ElasticMesh1 )
  CALL MeshDataOmega % InitiateBoundaryData( ElasticMesh1 )

  ! WRITE( *, * ) MeshDataOmega % Nptrs
  WRITE( *, * ) "BoundaryNptrs:: "
  WRITE( *, * ) MeshDataOmega % BoundaryNptrs
  STOP

  tElements = SIZE( MeshDataOmega % BoundaryData )
  DO iel = 1, tElements
    CALL MeshDataOmega % BoundaryData( iel ) % Display( )
  END DO

  STOP

  tElements = .tElements. MeshDataOmega

  DO iel = 1, tElements
    DummyInt = MeshDataOmega .GlobalNptrs. iNode 
    CALL BlankLines( )
    WRITE( *, "(I4, A)", ADVANCE="NO" ) iel, " "
    CALL MeshDataOmega % ElementToElements( iel ) % Display( )
    CALL BlankLines( )
  END DO

  STOP

  tNodes = .tNodes. MeshDataOmega

  DO iNode = 1, tNodes
    DummyInt = MeshDataOmega .GlobalNptrs. iNode 
    CALL BlankLines( )
    WRITE( *, "(2I4, A)", ADVANCE="NO" ) iNode, DummyInt, " "
    ! CALL MeshDataOmega % NodeToNodes( iNode ) % Display( )
    CALL MeshDataOmega % NodeToElements( iNode ) % Display( )
    CALL BlankLines( )
  END DO

END PROGRAM MAIN