!
! In this program we test MeshData_ class
!-------------------------------------------------------------------

PROGRAM MAIN
  USE EASIFEM

  TYPE( MSH2_ ) :: MSH
  TYPE( Mesh_ ) :: Omega, yABC
  TYPE( MeshData_ ) :: MeshInfo
  INTEGER( I4B )  ::  NSD,  iNode,  tElements,  tNodes, iel, &
    & DummyInt, iFacet, GlobalPt, iCell
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : ), FacetNptrs( : ), Pt2Elem( : ), &
    & BndyData( : )
  TYPE( MeshConnectivity_ ) :: OmegaToBoundary
  CLASS( GenericElement_ ), POINTER :: FacetElem, Elem
  NSD = 2
  CALL MSH % Initiate( FileName = "dam-soil", Extension = ".msh", &
    & Path = "./mesh1/", NSD = NSD )
  CALL Omega % Initiate( Element( ) )
  CALL MSH % getElements( Omega, NSD )
  CALL yABC % Initiate( Element( ) )
  CALL MSH % getElements( yABC, "HorizontalABC" )
  CALL MeshInfo % Initiate( Omega )
  CALL MeshInfo % InitiateNodeToElements( )
  CALL MeshInfo % InitiateBoundaryData( )
  tElements = .tElements. yABC

  CALL OmegaToBoundary % InitiateCellFacet( Omega, yABC, MeshInfo )

  WRITE( *, * ) OmegaToBoundary % CellFacet( 1, : )

END PROGRAM MAIN
