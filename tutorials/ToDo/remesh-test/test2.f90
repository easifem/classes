!-----------------------------------------------------------------------------
! In this program we test internal boundary data subroutine
!-----------------------------------------------------------------------------

PROGRAM MAIN
  USE EASIFEM

  TYPE( MSH2_ ) :: mshFile
  TYPE( Mesh_ ) :: Omega
  TYPE( MeshData_ ) :: Obj
  INTEGER( I4B )  ::  NSD,  iNode,  tElements,  tNodes, iel, &
    & DummyInt
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )

  NSD = 2
  CALL mshFile % Initiate( FileName="test2", Extension=".msh", Path="./test2/", NSD=NSD )
  CALL Omega % Initiate( Element( ) )
  CALL mshFile % getElements( Omega, "Omega1")
  CALL Obj % Initiate( Omega )
  CALL Obj % InitiateInternalBoundaryData(  )

END PROGRAM MAIN
