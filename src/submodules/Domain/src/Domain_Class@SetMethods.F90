! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

SUBMODULE(Domain_Class) SetMethods
USE BaseMethod
USE DomainConnectivity_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setSparsity1
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_setSparsity1"
  INTEGER( I4B ) :: imesh, dim, tmesh, lb, ub
  CLASS( Mesh_ ), POINTER :: meshobj
  ! main
  IF( .NOT. obj%isInitiated ) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "Domain is not initiated, first initiate")
  END IF
  meshobj => NULL()
  lb = LBOUND(obj%local_nptrs, 1)
  ub = UBOUND(obj%local_nptrs, 1)
  DO dim = 1, 3
    tmesh = obj%getTotalMesh( dim=dim )
    DO imesh = 1, tmesh
      meshobj => obj%getMeshPointer( dim=dim, entityNum=imesh )
      IF( ASSOCIATED( meshobj )  ) &
        & CALL meshobj%setSparsity( mat=mat, &
        & localNodeNumber=obj%local_nptrs, lbound=lb, ubound=ub )
    END DO
  END DO
  CALL setSparsity( mat )
  NULLIFY( meshobj )
END PROCEDURE Domain_setSparsity1

!----------------------------------------------------------------------------
!                                                               setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setSparsity2
  CHARACTER( LEN = * ), PARAMETER :: myName="Domain_setSparsity2"
  INTEGER( I4B ) :: rowMeshID, colMeshID, rowMeshSize, colMeshSize,  &
    & ivar, jvar, nsd( SIZE(domains ) )
  CLASS( Mesh_ ), POINTER :: rowMesh, colMesh
  CLASS( Domain_ ), POINTER :: rowDomain, colDomain
  TYPE( DomainConnectivity_ ) :: domainConn
  INTEGER( I4B ), POINTER :: nodeToNode( : )
  ! main
  !> check
  DO ivar = 1, SIZE( domains )
    IF( .NOT. ASSOCIATED(domains(ivar)%ptr) ) THEN
      CALL e%raiseError(modName//"::"//myName//" - "// &
        & 'DOMAINS( ' // TOSTRING(ivar) // ' ) NOT ASSOCIATED')
    ELSE
      IF( .NOT. domains(ivar)%ptr%isInitiated )  &
        & CALL e%raiseError(modName//"::"//myName//" - "// &
        & 'DOMAINS( ' // TOSTRING(ivar) // ' )%ptr NOT INITIATED')
    END IF
    nsd( ivar ) = domains( ivar )%ptr%getNSD()
  END DO
  !> check
  IF( ANY( nsd .NE. nsd( 1 ) ) ) &
    & CALL e%raiseError(modName//"::"//myName//" - "// &
    & 'It seems that NSD (number of spatial dimensions) of domains are &
    & not identical' )
  !> nullify first for safety
  rowMesh => NULL(); colMesh => NULL(); rowDomain=>NULL(); colDomain=>NULL()
  DO ivar = 1, SIZE( domains )
    rowDomain => domains( ivar )%ptr
    rowMeshSize=rowDomain%getTotalMesh( dim=nsd( ivar ) )
    DO jvar = 1, SIZE( domains )
      colDomain => domains( jvar )%ptr
      CALL domainConn%DEALLOCATE()
      CALL domainConn%InitiateNodeToNodeData( domain1=rowDomain, &
        & domain2=colDomain )
      nodeToNode => domainConn%getNodeToNodePointer()
      colMeshSize=colDomain%getTotalMesh( dim=nsd( jvar ) )
      !>
      DO rowMeshID = 1, rowMeshSize
          rowMesh => rowDomain%getMeshPointer( dim=nsd(ivar), &
            & entityNum=rowMeshID )
          IF( .NOT. ASSOCIATED( rowMesh ) ) CYCLE
        DO colMeshID = 1, colMeshSize
          colMesh => colDomain%getMeshPointer( dim=nsd(jvar), &
            & entityNum=colMeshID )
          IF( ASSOCIATED( colMesh ) ) &
            & CALL rowMesh%SetSparsity( mat=mat, &
            & colMesh=colMesh, &
            & nodeToNode = nodeToNode, &
            & rowGlobalToLocalNodeNum = rowDomain%local_nptrs, &
            & rowLBOUND = LBOUND(rowDomain%local_nptrs,1), &
            & rowUBOUND = UBOUND(rowDomain%local_nptrs,1), &
            & colGlobalToLocalNodeNum = colDomain%local_nptrs, &
            & colLBOUND = LBOUND(colDomain%local_nptrs,1), &
            & colUBOUND = UBOUND(colDomain%local_nptrs,1), &
            & ivar=ivar, jvar=jvar )
        END DO
      END DO
    END DO
  END DO
  CALL setSparsity( mat )
  NULLIFY( rowMesh, colMesh, rowDomain, colDomain, nodeToNode )
  CALL domainConn%DEALLOCATE()
END PROCEDURE Domain_setSparsity2

!----------------------------------------------------------------------------
!                                                          setTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setTotalMaterial
  INTEGER(I4B) :: ii
  CLASS(mesh_), POINTER :: meshptr
  !!
  DO ii = 1, obj%getTotalMesh(dim=dim)
    meshptr => obj%getMeshPointer(dim=dim, entityNum=ii)
    CALL meshptr%setTotalMaterial(n)
  END DO
  meshptr=>null()
END PROCEDURE Domain_setTotalMaterial

!----------------------------------------------------------------------------
!                                                          setTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setMaterial
  INTEGER(I4B) :: ii
  CLASS(mesh_), POINTER :: meshptr
  !!
  meshptr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
  CALL meshptr%setMaterial(medium=medium, material=material)
  meshptr=>null()
END PROCEDURE Domain_setMaterial

!----------------------------------------------------------------------------
!                                                        SetFacetElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setFacetElementType
  CLASS( Mesh_ ), POINTER :: masterMesh, slaveMesh
  INTEGER( I4B ) :: tsize, ii, jj, kk, iel, iface
  INTEGER( I4B ), ALLOCATABLE :: faceID( : ), faceNptrs( : )
  !!
  tsize = obj%getTotalMesh( dim=obj%nsd )
  !!
  DO ii = 1, tsize
    !!
    masterMesh => obj%getMeshPointer( dim=obj%nsd, entityNum=ii )
    !!
    DO iel = masterMesh%minElemNum, masterMesh%maxElemNum
      !!
      IF( .NOT. masterMesh%isElementPresent( iel ) ) CYCLE
      IF( .NOT. masterMesh%isBoundaryElement( iel ) ) CYCLE
      !!
      faceID = masterMesh%getBoundaryElementData( globalElement=iel )
      !!
      DO iface = 1, SIZE( faceID )
        !!
        kk = faceID( iface )
        faceNptrs = masterMesh%getFacetConnectivity( globalElement=iel, &
          & iface=kk )
        !!
        DO jj = 1, tsize
          IF( jj .NE. ii ) THEN
            slaveMesh => obj%getMeshPointer( dim=obj%nsd, entityNum=jj )
            IF( slaveMesh%isAllNodePresent( faceNptrs ) ) THEN
              CALL masterMesh%setFacetElementType( globalElement=iel, &
                & iface=kk, facetElementType=BOUNDARY_ELEMENT )
              EXIT
            END IF
          END IF
        END DO
        !!
      END DO
      !!
    END DO
    !!
  END DO
  !!
  NULLIFY( masterMesh, slaveMesh )
  !!
  IF( ALLOCATED( faceID ) ) DEALLOCATE( faceID )
  IF( ALLOCATED( faceNptrs ) ) DEALLOCATE( faceNptrs )
  !!
END PROCEDURE Domain_setFacetElementType

!----------------------------------------------------------------------------
!                                                      SetDomainFacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setDomainFacetElement
  CLASS( Mesh_ ), POINTER :: masterMesh, slaveMesh
  INTEGER( I4B ) :: tsize, ii, jj, kk, ll, mm, nn, iel, &
    & tDomFacet, tMeshFacet, tNeighbors, indx2( 2 )
  INTEGER( I4B ), ALLOCATABLE :: faceNptrs( : ), meshFacetMap( :, : ), &
    & dummyInt( : ), localToGlobalMesh( : ), cellNptrs( : ), faceType(:)
  LOGICAL( LGT ) :: faceFound
  !!
  !! main
  !!
  tsize = obj%getTotalMesh( dim=obj%nsd )
  !!
  DO ii = 1, tsize
    !!
    masterMesh => obj%getMeshPointer( dim=obj%nsd, entityNum=ii )
    tDomFacet = masterMesh%getTotalDomainFacetElements( )
    tMeshFacet = 0
    !!
    !! meshFacetMap( 1, : ) denotes BOUNDARY_ELEMENT or
    !! DOMAIN_BOUNDARY_ELEMENT
    !! meshFacetMap( 2, : ) denotes the slave-cell's mesh entity-number
    !!
    CALL Reallocate( meshFacetMap, 2, tDomFacet )
    CALL Reallocate( dummyInt, tSize )
    !!
    DO iel = 1, tDomFacet
      !!
      faceNptrs = masterMesh%getFacetConnectivity( &
        & facetElement=iel, &
        & elementType=DOMAIN_BOUNDARY_ELEMENT, &
        & isMaster=.TRUE. )
      !!
      faceFound = .FALSE.
      !!
      !! The code below checks if any other mesh contains the
      !! facetNptrs; if there exists such as mesh, then
      !! the face element is actually meshFacet.
      !!
      DO jj = 1, tsize
        IF( jj .NE. ii ) THEN
          !!
          slaveMesh => obj%getMeshPointer( dim=obj%nsd, entityNum=jj )
          !!
          IF( slaveMesh%isAllNodePresent( faceNptrs ) ) THEN
            !!
            faceFound = .TRUE.
            !! the following means iel face is mesh Facet
            meshFacetMap( 1, iel ) = BOUNDARY_ELEMENT
            !! the following means jj is the mesh number of iel facet
            meshFacetMap( 2, iel ) = jj
            tMeshFacet = tMeshFacet + 1
            EXIT
            !!
          END IF
        END IF
      END DO
      !!
      IF( .NOT. faceFound ) THEN
        !! the following means iel facet element is domainFacet
        meshFacetMap( 1, iel ) = DOMAIN_BOUNDARY_ELEMENT
        meshFacetMap( 2, iel ) = 0
      END IF
      !!
    END DO
    !!
    !! Now we know that there are tMeshFacet number of
    !! meshFacet element, and tDomFacet-tMeshFacet number of
    !! domainFacet elements.
    !!
    !! First we need to determine the number of neighboring meshes
    !! for ii mesh.That means distinct nonzero values in meshFacetMap(2,:)
    !!
    DO jj = 1, tDomFacet
      !!
      !! Get the mesh number
      !!
      kk = meshFacetMap( 2, jj )
      !!
      IF( kk .NE. 0 ) dummyInt( kk ) = dummyInt( kk ) + 1
      !!
    END DO
    !!
    !! Find out how many neighbour mesh is there
    !!
    tNeighbors = 0
    !!
    DO jj = 1, SIZE( dummyInt )
      IF( dummyInt( jj ) .NE. 0 ) tNeighbors = tNeighbors + 1
    END DO
    !!
    IF( ALLOCATED( masterMesh%meshFacetData ) ) &
      & DEALLOCATE( masterMesh%meshFacetData )
    ALLOCATE( masterMesh%meshFacetData( tNeighbors ) )
    CALL Reallocate( localToGlobalMesh, tNeighbors )
    !!
    kk = 0
    !!
    DO jj = 1, SIZE( dummyInt )
      IF( dummyInt( jj ) .NE. 0 ) THEN
        kk = kk + 1
        !! jj mesh is a neighbour, and kk is the local number of this mesh
        CALL masterMesh%meshFacetData( kk )%Initiate( dummyInt( jj ) )
        localToGlobalMesh( kk ) = jj
      END IF
    END DO
    !!
    !!
    !!
    DO ll = 1, tNeighbors
      !!
      indx2 = [BOUNDARY_ELEMENT, localToGlobalMesh( ll )]
      kk = 0
      !!
      DO jj = 1, tDomFacet
        !!
        IF( ALL(meshFacetMap( :, jj ) .EQ. indx2) ) THEN
          !!
          kk = kk + 1
          CALL masterMesh%meshFacetData( ll )%Set( &
            & facetElement=kk, &
            & domainFacetData=masterMesh%domainFacetData(jj) )
          !!
        END IF
      END DO
      !!
      slaveMesh => obj%getMeshPointer( dim=obj%nsd, &
        & entityNum=indx2( 2 ) )
      !!
      DO jj = 1, masterMesh%meshFacetData( ll )%Size()
        !!
        faceNptrs = masterMesh%getFacetConnectivity( &
          & facetElement=jj, &
          & elementType=BOUNDARY_ELEMENT, &
          & isMaster=.TRUE., &
          & facetBoundary=ll )
        !!
        DO kk = 1, slaveMesh%getTotalElements()
          !!
          mm = slaveMesh%getGlobalElemNumber( localElement=kk )
          !!
          IF( slaveMesh%isBoundaryElement( globalElement=mm ) ) THEN
            !!
            !! get facet type of slaveMesh
            !!
            faceType = slaveMesh%getFacetElementType( globalElement=mm )
            !!
            IF( ANY( faceType .EQ. BOUNDARY_ELEMENT ) ) THEN
              !!
              DO nn = 1, SIZE( slaveMesh%facetElements )
                !!
                cellNptrs = slaveMesh%getFacetConnectivity( &
                  & globalElement=mm, iface=nn )
                !!
                IF( faceNptrs .IN. cellNptrs ) THEN
                  !!
                  CALL masterMesh%meshFacetData( ll )%SetSlaveData( &
                    & facetElement=jj, &
                    & slaveCellNumber=mm, &
                    & slaveLocalFacetID=nn )
                  !!
                END IF
                !!
              END DO
              !!
            END IF
            !!
          END IF
          !!
        END DO
        !!
      END DO
      !!
    END DO
    !!
  END DO
  !!
END PROCEDURE Domain_setDomainFacetElement

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
