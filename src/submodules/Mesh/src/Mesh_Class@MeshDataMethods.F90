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

SUBMODULE(Mesh_Class) MeshDataMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_InitiateNodeToElements
  ! Define internal  variables
  INTEGER( I4B ) :: ii, jj,  globalElemNum
  INTEGER( I4B ), ALLOCATABLE :: local_nptrs( : )
  CHARACTER( LEN = * ), PARAMETER :: myName="mesh_InitiateNodeToElements"
  !!
  !! check
  !!
  IF( obj%isNodeToElementsInitiated ) THEN
    CALL e%raiseWarning(modName//"::"//myName//" - "// &
      & "NodeToElements information is already initiated. If you want to &
      & Reinitiate it then deallocate nodeData, first!!" )
    RETURN
  END IF
  !!
  obj%isNodeToElementsInitiated = .TRUE.
  DO ii = 1, obj%tElements
    globalElemNum = obj%getGlobalElemNumber( ii )
    local_nptrs = obj%getLocalNodeNumber(obj%getConnectivity(globalElemNum))
    DO jj = 1, SIZE( local_nptrs )
      CALL Append( obj%nodeData( local_nptrs( jj ) )%globalElements, &
        & globalElemNum )
    END DO
  END DO
  IF( ALLOCATED( local_nptrs ) ) DEALLOCATE( local_nptrs )
END PROCEDURE mesh_InitiateNodeToElements

!----------------------------------------------------------------------------
!                                                        InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_InitiateNodetoNodes
  ! Define internal  variables
  INTEGER( I4B ) :: iel, iLocalNode, tSize, iGlobalNode
  INTEGER( I4B ), ALLOCATABLE ::  globalNodes( : ), NearElements( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_InitiateNodetoNodes"
  !!
  !! check
  !!
  IF( obj%isNodeToNodesInitiated ) THEN
    CALL e%raiseWarning(modName//"::"//myName//" - "// &
      & "Node to node information is already initiated. If you want to &
      & Reinitiate it then deallocate nodeData, first!!" )
    RETURN
  END IF
  !!
  IF( .NOT. obj%isNodeToElementsInitiated ) &
    & CALL obj%InitiateNodeToElements( )
  !!
  obj%isNodeToNodesInitiated = .TRUE.
  DO iLocalNode = 1, obj%tNodes
    iGlobalNode = obj%getGlobalNodeNumber( iLocalNode )
    NearElements = obj%getNodeToElements( iGlobalNode )
    DO iel = 1, SIZE ( NearElements )
      globalNodes = obj%getConnectivity( NearElements(iel) )
      globalNodes = PACK( globalNodes, globalNodes .NE. iGlobalNode )
      CALL Append( obj%nodeData( iLocalNode )%globalNodes, globalNodes )
    END DO
    CALL RemoveDuplicates( obj%nodeData( iLocalNode )%globalNodes )
  END DO
  IF( ALLOCATED( globalNodes ) ) DEALLOCATE( globalNodes )
  IF( ALLOCATED( NearElements ) ) DEALLOCATE( NearElements )
END PROCEDURE mesh_InitiateNodetoNodes

!----------------------------------------------------------------------------
!                                                 InitiateElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_InitiateElementToElements
  ! Define internal  variables
  INTEGER( I4B ) :: i, j, r,  iel1, tFace, iFace1, NNS1, pt1, &
    & iel2, iFace2, NNS2, localElem1
  INTEGER( I4B ), ALLOCATABLE :: global_nptrs1( : ),   &
    & global_nptrsFace1( : ), n2e1( : ), global_nptrs2( : ), &
    & global_nptrsFace2( : )
  LOGICAL( LGT ) :: Found
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_InitiateElementToElements"
  !!
  !! check
  !!
  IF( .NOT. ASSOCIATED( obj%refelem ) ) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "Unable to identify the Reference element of the mesh, &
      & may be it is not set" )
  END IF
  !!
  !! check
  !!
  IF( obj%isElementToElementsInitiated ) THEN
    CALL e%raiseWarning(modName//"::"//myName//" - "// &
      & "element to element information is already initiated. If you want to &
      & Reinitiate it then deallocate nodeData, first!!" )
    RETURN
  END IF
  !!
  IF( .NOT. ALLOCATED( obj%FacetElements ) ) THEN
    obj%FacetElements = FacetElements( obj%refelem )
  END IF
  !!
  tFace = SIZE(obj%FacetElements)
    !! Total number of facet elements
  obj%isElementToElementsInitiated = .TRUE.
  !!
  IF( .NOT. obj%isNodeToElementsInitiated ) THEN
    CALL obj%InitiateNodeToElements()
  END IF
  !!
  DO localElem1 = 1, obj%tElements
    iel1 = obj%getGlobalElemNumber( localElem1 )
    global_nptrs1 = obj%getConnectivity(iel1)
    !!
    !! getting node numbers of element iel1
    !!
    DO iFace1 = 1, tFace
      FOUND = .FALSE.
      global_nptrsFace1 = global_nptrs1( getConnectivity( &
        & obj%FacetElements( iFace1) ) )
      !! getting global node number in face iFace1
      NNS1 = SIZE( global_nptrsFace1 )
      !! number of nodes in iFace1
      pt1 = global_nptrsFace1( 1 )
      !! select a point on facet iFace1
      n2e1 = obj%getNodeToElements( GlobalNode = pt1 )
      !! get elements connected to the node pt1
      DO iel2 = 1, SIZE( n2e1 )
        IF( iel1 .EQ. n2e1( iel2 ) ) CYCLE
        global_nptrs2 = obj%getConnectivity( n2e1( iel2 ) )
        DO iFace2 = 1, tFace
          !! getting total number of nodes in iFace2
          global_nptrsFace2 = global_nptrs2( &
            & getConnectivity( obj%FacetElements(iFace2)) )
          NNS2 =SIZE(global_nptrsFace2)
          r = 0
          DO i = 1, NNS2
            DO j = 1, NNS1
              IF( global_nptrsFace2( i ) .EQ. global_nptrsFace1( j ) ) THEN
                r = r + 1
              END IF
            END DO
          END DO
          IF( r .EQ. NNS1 ) THEN
            CALL APPEND( obj%elementData( localElem1 )%globalElements, &
              & [n2e1(iel2), iFace1, iFace2] )
            FOUND = .TRUE.
            EXIT
          END IF
        END DO
        IF( FOUND ) EXIT
      END DO
    END DO
    !!
    IF( INT( SIZE( obj%elementData( localElem1 )%globalElements ) / 3 ) &
      & .NE. tFace ) THEN
      obj%elementData( localElem1 )%elementType = BOUNDARY_ELEMENT
    END IF
  END DO
  !!
  IF( ALLOCATED( global_nptrs1 ) ) DEALLOCATE( global_nptrs1 )
  IF( ALLOCATED( global_nptrs2 ) ) DEALLOCATE( global_nptrs2 )
  IF( ALLOCATED( global_nptrsFace1 ) ) DEALLOCATE( global_nptrsFace1 )
  IF( ALLOCATED( global_nptrsFace2 ) ) DEALLOCATE( global_nptrsFace2 )
  IF( ALLOCATED( n2e1 ) ) DEALLOCATE( n2e1 )
END PROCEDURE mesh_InitiateElementToElements

!----------------------------------------------------------------------------
!                                                       InitiateBoundaryData
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_InitiateBoundaryData
  ! Define internal variables
  INTEGER( I4B ) :: iel, tFace, ii, jj, kk
  INTEGER( I4B ), ALLOCATABLE :: global_nptrs( : ), ElemToElem( :, : )
  CHARACTER( LEN = * ), PARAMETER :: myName="mesh_InitiateBoundaryData"
  !!
  !! check
  !!
  IF( obj%isBoundaryDataInitiated ) THEN
    CALL e%raiseWarning(modName//"::"//myName//" - "// &
      & "Boundary data information is already initiated. If you want to &
      & Reinitiate it then deallocate nodeData, first!!" )
    RETURN
  END IF
  !!
  obj%isBoundaryDataInitiated = .TRUE.
  !!
  IF( .NOT. obj%isElementToElementsInitiated ) &
    & CALL obj%InitiateElementToElements( )
  !!
  IF( .NOT. ALLOCATED( obj%FacetElements ) ) &
    & obj%FacetElements = FacetElements( obj%refelem )
  !!
  tFace = SIZE( obj%FacetElements )
  !!
  !! Case of single element in the mesh
  !!
  IF( obj%tElements .EQ. 1 ) THEN
    obj%elementData( 1 )%elementType = BOUNDARY_ELEMENT
    tFace = SIZE( obj%FacetElements )
    obj%elementData( 1 )%boundaryData =  [(ii, ii=1, tFace)]
  ELSE
    !
    ! Now we will include those elements in boundary elements
    ! which contains the boundary nodes
    !
    DO ii = 1, obj%tElements
      iel = obj%getGlobalElemNumber( ii )
      global_nptrs = obj%getConnectivity( iel )
      DO jj = 1, SIZE( global_nptrs )
        IF( obj%isBoundaryNode( global_nptrs( jj ) ) ) &
          & obj%elementData( ii )%elementType = BOUNDARY_ELEMENT
      END DO
    END DO
    !
    DO ii = 1, obj%tElements
      IF( obj%elementData(ii)%elementType .NE. BOUNDARY_ELEMENT ) CYCLE
      iel = obj%getGlobalElemNumber( ii )
      ElemToElem = obj%getElementToElements( globalElement=iel, &
        & onlyElements=.FALSE. )
      !! Because iel is a boundary element, not all its faces will
      !! have neighbours. Below, we calculate how many faces
      !! of iel does not have neighbors. These faces are
      !! called boundary faces.
      jj = tFace - SIZE( ElemToElem, 1 )
      CALL Reallocate( obj%elementData(ii)%boundaryData, jj )
      global_nptrs = obj%getConnectivity(iel)
      jj = 0
      DO kk = 1, tFace
        IF( ANY( kk .EQ. ElemToElem( :, 2 ) ) ) CYCLE
        jj = jj + 1
        obj%elementData( ii )%boundaryData( jj ) = kk
      END DO
    END DO
  END IF
  IF( ALLOCATED( global_nptrs ) ) DEALLOCATE( global_nptrs )
  IF( ALLOCATED( ElemToElem ) ) DEALLOCATE( ElemToElem )
END PROCEDURE mesh_InitiateBoundaryData

!----------------------------------------------------------------------------
!                                                     InitiateFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_InitiateFacetElements
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_InitiateFacetElements"
  INTEGER( I4B ) :: iel, ii, jj, iintface, idomainFace, kk, telements, &
    & tIntFace, tDomainFace
  INTEGER( I4B ), ALLOCATABLE :: e2e( :, : ), indx( : ), cellNptrs( : )
  !!
  !! main
  !!
  tDomainFace = 0
  tIntFace = 0
  !!
  DO iel = 1, obj%getTotalElements()
    !!
    jj = obj%getGlobalElemNumber( iel )
    !!
    IF( obj%isBoundaryElement( globalElement=jj ) ) THEN
      tDomainFace = tDomainFace + &
        & SIZE( obj%getBoundaryElementData( globalElement = jj ) )
    END IF
    !!
    e2e = obj%getElementToElements( globalElement=jj, onlyElements = .TRUE. )
    !!
    DO ii = 1, SIZE( e2e, 1 )
      IF( jj .LE. e2e( ii, 1 ) ) THEN
        tIntFace = tIntFace + 1
      END IF
    END DO
  END DO
  !!
  !! internalFacetData
  !!
  IF( ALLOCATED( obj%internalFacetData ) ) DEALLOCATE(obj%internalFacetData)
  ALLOCATE( obj%internalFacetData( tIntFace ) )
  !!
  !! domainFaceData
  !!
  IF( ALLOCATED( obj%boundaryFacetData ) ) DEALLOCATE(obj%boundaryFacetData)
  ALLOCATE( obj%boundaryFacetData( tDomainFace ) )
  !!
  !! facetElementType
  !!
  telements = obj%getTotalElements()
  CALL Reallocate( obj%facetElementType, SIZE(obj%facetElements), telements )
  !!
  iintface = 0; idomainFace = 0
  !!
  !! start the loop for each cell element of the mesh
  !!
  DO iel = 1, telements
    !!
    jj = obj%getGlobalElemNumber( iel )
    cellNptrs = obj%getConnectivity( globalElement=jj )
    e2e = obj%getElementToElements(globalElement=jj, onlyElements = .FALSE.)
    !!
    !! boundaryFacetData
    !!
    IF( obj%isBoundaryElement( globalElement=jj ) ) THEN
      !!
      indx = obj%getBoundaryElementData( globalElement = jj )
      !!
      DO ii = 1, SIZE( indx )
        !!
        kk = indx( ii )
        idomainFace = idomainFace+1
        obj%boundaryFacetData( idomainFace )%masterCellNumber = jj
        obj%boundaryFacetData( idomainFace )%masterLocalFacetID = kk
        obj%boundaryFacetData( idomainFace )%elementType = &
          & DOMAIN_BOUNDARY_ELEMENT
        obj%facetElementType( kk, iel ) = DOMAIN_BOUNDARY_ELEMENT
        !!
      END DO
      !!
    END IF
    !!
    !! internalFacetData
    !!
    DO ii = 1, SIZE( e2e, 1 )
      kk = e2e(ii, 2)
      obj%facetElementType( kk, iel ) = INTERNAL_ELEMENT
      IF( jj .LE. e2e( ii, 1 ) ) THEN
        iintface = iintface + 1
        obj%internalFacetData( iintface )%masterCellNumber = jj
        obj%internalFacetData( iintface )%slaveCellNumber = e2e(ii, 1)
        obj%internalFacetData( iintface )%masterlocalFacetID = e2e(ii,2)
        obj%internalFacetData( iintface )%slavelocalFacetID = e2e(ii,3)
      END IF
    END DO
    !!
  END DO
  !!
  IF( ALLOCATED( e2e ) ) DEALLOCATE( e2e )
  IF( ALLOCATED( indx ) ) DEALLOCATE( indx )
  IF( ALLOCATED( cellNptrs ) ) DEALLOCATE( cellNptrs )
  !!
END PROCEDURE mesh_InitiateFacetElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MeshDataMethods