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

SUBMODULE(DomainConnectivity_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_InitiateFacetToCellData1
  !!
  INTEGER( I4B ) :: iface, icell, ii
  INTEGER( I4B ), ALLOCATABLE :: nptrs( : ), pt2elem( : ), &
    & cellNptrs( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "dc_InitiateFacetToCellData1"
  !!
  !! main
  !!
  !!
  !! check
  !!
  IF( .NOT. ALLOCATED( obj%facetToCell ) ) THEN
    ALLOCATE( obj%facetToCell( facetMesh%getTotalElements( ) ) )
  END IF
  !!
  ii = 0
  !!
  DO iface = facetMesh%minElemNum, facetMesh%maxElemNum
    IF( .NOT. facetMesh%isElementPresent( globalElement=iface ) ) CYCLE
    nptrs = facetMesh%getConnectivity( globalElement=iface )
    !!
    !! It is important that all nodes of a facet element are present
    !! in the cellMesh.
    !!
    ii = ii + 1
    !!
    obj%facetToCell( ii )%facetID = iface
    !!
    IF( .NOT. cellMesh%isAllNodePresent( nptrs ) ) CYCLE
    !!
    !! Get the element in Cell mesh surrounding this node
    !!
    pt2elem = cellMesh%getNodetoElements( globalNode=nptrs )
    !!
    DO icell = 1, SIZE( pt2elem )
      !!
      cellNptrs = cellMesh%getConnectivity( globalElement=pt2elem( icell ) )
      !!
      IF( nptrs .IN. cellNptrs ) THEN
        IF( isMaster ) THEN
          obj%facetToCell( ii )%GlobalCellData( 1,1 ) = pt2elem( icell )
          obj%facetToCell( ii )%GlobalCellData( 3:4,1 ) = [dim, entityNum]
        ELSE
          obj%facetToCell( ii )%GlobalCellData( 1,2 ) = pt2elem( icell )
          obj%facetToCell( ii )%GlobalCellData( 3:4,2 ) = [dim, entityNum]
        END IF
        EXIT
      END IF
      !!
    END DO
    !!
  END DO
  !!
END PROCEDURE dc_InitiateFacetToCellData1

!----------------------------------------------------------------------------
!                                                    InitiateFacetToCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_InitiateFacetToCellData2
  CHARACTER( LEN = * ), PARAMETER :: myName="dc_InitiateFacetToCellData2"
  INTEGER( I4B ) :: dim_facet, icellMesh, tCellMesh, tface
  CLASS( Mesh_ ), POINTER :: meshptr
  !!
  !!
  meshptr => NULL( )
  dim_facet = facetMesh%getXidimension()
  tface = facetMesh%getTotalElements( )
  IF( ALLOCATED( obj%facetToCell ) ) DEALLOCATE( obj%facetToCell )
  ALLOCATE( obj%facetToCell( tface ) )
  !!
  !! Handling masterCell
  !!
  tCellMesh = masterDomain%getTotalMesh( dim=dim_facet+1 )
  !!
  DO icellMesh = 1, tCellMesh
    !!
    meshptr => masterDomain%getMeshPointer( dim=dim_facet+1, &
      & entityNum=icellMesh )
    !!
    CALL dc_InitiateFacetToCellData1( obj=obj, &
      & facetMesh=facetMesh, &
      & cellMesh=meshptr, &
      & dim=dim_facet+1, &
      & entityNum=icellMesh, &
      & isMaster=.TRUE. )
    !!
  END DO
  !!
  !! Handling slaveCell
  !!
  tCellMesh = slaveDomain%getTotalMesh( dim=dim_facet+1 )
  !!
  DO icellMesh = 1, tCellMesh
    !!
    meshptr => slaveDomain%getMeshPointer( dim=dim_facet+1, &
      & entityNum=icellMesh )
    !!
    CALL obj%InitiateFacetToCellData( &
      & facetMesh=facetMesh, &
      & cellMesh=meshptr, &
      & dim=dim_facet+1, &
      & entityNum=icellMesh, &
      & isMaster=.FALSE. )
    !!
  END DO
  !!
  NULLIFY( meshptr )
END PROCEDURE dc_InitiateFacetToCellData2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_InitiateFacetToCellData3
  !!
  INTEGER( I4B ) :: iface, icell, ii, colID
  INTEGER( I4B ), ALLOCATABLE :: nptrs( : ), pt2elem( : ), &
    & cellNptrs( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "dc_InitiateFacetToCellData3"
  !!
  !! main
  !!
  !!
  !! check
  !!
  IF( .NOT. ALLOCATED( obj%facetToCell ) ) THEN
    ALLOCATE( obj%facetToCell( facetMesh%getTotalElements( ) ) )
  END IF
  !!
  ii = 0
  !!
  DO iface = facetMesh%minElemNum, facetMesh%maxElemNum
    IF( .NOT. facetMesh%isElementPresent( globalElement=iface ) ) CYCLE
    nptrs = facetMesh%getConnectivity( globalElement=iface )
    !!
    !! It is important that all nodes of a facet element are present
    !! in the cellMesh.
    !!
    ii = ii + 1
    !!
    obj%facetToCell( ii )%facetID = iface
    !!
    IF( .NOT. cellMesh%isAllNodePresent( nptrs ) ) CYCLE
    !!
    !! Get the element in Cell mesh surrounding this node
    !!
    pt2elem = cellMesh%getNodetoElements( globalNode=nptrs )
    !!
    colID = 0
    !!
    DO icell = 1, SIZE( pt2elem )
      !!
      cellNptrs = cellMesh%getConnectivity( globalElement=pt2elem( icell ) )
      !!
      IF( nptrs .IN. cellNptrs ) THEN
        colID = colID + 1
        IF( colID .GT. 2 ) THEN
          CALL e%raiseError(modName//"::"//myName//" - "// &
            & "It seems the facet element has more than 2 cell element")
        ELSE
          obj%facetToCell( ii )%GlobalCellData( 1,colID ) = pt2elem( icell )
          obj%facetToCell( ii )%GlobalCellData( 3:4,colID ) = [dim, entityNum]
        END IF
      END IF
      !!
    END DO
    !!
    IF( colID .EQ. 0 ) THEN
      CALL e%raiseError(modName//"::"//myName//" - "// &
        & "It seems the facet element has no cell element")
    END IF
    !!
  END DO
  !!
END PROCEDURE dc_InitiateFacetToCellData3

!----------------------------------------------------------------------------
!                                                    InitiateFacetToCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_InitiateFacetToCellData4
  CHARACTER( LEN = * ), PARAMETER :: myName="dc_InitiateFacetToCellData4"
  INTEGER( I4B ) :: dim_facet, icellMesh, tCellMesh, tface, ii, iface, icell
  CLASS( Mesh_ ), POINTER :: cellMesh
  INTEGER( I4B ), ALLOCATABLE :: colID( : ), nptrs( : ), pt2elem( : ), &
    & cellNptrs( : )
  !!
  !! main
  !!
  cellMesh => NULL( )
  dim_facet = facetMesh%getXidimension( )
  tface = facetMesh%getTotalElements( )
  IF( ALLOCATED( obj%facetToCell ) ) DEALLOCATE( obj%facetToCell )
  ALLOCATE( obj%facetToCell( tface ) )
  ALLOCATE( colID( tface ) )
  colID = 0
  !!
  !! Handling masterCell
  !!
  tCellMesh = cellDomain%getTotalMesh( dim=dim_facet+1 )
  !!
  DO icellMesh = 1, tCellMesh
    !!
    cellMesh => cellDomain%getMeshPointer( dim=dim_facet+1, &
      & entityNum=icellMesh )
    !!
    ii = 0
    !!
    DO iface = facetMesh%minElemNum, facetMesh%maxElemNum
      !!
      IF( .NOT. facetMesh%isElementPresent( globalElement=iface ) ) CYCLE
      nptrs = facetMesh%getConnectivity( globalElement=iface )
      !!
      !! It is important that all nodes of a facet element are present
      !! in the cellMesh.
      !!
      ii = ii + 1
      !!
      obj%facetToCell( ii )%facetID = iface
      !!
      IF( .NOT. cellMesh%isAllNodePresent( nptrs ) ) CYCLE
      !!
      !! Get the element in Cell mesh surrounding this node
      !!
      pt2elem = cellMesh%getNodetoElements( globalNode=nptrs )
      !!
      ! colID = 0
      !!
      DO icell = 1, SIZE( pt2elem )
        !!
        cellNptrs = cellMesh%getConnectivity( globalElement=pt2elem( icell ) )
        !!
        IF( nptrs .IN. cellNptrs ) THEN
          colID( ii ) = colID( ii ) + 1
          IF( colID( ii ) .GT. 2 ) THEN
            CALL e%raiseError(modName//"::"//myName//" - "// &
              & "It seems the facet element = " // TOSTRING( iface ) // &
              & " has more than 2 cell element")
          ELSE
            obj%facetToCell( ii )%GlobalCellData( 1,colID( ii ) ) = &
              & pt2elem( icell )
            obj%facetToCell( ii )%GlobalCellData( 3:4,colID( ii ) ) = &
              & [dim_facet+1, icellMesh]
          END IF
        END IF
        !!
      END DO
      !!
      IF( colID( ii ) .EQ. 0 ) THEN
        CALL e%raiseError(modName//"::"//myName//" - "// &
          & "It seems the facet element = " // TOSTRING( iface ) // &
          & " has no cell element")
      END IF
      !!
    END DO
    !!
  END DO
  !!
  NULLIFY( cellMesh )
  DEALLOCATE( colID )
END PROCEDURE dc_InitiateFacetToCellData4

!----------------------------------------------------------------------------
!                                                          masterCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_masterCellNumber1
  ans = obj%facetToCell(localElement)%GlobalCellData(1, 1)
END PROCEDURE dc_masterCellNumber1

!----------------------------------------------------------------------------
!                                                           masterCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_masterCellNumber2
  INTEGER( I4B ) :: ii
  !!
  DO ii = 1, SIZE( localElement )
    ans = obj%facetToCell(localElement(ii))%GlobalCellData(1, 1)
  END DO
END PROCEDURE dc_masterCellNumber2

!----------------------------------------------------------------------------
!                                                            slaveCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_slaveCellNumber1
  ans = obj%facetToCell(localElement)%GlobalCellData(1, 2)
END PROCEDURE dc_slaveCellNumber1

!----------------------------------------------------------------------------
!                                                            slaveCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_slaveCellNumber2
  INTEGER( I4B ) :: ii
  !!
  DO ii = 1, SIZE( localElement )
    ans = obj%facetToCell(localElement(ii))%GlobalCellData(1, 2)
  END DO
END PROCEDURE dc_slaveCellNumber2

!----------------------------------------------------------------------------
!                                                               FacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_FacetLocalID1
  ! ans = obj % CellFacet( 2, FacetNum )
END PROCEDURE dc_FacetLocalID1

!----------------------------------------------------------------------------
!                                                               FacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_FacetLocalID2
  ! ans = obj % CellFacet( 2, FacetNum )
END PROCEDURE dc_FacetLocalID2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods

! !
! !
! LOGICAL( LGT ) :: found
! INTEGER( I4B ) :: ifacet, icell, elemNum, i, r, j, k
! CLASS( Element_ ), POINTER :: Elem, FacetElem
! INTEGER( I4B ), ALLOCATABLE :: FacetNptrs( : ), pt2elem( : ), bndyData( : )
! INTEGER( I4B ), ALLOCATABLE :: FM( :, : ), CellNptrs( : ),CellFacetNptrs(:)

! !<--- if cell mesh data not initiated then initiate it
! IF( .NOT. CellMeshData % isInitiated ) THEN
!   CALL CellMeshData % Initiate( CellMesh )
! END IF

! !<--- generate node to elements and boundary data
! CALL CellMeshData % InitiateNodeToElements( CellMesh )
! CALL CellMeshData % InitiateBoundaryData( CellMesh )

! !<--- allocate CellFacet size
! CALL Reallocate( obj % CellFacet, 2_I4B, FacetMesh % SIZE() )

! Elem => NULL( ); FacetElem => NULL( )

! DO ifacet = 1, SIZE( obj % CellFacet, 2 )
!   found = .false.

!   !<--- get facet element and its nptrs
!   FacetElem => FacetMesh % Elem( ifacet ) % Ptr
!   FacetNptrs = .Nptrs. FacetElem

!   !<--- select an arbitrary node in FacetNptr; if this is not
!   !<--- present inside the CellMesh then this means this iFacet is
!   !<--- orphan and we go back to the next Facet Element
!   IF( .NOT. CellMeshData % isNodePresent( FacetNptrs( 1 ) ) ) CYCLE

!   !<--- Now we have ensured that iFacet can be a facet element
!   !<--- so we get the element in Cell mesh surrouding this node
!   pt2elem = CellMeshData % NodetoElements( FacetNptrs( 1 ))

!   DO icell = 1, SIZE( pt2elem )
!     elemNum = pt2elem( icell )

!     !<--- if this element is not a boundary element then discard it
!     IF( .NOT. CellMeshData % isBoundaryElement( ElemNum ) ) CYCLE

!     bndyData = CellMeshData % BoundaryElementData( elemNum )
!     Elem => CellMesh % Elem( elemNum ) % Ptr
!     FM = FacetMatrix( Elem % refelem )
!     CellNptrs = .Nptrs. Elem

!     DO i = 1, SIZE( bndyData )
!       CellFacetNptrs = &
!       & CellNptrs( FM( BndyData( i ), 4 : FM( BndyData( i ), 3 ) + 3 ) )
!       r = 0

!       DO j = 1, SIZE( FacetNptrs )
!         DO k = 1, SIZE( CellFacetNptrs )
!           IF( FacetNptrs( j ) .EQ. CellFacetNptrs( k ) ) THEN
!             r = r + 1
!           END IF
!         END DO
!       END DO
!       IF( r .EQ. SIZE( FacetNptrs ) ) THEN
!         Found = .TRUE.
!         obj % CellFacet( 1, iFacet ) = elemNum
!         obj % CellFacet( 2, iFacet ) = BndyData( i )
!         EXIT
!       END IF
!     END DO
!     IF( Found ) EXIT
!   END DO
! END DO

! NULLIFY( FacetElem, Elem )
! IF( ALLOCATED( FacetNptrs ) ) DEALLOCATE( FacetNptrs )
! IF( ALLOCATED( pt2elem ) ) DEALLOCATE( pt2elem )
! IF( ALLOCATED( bndyData ) ) DEALLOCATE( bndyData )
! IF( ALLOCATED( FM ) ) DEALLOCATE( FM )
! IF( ALLOCATED( CellNptrs ) ) DEALLOCATE( CellNptrs )
! IF( ALLOCATED( CellFacetNptrs ) ) DEALLOCATE( CellFacetNptrs )