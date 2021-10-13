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
!                                                    InitiateFacetToCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_InitiateFacetToCellData1
  CHARACTER( LEN = * ), PARAMETER :: myName="dc_InitiateFacetToCellData1"
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & 'This routine is under construction and testing.' )

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

END PROCEDURE dc_InitiateFacetToCellData1

!----------------------------------------------------------------------------
!                                                                 CellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_CellNumber1
  ! ans = obj%CellFacet( 1, FacetNum )
END PROCEDURE dc_CellNumber1

!----------------------------------------------------------------------------
!                                                                 CellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_CellNumber2
  ! ans = obj % CellFacet( 1, FacetNum )
END PROCEDURE dc_CellNumber2

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
