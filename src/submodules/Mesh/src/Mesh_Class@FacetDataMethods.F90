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

SUBMODULE(Mesh_Class) FacetDataMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     InitiateFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_InitiateFacetElements
  CHARACTER( LEN = * ), PARAMETER :: myName = "mesh_InitiateFacetElements"
  INTEGER( I4B ) :: iel, ii, jj, iintface, idomainFace, kk, telements, &
    & tIntFace, tDomainFace
  INTEGER( I4B ), ALLOCATABLE :: e2e( :, : ), indx( : ), cellNptrs( : )
  !!
  !! check
  !!
  IF( obj%isFacetDataInitiated ) THEN
    CALL e%raiseWarning(modName//"::"//myName//" - "// &
      & "InternalFacetData and boundary facet data is already initiated. &
      & If you want to Reinitiate it then deallocate nodeData, first!!" )
    RETURN
  END IF
  !!
  !! main
  !!
  tDomainFace = 0
  tIntFace = 0
  obj%isFacetDataInitiated = .TRUE.
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

END SUBMODULE FacetDataMethods