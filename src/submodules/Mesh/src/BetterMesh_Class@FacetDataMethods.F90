! ThIs program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! ThIs program is free software: you can redistribute it and/or modify
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

SUBMODULE(BetterMesh_Class) FacetDataMethods
! USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     InitiateFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetElements
CHARACTER(*), PARAMETER :: myName = "obj_InitiateFacetElements()"
! INTEGER(I4B) :: iel, ii, jj, iintface, idomainFace, kk, telements, &
!   & tIntFace, tDomainFace
! INTEGER(I4B), ALLOCATABLE :: e2e(:, :), indx(:), cellNptrs(:)
! LOGICAL(LGT) :: problem, isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

!
! problem = obj%elemType .EQ. 0 .OR. obj%elemType .EQ. Point1
! IF (problem) RETURN
!
! problem = obj%isFacetDataInitiated
! IF (problem) THEN
!   CALL e%raiseInformation(modName//"::"//myName//" - "// &
!     & "[INTERNAL ERROR] :: InternalFacetData and boundary "//  &
!     & "facet data is already initiated. "//  &
!     & "If you want to Reinitiate it then deallocate nodeData, first!")
!   RETURN
! END IF
!
! problem = .NOT. obj%isElementToElementsInitiated
! IF (problem) CALL obj%InitiateElementToElements()
!
! problem = .NOT. obj%IsBoundaryDataInitiated
! IF (problem) CALL obj%InitiateBoundaryData()
!
! tDomainFace = 0
! tIntFace = 0
! obj%isFacetDataInitiated = .TRUE.
!
! telements = obj%GetTotalElements()
! DO iel = 1, telements
!
!   jj = obj%GetGlobalElemNumber(iel)
!
!   isok = obj%IsBoundaryElement(globalElement=jj)
!   IF (isok) THEN
!     indx = obj%GetBoundaryElementData(globalElement=jj)
!     tDomainFace = tDomainFace + SIZE(indx)
!   END IF
!
!   e2e = obj%GetElementToElements(globalElement=jj, onlyElements=.TRUE.)
!
!   DO ii = 1, SIZE(e2e, 1)
!     IF (jj .LE. e2e(ii, 1)) THEN
!       tIntFace = tIntFace + 1
!     END IF
!   END DO
! END DO
!
! ! internalFacetData
! IF (ALLOCATED(obj%internalFacetData)) DEALLOCATE (obj%internalFacetData)
! ALLOCATE (obj%internalFacetData(tIntFace))
!
! ! boundaryFacetData
! IF (ALLOCATED(obj%boundaryFacetData)) DEALLOCATE (obj%boundaryFacetData)
! ALLOCATE (obj%boundaryFacetData(tDomainFace))
!
! ! facetElementType
! telements = obj%GetTotalElements()
! CALL Reallocate(obj%facetElementType, SIZE(obj%facetElements), telements)
!
! iintface = 0; idomainFace = 0
!
! DO iel = 1, telements
!   jj = obj%GetGlobalElemNumber(iel)
!   cellNptrs = obj%GetConnectivity(globalElement=jj)
!   e2e = obj%GetElementToElements(globalElement=jj, onlyElements=.FALSE.)
!
!   ! boundaryFacetData
!   IF (obj%IsBoundaryElement(globalElement=jj)) THEN
!     indx = obj%GetBoundaryElementData(globalElement=jj)
!     DO ii = 1, SIZE(indx)
!       kk = indx(ii)
!       idomainFace = idomainFace + 1
!       obj%boundaryFacetData(idomainFace)%masterCellNumber = jj
!       obj%boundaryFacetData(idomainFace)%masterLocalFacetID = kk
!       obj%boundaryFacetData(idomainFace)%elementType = &
!         & DOMAIN_BOUNDARY_ELEMENT
!       obj%facetElementType(kk, iel) = DOMAIN_BOUNDARY_ELEMENT
!     END DO
!   END IF
!
!   ! internalFacetData
!   DO ii = 1, SIZE(e2e, 1)
!     kk = e2e(ii, 2)
!     obj%facetElementType(kk, iel) = INTERNAL_ELEMENT
!     IF (jj .LE. e2e(ii, 1)) THEN
!       iintface = iintface + 1
!       obj%internalFacetData(iintface)%masterCellNumber = jj
!       obj%internalFacetData(iintface)%slaveCellNumber = e2e(ii, 1)
!       obj%internalFacetData(iintface)%masterlocalFacetID = e2e(ii, 2)
!       obj%internalFacetData(iintface)%slavelocalFacetID = e2e(ii, 3)
!     END IF
!   END DO
! END DO
!
! IF (ALLOCATED(e2e)) DEALLOCATE (e2e)
! IF (ALLOCATED(indx)) DEALLOCATE (indx)
! IF (ALLOCATED(cellNptrs)) DEALLOCATE (cellNptrs)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_InitiateFacetElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE FacetDataMethods
