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

SUBMODULE(BetterMesh_Class) BoundaryDataMethods
! USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       InitiateBoundaryData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateBoundaryData
! Define internal variables
CHARACTER(*), PARAMETER :: myName = "obj_InitiateBoundaryData()"
! INTEGER(I4B) :: iel, tFace, ii, jj, kk
! INTEGER(I4B), ALLOCATABLE :: global_nptrs(:), ElemToElem(:, :)
!
! ! check
! IF (obj%elemType .EQ. 0 .OR. obj%elemType .EQ. Point1) RETURN
!
! ! check
! IF (.NOT. ASSOCIATED(obj%refelem)) THEN
!   CALL e%raiseError(modName//"::"//myName//" - "// &
!     & "Unable to identify the Reference element of the mesh, &
!     & may be it is not set")
! END IF
!
! ! check
! IF (obj%isBoundaryDataInitiated) THEN
!   CALL e%raiseWarning(modName//"::"//myName//" - "// &
!     & "Boundary data information is already initiated. If you want to &
!     & Reinitiate it then deallocate nodeData, first!")
!   RETURN
! END IF
!
! IF (.NOT. obj%isElementToElementsInitiated) &
!   & CALL obj%InitiateElementToElements()
!
! obj%isBoundaryDataInitiated = .TRUE.
!
! IF (.NOT. ALLOCATED(obj%FacetElements)) &
!   & obj%FacetElements = FacetElements(obj%refelem)
!
! tFace = SIZE(obj%FacetElements)
!
! ! Case of single element in the mesh
! IF (obj%tElements .EQ. 1) THEN
!   obj%elementData(1)%elementType = BOUNDARY_ELEMENT
!   tFace = SIZE(obj%FacetElements)
!   obj%elementData(1)%boundaryData = [(ii, ii=1, tFace)]
! ELSE
!
!   ! Now we will include those elements in boundary elements
!   ! which contains the boundary nodes
!   DO ii = 1, obj%tElements
!     iel = obj%getGlobalElemNumber(ii)
!     global_nptrs = obj%getConnectivity(iel)
!     DO jj = 1, SIZE(global_nptrs)
!       IF (obj%isBoundaryNode(global_nptrs(jj))) &
!         & obj%elementData(ii)%elementType = BOUNDARY_ELEMENT
!     END DO
!   END DO
!
!   DO ii = 1, obj%tElements
!     IF (obj%elementData(ii)%elementType .NE. BOUNDARY_ELEMENT) CYCLE
!     iel = obj%getGlobalElemNumber(ii)
!     ElemToElem = obj%getElementToElements(globalElement=iel, &
!       & onlyElements=.FALSE.)
!     ! Because iel is a boundary element, not all its faces will
!     ! have neighbours. Below, we calculate how many faces
!     ! of iel does not have neighbors. These faces are
!     ! called boundary faces.
!     jj = tFace - SIZE(ElemToElem, 1)
!     CALL Reallocate(obj%elementData(ii)%boundaryData, jj)
!     global_nptrs = obj%getConnectivity(iel)
!     jj = 0
!     DO kk = 1, tFace
!       IF (ANY(kk .EQ. ElemToElem(:, 2))) CYCLE
!       jj = jj + 1
!       obj%elementData(ii)%boundaryData(jj) = kk
!     END DO
!   END DO
! END IF
! IF (ALLOCATED(global_nptrs)) DEALLOCATE (global_nptrs)
! IF (ALLOCATED(ElemToElem)) DEALLOCATE (ElemToElem)

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_InitiateBoundaryData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE BoundaryDataMethods
