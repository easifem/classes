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

SUBMODULE(FEDomainConnectivity_Class) FacetMethods
USE ReallocateUtility
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE facet_to_cell_helper(obj, facetMesh, cellMesh, dim, entityNum, &
  & isMaster)
  CLASS(FEDomainConnectivity_), INTENT(INOUT) :: obj
  !! FEDomain connectivity data
  CLASS(AbstractMesh_), INTENT(INOUT) :: facetMesh
  !! Mesh of facet elements
  CLASS(AbstractMesh_), INTENT(INOUT) :: cellMesh
  !! Master mesh
  INTEGER(I4B), INTENT(IN) :: dim
  !! dim
  INTEGER(I4B), INTENT(IN) :: entityNum
  !! entityNum
  LOGICAL(LGT), INTENT(IN) :: isMaster
  !! if true then cell Mesh is master cell
  !! if false then cell mesh is slave cell

  ! INTEGER(I4B) :: iface, icell, ii, tfacet, cellGlobalNum, &
  !   & localFacetID, jj
  ! INTEGER(I4B), ALLOCATABLE :: nptrs(:), pt2elem(:), &
  !   & cellNptrs(:), facetNptrs(:)
  ! CHARACTER(*), PARAMETER :: myName = "facet_to_cell_helper()"
  !
  ! ii = 0
  !
  ! IF (.NOT. ALLOCATED(cellMesh%facetElements)) THEN
  !   CALL e%raiseError(modName//'::'//myName//' - '// &
  !     & 'AbstractMesh_::cellMesh%facetElements should be allocated!')
  !   RETURN
  ! END IF
  !
  ! tfacet = SIZE(cellMesh%facetElements)
  !
  ! DO iface = facetMesh%minElemNum, facetMesh%maxElemNum
  !   IF (.NOT. facetMesh%isElementPresent(globalElement=iface)) CYCLE
  !   nptrs = facetMesh%getConnectivity(globalElement=iface)
  !
  !   ! It is important that all nodes of a facet element are present
  !   ! in the cellMesh.
  !
  !   ii = ii + 1
  !
  !   obj%facetToCell(ii)%facetID = iface
  !
  !   IF (.NOT. cellMesh%isAllNodePresent(nptrs)) CYCLE
  !
  !   ! Get the element in Cell mesh surrounding this node
  !
  !   pt2elem = cellMesh%getNodetoElements(globalNode=nptrs)
  !
  !   DO icell = 1, SIZE(pt2elem)
  !
  !     cellNptrs = cellMesh%getConnectivity(globalElement=pt2elem(icell))
  !
  !     IF (nptrs.IN.cellNptrs) THEN
  !
  !       cellGlobalNum = pt2elem(icell)
  !
  !       localFacetID = 0
  !
  !       DO jj = 1, tfacet
  !
  !         facetNptrs = cellMesh%getFacetConnectivity(&
  !           & globalElement=cellGlobalNum, &
  !           & iface=jj)
  !
  !         IF (nptrs.in.facetNptrs) THEN
  !           localFacetID = jj
  !           EXIT
  !         END IF
  !
  !       END DO
  !
  !       IF (localFacetID .EQ. 0) THEN
  !         CALL e%raiseError(modName//'::'//myName//' - '// &
  !           & 'No local facet found')
  !       END IF
  !
  !       IF (isMaster) THEN
  !         obj%facetToCell(ii)%GlobalCellData(1, 1) = cellGlobalNum
  !         obj%facetToCell(ii)%GlobalCellData(2, 1) = localFacetID
  !         obj%facetToCell(ii)%GlobalCellData(3:4, 1) = [dim, entityNum]
  !       ELSE
  !         obj%facetToCell(ii)%GlobalCellData(1, 2) = cellGlobalNum
  !         obj%facetToCell(ii)%GlobalCellData(2, 2) = localFacetID
  !         obj%facetToCell(ii)%GlobalCellData(3:4, 2) = [dim, entityNum]
  !       END IF
  !
  !       EXIT
  !
  !     END IF
  !
  !   END DO
  ! END DO
  !
  ! IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  ! IF (ALLOCATED(pt2elem)) DEALLOCATE (pt2elem)
  ! IF (ALLOCATED(cellNptrs)) DEALLOCATE (cellNptrs)
  ! IF (ALLOCATED(facetNptrs)) DEALLOCATE (facetNptrs)

END SUBROUTINE facet_to_cell_helper

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetToCellData1
!
! INTEGER(I4B) :: tfacet
! CHARACTER(*), PARAMETER :: myName = "obj_InitiateFacetToCellData1"
! LOGICAL(LGT) :: isVar
!
! CALL e%raiseInformation(modName//'::'//myName//' - '// &
!   & '[START] InitiateFacetToCellData()')
!
! CALL cellMesh%GetParam(isNodeToElementsInitiated=isVar)
!
! IF (.NOT. isVar) THEN
!   CALL e%raiseInformation(modName//'::'//myName//' - '// &
!     & "In cellMesh node to elements data is not initiated, &
!     & calling cellMesh%InitiateNodeToElements()")
!   CALL cellMesh%InitiateNodeToElements()
! END IF
!
! IF (obj%isFacetToCell) THEN
!   CALL e%raiseInformation(modName//"::"//myName//" - "// &
!     & "It seems, obj%facetToCell data is already initiated")
!   RETURN
! END IF
!
! tfacet = facetMesh%getTotalElements()
! ALLOCATE (obj%facetToCell(tfacet))
! obj%isFacetToCell = .TRUE.
! CALL display("Calling facet_to_cell_helper()", unitno=stdout)
!
! CALL facet_to_cell_helper(obj, facetMesh, cellMesh, dim, entityNum, isMaster)
!
! CALL e%raiseInformation(modName//'::'//myName//' - '// &
!   & '[END] InitiateFacetToCellData()')

END PROCEDURE obj_InitiateFacetToCellData1

!----------------------------------------------------------------------------
!                                                    InitiateFacetToCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetToCellData2
! CHARACTER(*), PARAMETER :: myName = "obj_InitiateFacetToCellData2"
! INTEGER(I4B) :: dim_facet, icellMesh, tCellMesh, tface, nsd
! CLASS(AbstractMesh_), POINTER :: meshptr
! LOGICAL(LGT) :: isVar
!
! CALL e%raiseInformation(modName//'::'//myName//' - '// &
!   & '[START] InitiateFacetToCellData()')
!
! IF (obj%isFacetToCell) THEN
!   CALL e%raiseInformation(modName//"::"//myName//" - "// &
!     & "It seems, obj%facetToCell data is already initiated")
!   RETURN
! END IF
!
! meshptr => NULL()
! dim_facet = facetMesh%getXidimension()
! nsd = masterFEDomain%getNSD()
!
! IF (dim_facet .GE. nsd) THEN
!   CALL e%raiseError(modName//'::'//myName//' - '// &
!   & 'xidimension of facet mesh is >= to spatial dimension of masterFEDomain')
! END IF
!
! tface = facetMesh%getTotalElements()
! ! IF (ALLOCATED(obj%facetToCell)) DEALLOCATE (obj%facetToCell)
! ALLOCATE (obj%facetToCell(tface))
! obj%isFacetToCell = .TRUE.
!
! ! Handling masterCell
!
! tCellMesh = masterFEDomain%getTotalMesh(dim=dim_facet + 1)
!
! DO icellMesh = 1, tCellMesh
!
!   meshptr => masterFEDomain%getMeshPointer(dim=dim_facet + 1, &
!     & entityNum=icellMesh)
!
!   CALL meshptr%GetParam(isNodeToElementsInitiated=isVar)
!
!   IF (.NOT. isVar) THEN
!     CALL e%raiseInformation(modName//'::'//myName//' - '// &
!       & "In cellMesh node to elements data is not initiated, &
!       & calling cellMesh%InitiateNodeToElements()")
!     CALL meshptr%InitiateNodeToElements()
!   END IF
!
!   CALL facet_to_cell_helper(obj=obj, &
!     & facetMesh=facetMesh, &
!     & cellMesh=meshptr, &
!     & dim=dim_facet + 1, &
!     & entityNum=icellMesh, &
!     & isMaster=.TRUE.)
!
! END DO
!
! ! Handling slaveCell
!
! tCellMesh = slaveFEDomain%getTotalMesh(dim=dim_facet + 1)
!
! DO icellMesh = 1, tCellMesh
!
!   meshptr => slaveFEDomain%getMeshPointer(dim=dim_facet + 1, &
!     & entityNum=icellMesh)
!
!   CALL facet_to_cell_helper( &
!     & obj=obj, &
!     & facetMesh=facetMesh, &
!     & cellMesh=meshptr, &
!     & dim=dim_facet + 1, &
!     & entityNum=icellMesh, &
!     & isMaster=.FALSE.)
!
! END DO
!
! NULLIFY (meshptr)
!
! CALL e%raiseInformation(modName//'::'//myName//' - '// &
!   & '[END] InitiateFacetToCellData()')

END PROCEDURE obj_InitiateFacetToCellData2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetToCellData3
!
! INTEGER(I4B) :: iface, icell, ii, colID, tface, tfacet, &
! & cellGlobalNum, localFacetID, jj
! INTEGER(I4B), ALLOCATABLE :: nptrs(:), pt2elem(:), &
!   & cellNptrs(:), facetNptrs(:)
! CHARACTER(*), PARAMETER :: myName = "obj_InitiateFacetToCellData3"
! LOGICAL(LGT) :: isVar
!
! CALL e%raiseInformation(modName//'::'//myName//' - '// &
!   & '[START] InitiateFacetToCellData()')
!
! IF (.NOT. ALLOCATED(cellMesh%facetElements)) THEN
!   CALL e%raiseError(modName//'::'//myName//' - '// &
!     & 'AbstractMesh_::cellMesh%facetElements should be allocated!')
! ELSE
!   tfacet = SIZE(cellMesh%facetElements)
! END IF
!
! CALL cellMesh%GetParam(isNodeToElementsInitiated=isVar)
!
! IF (.NOT. isVar) THEN
!   CALL e%raiseInformation(modName//'::'//myName//' - '// &
!     & "In cellMesh node to elements data is not initiated, &
!     & calling cellMesh%InitiateNodeToElements()")
!   CALL cellMesh%InitiateNodeToElements()
! END IF
!
! IF (obj%isFacetToCell) THEN
!   CALL e%raiseInformation(modName//"::"//myName//" - "// &
!     & "It seems, obj%facetToCell data is already initiated")
!   RETURN
! END IF
!
! tface = facetMesh%getTotalElements()
! ALLOCATE (obj%facetToCell(tface))
!
! ii = 0
!
! DO iface = facetMesh%minElemNum, facetMesh%maxElemNum
!   IF (.NOT. facetMesh%isElementPresent(globalElement=iface)) CYCLE
!   nptrs = facetMesh%getConnectivity(globalElement=iface)
!
!   ! It is important that all nodes of a facet element are present
!   ! in the cellMesh.
!
!   ii = ii + 1
!
!   obj%facetToCell(ii)%facetID = iface
!
!   IF (.NOT. cellMesh%isAllNodePresent(nptrs)) CYCLE
!
!   ! Get the element in Cell mesh surrounding this node
!
!   pt2elem = cellMesh%getNodetoElements(globalNode=nptrs)
!
!   colID = 0
!
!   DO icell = 1, SIZE(pt2elem)
!
!     cellGlobalNum = pt2elem(icell)
!
!     cellNptrs = cellMesh%getConnectivity(globalElement=cellGlobalNum)
!
!     IF (nptrs.IN.cellNptrs) THEN
!
!       localFacetID = 0
!
!       DO jj = 1, tfacet
!
!         facetNptrs = cellMesh%getFacetConnectivity(&
!           & globalElement=cellGlobalNum, &
!           & iface=jj)
!
!         IF (nptrs.in.facetNptrs) THEN
!           localFacetID = jj
!           EXIT
!         END IF
!
!       END DO
!
!       IF (localFacetID .EQ. 0) THEN
!         CALL e%raiseError(modName//'::'//myName//' - '// &
!           & 'No local facet found')
!       END IF
!
!       colID = colID + 1
!
!       IF (colID .GT. 2) THEN
!         CALL e%raiseError(modName//"::"//myName//" - "// &
!           & "It seems the facet element has more than 2 cell element")
!       ELSE
!         obj%facetToCell(ii)%GlobalCellData(1, colID) = cellGlobalNum
!         obj%facetToCell(ii)%GlobalCellData(2, colID) = localFacetID
!         obj%facetToCell(ii)%GlobalCellData(3:4, colID) = [dim, entityNum]
!       END IF
!     END IF
!
!   END DO
!
!   IF (colID .EQ. 0) THEN
!     CALL e%raiseError(modName//"::"//myName//" - "// &
!       & "It seems the facet element has no cell element")
!   END IF
!
! END DO
!
! IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
! IF (ALLOCATED(pt2elem)) DEALLOCATE (pt2elem)
! IF (ALLOCATED(cellNptrs)) DEALLOCATE (cellNptrs)
! IF (ALLOCATED(facetNptrs)) DEALLOCATE (facetNptrs)
!
! CALL e%raiseInformation(modName//'::'//myName//' - '// &
!   & '[END] InitiateFacetToCellData()')
END PROCEDURE obj_InitiateFacetToCellData3

!----------------------------------------------------------------------------
!                                                    InitiateFacetToCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetToCellData4
! CHARACTER(*), PARAMETER :: myName = "obj_InitiateFacetToCellData4"
! INTEGER(I4B) :: dim_facet, icellMesh, tCellMesh, tface, ii, iface, icell, &
! & nsd, tfacet, cellGlobalNum, localFacetID, jj
! CLASS(AbstractMesh_), POINTER :: cellMesh
! INTEGER(I4B), ALLOCATABLE :: colID(:), nptrs(:), pt2elem(:), &
!   & cellNptrs(:), facetNptrs(:)
! LOGICAL(LGT) :: isVar
!
! CALL e%raiseInformation(modName//'::'//myName//' - '// &
!   & '[START] InitiateFacetToCellData()')
!
! IF (obj%isFacetToCell) THEN
!   CALL e%raiseInformation(modName//"::"//myName//" - "// &
!     & "It seems, obj%facetToCell data is already initiated")
!   RETURN
! END IF
!
! cellMesh => NULL()
! dim_facet = facetMesh%getXidimension()
! nsd = cellFEDomain%getNSD()
!
! IF (dim_facet .GE. nsd) THEN
!   CALL e%raiseError(modName//'::'//myName//' - '// &
!   & 'xidimension of facet mesh is >= to spatial dimension of cellFEDomain')
! END IF
!
! tface = facetMesh%getTotalElements()
! ! IF (ALLOCATED(obj%facetToCell)) DEALLOCATE (obj%facetToCell)
! ALLOCATE (obj%facetToCell(tface))
!
! ALLOCATE (colID(tface))
! colID = 0
! !
! ! Handling masterCell
! !
! tCellMesh = cellFEDomain%getTotalMesh(dim=dim_facet + 1)
!
! DO icellMesh = 1, tCellMesh
!
!   cellMesh => cellFEDomain%getMeshPointer(dim=dim_facet + 1, &
!     & entityNum=icellMesh)
!
!   !
!   ! Check if the mesh is not empty
!   !
!   IF (.NOT. ASSOCIATED(cellMesh)) THEN
!     CALL e%raiseError(modName//'::'//myName//' - '// &
!     & 'AbstractMesh_::cellMesh is not associated!')
!   END IF
!
!   IF (cellMesh%getTotalElements() .EQ. 0) CYCLE
!
!   IF (.NOT. ALLOCATED(cellMesh%facetElements)) THEN
!     CALL e%raiseError(modName//'::'//myName//' - '// &
!       & 'AbstractMesh_::cellMesh%facetElements should be allocated!')
!   ELSE
!     tfacet = SIZE(cellMesh%facetElements)
!   END IF
!
!   CALL cellMesh%GetParam(isNodeToElementsInitiated=isVar)
!
!   IF (.NOT. isVar) THEN
!     CALL e%raiseInformation(modName//'::'//myName//' - '// &
!       & "In cellMesh node to elements data is not initiated, &
!       & calling cellMesh%InitiateNodeToElements()")
!     CALL cellMesh%InitiateNodeToElements()
!   END IF
!
!   ii = 0
!
!   DO iface = facetMesh%minElemNum, facetMesh%maxElemNum
!
!     IF (.NOT. facetMesh%isElementPresent(globalElement=iface)) CYCLE
!     nptrs = facetMesh%getConnectivity(globalElement=iface)
!
!     ! It is important that all nodes of a facet element are present
!     ! in the cellMesh.
!
!     ii = ii + 1
!
!     obj%facetToCell(ii)%facetID = iface
!
!     IF (.NOT. cellMesh%isAllNodePresent(nptrs)) CYCLE
!
!     ! Get the element in Cell mesh surrounding this node
!
!     pt2elem = cellMesh%getNodetoElements(globalNode=nptrs)
!
!     ! colID = 0
!
!     DO icell = 1, SIZE(pt2elem)
!
!       cellGlobalNum = pt2elem(icell)
!       cellNptrs = cellMesh%getConnectivity(globalElement=cellGlobalNum)
!
!       IF (nptrs.IN.cellNptrs) THEN
!
!         localFacetID = 0
!
!         DO jj = 1, tfacet
!
!           facetNptrs = cellMesh%getFacetConnectivity(&
!             & globalElement=cellGlobalNum, &
!             & iface=jj)
!
!           IF (nptrs.in.facetNptrs) THEN
!             localFacetID = jj
!             EXIT
!           END IF
!
!         END DO
!
!         IF (localFacetID .EQ. 0) THEN
!           CALL e%raiseError(modName//'::'//myName//' - '// &
!             & 'No local facet found')
!         END IF
!
!         colID(ii) = colID(ii) + 1
!
!         IF (colID(ii) .GT. 2) THEN
!           CALL e%raiseError(modName//"::"//myName//" - "// &
!             & "It seems the facet element = "//TOSTRING(iface)// &
!             & " has more than 2 cell element")
!         ELSE
!           obj%facetToCell(ii)%GlobalCellData(1, colID(ii)) = &
!             & cellGlobalNum
!           obj%facetToCell(ii)%GlobalCellData(2, colID(ii)) = &
!             & localFacetID
!           obj%facetToCell(ii)%GlobalCellData(3:4, colID(ii)) = &
!             & [dim_facet + 1, icellMesh]
!         END IF
!       END IF
!
!     END DO
!
!     IF (colID(ii) .EQ. 0) THEN
!       CALL e%raiseWarning(modName//"::"//myName//" - "// &
!         & "It seems the facet element = "//TOSTRING(iface)// &
!         & " has no cell element"// &
!       & " . Nptrs of facet = "//tostring(nptrs))
!     END IF
!
!   END DO
!
! END DO
!
! NULLIFY (cellMesh)
! IF (ALLOCATED(colID)) DEALLOCATE (colID)
! IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
! IF (ALLOCATED(pt2elem)) DEALLOCATE (pt2elem)
! IF (ALLOCATED(cellNptrs)) DEALLOCATE (cellNptrs)
! IF (ALLOCATED(facetNptrs)) DEALLOCATE (facetNptrs)
!
! CALL e%raiseInformation(modName//'::'//myName//' - '// &
!   & '[END] InitiateFacetToCellData()')

END PROCEDURE obj_InitiateFacetToCellData4

!----------------------------------------------------------------------------
!                                                          masterCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MasterCellNumber1
ans = obj%facetToCell(localElement)%GlobalCellData(1, 1)
END PROCEDURE obj_MasterCellNumber1

!----------------------------------------------------------------------------
!                                                           masterCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MasterCellNumber2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(ii) = obj%facetToCell(localElement(ii))%GlobalCellData(1, 1)
END DO
END PROCEDURE obj_MasterCellNumber2

!----------------------------------------------------------------------------
!                                                           masterCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MasterCellNumber3
INTEGER(I4B) :: tsize

IF (ALLOCATED(obj%facetToCell)) THEN
  tsize = SIZE(obj%facetToCell)
ELSE
  tsize = 0
END IF

CALL Reallocate(ans, tsize)

CALL obj%GetMasterCellNumber(VALUE=ans)
END PROCEDURE obj_MasterCellNumber3

!----------------------------------------------------------------------------
!                                                           masterCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMasterCellNumber
INTEGER(I4B) :: ii, tsize

tsize = 0
IF (ALLOCATED(obj%facetToCell)) THEN
  tsize = SIZE(obj%facetToCell)
END IF

DO ii = 1, tsize
  VALUE(ii) = obj%facetToCell(ii)%GlobalCellData(1, 1)
END DO

END PROCEDURE obj_GetMasterCellNumber

!----------------------------------------------------------------------------
!                                                            slaveCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SlaveCellNumber1
ans = obj%facetToCell(localElement)%GlobalCellData(1, 2)
END PROCEDURE obj_SlaveCellNumber1

!----------------------------------------------------------------------------
!                                                            slaveCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SlaveCellNumber2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(ii) = obj%facetToCell(localElement(ii))%GlobalCellData(1, 2)
END DO
END PROCEDURE obj_SlaveCellNumber2

!----------------------------------------------------------------------------
!                                                            slaveCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SlaveCellNumber3
INTEGER(I4B) :: tsize

tsize = 0
IF (ALLOCATED(obj%facetToCell)) THEN
  tsize = SIZE(obj%facetToCell)
ELSE
  tsize = 0
END IF

CALL Reallocate(ans, tsize)
CALL obj%GetSlaveCellNumber(ans)
END PROCEDURE obj_SlaveCellNumber3

!----------------------------------------------------------------------------
!                                                          GetSlaveCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSlaveCellNumber
INTEGER(I4B) :: ii, tsize
tsize = 0
IF (ALLOCATED(obj%facetToCell)) THEN
  tsize = SIZE(obj%facetToCell)
END IF

DO ii = 1, tsize
  VALUE(ii) = obj%facetToCell(ii)%GlobalCellData(1, 2)
END DO
END PROCEDURE obj_GetSlaveCellNumber

!----------------------------------------------------------------------------
!                                                       masterFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MasterFacetLocalID1
ans = obj%facetToCell(localElement)%GlobalCellData(2, 1)
END PROCEDURE obj_MasterFacetLocalID1

!----------------------------------------------------------------------------
!                                                       masterFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MasterFacetLocalID2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(ii) = obj%facetToCell(localElement(ii))%GlobalCellData(2, 1)
END DO
END PROCEDURE obj_MasterFacetLocalID2

!----------------------------------------------------------------------------
!                                                       masterFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MasterFacetLocalID3
INTEGER(I4B) :: tsize

tsize = 0
IF (ALLOCATED(obj%facetToCell)) THEN
  tsize = SIZE(obj%facetToCell)
END IF

CALL Reallocate(ans, tsize)

CALL obj%GetMasterFacetLocalID(ans)

END PROCEDURE obj_MasterFacetLocalID3

!----------------------------------------------------------------------------
!                                                     GetMasterFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMasterFacetLocalID
INTEGER(I4B) :: ii, tsize

tsize = obj%GetTotalFacet()

DO ii = 1, tsize
  VALUE(ii) = obj%facetToCell(ii)%GlobalCellData(2, 1)
END DO
END PROCEDURE obj_GetMasterFacetLocalID

!----------------------------------------------------------------------------
!                                                       slaveFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SlaveFacetLocalID1
ans = obj%facetToCell(localElement)%GlobalCellData(2, 2)
END PROCEDURE obj_SlaveFacetLocalID1

!----------------------------------------------------------------------------
!                                                       slaveFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SlaveFacetLocalID2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(ii) = obj%facetToCell(localElement(ii))%GlobalCellData(2, 2)
END DO
END PROCEDURE obj_SlaveFacetLocalID2

!----------------------------------------------------------------------------
!                                                       slaveFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SlaveFacetLocalID3
INTEGER(I4B) :: tsize

tsize = obj%GetTotalFacet()

CALL Reallocate(ans, tsize)
CALL obj%GetSlaveCellNumber(ans)

END PROCEDURE obj_SlaveFacetLocalID3

!----------------------------------------------------------------------------
!                                                       GetSlaveFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSlaveFacetLocalID
INTEGER(I4B) :: ii, tsize

tsize = obj%GetTotalFacet()

DO ii = 1, tsize
  VALUE(ii) = obj%facetToCell(ii)%GlobalCellData(2, 2)
END DO
END PROCEDURE obj_GetSlaveFacetLocalID

!----------------------------------------------------------------------------
!                                                               masterDimTag
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MasterDimTag1
ans = obj%facetToCell(localElement)%GlobalCellData(3:4, 1)
END PROCEDURE obj_MasterDimTag1

!----------------------------------------------------------------------------
!                                                               masterDimTag
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MasterDimTag2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(:, ii) = obj%facetToCell(localElement(ii))%GlobalCellData(3:4, 1)
END DO
END PROCEDURE obj_MasterDimTag2

!----------------------------------------------------------------------------
!                                                               slaveDimTag
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MasterDimTag3
INTEGER(I4B) :: ii, tsize

tsize = obj%GetTotalFacet()
ii = 0
IF (ALLOCATED(obj%facetToCell)) THEN
  ii = 2
END IF

IF (isTranspose) THEN
  CALL Reallocate(ans, tsize, ii)
ELSE
  CALL Reallocate(ans, ii, tsize)
END IF

CALL obj%GetMasterDimTag(isTranspose=isTranspose, VALUE=ans)
END PROCEDURE obj_MasterDimTag3

!----------------------------------------------------------------------------
!                                                            GetMasterDimTag
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMasterDimTag
INTEGER(I4B) :: ii, tsize

tsize = obj%GetTotalFacet()

IF (isTranspose) THEN

  DO ii = 1, tsize
    VALUE(ii, 1) = obj%facetToCell(ii)%GlobalCellData(3, 1)
  END DO

  DO ii = 1, tsize
    VALUE(ii, 2) = obj%facetToCell(ii)%GlobalCellData(4, 1)
  END DO

  RETURN

END IF

DO ii = 1, tsize
  VALUE(1:2, ii) = obj%facetToCell(ii)%GlobalCellData(3:4, 1)
END DO

END PROCEDURE obj_GetMasterDimTag

!----------------------------------------------------------------------------
!                                                               slaveDimTag
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SlaveDimTag1
ans = obj%facetToCell(localElement)%GlobalCellData(3:4, 2)
END PROCEDURE obj_SlaveDimTag1

!----------------------------------------------------------------------------
!                                                               slaveDimTag
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SlaveDimTag2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(1:2, ii) = obj%facetToCell(localElement(ii))%GlobalCellData(3:4, 2)
END DO
END PROCEDURE obj_SlaveDimTag2

!----------------------------------------------------------------------------
!                                                               slaveDimTag
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SlaveDimTag3
INTEGER(I4B) :: ii, tsize

ii = 0
tsize = obj%GetTotalFacet()

IF (isTranspose) THEN
  CALL Reallocate(ans, tsize, ii)

ELSE
  CALL Reallocate(ans, ii, tsize)

END IF

CALL obj%GetSlaveDimTag(isTranspose=isTranspose, VALUE=ans)
END PROCEDURE obj_SlaveDimTag3

!----------------------------------------------------------------------------
!                                                             GetSlaveDimTag
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSlaveDimTag
INTEGER(I4B) :: ii, tsize

tsize = obj%GetTotalFacet()

IF (isTranspose) THEN

  DO ii = 1, tsize
    VALUE(ii, 1) = obj%facetToCell(ii)%GlobalCellData(3, 2)
    VALUE(ii, 2) = obj%facetToCell(ii)%GlobalCellData(4, 2)
  END DO

ELSE

  DO ii = 1, tsize
    VALUE(1:2, ii) = obj%facetToCell(ii)%GlobalCellData(3:4, 2)
  END DO

END IF

END PROCEDURE obj_GetSlaveDimTag

!----------------------------------------------------------------------------
!                                                             GlobalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GlobalFacetID1
ans = obj%facetToCell(localElement)%facetID
END PROCEDURE obj_GlobalFacetID1

!----------------------------------------------------------------------------
!                                                             GlobalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GlobalFacetID2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(ii) = obj%facetToCell(localElement(ii))%facetID
END DO
END PROCEDURE obj_GlobalFacetID2

!----------------------------------------------------------------------------
!                                                             GlobalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GlobalFacetID3
INTEGER(I4B) :: tsize

tsize = obj%GetTotalFacet()
CALL Reallocate(ans, tsize)

CALL obj%GetGlobalFacetID(ans)

END PROCEDURE obj_GlobalFacetID3

!----------------------------------------------------------------------------
!                                                          GetGlobalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalFacetID
INTEGER(I4B) :: ii, tsize

tsize = obj%GetTotalFacet()

DO ii = 1, tsize
  VALUE(ii) = obj%facetToCell(ii)%facetID
END DO
END PROCEDURE obj_GetGlobalFacetID

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalFacet
ans = 0
IF (ALLOCATED(obj%facetToCell)) THEN
  ans = SIZE(obj%facetToCell)
END IF
END PROCEDURE obj_GetTotalFacet

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE FacetMethods
