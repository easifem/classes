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

SUBMODULE(DomainConnectivity_Class) FacetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE facet_to_cell_helper(obj, facetMesh, cellMesh, dim, entityNum, &
                                isMaster)
  CLASS(DomainConnectivity_), INTENT(INOUT) :: obj
    !! Domain connectivity data
  CLASS(AbstractMesh_), INTENT(INOUT) :: facetMesh
    !! Mesh of facet elements
  CLASS(AbstractMesh_), INTENT(INOUT) :: cellMesh
    !! Master mesh
  INTEGER(I4B), INTENT(IN) :: dim
  INTEGER(I4B), INTENT(IN) :: entityNum
  LOGICAL(LGT), INTENT(IN) :: isMaster
    !! if true then cell Mesh is master cell
    !! if false then cell mesh is slave cell

  INTEGER(I4B) :: iface, icell, ii, tfacet, cellGlobalNum, &
    & localFacetID, jj
  INTEGER(I4B), ALLOCATABLE :: nptrs(:), pt2elem(:), &
    & cellNptrs(:), facetNptrs(:)
  CHARACTER(*), PARAMETER :: myName = "facet_to_cell_helper"

  ii = 0

  tfacet = cellMesh%GetTotalFacetElements()
  IF (tfacet .EQ. 0_I4B) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
     '[INTERNAL ERROR] :: Mesh_::cellMesh%facetElements should be allocated!')
  END IF

  DO iface = facetMesh%GetMinElemNumber(), facetMesh%GetMaxElemNumber()
    IF (.NOT. facetMesh%isElementPresent(globalElement=iface)) CYCLE
    nptrs = facetMesh%getConnectivity(globalElement=iface)

    ! It is important that all nodes of a facet element are present
    ! in the cellMesh.

    ii = ii + 1

    obj%facetToCell(ii)%facetID = iface

    IF (.NOT. cellMesh%isAllNodePresent(nptrs)) CYCLE

    ! Get the element in Cell mesh surrounding this node

    pt2elem = cellMesh%getNodetoElements(globalNode=nptrs)

    DO icell = 1, SIZE(pt2elem)

      cellNptrs = cellMesh%getConnectivity(globalElement=pt2elem(icell))

      IF (nptrs.IN.cellNptrs) THEN

        cellGlobalNum = pt2elem(icell)

        localFacetID = 0

        DO jj = 1, tfacet

          facetNptrs = cellMesh%getFacetConnectivity(&
            & globalElement=cellGlobalNum, &
            & iface=jj)

          IF (nptrs.in.facetNptrs) THEN
            localFacetID = jj
            EXIT
          END IF

        END DO

        IF (localFacetID .EQ. 0) THEN
          CALL e%RaiseError(modName//'::'//myName//' - '// &
            & 'No local facet found')
        END IF

        IF (isMaster) THEN
          obj%facetToCell(ii)%GlobalCellData(1, 1) = cellGlobalNum
          obj%facetToCell(ii)%GlobalCellData(2, 1) = localFacetID
          obj%facetToCell(ii)%GlobalCellData(3:4, 1) = [dim, entityNum]
        ELSE
          obj%facetToCell(ii)%GlobalCellData(1, 2) = cellGlobalNum
          obj%facetToCell(ii)%GlobalCellData(2, 2) = localFacetID
          obj%facetToCell(ii)%GlobalCellData(3:4, 2) = [dim, entityNum]
        END IF

        EXIT

      END IF

    END DO
  END DO

  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  IF (ALLOCATED(pt2elem)) DEALLOCATE (pt2elem)
  IF (ALLOCATED(cellNptrs)) DEALLOCATE (cellNptrs)
  IF (ALLOCATED(facetNptrs)) DEALLOCATE (facetNptrs)

END SUBROUTINE facet_to_cell_helper

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_InitiateFacetToCellData1
!
INTEGER(I4B) :: tfacet
CHARACTER(*), PARAMETER :: myName = "dc_InitiateFacetToCellData1"
LOGICAL(LGT) :: isVar

CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START] InitiateFacetToCellData()')

CALL cellMesh%GetParam(isNodeToElementsInitiated=isVar)

IF (.NOT. isVar) THEN
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & "In cellMesh node to elements data is not initiated, &
    & calling cellMesh%InitiateNodeToElements()")
  CALL cellMesh%InitiateNodeToElements()
END IF

IF (obj%isFacetToCell) THEN
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "It seems, obj%facetToCell data is already initiated")
  RETURN
END IF

tfacet = facetMesh%getTotalElements()
ALLOCATE (obj%facetToCell(tfacet))
obj%isFacetToCell = .TRUE.
CALL display("Calling facet_to_cell_helper()", unitno=stdout)

CALL facet_to_cell_helper(obj, facetMesh, cellMesh, dim, entityNum, isMaster)

CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[END] InitiateFacetToCellData()')

END PROCEDURE dc_InitiateFacetToCellData1

!----------------------------------------------------------------------------
!                                                    InitiateFacetToCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_InitiateFacetToCellData2
CHARACTER(*), PARAMETER :: myName = "dc_InitiateFacetToCellData2"
INTEGER(I4B) :: dim_facet, icellMesh, tCellMesh, tface, nsd
CLASS(AbstractMesh_), POINTER :: meshptr
LOGICAL(LGT) :: isVar

CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START] InitiateFacetToCellData()')

IF (obj%isFacetToCell) THEN
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "It seems, obj%facetToCell data is already initiated")
  RETURN
END IF

meshptr => NULL()
dim_facet = facetMesh%getXidimension()
nsd = masterDomain%getNSD()

IF (dim_facet .GE. nsd) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & 'xidimension of facet mesh is >= to spatial dimension of masterDomain')
END IF

tface = facetMesh%getTotalElements()
! IF (ALLOCATED(obj%facetToCell)) DEALLOCATE (obj%facetToCell)
ALLOCATE (obj%facetToCell(tface))
obj%isFacetToCell = .TRUE.

! Handling masterCell

tCellMesh = masterDomain%getTotalMesh(dim=dim_facet + 1)

DO icellMesh = 1, tCellMesh

  meshptr => masterDomain%getMeshPointer(dim=dim_facet + 1, &
    & entityNum=icellMesh)

  CALL meshptr%GetParam(isNodeToElementsInitiated=isVar)

  IF (.NOT. isVar) THEN
    CALL e%raiseInformation(modName//'::'//myName//' - '// &
      & "In cellMesh node to elements data is not initiated, &
      & calling cellMesh%InitiateNodeToElements()")
    CALL meshptr%InitiateNodeToElements()
  END IF

  CALL facet_to_cell_helper(obj=obj, &
    & facetMesh=facetMesh, &
    & cellMesh=meshptr, &
    & dim=dim_facet + 1, &
    & entityNum=icellMesh, &
    & isMaster=.TRUE.)

END DO

! Handling slaveCell

tCellMesh = slaveDomain%getTotalMesh(dim=dim_facet + 1)

DO icellMesh = 1, tCellMesh

  meshptr => slaveDomain%getMeshPointer(dim=dim_facet + 1, &
    & entityNum=icellMesh)

  CALL facet_to_cell_helper( &
    & obj=obj, &
    & facetMesh=facetMesh, &
    & cellMesh=meshptr, &
    & dim=dim_facet + 1, &
    & entityNum=icellMesh, &
    & isMaster=.FALSE.)

END DO

NULLIFY (meshptr)

CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[END] InitiateFacetToCellData()')

END PROCEDURE dc_InitiateFacetToCellData2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_InitiateFacetToCellData3
!
INTEGER(I4B) :: iface, icell, ii, colID, tface, tfacet, &
& cellGlobalNum, localFacetID, jj
INTEGER(I4B), ALLOCATABLE :: nptrs(:), pt2elem(:), &
  & cellNptrs(:), facetNptrs(:)
CHARACTER(*), PARAMETER :: myName = "dc_InitiateFacetToCellData3"
LOGICAL(LGT) :: isVar

CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START] InitiateFacetToCellData()')

tfacet = cellMesh%GetTotalFacetElements()
IF (tfacet .EQ. 0) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
   & '[INTERNAL ERROR] :: Mesh_::cellMesh%facetElements should be allocated!')
END IF

CALL cellMesh%GetParam(isNodeToElementsInitiated=isVar)

IF (.NOT. isVar) THEN
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & "In cellMesh node to elements data is not initiated, &
    & calling cellMesh%InitiateNodeToElements()")
  CALL cellMesh%InitiateNodeToElements()
END IF

IF (obj%isFacetToCell) THEN
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "It seems, obj%facetToCell data is already initiated")
  RETURN
END IF

tface = facetMesh%getTotalElements()
ALLOCATE (obj%facetToCell(tface))

ii = 0

DO iface = facetMesh%GetMinElemNumber(), facetMesh%GetMaxElemNumber()
  IF (.NOT. facetMesh%isElementPresent(globalElement=iface)) CYCLE
  nptrs = facetMesh%getConnectivity(globalElement=iface)

  ! It is important that all nodes of a facet element are present
  ! in the cellMesh.

  ii = ii + 1

  obj%facetToCell(ii)%facetID = iface

  IF (.NOT. cellMesh%isAllNodePresent(nptrs)) CYCLE

  ! Get the element in Cell mesh surrounding this node

  pt2elem = cellMesh%getNodetoElements(globalNode=nptrs)

  colID = 0

  DO icell = 1, SIZE(pt2elem)

    cellGlobalNum = pt2elem(icell)

    cellNptrs = cellMesh%getConnectivity(globalElement=cellGlobalNum)

    IF (nptrs.IN.cellNptrs) THEN

      localFacetID = 0

      DO jj = 1, tfacet

        facetNptrs = cellMesh%getFacetConnectivity(&
          & globalElement=cellGlobalNum, &
          & iface=jj)

        IF (nptrs.in.facetNptrs) THEN
          localFacetID = jj
          EXIT
        END IF

      END DO

      IF (localFacetID .EQ. 0) THEN
        CALL e%RaiseError(modName//'::'//myName//' - '// &
          & 'No local facet found')
      END IF

      colID = colID + 1

      IF (colID .GT. 2) THEN
        CALL e%RaiseError(modName//"::"//myName//" - "// &
          & "It seems the facet element has more than 2 cell element")
      ELSE
        obj%facetToCell(ii)%GlobalCellData(1, colID) = cellGlobalNum
        obj%facetToCell(ii)%GlobalCellData(2, colID) = localFacetID
        obj%facetToCell(ii)%GlobalCellData(3:4, colID) = [dim, entityNum]
      END IF
    END IF

  END DO

  IF (colID .EQ. 0) THEN
    CALL e%RaiseError(modName//"::"//myName//" - "// &
      & "It seems the facet element has no cell element")
  END IF

END DO

IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(pt2elem)) DEALLOCATE (pt2elem)
IF (ALLOCATED(cellNptrs)) DEALLOCATE (cellNptrs)
IF (ALLOCATED(facetNptrs)) DEALLOCATE (facetNptrs)

CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[END] InitiateFacetToCellData()')
END PROCEDURE dc_InitiateFacetToCellData3

!----------------------------------------------------------------------------
!                                                    InitiateFacetToCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_InitiateFacetToCellData4
CHARACTER(*), PARAMETER :: myName = "dc_InitiateFacetToCellData4"
INTEGER(I4B) :: dim_facet, icellMesh, tCellMesh, tface, ii, iface, icell, &
& nsd, tfacet, cellGlobalNum, localFacetID, jj
CLASS(AbstractMesh_), POINTER :: cellMesh
INTEGER(I4B), ALLOCATABLE :: colID(:), nptrs(:), pt2elem(:), &
  & cellNptrs(:), facetNptrs(:)
LOGICAL(LGT) :: isVar

CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[START] InitiateFacetToCellData()')

IF (obj%isFacetToCell) THEN
  CALL e%raiseInformation(modName//"::"//myName//" - "// &
    & "It seems, obj%facetToCell data is already initiated")
  RETURN
END IF

cellMesh => NULL()
dim_facet = facetMesh%getXidimension()
nsd = cellDomain%getNSD()

IF (dim_facet .GE. nsd) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & 'xidimension of facet mesh is >= to spatial dimension of cellDomain')
END IF

tface = facetMesh%getTotalElements()
! IF (ALLOCATED(obj%facetToCell)) DEALLOCATE (obj%facetToCell)
ALLOCATE (obj%facetToCell(tface))

ALLOCATE (colID(tface))
colID = 0
!
! Handling masterCell
!
tCellMesh = cellDomain%getTotalMesh(dim=dim_facet + 1)

DO icellMesh = 1, tCellMesh

  cellMesh => cellDomain%getMeshPointer(dim=dim_facet + 1, &
    & entityNum=icellMesh)

  !
  ! Check if the mesh is not empty
  !
  IF (.NOT. ASSOCIATED(cellMesh)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'Mesh_::cellMesh is not associated!')
  END IF

  IF (cellMesh%getTotalElements() .EQ. 0) CYCLE

  tfacet = cellMesh%GetTotalFacetElements()
  IF (tfacet .EQ. 0) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
     '[INTERNAL ERROR] :: Mesh_::cellMesh%facetElements should be allocated!')
  END IF

  CALL cellMesh%GetParam(isNodeToElementsInitiated=isVar)

  IF (.NOT. isVar) THEN
    CALL e%raiseInformation(modName//'::'//myName//' - '// &
      & "In cellMesh node to elements data is not initiated, &
      & calling cellMesh%InitiateNodeToElements()")
    CALL cellMesh%InitiateNodeToElements()
  END IF

  ii = 0

  DO iface = facetMesh%GetMinElemNumber(), facetMesh%GetMaxElemNumber()

    IF (.NOT. facetMesh%isElementPresent(globalElement=iface)) CYCLE
    nptrs = facetMesh%getConnectivity(globalElement=iface)

    ! It is important that all nodes of a facet element are present
    ! in the cellMesh.

    ii = ii + 1

    obj%facetToCell(ii)%facetID = iface

    IF (.NOT. cellMesh%isAllNodePresent(nptrs)) CYCLE

    ! Get the element in Cell mesh surrounding this node

    pt2elem = cellMesh%getNodetoElements(globalNode=nptrs)

    ! colID = 0

    DO icell = 1, SIZE(pt2elem)

      cellGlobalNum = pt2elem(icell)
      cellNptrs = cellMesh%getConnectivity(globalElement=cellGlobalNum)

      IF (nptrs.IN.cellNptrs) THEN

        localFacetID = 0

        DO jj = 1, tfacet

          facetNptrs = cellMesh%getFacetConnectivity(&
            & globalElement=cellGlobalNum, &
            & iface=jj)

          IF (nptrs.in.facetNptrs) THEN
            localFacetID = jj
            EXIT
          END IF

        END DO

        IF (localFacetID .EQ. 0) THEN
          CALL e%RaiseError(modName//'::'//myName//' - '// &
            & 'No local facet found')
        END IF

        colID(ii) = colID(ii) + 1

        IF (colID(ii) .GT. 2) THEN
          CALL e%RaiseError(modName//"::"//myName//" - "// &
            & "It seems the facet element = "//TOSTRING(iface)// &
            & " has more than 2 cell element")
        ELSE
          obj%facetToCell(ii)%GlobalCellData(1, colID(ii)) = &
            & cellGlobalNum
          obj%facetToCell(ii)%GlobalCellData(2, colID(ii)) = &
            & localFacetID
          obj%facetToCell(ii)%GlobalCellData(3:4, colID(ii)) = &
            & [dim_facet + 1, icellMesh]
        END IF
      END IF

    END DO

    IF (colID(ii) .EQ. 0) THEN
      CALL e%raiseWarning(modName//"::"//myName//" - "// &
        & "It seems the facet element = "//TOSTRING(iface)// &
        & " has no cell element"// &
      & " . Nptrs of facet = "//tostring(nptrs))
    END IF

  END DO

END DO

NULLIFY (cellMesh)
IF (ALLOCATED(colID)) DEALLOCATE (colID)
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
IF (ALLOCATED(pt2elem)) DEALLOCATE (pt2elem)
IF (ALLOCATED(cellNptrs)) DEALLOCATE (cellNptrs)
IF (ALLOCATED(facetNptrs)) DEALLOCATE (facetNptrs)

CALL e%raiseInformation(modName//'::'//myName//' - '// &
  & '[END] InitiateFacetToCellData()')

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
INTEGER(I4B) :: ii
!
DO ii = 1, SIZE(localElement)
  ans(ii) = obj%facetToCell(localElement(ii))%GlobalCellData(1, 1)
END DO
END PROCEDURE dc_masterCellNumber2

!----------------------------------------------------------------------------
!                                                           masterCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_masterCellNumber3
INTEGER(I4B) :: ii
IF (ALLOCATED(obj%facetToCell)) THEN
  CALL Reallocate(ans, SIZE(obj%facetToCell))
ELSE
  ALLOCATE (ans(0))
  RETURN
END IF
DO ii = 1, SIZE(ans)
  ans(ii) = obj%facetToCell(ii)%GlobalCellData(1, 1)
END DO
END PROCEDURE dc_masterCellNumber3

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
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(ii) = obj%facetToCell(localElement(ii))%GlobalCellData(1, 2)
END DO
END PROCEDURE dc_slaveCellNumber2

!----------------------------------------------------------------------------
!                                                            slaveCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_slaveCellNumber3
INTEGER(I4B) :: ii
IF (ALLOCATED(obj%facetToCell)) THEN
  CALL Reallocate(ans, SIZE(obj%facetToCell))
ELSE
  ALLOCATE (ans(0))
  RETURN
END IF
DO ii = 1, SIZE(ans)
  ans(ii) = obj%facetToCell(ii)%GlobalCellData(1, 2)
END DO
END PROCEDURE dc_slaveCellNumber3

!----------------------------------------------------------------------------
!                                                       masterFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_masterFacetLocalID1
ans = obj%facetToCell(localElement)%GlobalCellData(2, 1)
END PROCEDURE dc_masterFacetLocalID1

!----------------------------------------------------------------------------
!                                                       masterFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_masterFacetLocalID2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(ii) = obj%facetToCell(localElement(ii))%GlobalCellData(2, 1)
END DO
END PROCEDURE dc_masterFacetLocalID2

!----------------------------------------------------------------------------
!                                                       masterFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_masterFacetLocalID3
INTEGER(I4B) :: ii
IF (ALLOCATED(obj%facetToCell)) THEN
  CALL Reallocate(ans, SIZE(obj%facetToCell))
ELSE
  ALLOCATE (ans(0))
  RETURN
END IF
DO ii = 1, SIZE(ans)
  ans(ii) = obj%facetToCell(ii)%GlobalCellData(2, 1)
END DO
END PROCEDURE dc_masterFacetLocalID3

!----------------------------------------------------------------------------
!                                                       slaveFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_slaveFacetLocalID1
ans = obj%facetToCell(localElement)%GlobalCellData(2, 2)
END PROCEDURE dc_slaveFacetLocalID1

!----------------------------------------------------------------------------
!                                                       slaveFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_slaveFacetLocalID2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(ii) = obj%facetToCell(localElement(ii))%GlobalCellData(2, 2)
END DO
END PROCEDURE dc_slaveFacetLocalID2

!----------------------------------------------------------------------------
!                                                       slaveFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_slaveFacetLocalID3
INTEGER(I4B) :: ii
IF (ALLOCATED(obj%facetToCell)) THEN
  CALL Reallocate(ans, SIZE(obj%facetToCell))
ELSE
  ALLOCATE (ans(0))
  RETURN
END IF
DO ii = 1, SIZE(ans)
  ans(ii) = obj%facetToCell(ii)%GlobalCellData(2, 2)
END DO
END PROCEDURE dc_slaveFacetLocalID3

!----------------------------------------------------------------------------
!                                                               masterDimTag
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_masterDimTag1
ans = obj%facetToCell(localElement)%GlobalCellData(3:4, 1)
END PROCEDURE dc_masterDimTag1

!----------------------------------------------------------------------------
!                                                               masterDimTag
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_masterDimTag2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(:, ii) = obj%facetToCell(localElement(ii))%GlobalCellData(3:4, 1)
END DO
END PROCEDURE dc_masterDimTag2

!----------------------------------------------------------------------------
!                                                               slaveDimTag
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_masterDimTag3
INTEGER(I4B) :: ii
IF (isTranspose) THEN
  IF (ALLOCATED(obj%facetToCell)) THEN
    CALL Reallocate(ans, SIZE(obj%facetToCell), 2)
  ELSE
    ALLOCATE (ans(0, 0))
    RETURN
  END IF
  DO ii = 1, SIZE(ans, 1)
    ans(ii, :) = obj%facetToCell(ii)%GlobalCellData(3:4, 1)
  END DO
ELSE
  IF (ALLOCATED(obj%facetToCell)) THEN
    CALL Reallocate(ans, 2, SIZE(obj%facetToCell))
  ELSE
    ALLOCATE (ans(0, 0))
    RETURN
  END IF
  DO ii = 1, SIZE(ans, 2)
    ans(:, ii) = obj%facetToCell(ii)%GlobalCellData(3:4, 1)
  END DO
END IF
END PROCEDURE dc_masterDimTag3

!----------------------------------------------------------------------------
!                                                               slaveDimTag
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_slaveDimTag1
ans = obj%facetToCell(localElement)%GlobalCellData(3:4, 2)
END PROCEDURE dc_slaveDimTag1

!----------------------------------------------------------------------------
!                                                               slaveDimTag
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_slaveDimTag2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(:, ii) = obj%facetToCell(localElement(ii))%GlobalCellData(3:4, 2)
END DO
END PROCEDURE dc_slaveDimTag2

!----------------------------------------------------------------------------
!                                                               slaveDimTag
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_slaveDimTag3
INTEGER(I4B) :: ii
IF (isTranspose) THEN
  IF (ALLOCATED(obj%facetToCell)) THEN
    CALL Reallocate(ans, SIZE(obj%facetToCell), 2)
  ELSE
    ALLOCATE (ans(0, 0))
    RETURN
  END IF
  DO ii = 1, SIZE(ans, 1)
    ans(ii, :) = obj%facetToCell(ii)%GlobalCellData(3:4, 2)
  END DO
ELSE
  IF (ALLOCATED(obj%facetToCell)) THEN
    CALL Reallocate(ans, 2, SIZE(obj%facetToCell))
  ELSE
    ALLOCATE (ans(0, 0))
    RETURN
  END IF
  DO ii = 1, SIZE(ans, 2)
    ans(:, ii) = obj%facetToCell(ii)%GlobalCellData(3:4, 2)
  END DO
END IF
END PROCEDURE dc_slaveDimTag3

!----------------------------------------------------------------------------
!                                                             GlobalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_GlobalFacetID1
ans = obj%facetToCell(localElement)%facetID
END PROCEDURE dc_GlobalFacetID1

!----------------------------------------------------------------------------
!                                                             GlobalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_GlobalFacetID2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(localElement)
  ans(ii) = obj%facetToCell(localElement(ii))%facetID
END DO
END PROCEDURE dc_GlobalFacetID2

!----------------------------------------------------------------------------
!                                                             GlobalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_GlobalFacetID3
INTEGER(I4B) :: ii
IF (ALLOCATED(obj%facetToCell)) THEN
  CALL Reallocate(ans, SIZE(obj%facetToCell))
ELSE
  ALLOCATE (ans(0))
  RETURN
END IF
DO ii = 1, SIZE(ans)
  ans(ii) = obj%facetToCell(ii)%facetID
END DO
END PROCEDURE dc_GlobalFacetID3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_GetTotalFacet
IF (ALLOCATED(obj%facetToCell)) THEN
  ans = SIZE(obj%facetToCell)
ELSE
  ans = 0
END IF
END PROCEDURE dc_GetTotalFacet

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE FacetMethods
