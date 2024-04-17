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

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Domain connectivity class

MODULE DomainConnectivity_Class
USE BaseType
USE GlobalData, ONLY: I4B, LGT, DFP
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE ExceptionHandler_Class, ONLY: e
USE AbstractMesh_Class, ONLY: AbstractMesh_
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "DomainConnectivity_Class"
INTEGER(I4B), PUBLIC, PARAMETER :: pType = 1
INTEGER(I4B), PUBLIC, PARAMETER :: hType = 2
INTEGER(I4B), PUBLIC, PARAMETER :: rType = 3
INTEGER(I4B), PUBLIC, PARAMETER :: oversetType = 4

PUBLIC :: DomainConnectivity_
PUBLIC :: DomainConnectivityPointer_
PUBLIC :: DomainConnectivityDeallocate

!----------------------------------------------------------------------------
!                                                        FacetConnectivity_
!----------------------------------------------------------------------------

TYPE :: FacetConnectivity_
  INTEGER(I4B) :: facetID = 0
    !! global element number of facet element in facet mesh
  INTEGER(I4B) :: GlobalCellData(4, 2) = 0
    !! 1,1 --> Global element number of master cell
    !! 2,1 --> master cell's local facet number connected to facet-elem
    !! 3,1 --> master mesh dimension
    !! 4,1 --> master mesh entity number
    !! 1,2 --> Global element number of slave cell
    !! 2,2 --> slave cell's local facet number connected to facet-elem
    !! 3,2 --> slave mesh dimension
    !! 4,2 --> slave mesh entity number
END TYPE FacetConnectivity_

!----------------------------------------------------------------------------
!                                                      ElementConnectivity_
!----------------------------------------------------------------------------

TYPE :: ElementConnectivity_
  INTEGER(I4B) :: masterGlobalElemNum = 0
  INTEGER(I4B) :: masterLocalFacetID = 0
  INTEGER(I4B) :: slaveGlobalElemNum = 0
  INTEGER(I4B) :: slaveLocalFacetID = 0
END TYPE ElementConnectivity_

!----------------------------------------------------------------------------
!                                                       DomainConnectivity_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: This data type contains connectivity data between two [[domain_]]
!
!{!pages/DomainConnectivity_.md!}

TYPE :: DomainConnectivity_
  PRIVATE
  LOGICAL(LGT), PUBLIC :: isInitiated = .FALSE.
    !! True if an instance of [[DomainConnectivity_]] is initiated
  LOGICAL(LGT), PUBLIC :: isFacetToCell = .FALSE.
    !! True if FacetToCell data is allocated and initiated
  LOGICAL(LGT), PUBLIC :: isNodeToNode = .FALSE.
    !! True if nodeToNode data is initiate
  LOGICAL(LGT), PUBLIC :: isCellToCell = .FALSE.
    !! True if elemToElem data is initiated
  INTEGER(I4B), ALLOCATABLE :: nodeToNode(:)
    !! Node to node connectivity
    !! Size of NodeToNode is equal to the largest node number in
    !! domain-1 or mesh-1 (depending upon how the data is initiated)
    !! NodeToNode(i) => global node number in domain-2, corresponding to
    !! global node number `i` in domain-1
  INTEGER(I4B), ALLOCATABLE :: cellToCell(:)
    !! Cell to cell connectivity
    !! CellToCell(ielem) => global elem number in domain-2,
    !! corresponding to
    !! global node number `ielem` in domain-1
  INTEGER(I4B), ALLOCATABLE :: cellToCellExtraData(:, :)
    !! Currently, cellToCellExtraData has two rows
    !! the first row is dim
    !! the second row is entityNum
    !! the column represents the element number
    !! example: iel1 in domain1
    !! cellToCell(iel) gives iel2 in domain2
    !! cellToCellExtraData(1, iel1) gives
    !! dimension of mesh which contains iel2
    !! cellToCellExtraData(2, iel1) gives
    !! entityNum of mesh which contains iel2
    !! In this way,
    !! domain2%getMeshPointer(dim, entityNum)
    !! can give us the pointer to the mesh
    !! which contains the iel2
  TYPE(FacetConnectivity_), ALLOCATABLE :: facetToCell(:)
    !! Facet connectivity, Facet to cell data
  TYPE(ElementConnectivity_), ALLOCATABLE :: elemToElem(:)
    !! ElemToElem connectivity  data
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => dc_Deallocate
  !! Deallocate data stored in the object
  FINAL :: dc_Final
  !! finalizer
  !
  ! @NodeMethods
  !
  PROCEDURE, PASS(obj) :: dc_initiateNodeToNodeData1
  !! Initiate [[DomainConnectivity_:nodeToNode]]
  PROCEDURE, PASS(obj) :: dc_initiateNodeToNodeData2
  !! Initiate [[DomainConnectivity_:nodeToNode]]
  GENERIC, PUBLIC :: initiateNodeToNodeData => &
    & dc_initiateNodeToNodeData1, &
    & dc_initiateNodeToNodeData2
  !! Initiate [[DomainConnectivity_:nodeToNode]]
  PROCEDURE, PUBLIC, PASS(obj) :: getNodeToNodePointer => &
    & dc_getNodeToNodePointer
  !! Return pointer to the [[DomainConnectivity_:nodeToNode]]
  !
  ! @CellMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: dc_initiateCellToCellData1
  !! Initiates [[DomainConnectivity_:cellToCell]] data
  PROCEDURE, PUBLIC, PASS(obj) :: dc_initiateCellToCellData2
  !! Initiates [[DomainConnectivity_:cellToCell]] data
  GENERIC, PUBLIC :: initiateCellToCellData => &
    & dc_initiateCellToCellData1, &
    & dc_initiateCellToCellData2
  !! Initiates [[DomainConnectivity_:cellToCell]] data
  PROCEDURE, PUBLIC, PASS(obj) :: getCellToCellPointer => &
    & dc_getCellToCellPointer
  !! Return pointer to the [[DomainConnectivity_:CellToCell]]
  PROCEDURE, PUBLIC, PASS(obj) :: getDimEntityNum => &
    & dc_getDimEntityNum
  !! Returns the dim and entity num of mesh which contains
  !! the element (in domain2) which is connected to element
  !! in domain 1.
  !
  ! @FacetMethods
  !
  PROCEDURE, PRIVATE, PASS(obj) :: dc_initiateFacetToCellData1
  PROCEDURE, PRIVATE, PASS(obj) :: dc_initiateFacetToCellData2
  PROCEDURE, PRIVATE, PASS(obj) :: dc_initiateFacetToCellData3
  PROCEDURE, PRIVATE, PASS(obj) :: dc_initiateFacetToCellData4
  !! Initiate facet to cell connectivity
  !! [[DomainConnectivity_:facetToCell]]
  GENERIC, PUBLIC :: initiateFacetToCellData =>  &
    & dc_initiateFacetToCellData1, &
    & dc_initiateFacetToCellData2, &
    & dc_initiateFacetToCellData3, &
    & dc_initiateFacetToCellData4
  !! Initiate facet to cell connectivity
  !! [[DomainConnectivity_:facetToCell]]
  PROCEDURE, PRIVATE, PASS(obj) :: dc_masterCellNumber1
  !! Return the masterCell number of a given facet
  PROCEDURE, PRIVATE, PASS(obj) :: dc_masterCellNumber2
  !! Return the masterCell numbers of given facet elements
  PROCEDURE, PRIVATE, PASS(obj) :: dc_masterCellNumber3
  !! Return the masterCell numbers of given facet elements
  GENERIC, PUBLIC :: masterCellNumber =>  &
    & dc_masterCellNumber1, &
    & dc_masterCellNumber2, &
    & dc_masterCellNumber3
  PROCEDURE, PRIVATE, PASS(obj) :: dc_slaveCellNumber1
  !! Return the slaveCell number of a given facet
  PROCEDURE, PRIVATE, PASS(obj) :: dc_slaveCellNumber2
  !! Return the slaveCell numbers of given facet elements
  PROCEDURE, PRIVATE, PASS(obj) :: dc_slaveCellNumber3
  !! Return the slaveCell numbers of given facet elements
  GENERIC, PUBLIC :: slaveCellNumber =>  &
    & dc_slaveCellNumber1, &
    & dc_slaveCellNumber2, &
    & dc_slaveCellNumber3
  !! Return the cell numbers of given facet elements
  PROCEDURE, PRIVATE, PASS(obj) :: dc_masterFacetLocalID1
  !! Return the facet local id in cell element
  PROCEDURE, PRIVATE, PASS(obj) :: dc_masterFacetLocalID2
  !! Return the facet local id in cell element
  PROCEDURE, PRIVATE, PASS(obj) :: dc_masterFacetLocalID3
  !! Return the facet local id in cell element
  GENERIC, PUBLIC :: masterFacetLocalID =>  &
    & dc_masterFacetLocalID1, &
    & dc_masterFacetLocalID2, &
    & dc_masterFacetLocalID3
  !! Return the facet local id in cell element
  PROCEDURE, PRIVATE, PASS(obj) :: dc_slaveFacetLocalID1
  !! Return the facet local id in cell element
  PROCEDURE, PRIVATE, PASS(obj) :: dc_slaveFacetLocalID2
  !! Return the facet local id in cell element
  PROCEDURE, PRIVATE, PASS(obj) :: dc_slaveFacetLocalID3
  !! Return the facet local id in cell element
  GENERIC, PUBLIC :: slaveFacetLocalID =>  &
    & dc_slaveFacetLocalID1, &
    & dc_slaveFacetLocalID2, &
    & dc_slaveFacetLocalID3
  !! Return the facet local id in cell element
  PROCEDURE, PRIVATE, PASS(obj) :: dc_masterDimTag1
  !! (dim, entityNum) of master cell
  PROCEDURE, PRIVATE, PASS(obj) :: dc_masterDimTag2
  !! (dim, entityNum) of master cell
  PROCEDURE, PRIVATE, PASS(obj) :: dc_masterDimTag3
  !! (dim, entityNum) of master cell
  GENERIC, PUBLIC :: masterDimTag => &
    & dc_masterDimTag1, &
    & dc_masterDimTag2, &
    & dc_masterDimTag3
  !! (dim, entityNum) of master cell
  PROCEDURE, PRIVATE, PASS(obj) :: dc_slaveDimTag1
  !! (dim, entityNum) of slave cell
  PROCEDURE, PRIVATE, PASS(obj) :: dc_slaveDimTag2
  !! (dim, entityNum) of slave cell
  PROCEDURE, PRIVATE, PASS(obj) :: dc_slaveDimTag3
  !! (dim, entityNum) of slave cell
  GENERIC, PUBLIC :: slaveDimTag => &
    & dc_slaveDimTag1, &
    & dc_slaveDimTag2, &
    & dc_slaveDimTag3
  !! (dim, entityNum) of slave cell
  PROCEDURE, PRIVATE, PASS(obj) :: dc_GlobalFacetID1
  !! global facet id of local facet id is returned
  PROCEDURE, PRIVATE, PASS(obj) :: dc_GlobalFacetID2
  !! global facet id of local facet id is returned
  PROCEDURE, PRIVATE, PASS(obj) :: dc_GlobalFacetID3
  !! global facet id of local facet id is returned
  GENERIC, PUBLIC :: GlobalFacetID => &
    & dc_GlobalFacetID1, &
    & dc_GlobalFacetID2, &
    & dc_GlobalFacetID3
  !! global facet id of local facet id is returned
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalFacet => &
    & dc_GetTotalFacet
  !! returns size of facetToCell
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayFacetToCellData => &
    & dc_DisplayFacetToCellData
END TYPE DomainConnectivity_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: DomainConnectivityPointer_
  CLASS(DomainConnectivity_), POINTER :: Ptr => NULL()
END TYPE DomainConnectivityPointer_

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Deallocates the data stored in [[DomainConnectivity_]]
!
!# Introduction
!
! This subroutine deallocate the data stored in [[DomainConnectivity_]]
!

INTERFACE
  MODULE SUBROUTINE dc_Deallocate(obj)
    CLASS(DomainConnectivity_), INTENT(INOUT) :: obj
    !! Mesh connectivity object
  END SUBROUTINE dc_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary: Deallocate a vector of DomainConnectivity_

INTERFACE DomainConnectivityDeallocate
  MODULE SUBROUTINE dc_Deallocate2(obj)
    TYPE(DomainConnectivity_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE dc_Deallocate2
END INTERFACE DomainConnectivityDeallocate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary: Deallocate a vector of DomainConnectivityPointer_

INTERFACE DomainConnectivityDeallocate
  MODULE SUBROUTINE dc_Deallocate3(obj)
    TYPE(DomainConnectivityPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE dc_Deallocate3
END INTERFACE DomainConnectivityDeallocate

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE dc_Final(obj)
    TYPE(DomainConnectivity_), INTENT(INOUT) :: obj
  END SUBROUTINE dc_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                          DisplayFacetToCellData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 March 2022
! summary: Display FaceToCellData

INTERFACE
  MODULE SUBROUTINE dc_DisplayFacetToCellData(obj, msg, unitno)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE dc_DisplayFacetToCellData
END INTERFACE
!----------------------------------------------------------------------------
!                                  InitiateNodeToNodeData@NodeToNodeMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-10
! update: 2021-11-10
! summary: Generate node to node connectivity
!
!# Introduction
!
!  This subroutine generates the node to node connectivity between two meshes
!
! - `obj%nodeToNode` will be initiated
! - `domain1` main domain
! - `domain2` secondary domain
! - `dim1, entitynum1` dimension and entity number of mesh in `domain1`
! - `dim2, entitynum2` dimension and entity number of mesh in `domain2`
!
!@warn
!In this case bounds of [[DomainConnectivity_:nodeToNode]] will be from
!1 to mesh1%maxNptrs.
!@endwarn

INTERFACE
  MODULE SUBROUTINE dc_InitiateNodeToNodeData1(obj, domain1, domain2, &
    & dim1, dim2, entityNum1, entityNum2)
    CLASS(DomainConnectivity_), INTENT(INOUT) :: obj
    !! Domain connectivity object,
    !! [[DomainConnectivity:nodeToNode]] will be initiated
    CLASS(AbstractDomain_), INTENT(IN) :: domain1
    !! Primary domain, in nodeToNode(i), i denotes the
    !! global node number in domain1 domain.
    CLASS(AbstractDomain_), INTENT(IN) :: domain2
    !! secondary domain, => nodeToNode(i) denotes the
    !! global node number in `domain2` domain.
    INTEGER(I4B), INTENT(IN) :: dim1
    !! dimension of mesh in domain1
    INTEGER(I4B), INTENT(IN) :: dim2
    !! dimension of mesh in domain2
    INTEGER(I4B), INTENT(IN) :: entityNum1
    !! entity num of mesh in domain1
    INTEGER(I4B), INTENT(IN) :: entityNum2
    !! entity num of mesh in domain2
  END SUBROUTINE dc_InitiateNodeToNodeData1
END INTERFACE

!----------------------------------------------------------------------------
!                                         InitiateNodeToNodeData@NodeMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-10
! update: 2021-11-10
! summary: Generate node to node connectivity
!
!# Introduction
!
!  This subroutine generates the node to node connectivity between two domains
!
!@note
!In this routine nodeToNode connectivity info of all meshes in domain1 to
!all meshes in the domain2 will be generated!
!@endnote
!
! - `obj%nodeToNode` will be initiated
! - `domain1` main domain
! - `domain2` secondary domain

INTERFACE
  MODULE SUBROUTINE dc_InitiateNodeToNodeData2(obj, domain1, domain2)
    CLASS(DomainConnectivity_), INTENT(INOUT) :: obj
    !! Domain connectivity object
    CLASS(AbstractDomain_), INTENT(IN) :: domain1
    !! Primary domain, in nodeToNode(i), i denotes the
    !! global node number in domain1 domain.
    CLASS(AbstractDomain_), INTENT(IN) :: domain2
    !! Secondary domain => nodeToNode(i) denotes the
    !! global node number in domain2 domain.
  END SUBROUTINE dc_InitiateNodeToNodeData2
END INTERFACE

!----------------------------------------------------------------------------
!                                           getNodeToNodePointer@NodeMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-10
! update: 2021-11-10
! summary: Returns the node to node connectivity info
!
!# Introduction
!
!  This function returns the pointer
!  to [[DomainConnectivity_:nodeToNode]]
!

INTERFACE
  MODULE FUNCTION dc_getNodeToNodePointer(obj) RESULT(Ans)
    CLASS(DomainConnectivity_), TARGET, INTENT(IN) :: obj
    INTEGER(I4B), POINTER :: ans(:)
  END FUNCTION dc_getNodeToNodePointer
END INTERFACE

!----------------------------------------------------------------------------
!                                         InitiateCellToCellData@CellMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-10
! update: 2021-11-10
! summary: Generate cell to cell connectivity
!
!# Introduction
!
!This subroutine generates the cell to cell connectivity between
!two meshes
!
! - `obj%cellToCell` will be initiated
! - `domain1` main domain
! - `domain2` secondary domain
! - `dim1, entitynum1` dimension and entity number of mesh in `domain1`
! - `dim2, entitynum2` dimension and entity number of mesh in `domain2`
!
! Following points should be noted
!
! - The topology of elements in both meshes should be the same, this
! means that if one mesh is triangle then other mesh should be a triangle
! - The xidim of the elements in both meshes should be the same, this means
! that if the mesh1 is surface mesh then mesh2 should be a surface mesh
! - This routine needs [[DomainConnectivity_:nodeToNode]] information, so
! make sure it is initiated before calling this routine.

INTERFACE
  MODULE SUBROUTINE dc_initiateCellToCellData1(obj, domain1, domain2, &
    & dim1, dim2, entityNum1, entityNum2)
    CLASS(DomainConnectivity_), INTENT(INOUT) :: obj
    !! Domain connectivity object,
    !! [[DomainConnectivity:cellToCell]] will be initiated
    CLASS(AbstractDomain_), INTENT(IN) :: domain1
    !! Primary domain, in cellToCell(i), i denotes the
    !! global element number in domain1 domain.
    CLASS(AbstractDomain_), INTENT(IN) :: domain2
    !! secondary domain, => cellToCell(i) denotes the
    !! global cell number in `domain2` domain.
    INTEGER(I4B), INTENT(IN) :: dim1
    !! dimension of mesh in domain1
    INTEGER(I4B), INTENT(IN) :: dim2
    !! dimension of mesh in domain2
    INTEGER(I4B), INTENT(IN) :: entityNum1
    !! entity num of mesh in domain1
    INTEGER(I4B), INTENT(IN) :: entityNum2
    !! entity num of mesh in domain2
  END SUBROUTINE dc_initiateCellToCellData1
END INTERFACE

!----------------------------------------------------------------------------
!                                        InitiateCellToCellData@NodeMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-10
! update: 2021-11-10
! summary: Generate cell to cell connectivity
!
!# Introduction
!
!This subroutine generates the cell to cell connectivity between
!two domains.
!
! - `obj%cellToCell` will be initiated
! - `domain1` main domain
! - `domain2` secondary domain
!
!@note
!All **CELL** elements in domain-1 will be mapped to **CELL**
!elements in domain-2.
!@endnote
!
!@note
!If cellToCell(iel) is equal to zero then it means there is
!no element found in domain-2 corresponding to element number
!iel in domain-1.
!@endnote
!
!@note
!The size of [[DomainConnectivity_:cellToCell]] is the largest
!element number present in domain1.
!@endnote
!
!@todo
!TODO
!Currently, lowerbound and upper bound of cellToCell is 1 and
!domain1%maxElemNumber. In future it the lower bound will be
!domain1%minElemNumber.
!@endtodo
!
!@note
!Following points should be noted before calling this routine
!
! - This routine provides map between cell elements
!of one domain to cell elements of another domain.
! - The topology of the both elements should be the same
! - There is one to one mapping between elements of domain 1
! and elements of domain2
! - This routine works well for two domains of same region
! with same/different order. For example, domain of tri3 and domain
! of tri6 elements.
!@endnote

INTERFACE
  MODULE SUBROUTINE dc_InitiateCellToCellData2(obj, domain1, domain2)
    CLASS(DomainConnectivity_), INTENT(INOUT) :: obj
    !! Domain connectivity object
    CLASS(AbstractDomain_), INTENT(IN) :: domain1
    !! Primary domain, in CellToCell(i), i denotes the
    !! global element number in domain1 domain.
    CLASS(AbstractDomain_), INTENT(IN) :: domain2
    !! Secondary domain => CellToCell(i) denotes the
    !! global element number in domain2 domain.
  END SUBROUTINE dc_InitiateCellToCellData2
END INTERFACE

!----------------------------------------------------------------------------
!                                           getCellToCellPointer@CellMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-10
! update: 2021-11-10
! summary: Returns pointer to  cell-to-cell data
!
!# Introduction
!
! This function returns the pointer to [[DomainConnectivity_:CellToCell]]

INTERFACE
  MODULE FUNCTION dc_getCellToCellPointer(obj) RESULT(Ans)
    CLASS(DomainConnectivity_), TARGET, INTENT(IN) :: obj
    INTEGER(I4B), POINTER :: ans(:)
  END FUNCTION dc_getCellToCellPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                getDimEntityNum@CellMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-10
! update: 2021-11-10
! summary: Returns pointer to  cell-to-cell data
!
!# Introduction
!
! This function returns the pointer to [[DomainConnectivity_:CellToCell]]

INTERFACE
  MODULE PURE FUNCTION dc_getDimEntityNum(obj, globalElement) RESULT(Ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B) :: ans(2)
  END FUNCTION dc_getDimEntityNum
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateFacetToCellData@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2022
! summary: This is a helper routine for dc_InitiateFacetToCellData1
!
!# Introduction
!
! - This routine initiate `facetToCell` for given facetMesh and CellMesh
! - In this case facetMesh should be a boundary of cellMesh
! - This routine should not be used for internal boundary.

INTERFACE
  MODULE SUBROUTINE dc_InitiateFacetToCellData1(obj, facetMesh, &
    & cellMesh, dim, entityNum, isMaster)
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
  END SUBROUTINE dc_InitiateFacetToCellData1
END INTERFACE

!----------------------------------------------------------------------------
!                                      InitiateFacetToCellData@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Generate the connectivity matrix between cell and facet mesh.
!
!# Introduction
!
! This subroutine generates the faceToCell connectivity data between
! between masterDomain, slaveDomain and faceMesh.
!
! In this case facetMesh should a boundary of masterDomain and slaveDomain
! In otherwords, facetMesh cannot represent the internal boundary.
! This routine calls `dc_InitiateFacetToCellData1` routine.

INTERFACE
  MODULE SUBROUTINE dc_InitiateFacetToCellData2(obj, facetMesh, &
    & masterDomain, slaveDomain)
    CLASS(DomainConnectivity_), INTENT(INOUT) :: obj
    !! Mesh connectivity data
    CLASS(AbstractMesh_), INTENT(INOUT) :: facetMesh
    !! Mesh of facet elements
    CLASS(AbstractDomain_), INTENT(INOUT) :: masterDomain
    !! Domain of master elements
    CLASS(AbstractDomain_), INTENT(INOUT) :: slaveDomain
    !! Domain of slave elements
  END SUBROUTINE dc_InitiateFacetToCellData2
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateFacetToCellData@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2022
! summary: This is a helper routine for dc_InitiateFacetToCellData1
!
!# Introduction
!
! - This routine initiate `facetToCell` for given facetMesh and CellMesh
! - In this case facetMesh can be an internal boundary of cellMesh

INTERFACE
  MODULE SUBROUTINE dc_InitiateFacetToCellData3(obj, facetMesh, &
    & cellMesh, dim, entityNum)
    CLASS(DomainConnectivity_), INTENT(INOUT) :: obj
    !! Domain connectivity data
    CLASS(AbstractMesh_), INTENT(INOUT) :: facetMesh
    !! Mesh of facet elements
    CLASS(AbstractMesh_), INTENT(INOUT) :: cellMesh
    !! Master mesh
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: entityNum
  END SUBROUTINE dc_InitiateFacetToCellData3
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateFacetToCellData@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2022
! summary: This is a helper routine for dc_InitiateFacetToCellData1
!
!# Introduction
!
! - This routine initiate `facetToCell` for given facetMesh and CellMesh
! - In this case facetMesh can be an internal boundary of cellMesh

INTERFACE
  MODULE SUBROUTINE dc_InitiateFacetToCellData4(obj, facetMesh, cellDomain)
    CLASS(DomainConnectivity_), INTENT(INOUT) :: obj
    !! Domain connectivity data
    CLASS(AbstractMesh_), INTENT(INOUT) :: facetMesh
    !! Mesh of facet elements
    CLASS(AbstractDomain_), INTENT(INOUT) :: cellDomain
    !! Master mesh
  END SUBROUTINE dc_InitiateFacetToCellData4
END INTERFACE

!----------------------------------------------------------------------------
!                                                    CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns master cell number of given facet number
!
!# Introduction
!
! - Returns master cell number of given facet number
! - If cell number is zero it means facet element does not have a master cell

INTERFACE
  MODULE PURE FUNCTION dc_MasterCellNumber1(obj, localElement) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement
    !! Facet element number
    INTEGER(I4B) :: ans
    !! Cell number
  END FUNCTION dc_MasterCellNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns master cell number of given facet number
!
!# Introduction
!
! - Returns master cell number of given facet number
! - if master cell number is zero it means facet element is an orphan

INTERFACE
  MODULE PURE FUNCTION dc_MasterCellNumber2(obj, localElement) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(SIZE(localElement))
    !! List of cell element numbers
  END FUNCTION dc_MasterCellNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns master cell number of given facet number

INTERFACE
  MODULE PURE FUNCTION dc_MasterCellNumber3(obj) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! List of cell element numbers
  END FUNCTION dc_MasterCellNumber3
END INTERFACE

!----------------------------------------------------------------------------
!                                                    CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns slave cell number of given facet number
!
!# Introduction
!
! - Returns slave cell number of given facet number
! - If slave cell number is zero it means facet element is an orphan

INTERFACE
  MODULE PURE FUNCTION dc_SlaveCellNumber1(obj, localElement) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement
    !! Facet element number
    INTEGER(I4B) :: ans
    !! Cell number
  END FUNCTION dc_SlaveCellNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns slave cell number of given facet number
!
!# Introduction
!
! - Returns slave cell number of given facet number
! - if slave cell number is zero it means facet element is an orphan

INTERFACE
  MODULE PURE FUNCTION dc_SlaveCellNumber2(obj, localElement) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(SIZE(localElement))
    !! List of cell element numbers
  END FUNCTION dc_SlaveCellNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns slave cell number of given facet number

INTERFACE
  MODULE PURE FUNCTION dc_SlaveCellNumber3(obj) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! List of cell element numbers
  END FUNCTION dc_SlaveCellNumber3
END INTERFACE

!----------------------------------------------------------------------------
!                                          masterFacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the local facet id in master cell element
!
!# Introduction
!
! Returns the local facet id in master cell element which is in contact with
! facet element

INTERFACE
  MODULE PURE FUNCTION dc_masterFacetLocalID1(obj, localElement) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity object
    INTEGER(I4B), INTENT(IN) :: localElement
    !! Facet element number
    INTEGER(I4B) :: ans
    !! Local facet ID
  END FUNCTION dc_masterFacetLocalID1
END INTERFACE

!----------------------------------------------------------------------------
!                                           masterFacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the local facet id in master cell element
!
!# Introduction
!
! Returns the local facet id in master cell element which is in contact with
! facet element
!
!## Usage
!
!```fortran
! id = obj % FacetLocalID( FacetNum )
!```

INTERFACE
  MODULE PURE FUNCTION dc_masterFacetLocalID2(obj, localElement) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(SIZE(localElement))
    !! List of local facet IDs
  END FUNCTION dc_masterFacetLocalID2
END INTERFACE

!----------------------------------------------------------------------------
!                                           masterFacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the local facet id in master cell element

INTERFACE
  MODULE PURE FUNCTION dc_masterFacetLocalID3(obj) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! List of local facet IDs
  END FUNCTION dc_masterFacetLocalID3
END INTERFACE

!----------------------------------------------------------------------------
!                                          slaveFacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the local facet id in slave cell element
!
!# Introduction
!
! Returns the local facet id in slave cell element which is in contact with
! facet element

INTERFACE
  MODULE PURE FUNCTION dc_slaveFacetLocalID1(obj, localElement) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity object
    INTEGER(I4B), INTENT(IN) :: localElement
    !! Facet element number
    INTEGER(I4B) :: ans
    !! Local facet ID
  END FUNCTION dc_slaveFacetLocalID1
END INTERFACE

!----------------------------------------------------------------------------
!                                           slaveFacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the local facet id in slave cell element

INTERFACE
  MODULE PURE FUNCTION dc_slaveFacetLocalID2(obj, localElement) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(SIZE(localElement))
    !! List of local facet IDs
  END FUNCTION dc_slaveFacetLocalID2
END INTERFACE

!----------------------------------------------------------------------------
!                                           slaveFacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the local facet id in slave cell element

INTERFACE
  MODULE PURE FUNCTION dc_slaveFacetLocalID3(obj) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! List of local facet IDs
  END FUNCTION dc_slaveFacetLocalID3
END INTERFACE

!----------------------------------------------------------------------------
!                                                  masterDimTag@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the (dimtag, entityNum) in master cell element

INTERFACE
  MODULE PURE FUNCTION dc_masterDimTag1(obj, localElement) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity object
    INTEGER(I4B), INTENT(IN) :: localElement
    !! Facet element number
    INTEGER(I4B) :: ans(2)
    !! dim, entityNum
  END FUNCTION dc_masterDimTag1
END INTERFACE

!----------------------------------------------------------------------------
!                                           masterDimTag@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the (dimtag, entityNum) in master cell element

INTERFACE
  MODULE PURE FUNCTION dc_masterDimTag2(obj, localElement) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(2, SIZE(localElement))
    !! dim, entityNum
  END FUNCTION dc_masterDimTag2
END INTERFACE

!----------------------------------------------------------------------------
!                                           masterDimTag@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the (dimtag, entityNum) in master cell element

INTERFACE
  MODULE PURE FUNCTION dc_masterDimTag3(obj, isTranspose) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    LOGICAL(LGT), INTENT(IN) :: isTranspose
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! dim, entityNum
  END FUNCTION dc_masterDimTag3
END INTERFACE

!----------------------------------------------------------------------------
!                                                  slaveDimTag@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the (dimtag, entityNum) in slave cell element

INTERFACE
  MODULE PURE FUNCTION dc_slaveDimTag1(obj, localElement) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity object
    INTEGER(I4B), INTENT(IN) :: localElement
    !! Facet element number
    INTEGER(I4B) :: ans(2)
    !! dim, entityNum
  END FUNCTION dc_slaveDimTag1
END INTERFACE

!----------------------------------------------------------------------------
!                                           slaveDimTag@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the (dimtag, entityNum) in slave cell element

INTERFACE
  MODULE PURE FUNCTION dc_slaveDimTag2(obj, localElement) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(2, SIZE(localElement))
    !! dim, entityNum
  END FUNCTION dc_slaveDimTag2
END INTERFACE

!----------------------------------------------------------------------------
!                                           slaveDimTag@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the (dimtag, entityNum) in slave cell element

INTERFACE
  MODULE PURE FUNCTION dc_slaveDimTag3(obj, isTranspose) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    LOGICAL(LGT), INTENT(IN) :: isTranspose
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! dim, entityNum
  END FUNCTION dc_slaveDimTag3
END INTERFACE

!----------------------------------------------------------------------------
!                                                GlobalFacetID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the local global facet id

INTERFACE
  MODULE PURE FUNCTION dc_GlobalFacetID1(obj, localElement) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity object
    INTEGER(I4B), INTENT(IN) :: localElement
    !! Facet element number
    INTEGER(I4B) :: ans
  END FUNCTION dc_GlobalFacetID1
END INTERFACE

!----------------------------------------------------------------------------
!                                           GlobalFacetID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the global facet id

INTERFACE
  MODULE PURE FUNCTION dc_GlobalFacetID2(obj, localElement) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(SIZE(localElement))
  END FUNCTION dc_GlobalFacetID2
END INTERFACE

!----------------------------------------------------------------------------
!                                           GlobalFacetID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the global facet id

INTERFACE
  MODULE PURE FUNCTION dc_GlobalFacetID3(obj) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION dc_GlobalFacetID3
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION dc_GetTotalFacet(obj) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION dc_GetTotalFacet
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DomainConnectivity_Class
