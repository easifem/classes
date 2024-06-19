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
! date: 2024-04-08
! summary: FEDomain connectivity class

MODULE FEDomainConnectivity_Class
USE GlobalData, ONLY: LGT, DFP, I4B
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

PRIVATE

PUBLIC :: FEDomainConnectivity_
PUBLIC :: FEDomainConnectivityPointer_
PUBLIC :: FEDomainConnectivityDeallocate

CHARACTER(*), PARAMETER :: modName = "FEDomainConnectivity_Class"
INTEGER(I4B), PARAMETER :: pType = 1
INTEGER(I4B), PARAMETER :: hType = 2
INTEGER(I4B), PARAMETER :: rType = 3
INTEGER(I4B), PARAMETER :: oversetType = 4

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
!                                                       FEDomainConnectivity_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-31
! summary: This data type contains connectivity data between two [[domain_]]
!
!{!pages/docs-api/FEDomainConnectivity/FEDomainConnectivity_.md!}

TYPE :: FEDomainConnectivity_
  PRIVATE
  LOGICAL(LGT), PUBLIC :: isInitiated = .FALSE.
    !! True if an instance of [[FEDomainConnectivity_]] is Initiated
  LOGICAL(LGT), PUBLIC :: isFacetToCell = .FALSE.
    !! True if FacetToCell data is allocated and Initiated
  LOGICAL(LGT), PUBLIC :: isNodeToNode = .FALSE.
    !! True if nodeToNode data is Initiate
  LOGICAL(LGT), PUBLIC :: isCellToCell = .FALSE.
    !! True if elemToElem data is Initiated
  INTEGER(I4B), ALLOCATABLE :: nodeToNode(:)
    !! Node to node connectivity
    !! Size of NodeToNode is equal to the largest node number in
    !! domain-1 or mesh-1 (depending upon how the data is Initiated)
    !! NodeToNode(i) => global node number in domain-2, corresponding to
    !! global node number `i` in domain-1
  INTEGER(I4B), ALLOCATABLE :: cellToCell(:)
    !! Cell to cell connectivity
    !! CellToCell(ielem) => global elem number in domain-2,
    !! corresponding to
    !! global elem number `ielem` in domain-1
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
    !! domain2%getFEMeshPointer(dim, entityNum)
    !! can give us the pointer to the mesh
    !! which contains the iel2
  TYPE(FacetConnectivity_), ALLOCATABLE :: facetToCell(:)
    !! Facet connectivity, Facet to cell data
  TYPE(ElementConnectivity_), ALLOCATABLE :: elemToElem(:)
    !! ElemToElem connectivity  data

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate1
  !! Deallocate data stored in the object

  FINAL :: obj_Final
  !! finalizer

  ! SET:
  ! @NodeMethods

  PROCEDURE, PASS(obj) :: InitiateNodeToNodeData1 => &
    obj_InitiateNodeToNodeData1
  !! Initiate [[FEDomainConnectivity_:nodeToNode]] between two domains

  PROCEDURE, PASS(obj) :: InitiateNodeToNodeData2 => &
    obj_InitiateNodeToNodeData2
  !! Initiate [[FEDomainConnectivity_:nodeToNode]] between two meshes

  GENERIC, PUBLIC :: InitiateNodeToNodeData => &
    InitiateNodeToNodeData1, InitiateNodeToNodeData2
  !! Initiate [[FEDomainConnectivity_:nodeToNode]]

  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeToNodePointer => &
    obj_GetNodeToNodePointer
  !! Return pointer to the [[FEDomainConnectivity_:nodeToNode]]

  ! SET:
  ! @CellMethods

  PROCEDURE, PASS(obj) :: obj_InitiateCellToCellData1
  !! Initiates [[FEDomainConnectivity_:cellToCell]] data between two domains

  PROCEDURE, PASS(obj) :: obj_InitiateCellToCellData2
  !! Initiates [[FEDomainConnectivity_:cellToCell]] data between two meshes

  GENERIC, PUBLIC :: InitiateCellToCellData => &
    obj_InitiateCellToCellData1, obj_InitiateCellToCellData2

  PROCEDURE, PUBLIC, PASS(obj) :: GetCellToCellPointer => &
    obj_GetCellToCellPointer
  !! Return pointer to the [[FEDomainConnectivity_:CellToCell]]

  PROCEDURE, PUBLIC, PASS(obj) :: GetDimEntityNum => &
    obj_GetDimEntityNum
  !! Returns the dim and entity num of mesh which contains
  !! the element (in domain2) which is connected to element
  !! in domain 1.

  ! SET:
  ! @FacetMethods

  PROCEDURE, PASS(obj) :: obj_InitiateFacetToCellData1
  PROCEDURE, PASS(obj) :: obj_InitiateFacetToCellData2
  PROCEDURE, PASS(obj) :: obj_InitiateFacetToCellData3
  PROCEDURE, PASS(obj) :: obj_InitiateFacetToCellData4
  !! Initiate facet to cell connectivity
  !! [[FEDomainConnectivity_:facetToCell]]
  GENERIC, PUBLIC :: InitiateFacetToCellData => &
    obj_InitiateFacetToCellData1, &
    obj_InitiateFacetToCellData2, &
    obj_InitiateFacetToCellData3, &
    obj_InitiateFacetToCellData4
  !! Initiate facet to cell connectivity
  !! [[FEDomainConnectivity_:facetToCell]]

  PROCEDURE, PASS(obj) :: MasterCellNumber1 => obj_MasterCellNumber1
  !! Return the masterCell number of a given facet
  PROCEDURE, PASS(obj) :: MasterCellNumber2 => obj_MasterCellNumber2
  !! Return the masterCell numbers of given facet elements
  PROCEDURE, PASS(obj) :: MasterCellNumber3 => obj_MasterCellNumber3
  !! Return the masterCell numbers of given facet elements
  GENERIC, PUBLIC :: MasterCellNumber => &
    MasterCellNumber1, &
    MasterCellNumber2, &
    MasterCellNumber3

  PROCEDURE, PUBLIC, PASS(obj) :: GetMasterCellNumber => &
    obj_GetMasterCellNumber

  PROCEDURE, PUBLIC, PASS(obj) :: GetSlaveCellNumber => &
    obj_GetSlaveCellNumber

  PROCEDURE, PASS(obj) :: SlaveCellNumber1 => obj_SlaveCellNumber1
  !! Return the SlaveCell number of a given facet
  PROCEDURE, PASS(obj) :: SlaveCellNumber2 => obj_SlaveCellNumber2
  !! Return the SlaveCell numbers of given facet elements
  PROCEDURE, PASS(obj) :: SlaveCellNumber3 => obj_SlaveCellNumber3
  !! Return the SlaveCell numbers of given facet elements
  GENERIC, PUBLIC :: SlaveCellNumber => &
    SlaveCellNumber1, &
    SlaveCellNumber2, &
    SlaveCellNumber3

  PROCEDURE, PASS(obj) :: MasterFacetLocalID1 => obj_MasterFacetLocalID1
  !! Return the facet local id in cell element
  PROCEDURE, PASS(obj) :: MasterFacetLocalID2 => obj_MasterFacetLocalID2
  !! Return the facet local id in cell element
  PROCEDURE, PASS(obj) :: MasterFacetLocalID3 => obj_MasterFacetLocalID3
  !! Return the facet local id in cell element
  GENERIC, PUBLIC :: MasterFacetLocalID => &
    MasterFacetLocalID1, &
    MasterFacetLocalID2, &
    MasterFacetLocalID3
  !! Return the facet local id in cell element

  PROCEDURE, PUBLIC, PASS(obj) :: GetMasterFacetLocalID => &
    obj_GetMasterFacetLocalID

  PROCEDURE, PASS(obj) :: SlaveFacetLocalID1 => obj_SlaveFacetLocalID1
  !! Return the facet local id in cell element
  PROCEDURE, PASS(obj) :: SlaveFacetLocalID2 => obj_SlaveFacetLocalID2
  !! Return the facet local id in cell element
  PROCEDURE, PASS(obj) :: SlaveFacetLocalID3 => obj_SlaveFacetLocalID3
  !! Return the facet local id in cell element
  GENERIC, PUBLIC :: SlaveFacetLocalID => &
    SlaveFacetLocalID1, &
    SlaveFacetLocalID2, &
    SlaveFacetLocalID3
  !! Return the facet local id in cell element

  PROCEDURE, PUBLIC, PASS(obj) :: GetSlaveFacetLocalID => &
    obj_GetSlaveFacetLocalID

  PROCEDURE, PASS(obj) :: MasterDimTag1 => obj_MasterDimTag1
  !! (dim, entityNum) of Master cell
  PROCEDURE, PASS(obj) :: MasterDimTag2 => obj_MasterDimTag2
  !! (dim, entityNum) of Master cell
  PROCEDURE, PASS(obj) :: MasterDimTag3 => obj_MasterDimTag3
  !! (dim, entityNum) of Master cell
  GENERIC, PUBLIC :: MasterDimTag => &
    MasterDimTag1, &
    MasterDimTag2, &
    MasterDimTag3
  !! (dim, entityNum) of master cell

  PROCEDURE, PUBLIC, PASS(obj) :: GetMasterDimTag => obj_GetMasterDimTag

  PROCEDURE, PASS(obj) :: SlaveDimTag1 => obj_SlaveDimTag1
  !! (dim, entityNum) of Slave cell
  PROCEDURE, PASS(obj) :: SlaveDimTag2 => obj_SlaveDimTag2
  !! (dim, entityNum) of Slave cell
  PROCEDURE, PASS(obj) :: SlaveDimTag3 => obj_SlaveDimTag3
  !! (dim, entityNum) of Slave cell
  GENERIC, PUBLIC :: SlaveDimTag => &
    SlaveDimTag1, &
    SlaveDimTag2, &
    SlaveDimTag3
  !! (dim, entityNum) of Slave cell

  PROCEDURE, PUBLIC, PASS(obj) :: GetSlaveDimTag => obj_GetSlaveDimTag

  PROCEDURE, PRIVATE, PASS(obj) :: GlobalFacetID1 => obj_GlobalFacetID1
  !! global facet id of local facet id is returned
  PROCEDURE, PRIVATE, PASS(obj) :: GlobalFacetID2 => obj_GlobalFacetID2
  !! global facet id of local facet id is returned
  PROCEDURE, PRIVATE, PASS(obj) :: GlobalFacetID3 => obj_GlobalFacetID3
  !! global facet id of local facet id is returned
  GENERIC, PUBLIC :: GlobalFacetID => &
    GlobalFacetID1, &
    GlobalFacetID2, &
    GlobalFacetID3
  !! global facet id of local facet id is returned

  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalFacetID => obj_GetGlobalFacetID

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalFacet => obj_GetTotalFacet
  !! returns size of facetToCell

  PROCEDURE, PUBLIC, PASS(obj) :: DisplayFacetToCellData => &
    obj_DisplayFacetToCellData

END TYPE FEDomainConnectivity_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: FEDomainConnectivityPointer_
  CLASS(FEDomainConnectivity_), POINTER :: Ptr => NULL()
END TYPE FEDomainConnectivityPointer_

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Deallocates the data stored in [[FEDomainConnectivity_]]
!
!# Introduction
!
! This subroutine deallocate the data stored in [[FEDomainConnectivity_]]
!

INTERFACE
  MODULE SUBROUTINE obj_Deallocate1(obj)
    CLASS(FEDomainConnectivity_), INTENT(INOUT) :: obj
    !! FEMesh connectivity object
  END SUBROUTINE obj_Deallocate1
END INTERFACE

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary: Deallocate a vector of FEDomainConnectivity_

INTERFACE FEDomainConnectivityDeallocate
  MODULE SUBROUTINE obj_Deallocate2(obj)
    TYPE(FEDomainConnectivity_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE obj_Deallocate2
END INTERFACE FEDomainConnectivityDeallocate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary: Deallocate a vector of FEDomainConnectivityPointer_

INTERFACE FEDomainConnectivityDeallocate
  MODULE SUBROUTINE obj_Deallocate3(obj)
    TYPE(FEDomainConnectivityPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE obj_Deallocate3
END INTERFACE FEDomainConnectivityDeallocate

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(FEDomainConnectivity_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                          DisplayFacetToCellData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Display FaceToCellData

INTERFACE
  MODULE SUBROUTINE obj_DisplayFacetToCellData(obj, msg, unitno)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayFacetToCellData
END INTERFACE

!----------------------------------------------------------------------------
!                                         InitiateNodeToNodeData@NodeMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-10
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
! - `obj%nodeToNode` will be Initiated
! - `domain1` main domain
! - `domain2` secondary domain

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodeToNodeData1(obj, domain1, domain2)
    CLASS(FEDomainConnectivity_), INTENT(INOUT) :: obj
    !! FEDomain connectivity object
    CLASS(AbstractDomain_), INTENT(INOUT) :: domain1
    !! Primary domain, in nodeToNode(i), i denotes the
    !! global node number in domain1 domain.
    CLASS(AbstractDomain_), INTENT(INOUT) :: domain2
    !! Secondary domain => nodeToNode(i) denotes the
    !! global node number in domain2 domain.
  END SUBROUTINE obj_InitiateNodeToNodeData1
END INTERFACE

!----------------------------------------------------------------------------
!                                         InitiateNodeToNodeData@NodeMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-06-09
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
! - `obj%nodeToNode` will be Initiated
! - `mesh1` main mesh
! - `mesh2` secondary mesh

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodeToNodeData2(obj, mesh1, mesh2)
    CLASS(FEDomainConnectivity_), INTENT(INOUT) :: obj
    !! FEDomain connectivity object
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh1
    !! Primary domain, in nodeToNode(i), i denotes the
    !! global node number in domain1 domain.
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh2
    !! Secondary domain => nodeToNode(i) denotes the
    !! global node number in domain2 domain.
  END SUBROUTINE obj_InitiateNodeToNodeData2
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
!  to [[FEDomainConnectivity_:nodeToNode]]
!

INTERFACE
  MODULE FUNCTION obj_GetNodeToNodePointer(obj) RESULT(Ans)
    CLASS(FEDomainConnectivity_), TARGET, INTENT(IN) :: obj
    INTEGER(I4B), POINTER :: ans(:)
  END FUNCTION obj_GetNodeToNodePointer
END INTERFACE

!----------------------------------------------------------------------------
!                                        InitiateCellToCellData@NodeMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-10
! summary: Generate cell to cell connectivity
!
!# Introduction
!
!This subroutine generates the cell to cell connectivity between
!two domains.
!
! - `obj%cellToCell` will be Initiated
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
!The size of [[FEDomainConnectivity_:cellToCell]] is the largest
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
  MODULE SUBROUTINE obj_InitiateCellToCellData1(obj, domain1, domain2)
    CLASS(FEDomainConnectivity_), INTENT(INOUT) :: obj
    !! FEDomain connectivity object
    CLASS(AbstractDomain_), INTENT(INOUT) :: domain1
    !! Primary domain, in CellToCell(i), i denotes the
    !! global element number in domain1 domain.
    CLASS(AbstractDomain_), INTENT(INOUT) :: domain2
    !! Secondary domain => CellToCell(i) denotes the
    !! global element number in domain2 domain.
  END SUBROUTINE obj_InitiateCellToCellData1
END INTERFACE

!----------------------------------------------------------------------------
!                                        InitiateCellToCellData@NodeMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-10
! summary: Generate cell to cell connectivity
!
!# Introduction
!
!This subroutine generates the cell to cell connectivity between
!two meshes.
!
! - `obj%cellToCell` will be Initiated
! - `mesh1` main mesh
! - `mesh2` secondary mesh
!
!@note
!All **CELL** elements in mesh-1 will be mapped to **CELL**
!elements in mesh-2.
!@endnote
!
!@note
!If cellToCell(iel) is equal to zero then it means there is
!no element found in mesh-2 corresponding to element number
!iel in mesh-1.
!@endnote
!
!@note
!The size of [[FEmeshConnectivity_:cellToCell]] is total
! number of elements in in mesh1.
!@endnote
!
!@note
!Following points should be noted before calling this routine
!
! - This routine provides map between cell elements of one mesh to
! cell elements of another mesh.
! - The topology of the both elements should be the same
! - There is one to one mapping between elements of mesh 1
! and elements of mesh2
! - This routine works well for two meshs of same region
! with same/different order. For example, mesh of tri3 and mesh
! of tri6 elements.
!@endnote

INTERFACE
  MODULE SUBROUTINE obj_InitiateCellToCellData2(obj, mesh1, mesh2)
    CLASS(FEDomainConnectivity_), INTENT(INOUT) :: obj
    !! FEDomain connectivity object
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh1
    !! Primary domain, in CellToCell(i), i denotes the
    !! global element number in domain1 domain.
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh2
    !! Secondary domain => CellToCell(i) denotes the
    !! global element number in domain2 domain.
  END SUBROUTINE obj_InitiateCellToCellData2
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
! This function returns the pointer to [[FEDomainConnectivity_:CellToCell]]

INTERFACE
  MODULE FUNCTION obj_GetCellToCellPointer(obj) RESULT(Ans)
    CLASS(FEDomainConnectivity_), TARGET, INTENT(IN) :: obj
    INTEGER(I4B), POINTER :: ans(:)
  END FUNCTION obj_GetCellToCellPointer
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
! This function returns the pointer to [[FEDomainConnectivity_:CellToCell]]

INTERFACE
  MODULE PURE FUNCTION obj_GetDimEntityNum(obj, globalElement) RESULT(Ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B) :: ans(2)
  END FUNCTION obj_GetDimEntityNum
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateFacetToCellData@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2022
! summary: This is a helper routine for obj_InitiateFacetToCellData1
!
!# Introduction
!
! - This routine Initiate `facetToCell` for given facetFEMesh and CellFEMesh
! - In this case facetFEMesh should be a boundary of cellFEMesh
! - This routine should not be used for internal boundary.

INTERFACE
  MODULE SUBROUTINE obj_InitiateFacetToCellData1(obj, facetFEMesh, &
                                         cellFEMesh, dim, entityNum, isMaster)
    CLASS(FEDomainConnectivity_), INTENT(INOUT) :: obj
    !! FEDomain connectivity data
    CLASS(AbstractMesh_), INTENT(INOUT) :: facetFEMesh
    !! FEMesh of facet elements
    CLASS(AbstractMesh_), INTENT(INOUT) :: cellFEMesh
    !! Master mesh
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: entityNum
    LOGICAL(LGT), INTENT(IN) :: isMaster
    !! if true then cell FEMesh is master cell
    !! if false then cell mesh is slave cell
  END SUBROUTINE obj_InitiateFacetToCellData1
END INTERFACE

!----------------------------------------------------------------------------
!                                      InitiateFacetToCellData@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Generate the connectivity matrix between cell and facet mesh.
!
!# Introduction
!
! This subroutine generates the faceToCell connectivity data between
! between masterFEDomain, slaveFEDomain and faceFEMesh.
!
! In this case facetFEMesh should a boundary of masterFEDomain and slaveFEDomain
! In otherwords, facetFEMesh cannot represent the internal boundary.
! This routine calls `obj_InitiateFacetToCellData1` routine.

INTERFACE
  MODULE SUBROUTINE obj_InitiateFacetToCellData2(obj, facetFEMesh, &
                                                masterFEDomain, slaveFEDomain)
    CLASS(FEDomainConnectivity_), INTENT(INOUT) :: obj
    !! FEMesh connectivity data
    CLASS(AbstractMesh_), INTENT(INOUT) :: facetFEMesh
    !! FEMesh of facet elements
    CLASS(AbstractDomain_), INTENT(INOUT) :: masterFEDomain
    !! FEDomain of master elements
    CLASS(AbstractDomain_), INTENT(INOUT) :: slaveFEDomain
    !! FEDomain of slave elements
  END SUBROUTINE obj_InitiateFacetToCellData2
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateFacetToCellData@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2022
! summary: This is a helper routine for obj_InitiateFacetToCellData1
!
!# Introduction
!
! - This routine Initiate `facetToCell` for given facetFEMesh and CellFEMesh
! - In this case facetFEMesh can be an internal boundary of cellFEMesh

INTERFACE
  MODULE SUBROUTINE obj_InitiateFacetToCellData3(obj, facetFEMesh, &
                                                 cellFEMesh, dim, entityNum)
    CLASS(FEDomainConnectivity_), INTENT(INOUT) :: obj
    !! FEDomain connectivity data
    CLASS(AbstractMesh_), INTENT(INOUT) :: facetFEMesh
    !! FEMesh of facet elements
    CLASS(AbstractMesh_), INTENT(INOUT) :: cellFEMesh
    !! Master mesh
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: entityNum
  END SUBROUTINE obj_InitiateFacetToCellData3
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateFacetToCellData@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 March 2022
! summary: This is a helper routine for obj_InitiateFacetToCellData1
!
!# Introduction
!
! - This routine Initiate `facetToCell` for given facetFEMesh and CellFEMesh
! - In this case facetFEMesh can be an internal boundary of cellFEMesh

INTERFACE
MODULE SUBROUTINE obj_InitiateFacetToCellData4(obj, facetFEMesh, cellFEDomain)
    CLASS(FEDomainConnectivity_), INTENT(INOUT) :: obj
    !! FEDomain connectivity data
    CLASS(AbstractMesh_), INTENT(INOUT) :: facetFEMesh
    !! FEMesh of facet elements
    CLASS(AbstractDomain_), INTENT(INOUT) :: cellFEDomain
    !! Master mesh
  END SUBROUTINE obj_InitiateFacetToCellData4
END INTERFACE

!----------------------------------------------------------------------------
!                                                    CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns master cell number of given facet number
!
!# Introduction
!
! - Returns master cell number of given facet number
! - If cell number is zero it means facet element does not have a master cell

INTERFACE
  MODULE PURE FUNCTION obj_MasterCellNumber1(obj, localElement) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement
    !! Facet element number
    INTEGER(I4B) :: ans
    !! Cell number
  END FUNCTION obj_MasterCellNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns master cell number of given facet number
!
!# Introduction
!
! - Returns master cell number of given facet number
! - if master cell number is zero it means facet element is an orphan

INTERFACE
  MODULE PURE FUNCTION obj_MasterCellNumber2(obj, localElement) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(SIZE(localElement))
    !! List of cell element numbers
  END FUNCTION obj_MasterCellNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns master cell number of given facet number

INTERFACE
  MODULE PURE FUNCTION obj_MasterCellNumber3(obj) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! List of cell element numbers
  END FUNCTION obj_MasterCellNumber3
END INTERFACE

!----------------------------------------------------------------------------
!                                                    CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns master cell number of given facet number (no allocation)

INTERFACE
  MODULE PURE SUBROUTINE obj_GetMasterCellNumber(obj, VALUE)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), INTENT(INOUT) :: VALUE(:)
    !! List of cell element numbers
  END SUBROUTINE obj_GetMasterCellNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                    CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns slave cell number of given facet number
!
!# Introduction
!
! - Returns slave cell number of given facet number
! - If slave cell number is zero it means facet element is an orphan

INTERFACE
  MODULE PURE FUNCTION obj_SlaveCellNumber1(obj, localElement) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement
    !! Facet element number
    INTEGER(I4B) :: ans
    !! Cell number
  END FUNCTION obj_SlaveCellNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns slave cell number of given facet number
!
!# Introduction
!
! - Returns slave cell number of given facet number
! - if slave cell number is zero it means facet element is an orphan

INTERFACE
  MODULE PURE FUNCTION obj_SlaveCellNumber2(obj, localElement) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(SIZE(localElement))
    !! List of cell element numbers
  END FUNCTION obj_SlaveCellNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns slave cell number of given facet number

INTERFACE
  MODULE PURE FUNCTION obj_SlaveCellNumber3(obj) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! List of cell element numbers
  END FUNCTION obj_SlaveCellNumber3
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetSlaveCellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns slave cell number of given facet number

INTERFACE
  MODULE PURE SUBROUTINE obj_GetSlaveCellNumber(obj, VALUE)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), INTENT(INOUT) :: VALUE(:)
    !! List of cell element numbers
  END SUBROUTINE obj_GetSlaveCellNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                          masterFacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the local facet id in master cell element
!
!# Introduction
!
! Returns the local facet id in master cell element which is in contact with
! facet element

INTERFACE
  MODULE PURE FUNCTION obj_MasterFacetLocalID1(obj, localElement) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity object
    INTEGER(I4B), INTENT(IN) :: localElement
    !! Facet element number
    INTEGER(I4B) :: ans
    !! Local facet ID
  END FUNCTION obj_MasterFacetLocalID1
END INTERFACE

!----------------------------------------------------------------------------
!                                           masterFacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
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
  MODULE PURE FUNCTION obj_MasterFacetLocalID2(obj, localElement) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(SIZE(localElement))
    !! List of local facet IDs
  END FUNCTION obj_MasterFacetLocalID2
END INTERFACE

!----------------------------------------------------------------------------
!                                           masterFacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the local facet id in master cell element

INTERFACE
  MODULE PURE FUNCTION obj_MasterFacetLocalID3(obj) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! List of local facet IDs
  END FUNCTION obj_MasterFacetLocalID3
END INTERFACE

!----------------------------------------------------------------------------
!                                        GetMasterFacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the local facet id in master cell element

INTERFACE
  MODULE PURE SUBROUTINE obj_GetMasterFacetLocalID(obj, VALUE)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), INTENT(INOUT) :: VALUE(:)
    !! List of local facet IDs
  END SUBROUTINE obj_GetMasterFacetLocalID
END INTERFACE

!----------------------------------------------------------------------------
!                                          slaveFacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the local facet id in slave cell element
!
!# Introduction
!
! Returns the local facet id in slave cell element which is in contact with
! facet element

INTERFACE
  MODULE PURE FUNCTION obj_SlaveFacetLocalID1(obj, localElement) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity object
    INTEGER(I4B), INTENT(IN) :: localElement
    !! Facet element number
    INTEGER(I4B) :: ans
    !! Local facet ID
  END FUNCTION obj_SlaveFacetLocalID1
END INTERFACE

!----------------------------------------------------------------------------
!                                           slaveFacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the local facet id in slave cell element

INTERFACE
  MODULE PURE FUNCTION obj_SlaveFacetLocalID2(obj, localElement) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(SIZE(localElement))
    !! List of local facet IDs
  END FUNCTION obj_SlaveFacetLocalID2
END INTERFACE

!----------------------------------------------------------------------------
!                                           slaveFacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the local facet id in slave cell element

INTERFACE
  MODULE PURE FUNCTION obj_SlaveFacetLocalID3(obj) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! List of local facet IDs
  END FUNCTION obj_SlaveFacetLocalID3
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetSlaveFacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the local facet id in slave cell element

INTERFACE
  MODULE PURE SUBROUTINE obj_GetSlaveFacetLocalID(obj, VALUE)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), INTENT(INOUT) :: VALUE(:)
    !! List of local facet IDs
  END SUBROUTINE obj_GetSlaveFacetLocalID
END INTERFACE

!----------------------------------------------------------------------------
!                                                  masterDimTag@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the (dimtag, entityNum) in master cell element

INTERFACE
  MODULE PURE FUNCTION obj_MasterDimTag1(obj, localElement) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity object
    INTEGER(I4B), INTENT(IN) :: localElement
    !! Facet element number
    INTEGER(I4B) :: ans(2)
    !! dim, entityNum
  END FUNCTION obj_MasterDimTag1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 masterDimTag@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the (dimtag, entityNum) in master cell element

INTERFACE
  MODULE PURE FUNCTION obj_MasterDimTag2(obj, localElement) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(2, SIZE(localElement))
    !! dim, entityNum
  END FUNCTION obj_MasterDimTag2
END INTERFACE

!----------------------------------------------------------------------------
!                                                  masterDimTag@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the (dimtag, entityNum) in master cell element

INTERFACE
  MODULE PURE FUNCTION obj_MasterDimTag3(obj, isTranspose) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    LOGICAL(LGT), INTENT(IN) :: isTranspose
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! dim, entityNum
  END FUNCTION obj_MasterDimTag3
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetMasterDimTag@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the (dimtag, entityNum) in master cell element

INTERFACE
  MODULE PURE SUBROUTINE obj_GetMasterDimTag(obj, isTranspose, VALUE)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    LOGICAL(LGT), INTENT(IN) :: isTranspose
    INTEGER(I4B), INTENT(INOUT) :: VALUE(:, :)
    !! dim, entityNum
  END SUBROUTINE obj_GetMasterDimTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                  slaveDimTag@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the (dimtag, entityNum) in slave cell element

INTERFACE
  MODULE PURE FUNCTION obj_SlaveDimTag1(obj, localElement) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity object
    INTEGER(I4B), INTENT(IN) :: localElement
    !! Facet element number
    INTEGER(I4B) :: ans(2)
    !! dim, entityNum
  END FUNCTION obj_SlaveDimTag1
END INTERFACE

!----------------------------------------------------------------------------
!                                           slaveDimTag@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the (dimtag, entityNum) in slave cell element

INTERFACE
  MODULE PURE FUNCTION obj_SlaveDimTag2(obj, localElement) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(2, SIZE(localElement))
    !! dim, entityNum
  END FUNCTION obj_SlaveDimTag2
END INTERFACE

!----------------------------------------------------------------------------
!                                           slaveDimTag@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the (dimtag, entityNum) in slave cell element

INTERFACE
  MODULE PURE FUNCTION obj_SlaveDimTag3(obj, isTranspose) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    LOGICAL(LGT), INTENT(IN) :: isTranspose
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! dim, entityNum
  END FUNCTION obj_SlaveDimTag3
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetSlaveDimTag@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the (dimtag, entityNum) in Slave cell element

INTERFACE
  MODULE PURE SUBROUTINE obj_GetSlaveDimTag(obj, isTranspose, VALUE)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    LOGICAL(LGT), INTENT(IN) :: isTranspose
    INTEGER(I4B), INTENT(INOUT) :: VALUE(:, :)
    !! dim, entityNum
  END SUBROUTINE obj_GetSlaveDimTag
END INTERFACE

!----------------------------------------------------------------------------
!                                                GlobalFacetID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the local global facet id

INTERFACE
  MODULE PURE FUNCTION obj_GlobalFacetID1(obj, localElement) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity object
    INTEGER(I4B), INTENT(IN) :: localElement
    !! Facet element number
    INTEGER(I4B) :: ans
  END FUNCTION obj_GlobalFacetID1
END INTERFACE

!----------------------------------------------------------------------------
!                                           GlobalFacetID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the global facet id

INTERFACE
  MODULE PURE FUNCTION obj_GlobalFacetID2(obj, localElement) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), INTENT(IN) :: localElement(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(SIZE(localElement))
  END FUNCTION obj_GlobalFacetID2
END INTERFACE

!----------------------------------------------------------------------------
!                                           GlobalFacetID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the global facet id

INTERFACE
  MODULE PURE FUNCTION obj_GlobalFacetID3(obj) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GlobalFacetID3
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetGlobalFacetID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Returns the global facet id

INTERFACE
  MODULE PURE SUBROUTINE obj_GetGlobalFacetID(obj, VALUE)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    !! FEMesh connectivity data
    INTEGER(I4B), INTENT(INOUT) :: VALUE(:)
  END SUBROUTINE obj_GetGlobalFacetID
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_GetTotalFacet(obj) RESULT(ans)
    CLASS(FEDomainConnectivity_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalFacet
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEDomainConnectivity_Class
