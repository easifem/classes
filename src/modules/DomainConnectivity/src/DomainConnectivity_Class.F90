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
USE GlobalData
USE Mesh_Class
USE Domain_Class
USE ExceptionHandler_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "DomainConnectivity_Class"
TYPE(ExceptionHandler_) :: e

!----------------------------------------------------------------------------
!                                                        FacetConnectivity_
!----------------------------------------------------------------------------

TYPE :: FacetConnectivity_
  INTEGER(I4B) :: facetID = 0
    !! global element number of facet element in facet cell
  INTEGER(I4B) :: masterGlobalCellNum = 0
    !! global element number of master cell
  INTEGER(I4B) :: masterLocalFacetID = 0
    !! Local facet of master cell which is connected to the facet mesh
    !! element
  INTEGER(I4B) :: slaveCellID = 0
    !! global element number of slave cell
  INTEGER(I4B) :: slaveLocalFacetID = 0
    !! Local facet of slave cell which is connected to the facet mesh element
END TYPE FacetConnectivity_

!----------------------------------------------------------------------------
!                                                        NodeConnectivity_
!----------------------------------------------------------------------------

TYPE :: NodeConnectivity_
  INTEGER(I4B) :: masterGlobalNodeNum = 0
  INTEGER(I4B) :: slaveGlobalNodeNum = 0
END TYPE NodeConnectivity_

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
  INTEGER(I4B), ALLOCATABLE :: cellToCellExtraData(:,:)
    !! Currently, cellToCellExtraData has two rows
    !! the first row is dim
    !! the second row is entityNum
    !! the column represents the element number
    !! example: iel1 in domain1
    !!          cellToCell(iel) gives iel2 in domain2
    !!          cellToCellExtraData(1, iel1) gives
    !!          dimension of mesh which contains iel2
    !!          cellToCellExtraData(2, iel1) gives
    !!          entityNum of mesh which contains iel2
    !!          In this way,
    !!          domain2%getMeshPointer(dim, entityNum)
    !!          can give us the pointer to the mesh
    !!          which contains the iel2
  TYPE(FacetConnectivity_), ALLOCATABLE :: facetToCell(:)
    !! Facet connectivity, Facet to cell data
  TYPE(ElementConnectivity_), ALLOCATABLE :: elemToElem(:)
    !! ElemToElem connectivity  data
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: addSurrogate => dc_addSurrogate
  !! Add surrogate to the module error handler
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => dc_Deallocate
  !! Deallocate data stored in the object
  FINAL :: dc_Final
  !! finalizer
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
  !!
  !! @CellMethods
  !!
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
  PROCEDURE, PRIVATE, PASS(obj) :: dc_initiateFacetToCellData1
  !! Initiate facet to cell connectivity
  !! [[DomainConnectivity_:facetToCell]]
  GENERIC, PUBLIC :: initiateFacetToCellData1 =>  &
    & dc_initiateFacetToCellData1
  !! Initiate facet to cell connectivity
  !! [[DomainConnectivity_:facetToCell]]
  PROCEDURE, PASS(obj) :: dc_cellNumber1
  !! Return the cell number of a given facet
  PROCEDURE, PASS(obj) :: dc_cellNumber2
  !! Return the cell numbers of given facet elements
  GENERIC, PUBLIC :: cellNumber =>  &
    & dc_cellNumber1, &
    & dc_cellNumber2
  !! Return the cell numbers of given facet elements
  PROCEDURE, PUBLIC, PASS(obj) :: dc_facetLocalID1
  !! Return the facet local id in cell element
  PROCEDURE, PUBLIC, PASS(obj) :: dc_facetLocalID2
  !! Return the facet local id in cell element
  GENERIC, PUBLIC :: facetLocalID =>  &
    & dc_facetLocalID1, &
    & dc_facetLocalID2
  !! Return the facet local id in cell element
END TYPE DomainConnectivity_

PUBLIC :: DomainConnectivity_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: DomainConnectivityPointer_
  CLASS(DomainConnectivity_), POINTER :: Ptr => NULL()
END TYPE DomainConnectivityPointer_

PUBLIC :: DomainConnectivityPointer_

!----------------------------------------------------------------------------
!                                            AddSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: This routine addes surrogate to module exception handler

INTERFACE
  MODULE SUBROUTINE dc_AddSurrogate(obj, userObj)
    CLASS(DomainConnectivity_), INTENT(INOUT) :: obj
    TYPE(ExceptionHandler_), INTENT(IN) :: userObj
  END SUBROUTINE dc_AddSurrogate
END INTERFACE

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
  MODULE PURE SUBROUTINE dc_Deallocate(obj)
    CLASS(DomainConnectivity_), INTENT(INOUT) :: obj
    !! Mesh connectivity object
  END SUBROUTINE dc_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE dc_Final(obj)
    TYPE(DomainConnectivity_), INTENT(INOUT) :: obj
  END SUBROUTINE dc_Final
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
    CLASS(Domain_), INTENT(IN) :: domain1
    !! Primary domain, in nodeToNode(i), i denotes the
    !! global node number in domain1 domain.
    CLASS(Domain_), INTENT(IN) :: domain2
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
    CLASS(Domain_), INTENT(IN) :: domain1
    !! Primary domain, in nodeToNode(i), i denotes the
    !! global node number in domain1 domain.
    CLASS(Domain_), INTENT(IN) :: domain2
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
    CLASS(Domain_), INTENT(IN) :: domain1
    !! Primary domain, in cellToCell(i), i denotes the
    !! global element number in domain1 domain.
    CLASS(Domain_), INTENT(IN) :: domain2
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
    CLASS(Domain_), INTENT(IN) :: domain1
    !! Primary domain, in CellToCell(i), i denotes the
    !! global element number in domain1 domain.
    CLASS(Domain_), INTENT(IN) :: domain2
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
!                                           getDimEntityNum@CellMethods
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
!                               InitiateFacetToCellData@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Generate the connectivity matrix between cell and facet mesh.
!
!# Introduction
!
! This subroutine generate the connectivity matrix called obj % CellFacet
! between cell and facet mesh.
!
!  - The output result will be an integer array with 2 rows
!  - First row contains the element number of `CellMesh`
!  - Second row contains the local facet number of cell element which
!  connects to the facet mesh element.
!  - Each column of `obj % CellFacet` corresponds to an Element of
!  `FacetMesh`; total number of columns are same as total number of elem
!  in the `FacetMesh`
!  - if an element of `FacetMesh` is orphan then its corresponding entry
!  is set to zero in `obj % CellFacet` matrix

INTERFACE
  MODULE SUBROUTINE dc_InitiateFacetToCellData1(obj, Facet, Cell)
    CLASS(DomainConnectivity_), INTENT(INOUT) :: obj
  !! Mesh connectivity data
    CLASS(Mesh_), INTENT(INOUT) :: Facet
  !! Mesh of cell elements
    CLASS(Mesh_), INTENT(INOUT) :: Cell
  !! Mesh of facet mesh
  END SUBROUTINE dc_InitiateFacetToCellData1
END INTERFACE

!----------------------------------------------------------------------------
!                                              CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns cell number of given facet number
!
!# Introduction
!
! - Returns cell number of given facet number
! - if cell number is zero it means facet element is an orphan

INTERFACE
  MODULE PURE FUNCTION dc_CellNumber1(obj, FacetNum) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), INTENT(IN) :: FacetNum
    !! Facet element number
    INTEGER(I4B) :: ans
    !! Cell number
  END FUNCTION dc_CellNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              CellNumber@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns cell number of given facet number
!
!# Introduction
!
! - Returns cell number of given facet number
! - if cell number is zero it means facet element is an orphan

INTERFACE
  MODULE PURE FUNCTION dc_CellNumber2(obj, FacetNum) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), INTENT(IN) :: FacetNum(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(SIZE(FacetNum))
    !! List of cell element numbers
  END FUNCTION dc_CellNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                            FacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the local facet id of cell element
!
!# Introduction
!
! Returns the local facet id of cell element which is in contact with
! facet element

INTERFACE
  MODULE PURE FUNCTION dc_FacetLocalID1(obj, FacetNum) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity object
    INTEGER(I4B), INTENT(IN) :: FacetNum
    !! Facet element number
    INTEGER(I4B) :: ans
    !! Local facet ID
  END FUNCTION dc_FacetLocalID1
END INTERFACE

!----------------------------------------------------------------------------
!                                            FacetLocalID@FacetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Returns the local facet id of cell element
!
!# Introduction
!
! Returns the local facet id of cell element which is in contact with
! facet element
!
!## Usage
!
!```fortran
!        id = obj % FacetLocalID( FacetNum )
!```

INTERFACE
  MODULE PURE FUNCTION dc_FacetLocalID2(obj, FacetNum) RESULT(ans)
    CLASS(DomainConnectivity_), INTENT(IN) :: obj
    !! Mesh connectivity data
    INTEGER(I4B), INTENT(IN) :: FacetNum(:)
    !! List of facet element numbers
    INTEGER(I4B) :: ans(SIZE(FacetNum))
    !! List of local facet IDs
  END FUNCTION dc_FacetLocalID2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DomainConnectivity_Class
