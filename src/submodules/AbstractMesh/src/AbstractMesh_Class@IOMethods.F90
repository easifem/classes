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

SUBMODULE(AbstractMesh_Class) IOMethods
USE GlobalData, ONLY: stdout
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE Display_Method, ONLY: EqualLine
USE Display_Method, ONLY: BlankLines
USE ReallocateUtility, ONLY: Reallocate
USE ArangeUtility, ONLY: Arange
USE InputUtility, ONLY: Input
USE ReferenceElement_Method, ONLY: ElementName
USE ElemData_Class, ONLY: INTERNAL_ELEMENT
USE ElemData_Class, ONLY: BOUNDARY_ELEMENT
USE ElemData_Class, ONLY: DOMAIN_BOUNDARY_ELEMENT
USE ElemData_Class, ONLY: ElemData_Display
USE FacetData_Class, ONLY: FacetData_Display
USE FacetData_Class, ONLY: FacetData_Display_Filter
USE NodeData_Class, ONLY: NodeData_Display
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractMesh_Class@IOMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

  LOGICAL(LGT) :: abool
  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL Display(msg, unitno=unitno)

  CALL Display(obj%isInitiated, "Mesh object initiated: ", &
               unitno=unitno)

  IF (.NOT. obj%isInitiated) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF

  CALL Display(obj%showTime, "showTime: ", unitno=unitno)

  CALL Display(obj%readFromFile, "readFromFile: ", unitno=unitno)

  CALL Display(obj%isNodeToElementsInitiated, "isNodeToElementsInitiated: ", &
               unitno=unitno)

  CALL Display(obj%isNodeToNodesInitiated, "isNodeToNodesInitiated: ", &
               unitno=unitno)

  CALL Display(obj%isExtraNodeToNodesInitiated, &
               "isExtraNodeToNodesInitiated: ", unitno=unitno)

  CALL Display(obj%isElementToElementsInitiated, &
               "isElementToElementsInitiated: ", unitno=unitno)

  CALL Display(obj%isEdgeConnectivityInitiated, &
               "isEdgeConnectivityInitiated: ", unitno=unitno)

  CALL Display(obj%isFaceConnectivityInitiated, &
               "isFaceConnectivityInitiated: ", unitno=unitno)

  CALL Display(obj%isBoundaryDataInitiated, &
               "isBoundaryDataInitiated: ", unitno=unitno)

  CALL Display(obj%isFacetDataInitiated, "isFacetDataInitiated: ", &
               unitno=unitno)

  CALL Display(obj%uid, "uid: ", unitno=unitno)

  CALL Display("Total elements (topology wise)", unitno=unitno)
  CALL EqualLine(unitno=unitno)
  CALL Display(obj%tElements_topology_wise(1), "point: ", &
               unitno=unitno)
  CALL Display(obj%tElements_topology_wise(2), "line: ", &
               unitno=unitno)
  CALL Display(obj%tElements_topology_wise(3), "triangle: ", &
               unitno=unitno)
  CALL Display(obj%tElements_topology_wise(4), "quadrangle: ", &
               unitno=unitno)
  CALL Display(obj%tElements_topology_wise(5), "tetrahedron: ", &
               unitno=unitno)
  CALL Display(obj%tElements_topology_wise(6), "hexahedron: ", &
               unitno=unitno)
  CALL Display(obj%tElements_topology_wise(7), "prism: ", &
               unitno=unitno)
  CALL Display(obj%tElements_topology_wise(8), "pyramid: ", &
               unitno=unitno)
  CALL BlankLines(unitno=unitno)

  CALL Display(obj%tElemTopologies, "Total topologies: ", &
               unitno=unitno)

  DO ii = 1, obj%tElemTopologies
    CALL Display("Topologies(" // ToString(ii) // "): " // &
                 ElementName(obj%elemTopologies(ii)), unitno=unitno)
  END DO

  CALL Display(obj%maxNNE, "maxNNE: ", unitno=unitno)

  CALL Display(obj%isMaxElemOrderSet, "isMaxElemOrderSet: ", &
               unitno=unitno)

  CALL Display(obj%maxElemOrder, "maxElemOrder: ", unitno=unitno)

  CALL Display(obj%nsd, "nsd: ", unitno=unitno)

  CALL Display(obj%xidim, "xidim: ", unitno=unitno)

  CALL Display(obj%maxNptrs, "maxNptrs: ", unitno=unitno)

  CALL Display(obj%minNptrs, "minNptrs: ", unitno=unitno)

  CALL Display(obj%maxElemNum, "maxElemNum: ", unitno=unitno)

  CALL Display(obj%minElemNum, "minElemNum: ", unitno=unitno)

  CALL Display(obj%tNodes, "tNodes: ", unitno=unitno)
  CALL Display(obj%tEdges, "tEdges: ", unitno=unitno)
  CALL Display(obj%tFaces, "tFaces: ", unitno=unitno)

  CALL Display(obj%tElements, "tElements: ", unitno=unitno)

  CALL Display(obj%minX, "minX: ", unitno=unitno)

  CALL Display(obj%maxX, "maxX: ", unitno=unitno)

  CALL Display(obj%minY, "minY: ", unitno=unitno)

  CALL Display(obj%maxY, "maxY: ", unitno=unitno)

  CALL Display(obj%minZ, "minZ: ", unitno=unitno)

  CALL Display(obj%maxZ, "maxZ: ", unitno=unitno)

  CALL Display(obj%X, "X: ", unitno=unitno)

  CALL Display(obj%Y, "Y: ", unitno=unitno)

  CALL Display(obj%Z, "Z: ", unitno=unitno)

  abool = ALLOCATED(obj%boundingEntity)
  CALL Display(abool, "boundingEntity ALLOCATED: ", unitno=unitno)
  IF (abool) THEN
    CALL Display(obj%boundingEntity, "boundingEntity: ", unitno=unitno)
  END IF

  abool = ALLOCATED(obj%local_elemNumber)
  CALL Display(abool, "local_elemNumber ALLOCATED: ", unitno=unitno)

  abool = ALLOCATED(obj%local_nptrs)
  CALL Display(abool, "local_nptrs ALLOCATED: ", unitno=unitno)

  abool = ALLOCATED(obj%quality)
  CALL Display(abool, "quality ALLOCATED: ", unitno=unitno)
  IF (abool) THEN
    CALL Display(SHAPE(obj%quality), "quality shape: ", unitno=unitno)
  END IF

  abool = ALLOCATED(obj%nodeData)
  CALL Display(abool, "nodeData ALLOCATED: ", unitno=unitno)
  IF (abool) THEN
    CALL Display(SIZE(obj%nodeData), "nodeData Size: ", unitno=unitno)
  END IF

  abool = ALLOCATED(obj%elementData)
  CALL Display(abool, "elementData ALLOCATED: ", unitno=unitno)
  IF (abool) THEN
    CALL Display(SIZE(obj%elementData), "elementData Size: ", unitno=unitno)
  END IF

  abool = ALLOCATED(obj%facetData)
  CALL Display(abool, "facetData ALLOCATED: ", unitno=unitno)
  IF (abool) THEN
    CALL Display(SIZE(obj%facetData), "facetData Size: ", unitno=unitno)
  END IF

  abool = ASSOCIATED(obj%kdtree)
  CALL Display(abool, "kdtree Associated: ", unitno=unitno)

  abool = ALLOCATED(obj%kdresult)
  CALL Display(abool, "kdresult Allocated: ", unitno=unitno)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                        DisplayElementData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayElementData
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_DisplayElementData()"
#endif

  INTEGER(I4B) :: ii, telements

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL Display(msg, unitno=unitno)

  IF (PRESENT(globalElement)) THEN
    ii = obj%GetLocalElemNumber(globalElement=globalElement, isLocal=isLocal)
    CALL elemData_Display(obj=obj%elementData(ii)%ptr, &
                          msg="elementData(" // ToString(ii) // "): ", &
                          unitno=unitno)
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF

  telements = obj%GetTotalElements()

  DO ii = 1, telements
    CALL elemData_Display(obj=obj%elementData(ii)%ptr, &
                          msg="elementData(" // ToString(ii) // "): ", &
                          unitno=unitno)
    CALL BlankLines(nol=1, unitno=unitno)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_DisplayElementData

!----------------------------------------------------------------------------
!                                                            DisplayNodeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayNodeData
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_DisplayNodeData()"
#endif

  INTEGER(I4B) :: ii, tNodes
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tNodes = obj%GetTotalNodes()
  CALL Display(msg, unitno=unitno)
  DO ii = 1, tNodes

    CALL nodeData_Display(obj%nodeData(ii)%ptr, &
                          msg="nodeData(" // ToString(ii) // "): ", &
                          unitno=unitno)

    CALL BlankLines(nol=1, unitno=unitno)

  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_DisplayNodeData

!----------------------------------------------------------------------------
!                                                  DisplayFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayFacetData
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_DisplayFacetData()"
#endif

  INTEGER(I4B) :: ii, n
  LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL Display(msg, unitno=unitno)

  abool = ALLOCATED(obj%facetData)
  IF (abool) THEN
    n = SIZE(obj%facetData)
  ELSE
    n = 0
  END IF

  CALL Display(abool, "facetData ALLOCATED: ", unitno=unitno)

  DO ii = 1, n

    CALL FacetData_Display(obj=obj%facetData(ii), &
                           msg="facetData(" // ToString(ii) // "): ", &
                           unitno=unitno)

    CALL BlankLines(nol=1, unitno=unitno)

  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_DisplayFacetData

!----------------------------------------------------------------------------
!                                                  DisplayInternalFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayInternalFacetData
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_DisplayInternalFacetData()"
#endif

  INTEGER(I4B) :: ii, n
  INTEGER(I4B), PARAMETER :: filter = INTERNAL_ELEMENT
  LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL Display(msg, unitno=unitno)

  abool = ALLOCATED(obj%facetData)
  IF (abool) THEN; n = SIZE(obj%facetData); ELSE; n = 0; END IF

  CALL Display(abool, "facetData ALLOCATED: ", unitno=unitno)

  DO ii = 1, n

    CALL FacetData_Display_Filter(obj=obj%facetData(ii), filter=filter, &
                                  msg="internalFacetData(" // &
                                  ToString(ii) // "): ", unitno=unitno)
    CALL BlankLines(nol=1, unitno=unitno)

  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_DisplayInternalFacetData

!----------------------------------------------------------------------------
!                                                   DisplayBoundaryFacetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayBoundaryFacetData
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_DisplayBoundaryFacetData()"
#endif

  INTEGER(I4B) :: ii, n
  LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL Display(msg, unitno=unitno)

  abool = ALLOCATED(obj%facetData)
  IF (abool) THEN; n = SIZE(obj%facetData); ELSE; n = 0; END IF

  CALL Display(abool, "facetData ALLOCATED: ", unitno=unitno)

  DO ii = 1, n

    CALL FacetData_Display_Filter(obj=obj%facetData(ii), &
                                  filter=BOUNDARY_ELEMENT, &
                                  msg="boundaryFacetData(" // &
                                  ToString(ii) // "): ", unitno=unitno)

    CALL FacetData_Display_Filter(obj=obj%facetData(ii), &
                                  filter=DOMAIN_BOUNDARY_ELEMENT, &
                                  msg="domainBoundaryFacetData(" // &
                                  ToString(ii) // "): ", unitno=unitno)

    CALL BlankLines(nol=1, unitno=unitno)

  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_DisplayBoundaryFacetData

!----------------------------------------------------------------------------
!                                                           DisplayMeshInfo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayMeshInfo
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_DisplayMeshInfo()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL Display(msg, unitno=unitno)
  CALL EqualLine(unitno=unitno)
  CALL Display(obj%GetTotalNodes(), "total nodes: ", unitno=unitno)
  CALL Display(obj%GetTotalElements(), "total elements: ", unitno=unitno)
  CALL Display(obj%GetTotalEdges(), "tEdges: ", unitno=unitno)
  CALL Display(obj%GetTotalFaces(), "tFaces: ", unitno=unitno)
  CALL EqualLine(unitno=unitno)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_DisplayMeshInfo

!----------------------------------------------------------------------------
!                                                            Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
