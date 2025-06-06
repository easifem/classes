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

MODULE Elemdata_Class
USE GlobalData, ONLY: I4B, DFP, LGT, INT8

USE Display_Method, ONLY: Display

USE ReferenceElement_Method, ONLY: PARAM_REFELEM_MAX_FACES, &
                                   RefElemGetGeoParam, &
                                   ElementName, &
                                   GetFaceElemType, &
                                   GetEdgeConnectivity, &
                                   PARAM_REFELEM_MAX_EDGES, &
                                   ElementTopology, &
                                   GetElementIndex, &
                                   ElementOrder, ReferenceElementInfo

USE AbstractMeshParam, ONLY: PARAM_MAX_NNE

USE InterpolationUtility, ONLY: GetTotalInDOF

USE ReferenceQuadrangle_Method, ONLY: HelpFaceData_Quadrangle, &
                                      FaceShapeMetaData_Quadrangle

USE SortUtility, ONLY: Sort, QuickSort

USE ReallocateUtility, ONLY: Reallocate

USE ExceptionHandler_Class, ONLY: e

USE BaseType, ONLY: elemopt => TypeElemNameOpt

USE IntegerUtility, ONLY: OPERATOR(.IN.)

IMPLICIT NONE

PRIVATE

PUBLIC :: Elemdata_
PUBLIC :: ElemdataPointer_
PUBLIC :: Display
PUBLIC :: TypeElem
PUBLIC :: Elemdata_Set
PUBLIC :: Elemdata_Pointer
PUBLIC :: Elemdata_Deallocate
PUBLIC :: Elemdata_Display
PUBLIC :: Elemdata_lt
PUBLIC :: Elemdata_eq
PUBLIC :: Elemdata_SetID
PUBLIC :: Elemdata_Copy
PUBLIC :: Elemdata_GetGlobalFaceCon
PUBLIC :: Elemdata_SetTotalMaterial
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: Elemdata_GetConnectivity
PUBLIC :: Elemdata_GetConnectivity2
PUBLIC :: Elemdata_GetTotalEntities
PUBLIC :: Elemdata_GetVertex
PUBLIC :: Elemdata_GetEdge
PUBLIC :: Elemdata_GetFace
PUBLIC :: Elemdata_GetCell
PUBLIC :: Elemdata_GetTotalEdgeDOF
PUBLIC :: Elemdata_GetTotalFaceDOF
PUBLIC :: Elemdata_GetTotalCellDOF
PUBLIC :: Elemdata_GetElementToElements
PUBLIC :: Elemdata_GetEdgeConnectivity
PUBLIC :: Elemdata_isActive
PUBLIC :: Elemdata_globalElemNum
PUBLIC :: Elemdata_localElemNum
PUBLIC :: Elemdata_elementType
PUBLIC :: Elemdata_name
PUBLIC :: Elemdata_topoName
PUBLIC :: Elemdata_topoIndx
PUBLIC :: Elemdata_meshID
PUBLIC :: Elemdata_GetTotalMaterial
PUBLIC :: Elemdata_GetTotalGlobalNodes
PUBLIC :: Elemdata_GetTotalGlobalVertexNodes
PUBLIC :: Elemdata_GetTotalEdgeOrient
PUBLIC :: Elemdata_GetTotalGlobalFaces
PUBLIC :: Elemdata_GetTotalFaceOrient
PUBLIC :: Elemdata_GetTotalGlobalElements
PUBLIC :: Elemdata_GetTotalBoundaryData
PUBLIC :: Elemdata_GetMaterial
PUBLIC :: Elemdata_GetGlobalNodes
PUBLIC :: Elemdata_GetGlobalEdges
PUBLIC :: Elemdata_GetEdgeOrient
PUBLIC :: Elemdata_GetGlobalFaces
PUBLIC :: Elemdata_GetFaceOrient
PUBLIC :: Elemdata_GetGlobalElements
PUBLIC :: Elemdata_GetBoundaryData
PUBLIC :: Elemdata_GetGlobalNodesPointer
PUBLIC :: Elemdata_GetOrientation
PUBLIC :: Elemdata_IsBoundaryElement
PUBLIC :: Elemdata_FindFace
PUBLIC :: Elemdata_FindEdge
PUBLIC :: Elemdata_GetGlobalFaceNumber
PUBLIC :: Elemdata_GetGlobalEdgeNumber
PUBLIC :: Elemdata_Order

INTEGER(I4B), PARAMETER, PUBLIC :: INTERNAL_ELEMENT = 1
INTEGER(I4B), PARAMETER, PUBLIC :: BOUNDARY_ELEMENT = -1
INTEGER(I4B), PARAMETER, PUBLIC :: DOMAIN_BOUNDARY_ELEMENT = -2
INTEGER(I4B), PARAMETER, PUBLIC :: GHOST_ELEMENT = -4

! INTEGER(I4B), PARAMETER :: MAX_NUM_OVERLAPPED_CONTINNUM = 4

CHARACTER(*), PARAMETER :: modName = "Elemdata_Class"

INTERFACE Display
  MODULE PROCEDURE Elemdata_Display
END INTERFACE Display

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE Elemdata_Copy
END INTERFACE

INTERFACE Elemdata_GetElementToElements
  MODULE PROCEDURE Elemdata_GetElementToElements1, &
    Elemdata_GetElementToElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Elemdata_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-26
! summary: Data type for storing element data

TYPE :: Elemdata_
  LOGICAL(LGT) :: isActive = .TRUE.
  !! Is element in active stage
  INTEGER(I4B) :: globalElemNum = 0_I4B
  !! global element number
  !! cell connectivity number
  INTEGER(I4B) :: localElemNum = 0_I4B
  !! local element number
  INTEGER(I4B) :: elementType = INTERNAL_ELEMENT
  !! BOUNDARY_ELEMENT: If the element contqains the boundary node
  !! it will be called the boundary element
  !! INTERNAL_ELEMENT: If the element does not contain the boundary node
  !! then it will be called the internal element
  !! TODO: Change elementType to Int8
  INTEGER(I4B) :: name = 0
  !! This is name of the element
  !! It can be Triangle, Triangle3, Triangle6, etc.
  !! Quadrangle, Quadrangle4, Quadrangle8, etc.
  INTEGER(I4B) :: topoName = 0
  !! topology name of the element
  !! Point, Line, Triangle, Quadrangle, Hexahedron, Tetrahedron
  !! Prism, Pyramid
  INTEGER(I4B) :: meshID = 0
  !! ID of mesh to which the element belong
  !! This is a gmsh concept
  !! TODO: Change elementType to Int8
  INTEGER(INT8), ALLOCATABLE :: material(:)
  !! materials mapped to the mesh
  !! material(1) is the material-id (type of material) of medium 1
  !! material(2) is the material-id (type of material) of medium 2
  !!
  !! ...
  !!
  !! For example, soil is a porous medium with n = 1,
  !! fluid is a medium with n =2
  !! then material(1) denotes the type of soil => clay, sand, silt
  !! and material(2) denotes the type of fluid => water, oil, air
  !! TODO: Change material to Int8
  INTEGER(I4B), ALLOCATABLE :: globalNodes(:)
  !! nodes contained in the element, connectivity
  !! Vertex connectivity
  INTEGER(I4B), ALLOCATABLE :: globalEdges(:)
  !! Edge connectivity
  !! Edge is defined for 3D elements only
  INTEGER(INT8), ALLOCATABLE :: edgeOrient(:)
  !! Orientation of edge
  INTEGER(I4B), ALLOCATABLE :: globalFaces(:)
  !! Face connectivity
  INTEGER(INT8), ALLOCATABLE :: faceOrient(:, :)
  !! Orientation of face
  INTEGER(I4B), ALLOCATABLE :: globalElements(:)
  !! Contains the information about the element surrounding an element
  !! Lets us say that `globalElem1`, `globalElem2`, `globalElem3`
  !! surrounds a local element ielem (its global element number is
  !! globalElem), then
  !! - globalElements( [1,2,3] ) contains globalElem1, pFace, nFace
  !! - globalElements( [4,5,6] ) contains globalElem2, pFace, nFace
  !! - globalElements( [7,8,9] ) contains globalElem3, pFace, nFace.
  !! Here,
  !! - pFace is the local facet number of parent element
  !! globalElem (ielem) which is connected to the nFace of the neighbor
  !! element
  !! All element numbers are global element number
  INTEGER(I4B), ALLOCATABLE :: boundaryData(:)
  !! If `iel` is boundary element, then boundaryData contains
  !! the local facet number of iel which concides with the
  !! mesh boundary.
  !! If an element contains the boundary node then it is considered
  !! as a boundary element.
  !! It may happen that a boundary element has no boundary face, in which
  !! case boundaryData will have zero size
END TYPE Elemdata_

!----------------------------------------------------------------------------
!                                                           ElemdataPointer_
!----------------------------------------------------------------------------

TYPE ElemdataPointer_
  CLASS(Elemdata_), POINTER :: ptr => NULL()
END TYPE ElemdataPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-07
! summary:  List of element data type

TYPE ElemdataType_
  INTEGER(I4B) :: internal = INTERNAL_ELEMENT
  INTEGER(I4B) :: boundary = BOUNDARY_ELEMENT
  INTEGER(I4B) :: domainBoundary = DOMAIN_BOUNDARY_ELEMENT
  INTEGER(I4B) :: ghost = GHOST_ELEMENT
END TYPE ElemdataType_

TYPE(ElemdataType_), PARAMETER :: TypeElem = ElemdataType_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2024-01-31
! summary:  Copy

SUBROUTINE Elemdata_Copy(obj1, obj2)
  TYPE(Elemdata_), INTENT(INOUT) :: obj1
  TYPE(Elemdata_), INTENT(IN) :: obj2

  obj1%isActive = obj2%isActive
  obj1%globalElemNum = obj2%globalElemNum
  obj1%localElemNum = obj2%localElemNum
  obj1%elementType = obj2%elementType
  obj1%name = obj2%name
  obj1%topoName = obj2%topoName
  obj1%meshID = obj2%meshID

  IF (ALLOCATED(obj2%material)) obj1%material = obj2%material
  IF (ALLOCATED(obj2%globalNodes)) obj1%globalNodes = obj2%globalNodes
  IF (ALLOCATED(obj2%globalEdges)) obj1%globalEdges = obj2%globalEdges
  IF (ALLOCATED(obj2%edgeOrient)) obj1%edgeOrient = obj2%edgeOrient
  IF (ALLOCATED(obj2%globalFaces)) obj1%globalFaces = obj2%globalFaces
  IF (ALLOCATED(obj2%faceOrient)) obj1%faceOrient = obj2%faceOrient
  IF (ALLOCATED(obj2%globalElements)) obj1%globalElements  &
    & = obj2%globalElements
  IF (ALLOCATED(obj2%boundaryData)) obj1%boundaryData&
    & = obj2%boundaryData

END SUBROUTINE Elemdata_Copy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-26
! summary: Display a single instance of element data

SUBROUTINE Elemdata_Display(obj, msg, unitno)
  TYPE(Elemdata_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  LOGICAL(LGT) :: abool

  CALL Display(TRIM(msg), unitno=unitno)
  CALL Display(obj%isActive, msg="isActive: ", unitno=unitno)
  CALL Display(obj%globalElemNum, msg="globalElemNum: ", unitno=unitno)
  CALL Display(obj%localElemNum, msg="localElemNum: ", unitno=unitno)
  CALL Display(Elemdata_ElemType2String(obj%elementType), "elementType: ",  &
    & unitno=unitno)
  CALL Display(ElementName(obj%name), "elementName: ", unitno=unitno)
  CALL Display(ElementName(obj%topoName), "topology: ", unitno=unitno)

  ! display material if it is allocated
  abool = ALLOCATED(obj%material)
  CALL Display(abool, "material ALLOCATED: ", unitno=unitno)
  IF (abool) THEN
    CALL Display(obj%material, msg="material: ", unitno=unitno)
  END IF

  ! display material if it is allocated
  IF (ALLOCATED(obj%material)) THEN
    CALL Display(obj%material, msg="material: ", unitno=unitno)
  END IF

  ! globalNodes
  abool = ALLOCATED(obj%globalNodes)
  CALL Display(abool, "globalNodes ALLOCATED: ", unitno=unitno)
  IF (abool) THEN
    CALL Display(obj%globalNodes, msg="globalNodes: ", unitno=unitno)
  END IF

  ! globalEdges
  abool = ALLOCATED(obj%globalEdges)
  CALL Display(abool, "globalEdges ALLOCATED: ", unitno=unitno)
  IF (abool) THEN
    CALL Display(obj%globalEdges, msg="globalEdges: ", unitno=unitno)
  END IF

  abool = ALLOCATED(obj%edgeOrient)
  CALL Display(abool, "edgeOrient ALLOCATED: ", unitno=unitno)
  IF (abool) THEN
    CALL Display(obj%edgeOrient, msg="edgeOrient: ", unitno=unitno)
  END IF

  ! globalFaces
  abool = ALLOCATED(obj%globalFaces)
  CALL Display(abool, "globalFaces ALLOCATED: ", unitno=unitno)
  IF (abool) THEN
    CALL Display(obj%globalFaces, msg="globalFaces: ", unitno=unitno)
  END IF

  abool = ALLOCATED(obj%faceOrient)
  CALL Display(abool, "faceOrient ALLOCATED: ", unitno=unitno)
  IF (abool) THEN
    CALL Display(obj%faceOrient, msg="faceOrient: ", unitno=unitno)
  END IF

  ! globalElements
  abool = ALLOCATED(obj%globalElements)
  CALL Display(abool, "globalElements ALLOCATED: ", unitno=unitno)
  IF (abool) THEN
    CALL Display(obj%globalElements, msg="globalElements: ", &
    & unitno=unitno, full=.TRUE.)
  END IF

  ! boundaryData
  abool = ALLOCATED(obj%boundaryData)
  CALL Display(abool, "boundaryData ALLOCATED: ", unitno=unitno)
  IF (abool) THEN
    CALL Display(obj%boundaryData, msg="boundaryData: ", unitno=unitno)
  END IF
END SUBROUTINE Elemdata_Display

!----------------------------------------------------------------------------
!                                                   Elemdata_ElemType2String
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-29
! summary:  convert elementType to name

FUNCTION Elemdata_ElemType2String(elementType) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: elementType
  CHARACTER(:), ALLOCATABLE :: ans

  SELECT CASE (elementType)
  CASE (INTERNAL_ELEMENT)
    ans = "INTERNAL_ELEMENT"
  CASE (BOUNDARY_ELEMENT)
    ans = "BOUNDARY_ELEMENT"
  CASE (DOMAIN_BOUNDARY_ELEMENT)
    ans = "DOMAIN_BOUNDARY_ELEMENT"
  CASE (GHOST_ELEMENT)
    ans = "GHOST_ELEMENT"
  CASE DEFAULT
    ans = "NONE"
  END SELECT
END FUNCTION Elemdata_ElemType2String

!----------------------------------------------------------------------------
!                                                         ElemdataDeallocate
!----------------------------------------------------------------------------

SUBROUTINE Elemdata_Deallocate(obj)
  TYPE(Elemdata_), INTENT(INOUT) :: obj
  obj%isActive = .TRUE.
  obj%globalElemNum = 0
  obj%localElemNum = 0
  obj%elementType = INTERNAL_ELEMENT
  obj%name = 0
  obj%topoName = 0
  obj%meshID = 0

  CALL Reallocate(obj%material, 0)
  CALL Reallocate(obj%globalNodes, 0)
  CALL Reallocate(obj%globalEdges, 0)
  CALL Reallocate(obj%edgeOrient, 0)
  CALL Reallocate(obj%globalFaces, 0)
  CALL Reallocate(obj%faceOrient, 0, 0)
  CALL Reallocate(obj%globalElements, 0)
  CALL Reallocate(obj%boundaryData, 0)

END SUBROUTINE Elemdata_Deallocate

!----------------------------------------------------------------------------
!                                                         SetTotalMaterial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-12
! summary:  Set total number of materials
!
! this subroutine allocates materials in obj

PURE SUBROUTINE Elemdata_SetTotalMaterial(obj, n)
  TYPE(Elemdata_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: n

  ! internal variables
  INTEGER(INT8), ALLOCATABLE :: temp_material(:)
  INTEGER(I4B) :: n0

  IF (ALLOCATED(obj%material)) THEN
    n0 = SIZE(obj%material)
    CALL Reallocate(temp_material, n0 + n)
    temp_material(1:n0) = obj%material(1:n0)
    CALL MOVE_ALLOC(from=temp_material, to=obj%material)

  ELSE
    CALL Reallocate(obj%material, n)
  END IF

END SUBROUTINE Elemdata_SetTotalMaterial

!----------------------------------------------------------------------------
!                                                           ElemdataInitiate
!----------------------------------------------------------------------------

PURE SUBROUTINE Elemdata_Set(obj, globalElemNum, localElemNum, &
        elementType, globalNodes, globalElements, boundaryData, globalEdges, &
   globalFaces, name, topoName, isActive, meshID, medium, material, materials)
  ! obj%elementData(ii)%globalElemNum = elemNumber(ii)
  ! obj%elementData(ii)%localElemNum = ii
  ! obj%elementData(ii)%globalNodes = connectivity(:, ii)
  TYPE(Elemdata_), INTENT(INOUT) :: obj
  !! element data object
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalElemNum
  !! global element number
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: localElemNum
  !! local element number
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: elementType
  !! element type: internal element, boundary element, etc.
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNodes(:)
  !! vertex connectivity
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalElements(:)
  !! element to element mapping
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: boundaryData(:)
  !! boundary data
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalEdges(:)
  !! edge connectivity
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalFaces(:)
  !! gace connectivity
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: name
  !! Type of element, triangle, triangle3, Quadrangle4, etc
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: topoName
  !! topology  name of the element
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isActive
  !! is element active
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshID
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: medium
  !! medium id like soil,water, etc
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: material
  !! material like soil1, soil2, wate1, water2, etc
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: materials(:)
  !! materials

  IF (PRESENT(globalElemNum)) obj%globalElemNum = globalElemNum
  IF (PRESENT(localElemNum)) obj%localElemNum = localElemNum
  IF (PRESENT(elementType)) obj%elementType = elementType
  IF (PRESENT(globalNodes)) obj%globalNodes = globalNodes
  IF (PRESENT(globalElements)) obj%globalElements = globalElements
  IF (PRESENT(boundaryData)) obj%boundaryData = boundaryData
  IF (PRESENT(globalEdges)) obj%globalEdges = globalEdges
  IF (PRESENT(globalFaces)) obj%globalFaces = globalFaces
  IF (PRESENT(name)) THEN
    obj%name = name
    obj%topoName = ElementTopology(name)
  END IF
  IF (PRESENT(topoName)) obj%topoName = topoName
  IF (PRESENT(isActive)) obj%isActive = isActive
  IF (PRESENT(meshID)) obj%meshID = meshID

  ! set obj%material(medium) to material if present
  IF (PRESENT(medium) .AND. PRESENT(material)) THEN
    obj%material(medium) = INT(material, kind=INT8)
  END IF

  ! set materials to obj%material if materials is present
  IF (PRESENT(materials)) THEN
    obj%material = INT(materials, kind=INT8)
  END IF

END SUBROUTINE Elemdata_Set

!----------------------------------------------------------------------------
!                                                          Elemdata_Pointer
!----------------------------------------------------------------------------

FUNCTION Elemdata_Pointer() RESULT(ans)
  CLASS(Elemdata_), POINTER :: ans
  ALLOCATE (Elemdata_ :: ans)
END FUNCTION Elemdata_Pointer

!----------------------------------------------------------------------------
!                                                               Elemdata_lt
!----------------------------------------------------------------------------

FUNCTION Elemdata_lt(obj, obj2) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  TYPE(Elemdata_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%globalElemNum .GT. obj2%globalElemNum
END FUNCTION Elemdata_lt

!----------------------------------------------------------------------------
!                                                               Elemdata_eq
!----------------------------------------------------------------------------

FUNCTION Elemdata_eq(obj, obj2) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  TYPE(Elemdata_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%globalElemNum .EQ. obj2%globalElemNum
END FUNCTION Elemdata_eq

!----------------------------------------------------------------------------
!                                                         Elemdata_SetID
!----------------------------------------------------------------------------

SUBROUTINE Elemdata_SetID(obj, id)
  TYPE(Elemdata_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: id
  obj%localElemNum = id
END SUBROUTINE Elemdata_SetID

!----------------------------------------------------------------------------
!                                                 Elemdata_GetGlobalFaceCon
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-12
! summary:  Returns the vertex connectivity of global face of elements

SUBROUTINE Elemdata_GetGlobalFaceCon(obj, globalFaceCon, localFaceCon)
  TYPE(Elemdata_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(INOUT) :: globalFaceCon(:, :)
  INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: localFaceCon(:, :)

  INTEGER(I4B) :: tFaces, &
                  tNodes, localFaces0(4_I4B, PARAM_REFELEM_MAX_FACES), &
                  faceElemType(PARAM_REFELEM_MAX_FACES), &
                  tFaceNodes(PARAM_REFELEM_MAX_FACES), &
                  iface, face_temp(4), aint

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Elemdata_GetGlobalFaceCon()"
#endif

  CALL RefElemGetGeoParam(elemType=obj%name, &
                          tFaces=tFaces, tNodes=tNodes, faceCon=localFaces0, &
                          faceOpt=1_I4B, faceElemType=faceElemType, &
                          tFaceNodes=tFaceNodes)

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')

  DO iface = 1, tFaces
    aint = tFaceNodes(iface)
    face_temp(1:aint) = obj%globalNodes(localFaces0(1:aint, iface))

    CALL FaceShapeMetaData_Quadrangle(face=face_temp(1:aint), &
                                   sorted_face=globalFaceCon(1:aint, iface), &
                                      localFaces=localFaceCon(1:aint, iface))
  END DO

END SUBROUTINE Elemdata_GetGlobalFaceCon

!----------------------------------------------------------------------------
!                                                   Elemdata_GetConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2025-05-10
! summary:  Returns the connectvity of the element
!
!# Introduction
!
! This subroutine returns the connectivity of the element.
! - tsize is the size of data written in con
! - con is the connectivity array, it should be allocated
! - opt is the type of connectivity, following options are allowed
!  - "V" or "v" : vertex connectivity
!  - "E" or "e" : edge connectivity
!  - "F" or "f" : face connectivity
!  - "C" or "c" : cell connectivity
!  - "A" or "a" : all connectivity

SUBROUTINE Elemdata_GetConnectivity(obj, con, tsize, opt)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(INOUT) :: con(:)
  !! connectivity array, it should be allocated
  INTEGER(I4B), INTENT(OUT) :: tsize
  !! size of data written in con
  CHARACTER(*), INTENT(IN), OPTIONAL :: opt
  !! - opt is the type of connectivity, following options are allowed
  !!  - "V" or "v" : vertex connectivity
  !!  - "E" or "e" : edge connectivity
  !!  - "F" or "f" : face connectivity
  !!  - "C" or "c" : cell connectivity
  !!  - "A" or "a" : all connectivity

  !! internal variable
  CHARACTER(1) :: opt0
  INTEGER(I4B) :: aint, ii, jj

  opt0 = 'V'
  IF (PRESENT(opt)) opt0(1:1) = opt(1:1)

  SELECT CASE (opt0)
  CASE ("V", "v")
    tsize = SIZE(obj%globalNodes)
    DO ii = 1, tsize; con(ii) = obj%globalNodes(ii); END DO

  CASE ("E", "e")
    tsize = SIZE(obj%globalEdges)
    DO ii = 1, tsize; con(ii) = obj%globalEdges(ii); END DO

  CASE ("F", "f")
    tsize = SIZE(obj%globalFaces)
    DO ii = 1, tsize; con(ii) = obj%globalFaces(ii); END DO

  CASE ("C", "c")
    tsize = 1
    con(1) = obj%globalElemNum

  CASE ("A", "a")
    aint = 1
    tsize = SIZE(obj%globalNodes)
    jj = 0
    DO ii = aint, tsize
      jj = jj + 1
      con(ii) = obj%globalNodes(jj)
    END DO

    aint = tsize + 1
    tsize = tsize + SIZE(obj%globalEdges)
    jj = 0
    DO ii = aint, tsize
      jj = jj + 1
      con(ii) = obj%globalEdges(jj)
    END DO

    aint = tsize + 1
    tsize = tsize + SIZE(obj%globalFaces)
    jj = 0
    DO ii = aint, tsize
      jj = jj + 1
      con(ii) = obj%globalFaces(jj)
    END DO

    aint = tsize + 1
    tsize = tsize + 1
    con(aint) = obj%globalElemNum

  END SELECT

END SUBROUTINE Elemdata_GetConnectivity

!----------------------------------------------------------------------------
!                                               Elemdata_GetConnectivity2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Returns the connectvity of the element

SUBROUTINE Elemdata_GetConnectivity2(obj, cellCon, faceCon, edgeCon, nodeCon, &
                                     tCellCon, tFaceCon, tEdgeCon, tNodeCon)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(INOUT) :: cellCon(:)
  !! cell connectivity
  INTEGER(I4B), INTENT(INOUT) :: faceCon(:)
  !! face connectivity
  INTEGER(I4B), INTENT(INOUT) :: edgeCon(:)
  !! edge connectivity
  INTEGER(I4B), INTENT(INOUT) :: nodeCon(:)
  !! node connectivity
  INTEGER(I4B), INTENT(OUT) :: tCellCon
  !! total data written in cellCon
  INTEGER(I4B), INTENT(OUT) :: tFaceCon
  !! total data written in faceCon
  INTEGER(I4B), INTENT(OUT) :: tEdgeCon
  !! total data written in edgeCon
  INTEGER(I4B), INTENT(OUT) :: tNodeCon
  !! total data written in nodeCon

  !! internal variable
  INTEGER(I4B) :: ii

  tNodeCon = SIZE(obj%globalNodes)
  DO ii = 1, tNodeCon
    nodeCon(ii) = obj%globalNodes(ii)
  END DO

  tEdgeCon = SIZE(obj%globalEdges)
  DO ii = 1, tEdgeCon
    edgeCon(ii) = obj%globalEdges(ii)
  END DO

  tFaceCon = SIZE(obj%globalFaces)
  DO ii = 1, tFaceCon
    faceCon(ii) = obj%globalFaces(ii)
  END DO

  tCellCon = 1
  cellCon(1) = obj%globalElemNum

END SUBROUTINE Elemdata_GetConnectivity2

!----------------------------------------------------------------------------
!                                                 Elemdata_GetTotalEntities
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-15
! summary: Returns total number of vertex, edge, faces, and cells in element

FUNCTION Elemdata_GetTotalEntities(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(in) :: obj
  INTEGER(I4B) :: ans(4)
  ans(1) = SIZE(obj%globalNodes)
  ans(2) = SIZE(obj%globalEdges)
  ans(3) = SIZE(obj%globalFaces)
  ans(4) = 1
END FUNCTION Elemdata_GetTotalEntities

!----------------------------------------------------------------------------
!                                                     Elemdata_GetEdge
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get the global vertex number of a local vertex number

FUNCTION Elemdata_GetVertex(obj, ii) RESULT(ans)
  TYPE(Elemdata_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(in) :: ii
  !! local vertex number
  INTEGER(I4B) :: ans
  !! global vertex number

  ans = obj%globalNodes(ii)
END FUNCTION Elemdata_GetVertex

!----------------------------------------------------------------------------
!                                                     Elemdata_GetEdge
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get the global edge number of a local edge number

FUNCTION Elemdata_GetEdge(obj, ii) RESULT(ans)
  TYPE(Elemdata_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(in) :: ii
  !! local edge number
  INTEGER(I4B) :: ans
  !! global edge number
  ans = obj%globalEdges(ii)
END FUNCTION Elemdata_GetEdge

!----------------------------------------------------------------------------
!                                                     Elemdata_GetFace
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get the global face number of a local face number

FUNCTION Elemdata_GetFace(obj, ii) RESULT(ans)
  TYPE(Elemdata_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(in) :: ii
  !! local face number
  INTEGER(I4B) :: ans
  !! global face number
  ans = obj%globalFaces(ii)
END FUNCTION Elemdata_GetFace

!----------------------------------------------------------------------------
!                                                     Elemdata_GetCell
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get the global element number (cell number) of a local element number

FUNCTION Elemdata_GetCell(obj, islocal) RESULT(ans)
  TYPE(Elemdata_), INTENT(in) :: obj
  LOGICAL(LGT), INTENT(in) :: islocal
  !! If true then local cell number is returned
  INTEGER(I4B) :: ans
  !! global or local element number (cell number)

  IF (islocal) THEN
    ans = obj%localElemNum
  ELSE
    ans = obj%globalElemNum
  END IF
END FUNCTION Elemdata_GetCell

!----------------------------------------------------------------------------
!                                                   Elemdata_GetTotalEdgeDOF
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get the total number of edge dof on the edge of an element
!
!# Introduction
!
! All dofs are internal to the edge, that is, vertex dof are not included

FUNCTION Elemdata_GetTotalEdgeDOF(obj, ii, order, baseContinuity, &
                                  baseInterpolation) RESULT(ans)
  TYPE(Elemdata_), INTENT(in) :: obj
  !! Element data object
  INTEGER(I4B), INTENT(IN) :: ii
  !! Local edge number
  INTEGER(I4B), INTENT(IN) :: order
  !! Order on the edge
  CHARACTER(*), INTENT(IN) :: baseContinuity
  !! base continuity: H1, HDiv, HCurl
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  !! base interpolation type
  INTEGER(I4B) :: ans
  !! Total number of dof on edge
  ans = MAX(order - 2, 0_I4B)
END FUNCTION Elemdata_GetTotalEdgeDOF

!----------------------------------------------------------------------------
!                                                   Elemdata_GetTotalFaceDOF
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get the total number of face dof on the face of an element
!
!# Introduction
!
! All dofs are internal to face, that is edge and vertex dof are not
! included

FUNCTION Elemdata_GetTotalFaceDOF(obj, ii, order, baseContinuity, &
                                  baseInterpolation) RESULT(ans)
  TYPE(Elemdata_), INTENT(in) :: obj
  !! Element data object
  INTEGER(I4B), INTENT(IN) :: ii
  !! Local face number
  INTEGER(I4B), INTENT(IN) :: order
  !! Order on the edge
  CHARACTER(*), INTENT(IN) :: baseContinuity
  !! base continuity: H1, HDiv, HCurl
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  !! base interpolation type
  INTEGER(I4B) :: ans
  !! Total number of dof on edge

  ! Internal variables
  INTEGER(I4B) :: faceElemType(PARAM_REFELEM_MAX_FACES)

  !! Get faceElemType

  faceelemtype = 0

  CALL GetFaceElemType(elemType=obj%name, faceElemType=faceElemType)

  !! Get the
  ans = GetTotalInDOF(order=order, elemType=faceElemType(ii), &
           baseContinuity=baseContinuity, baseInterpolation=baseInterpolation)

  ans = MAX(ans, 0_I4B)

END FUNCTION Elemdata_GetTotalFaceDOF

!----------------------------------------------------------------------------
!                                                   Elemdata_GetTotalCellDOF
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get the total number of cell dof inside the cell of an element
!
!# Introduction
!
! Get the total number of cell dof inside the cell of an element.
! All dofs are internal to cell.
! It does not include vertex, edge and face dof.

FUNCTION Elemdata_GetTotalCellDOF(obj, order, baseContinuity, &
                                  baseInterpolation) RESULT(ans)
  TYPE(Elemdata_), INTENT(in) :: obj
  !! Element data object
  INTEGER(I4B), INTENT(IN) :: order
  !! Order on the edge
  CHARACTER(*), INTENT(IN) :: baseContinuity
  !! base continuity: H1, HDiv, HCurl
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  !! base interpolation type
  INTEGER(I4B) :: ans
  !! Total number of dof on edge

  ans = GetTotalInDOF(order=order, elemType=obj%name, &
           baseContinuity=baseContinuity, baseInterpolation=baseInterpolation)

  ans = MAX(ans, 0_I4B)
END FUNCTION Elemdata_GetTotalCellDOF

!----------------------------------------------------------------------------
!                                              Elemdata_GetElementToElements
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Get the global element number of neighbor elements.

SUBROUTINE Elemdata_GetElementToElements1(obj, ans, tsize)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  !! Element to element, it should be allocated by user before calling
  INTEGER(I4B), INTENT(OUT) :: tsize
  !! The size of data written to ans

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%globalElements) / 3

  DO ii = 1, tsize
    ans(ii) = obj%globalElements((ii - 1) * 3 + 1)
  END DO

END SUBROUTINE Elemdata_GetElementToElements1

!----------------------------------------------------------------------------
!                                              Elemdata_GetElementToElements
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Get the element to element connectivity
!
!# Introduction
!
! This subroutine returns the element to element connectivity.
! It also returns the local facet number information which are in
! contact with each other.

SUBROUTINE Elemdata_GetElementToElements2(obj, ans, nrow, ncol, &
                                          includeBoundaryElement)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
  !! Element to element, it should be allocated by user before calling
  !! each row denotes the information of a neighbor element.
  !! Therefore nrow is the total number of neighboring elements
  !! number of columns is 3.
  !! The first column is global element of the neighbor element
  !! The second column is local face number of parent element (this element)
  !! The third column is local face number of neighbor element
  INTEGER(I4B), INTENT(OUT) :: nrow
  !! Number of rows written to ans
  INTEGER(I4B), INTENT(OUT) :: ncol
  !! Number of columns written to ans
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: includeBoundaryElement
  !! If includeBoundaryElement is present and true,
  !! then the boundary element data is included in ans
  !! In this case the current element is considered as the boundary element
  !! ans(nrow, 1) contains the global element number of the current element
  !! ans(nrow, 2) contains the boundary data (local face number which coincides with
  !! the mesh boundary)
  !! ans(nrow, 3) is set to 0

  INTEGER(I4B) :: ii, jj

  nrow = SIZE(obj%globalElements) / 3
  ncol = 3

  DO ii = 1, nrow
    DO jj = 1, ncol
      ans(ii, jj) = obj%globalElements((ii - 1) * 3 + jj)
    END DO
  END DO

  IF (PRESENT(includeBoundaryElement)) THEN
    IF (includeBoundaryElement) THEN

      DO ii = 1, SIZE(obj%boundaryData)
        nrow = nrow + 1
        ans(nrow, 1) = obj%globalElemNum
        ans(nrow, 2) = obj%boundaryData(ii)
        ans(nrow, 3) = 0
      END DO

    END IF
  END IF

END SUBROUTINE Elemdata_GetElementToElements2

!----------------------------------------------------------------------------
!                                               Elemdata_GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Get the end points of the edge

SUBROUTINE Elemdata_GetEdgeConnectivity(obj, ans, tsize, ii)
  TYPE(Elemdata_), INTENT(IN) :: obj
  !! Element data object
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  !! edge connectivity, node numbers are global
  INTEGER(I4B), INTENT(OUT) :: tsize
  !! total data written to ans
  INTEGER(I4B), INTENT(IN) :: ii
  !! Edge number (local)

  INTEGER(I4B) :: ncol, jj, con(PARAM_MAX_NNE, PARAM_REFELEM_MAX_EDGES)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Elemdata_GetEdgeConnectivity()"
  LOGICAL(LGT) :: problem
#endif

  CALL GetEdgeConnectivity(elemType=obj%name, con=con, opt=1_I4B, &
                           nrow=tsize, ncol=ncol)

#ifdef DEBUG_VER
  problem = ii .GT. ncol
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR]: Edge number is greater than the number of edges')
  END IF
#endif

  DO jj = 1, tsize
    ans(jj) = con(jj, ii)
  END DO
END SUBROUTINE Elemdata_GetEdgeConnectivity

!----------------------------------------------------------------------------
!                                                                  isActive
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  This function returns true if the element is active

PURE FUNCTION Elemdata_isActive(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  LOGICAL(LGT) :: ans
  ans = obj%isActive
END FUNCTION Elemdata_isActive

!----------------------------------------------------------------------------
!                                                              globalElemNum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  This function returns the global element number

PURE FUNCTION Elemdata_globalElemNum(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%globalElemNum
END FUNCTION Elemdata_globalElemNum

!----------------------------------------------------------------------------
!                                                              localElemNum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  This function returns the local element number

PURE FUNCTION Elemdata_localElemNum(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%localElemNum
END FUNCTION Elemdata_localElemNum

!----------------------------------------------------------------------------
!                                                              elementType
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary: This function returns element type

PURE FUNCTION Elemdata_elementType(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%elementType
END FUNCTION Elemdata_elementType

!----------------------------------------------------------------------------
!                                                                      name
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  This function returns the name of the element

PURE FUNCTION Elemdata_name(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%name
END FUNCTION Elemdata_name

!----------------------------------------------------------------------------
!                                                                 topoName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  This function returns the topology name of the element

PURE FUNCTION Elemdata_topoName(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%topoName
END FUNCTION Elemdata_topoName

!----------------------------------------------------------------------------
!                                                                  topoIndx
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  This function returns the topology index of the element

PURE FUNCTION Elemdata_topoIndx(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = GetElementIndex(obj%topoName)
END FUNCTION Elemdata_topoIndx

!----------------------------------------------------------------------------
!                                                                    meshid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Returns the mesh id of the element

PURE FUNCTION Elemdata_meshid(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%meshid
END FUNCTION Elemdata_meshid

!----------------------------------------------------------------------------
!                                                           GetTotalMaterial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Get total number of materials assigned to the element

PURE FUNCTION Elemdata_GetTotalMaterial(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%material)
END FUNCTION Elemdata_GetTotalMaterial

!----------------------------------------------------------------------------
!                                                       GetTotalGlobalNodes
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Get total number of global nodes in the element

PURE FUNCTION Elemdata_GetTotalGlobalNodes(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%globalNodes)
END FUNCTION Elemdata_GetTotalGlobalNodes

!----------------------------------------------------------------------------
!                                                   GetTotalGlobalVertexNodes
!----------------------------------------------------------------------------

PURE FUNCTION Elemdata_GetTotalGlobalVertexNodes(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans

  INTEGER(I4B) :: indx

  indx = GetElementIndex(obj%topoName)
  ans = ReferenceElementInfo%tPoints(indx)
END FUNCTION Elemdata_GetTotalGlobalVertexNodes

!----------------------------------------------------------------------------
!                                                       GetTotalGlobalEdges
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Get the total number of global edges in the element

PURE FUNCTION Elemdata_GetTotalGlobalEdges(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%globalEdges)
END FUNCTION Elemdata_GetTotalGlobalEdges

!----------------------------------------------------------------------------
!                                                       GetTotalEdgeOrient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Get the size of edgeOrient vector

PURE FUNCTION Elemdata_GetTotalEdgeOrient(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%edgeOrient)
END FUNCTION Elemdata_GetTotalEdgeOrient

!----------------------------------------------------------------------------
!                                                       GetTotalGlobalFaces
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2025-05-10
! summary:  Get the size of globalFaces vector

PURE FUNCTION Elemdata_GetTotalGlobalFaces(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%globalFaces)
END FUNCTION Elemdata_GetTotalGlobalFaces

!----------------------------------------------------------------------------
!                                                       GetTotalFaceOrient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2025-05-10
! summary:  Get the size of faceOrient vector

PURE FUNCTION Elemdata_GetTotalFaceOrient(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%faceOrient)
END FUNCTION Elemdata_GetTotalFaceOrient

!----------------------------------------------------------------------------
!                                                     GetTotalGlobalElements
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Get the size of globalElements vector

PURE FUNCTION Elemdata_GetTotalGlobalElements(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%globalElements)
END FUNCTION Elemdata_GetTotalGlobalElements

!----------------------------------------------------------------------------
!                                                     GetTotalBoundaryData
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary: Get the size of boundaryData vector

PURE FUNCTION Elemdata_GetTotalBoundaryData(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%boundaryData)
END FUNCTION Elemdata_GetTotalBoundaryData

!----------------------------------------------------------------------------
!                                                               GetMaterial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2025-05-10
! summary:  Get list of materials assigned to the element

PURE SUBROUTINE Elemdata_GetMaterial(obj, ans, tsize)
  TYPE(Elemdata_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%material)

  DO ii = 1, tsize
    ans(ii) = obj%material(ii)
  END DO

END SUBROUTINE Elemdata_GetMaterial

!----------------------------------------------------------------------------
!                                                            GetGlobalNodes
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Get the list of global nodes in the element

PURE SUBROUTINE Elemdata_GetGlobalNodes(obj, ans, tsize)
  TYPE(Elemdata_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%globalNodes)

  DO ii = 1, tsize
    ans(ii) = obj%globalNodes(ii)
  END DO

END SUBROUTINE Elemdata_GetGlobalNodes

!----------------------------------------------------------------------------
!                                                            GetGlobalEdges
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Get the list of global edges in the element

PURE SUBROUTINE Elemdata_GetGlobalEdges(obj, ans, tsize)
  TYPE(Elemdata_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%globalEdges)

  DO ii = 1, tsize
    ans(ii) = obj%globalEdges(ii)
  END DO

END SUBROUTINE Elemdata_GetGlobalEdges

!----------------------------------------------------------------------------
!                                                            GetGlobalFaces
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Get the list of global faces in the element

PURE SUBROUTINE Elemdata_GetGlobalFaces(obj, ans, tsize)
  TYPE(Elemdata_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%globalFaces)

  DO ii = 1, tsize
    ans(ii) = obj%globalFaces(ii)
  END DO

END SUBROUTINE Elemdata_GetGlobalFaces

!----------------------------------------------------------------------------
!                                                       GetGlobalFaceNumber
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2025-05-10
! summary:  Get the global face number from a local face number

PURE FUNCTION Elemdata_GetGlobalFaceNumber(obj, localFaceNumber) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: localFaceNumber
  !! local face number
  INTEGER(I4B) :: ans
  !! global face number
  ans = obj%globalFaces(localFaceNumber)
END FUNCTION Elemdata_GetGlobalFaceNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Get the global edge number from a local edge number

PURE FUNCTION Elemdata_GetGlobalEdgeNumber(obj, localEdgeNumber) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: localEdgeNumber
  !! local edge number
  INTEGER(I4B) :: ans
  !! global edge number
  ans = obj%globalEdges(localEdgeNumber)
END FUNCTION Elemdata_GetGlobalEdgeNumber

!----------------------------------------------------------------------------
!                                                          GetGlobalElements
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  This subroutine returns the list globalElements in the element.

PURE SUBROUTINE Elemdata_GetGlobalElements(obj, ans, tsize)
  TYPE(Elemdata_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%globalElements)

  DO ii = 1, tsize
    ans(ii) = obj%globalElements(ii)
  END DO

END SUBROUTINE Elemdata_GetGlobalElements

!----------------------------------------------------------------------------
!                                                          GetBoundaryData
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  This subroutine returns the list of boundary data in the element.

PURE SUBROUTINE Elemdata_GetBoundaryData(obj, ans, tsize)
  TYPE(Elemdata_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  !! boundary data, see the boundaryData in the type definition
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%boundaryData)

  DO ii = 1, tsize
    ans(ii) = obj%boundaryData(ii)
  END DO

END SUBROUTINE Elemdata_GetBoundaryData

!----------------------------------------------------------------------------
!                                                     GetGlobalNodesPointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  This function returns the pointer to the globalNodes

FUNCTION Elemdata_GetGlobalNodesPointer(obj) RESULT(ans)
  TYPE(Elemdata_), TARGET, INTENT(IN) :: obj
  INTEGER(I4B), POINTER :: ans(:)
  ans => obj%globalNodes
END FUNCTION Elemdata_GetGlobalNodesPointer

!----------------------------------------------------------------------------
!                                                            GetEdgeOrient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2025-05-10
! summary:  This subroutine returns the edgeOrient in the element

PURE SUBROUTINE Elemdata_GetEdgeOrient(obj, ans, tsize)
  TYPE(Elemdata_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%edgeOrient)

  DO ii = 1, tsize
    ans(ii) = obj%edgeOrient(ii)
  END DO

END SUBROUTINE Elemdata_GetEdgeOrient

!----------------------------------------------------------------------------
!                                                            GetFaceOrient
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  This subroutine returns the faceOrient in the element

PURE SUBROUTINE Elemdata_GetFaceOrient(obj, ans, nrow, ncol)
  TYPE(Elemdata_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  INTEGER(I4B) :: ii, jj

  nrow = SIZE(obj%faceOrient, 1)
  ncol = SIZE(obj%faceOrient, 2)

  DO jj = 1, ncol
    DO ii = 1, nrow
      ans(ii, jj) = INT(obj%faceOrient(ii, jj), kind=I4B)
    END DO
  END DO

END SUBROUTINE Elemdata_GetFaceOrient

!----------------------------------------------------------------------------
!                                                             GetOrientation
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Get the orientation related vectors from the element

PURE SUBROUTINE Elemdata_GetOrientation(obj, cellOrient, faceOrient, &
                            edgeOrient, tCellOrient, tFaceOrient, tEdgeOrient)
  TYPE(Elemdata_), INTENT(IN) :: obj
  !! element dataa
  INTEGER(I4B), INTENT(INOUT) :: cellOrient(:)
  !! cell connectivity of element
  INTEGER(I4B), INTENT(INOUT) :: faceOrient(:, :)
  !! face connectivity of element
  INTEGER(I4B), INTENT(INOUT) :: edgeOrient(:)
  !! edge connectivity of element
  INTEGER(I4B), INTENT(OUT) :: tCellOrient
  !! size of data written in cellCon
  INTEGER(I4B), INTENT(OUT) :: tFaceOrient(2)
  !! size of data written in faceCon
  INTEGER(I4B), INTENT(OUT) :: tEdgeOrient
  !! size of data written in edgecon

  tCellOrient = 3
  cellOrient(1:3) = 1

  CALL Elemdata_GetFaceOrient(obj=obj, ans=faceOrient, nrow=tFaceOrient(1), &
                              ncol=tFaceOrient(2))

  CALL Elemdata_GetEdgeOrient(obj=obj, ans=edgeOrient, tsize=tEdgeOrient)

END SUBROUTINE Elemdata_GetOrientation

!----------------------------------------------------------------------------
!                                                                  FindFace
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-05-10
! summary:  Returns the local face number of the elem from faceCon

SUBROUTINE Elemdata_FindFace(obj, faceCon, isFace, &
                             localFaceNumber, onlyBoundaryElement)
  TYPE(Elemdata_), INTENT(IN) :: obj
  !! abstract mesh
  INTEGER(I4B), INTENT(IN) :: faceCon(:)
  !! vertex connectivity of face
  LOGICAL(LGT), INTENT(OUT) :: isFace
  !! if faceCon is a face of globalElement then it is true, else false
  INTEGER(I4B), INTENT(OUT) :: localFaceNumber
  !! local face number if found, else 0
  LOGICAL(LGT), INTENT(IN) :: onlyBoundaryElement
  !! if true then we will search if the element is boundary element

  LOGICAL(LGT) :: isok
  INTEGER(I4B), PARAMETER :: faceopt = 1
  INTEGER(I4B) :: tFaces, localFaces0(4, PARAM_REFELEM_MAX_FACES), &
                  tFaceNodes(PARAM_REFELEM_MAX_FACES), iface, &
                  found(4), want(4), found_size, want_size

  isFace = .FALSE.
  localFaceNumber = 0

  IF (onlyBoundaryElement) THEN
    isok = Elemdata_IsBoundaryElement(obj=obj)
    IF (.NOT. isok) RETURN
  END IF

  isFace = faceCon.IN.obj%globalNodes

  IF (.NOT. isFace) RETURN

  ! get the local face number
  CALL RefElemGetGeoParam(elemType=obj%name, &
                          tFaces=tFaces, faceCon=localFaces0, &
                          faceOpt=faceopt, tFaceNodes=tFaceNodes)

  want_size = MIN(SIZE(faceCon), 4)
  want(1:want_size) = Sort(faceCon(1:want_size))

  DO iface = 1, tFaces
    found_size = tFaceNodes(iface)
    ! isok = found_size .EQ. want_size
    ! IF (.NOT. isok) CYCLE

    found(1:found_size) = obj%globalNodes(localFaces0(1:found_size, iface))
    CALL QuickSort(found, 1, found_size)

    isok = ALL(found(1:found_size) .EQ. want(1:found_size))
    IF (isok) THEN
      localFaceNumber = iface
      EXIT
    END IF

  END DO

END SUBROUTINE Elemdata_FindFace

!----------------------------------------------------------------------------
!                                                           FindEdge
!----------------------------------------------------------------------------

SUBROUTINE Elemdata_FindEdge(obj, edgeCon, isEdge, localEdgeNumber, &
                             onlyBoundaryElement)
  TYPE(Elemdata_), INTENT(IN) :: obj
  !! abstract mesh
  INTEGER(I4B), INTENT(IN) :: edgeCon(:)
  !! vertex connectivity of Edge
  LOGICAL(LGT), INTENT(OUT) :: isEdge
  !! if EdgeCon is a Edge of globalElement then it is true, else false
  INTEGER(I4B), INTENT(OUT) :: localEdgeNumber
  !! local Edge number if found, else 0
  LOGICAL(LGT), INTENT(IN) :: onlyBoundaryElement
  !! if true then we will search if the element is boundary element

  LOGICAL(LGT) :: isok
  INTEGER(I4B), PARAMETER :: edgeOpt = 1
  INTEGER(I4B) :: tEdges, localEdges0(2, PARAM_REFELEM_MAX_EDGES), &
                  iedge, found(2), want(2), aint

  isEdge = .FALSE.
  localEdgeNumber = 0

  IF (onlyBoundaryElement) THEN
    isok = obj%elementType .EQ. BOUNDARY_ELEMENT
    IF (.NOT. isok) RETURN
  END IF

  isEdge = edgeCon.IN.obj%globalNodes

  IF (.NOT. isEdge) RETURN

  ! get the local Edge number
  CALL RefElemGetGeoParam(elemType=obj%name, tEdges=tEdges, edgeCon=localEdges0, &
                          edgeOpt=edgeOpt)

  want(1:2) = Sort(edgeCon(1:2))

  DO iedge = 1, tEdges
    found(1:2) = obj%globalNodes(localEdges0(1:2, iedge))

    IF (found(1) .GT. found(2)) THEN
      aint = found(1)
      found(1) = found(2)
      found(2) = aint
    END IF

    isok = ALL(found(1:2) .EQ. want(1:2))
    IF (isok) THEN
      localEdgeNumber = iEdge
      EXIT
    END IF

  END DO

END SUBROUTINE Elemdata_FindEdge

!----------------------------------------------------------------------------
!                                                         IsboundaryElement
!----------------------------------------------------------------------------

FUNCTION Elemdata_IsBoundaryElement(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  LOGICAL(LGT) :: ans
  ans = (obj%elementType .EQ. TypeElem%boundary) .OR. &
        (obj%elementType .EQ. TypeElem%domainBoundary)
END FUNCTION Elemdata_IsBoundaryElement

!----------------------------------------------------------------------------
!                                                           Elemdata_Order
!----------------------------------------------------------------------------

PURE FUNCTION Elemdata_Order(obj) RESULT(ans)
  TYPE(Elemdata_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = ElementOrder(obj%name)
END FUNCTION Elemdata_Order

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Elemdata_Class
