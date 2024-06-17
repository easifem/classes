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

MODULE ElemData_Class
USE GlobalData, ONLY: I4B, DFP, LGT, INT8
USE Display_Method, ONLY: Display
USE ReferenceElement_Method, ONLY: PARAM_REFELEM_MAX_FACES, &
                                   RefElemGetGeoParam, &
                                   ElementName, &
                                   GetFaceElemType, &
                                   GetEdgeConnectivity, &
                                   PARAM_REFELEM_MAX_EDGES

USE AbstractMeshParam, ONLY: PARAM_MAX_NNE

USE InterpolationUtility, ONLY: GetTotalInDOF
USE ReferenceQuadrangle_Method, ONLY: HelpFaceData_Quadrangle, &
                                      FaceShapeMetaData_Quadrangle
USE SortUtility, ONLY: Sort
USE ReallocateUtility, ONLY: Reallocate
USE SafeSizeUtility, ONLY: SafeSize
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE
PRIVATE

PUBLIC :: ElemData_
PUBLIC :: ElemDataPointer_
PUBLIC :: Display
PUBLIC :: TypeElem
PUBLIC :: ElemData_Set
PUBLIC :: ElemData_Pointer
PUBLIC :: ElemData_Deallocate
PUBLIC :: ElemData_Display
PUBLIC :: ElemData_lt
PUBLIC :: ElemData_eq
PUBLIC :: ElemData_SetID
PUBLIC :: ElemData_Copy
PUBLIC :: ElemData_GetGlobalFaceCon
PUBLIC :: ElemData_SetTotalMaterial
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: ElemData_GetConnectivity
PUBLIC :: ElemData_GetTotalEntities
PUBLIC :: ElemData_GetVertex
PUBLIC :: ElemData_GetEdge
PUBLIC :: ElemData_GetFace
PUBLIC :: ElemData_GetCell
PUBLIC :: ElemData_GetTotalEdgeDOF
PUBLIC :: ElemData_GetTotalFaceDOF
PUBLIC :: ElemData_GetTotalCellDOF
PUBLIC :: ElemData_GetElementToElements
PUBLIC :: ElemData_GetEdgeConnectivity
PUBLIC :: ElemData_isActive
PUBLIC :: ElemData_globalElemNum
PUBLIC :: ElemData_localElemNum
PUBLIC :: ElemData_elementType
PUBLIC :: ElemData_name
PUBLIC :: ElemData_meshID
PUBLIC :: ElemData_GetTotalMaterial
PUBLIC :: ElemData_GetTotalGlobalNodes
PUBLIC :: ElemData_GetTotalEdgeOrient
PUBLIC :: ElemData_GetTotalGlobalFaces
PUBLIC :: ElemData_GetTotalFaceOrient
PUBLIC :: ElemData_GetTotalGlobalElements
PUBLIC :: ElemData_GetTotalBoundaryData
PUBLIC :: ElemData_GetMaterial
PUBLIC :: ElemData_GetGlobalNodes
PUBLIC :: ElemData_GetGlobalEdges
PUBLIC :: ElemData_GetEdgeOrient
PUBLIC :: ElemData_GetGlobalFaces
PUBLIC :: ElemData_GetFaceOrient
PUBLIC :: ElemData_GetGlobalElements
PUBLIC :: ElemData_GetBoundaryData
PUBLIC :: ElemData_GetGlobalNodesPointer

INTEGER(I4B), PARAMETER, PUBLIC :: INTERNAL_ELEMENT = 1
INTEGER(I4B), PARAMETER, PUBLIC :: BOUNDARY_ELEMENT = -1
INTEGER(I4B), PARAMETER, PUBLIC :: DOMAIN_BOUNDARY_ELEMENT = -2
INTEGER(I4B), PARAMETER, PUBLIC :: GHOST_ELEMENT = -4

! INTEGER(I4B), PARAMETER :: MAX_NUM_OVERLAPPED_CONTINNUM = 4

CHARACTER(*), PARAMETER :: modName = "ElemData_Class"

INTERFACE Display
  MODULE PROCEDURE ElemData_Display
END INTERFACE Display

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE ElemData_Copy
END INTERFACE

INTERFACE ElemData_GetElementToElements
  MODULE PROCEDURE ElemData_GetElementToElements1, &
    ElemData_GetElementToElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ElemData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-26
! summary: Data type for storing element data

TYPE :: ElemData_
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
    !! If `iel` is boundary element, then boudnaryData contains
    !! the local facet number of iel which concides with the
    !! mesh boundary.
    !! If an element contains the boundary node then it is considered
    !! as a boundary element.
    !! It may happen that a boundary element has no boundary face, in which
    !! case boundaryData will have zero size
END TYPE ElemData_

!----------------------------------------------------------------------------
!                                                           ElemDataPointer_
!----------------------------------------------------------------------------

TYPE ElemDataPointer_
  CLASS(ElemData_), POINTER :: ptr => NULL()
END TYPE ElemDataPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-07
! summary:  List of element data type

TYPE ElemDataType_
  INTEGER(I4B) :: internal = INTERNAL_ELEMENT
  INTEGER(I4B) :: boundary = BOUNDARY_ELEMENT
  INTEGER(I4B) :: domainBoundary = DOMAIN_BOUNDARY_ELEMENT
  INTEGER(I4B) :: ghost = GHOST_ELEMENT
END TYPE ElemDataType_

TYPE(ElemDataType_), PARAMETER :: TypeElem = ElemDataType_()

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

SUBROUTINE ElemData_Copy(obj1, obj2)
  TYPE(ElemData_), INTENT(INOUT) :: obj1
  TYPE(ElemData_), INTENT(IN) :: obj2

  obj1%isActive = obj2%isActive
  obj1%globalElemNum = obj2%globalElemNum
  obj1%localElemNum = obj2%localElemNum
  obj1%elementType = obj2%elementType
  obj1%name = obj2%name
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

END SUBROUTINE ElemData_Copy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-26
! summary: Display a single instance of element data

SUBROUTINE ElemData_Display(obj, msg, unitno)
  TYPE(ElemData_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  LOGICAL(LGT) :: abool

  CALL Display(TRIM(msg), unitno=unitno)
  CALL Display(obj%isActive, msg="isActive: ", unitno=unitno)
  CALL Display(obj%globalElemNum, msg="globalElemNum: ", unitno=unitno)
  CALL Display(obj%localElemNum, msg="localElemNum: ", unitno=unitno)
  CALL Display(ElemData_ElemType2String(obj%elementType), "elementType: ",  &
    & unitno=unitno)
  CALL Display(ElementName(obj%name), "elementName: ", unitno=unitno)

  ! display material if it is allocated
  abool = ALLOCATED(obj%material)
  CALL Display(abool, "material ALLOCATED: ", unitno=unitno)
  IF (abool) THEN
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
END SUBROUTINE ElemData_Display

!----------------------------------------------------------------------------
!                                                   ElemData_ElemType2String
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-29
! summary:  convert elementType to name

FUNCTION ElemData_ElemType2String(elementType) RESULT(ans)
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
END FUNCTION ElemData_ElemType2String

!----------------------------------------------------------------------------
!                                                         ElemDataDeallocate
!----------------------------------------------------------------------------

SUBROUTINE ElemData_Deallocate(obj)
  TYPE(ElemData_), INTENT(INOUT) :: obj
  obj%isActive = .TRUE.
  obj%globalElemNum = 0
  obj%localElemNum = 0
  obj%elementType = INTERNAL_ELEMENT
  obj%name = 0
  obj%meshID = 0
  IF (ALLOCATED(obj%material)) DEALLOCATE (obj%material)
  IF (ALLOCATED(obj%globalNodes)) DEALLOCATE (obj%globalNodes)
  IF (ALLOCATED(obj%globalEdges)) DEALLOCATE (obj%globalEdges)
  IF (ALLOCATED(obj%edgeOrient)) DEALLOCATE (obj%edgeOrient)
  IF (ALLOCATED(obj%globalFaces)) DEALLOCATE (obj%globalFaces)
  IF (ALLOCATED(obj%faceOrient)) DEALLOCATE (obj%faceOrient)
  IF (ALLOCATED(obj%globalElements)) DEALLOCATE (obj%globalElements)
  IF (ALLOCATED(obj%boundaryData)) DEALLOCATE (obj%boundaryData)
END SUBROUTINE ElemData_Deallocate

!----------------------------------------------------------------------------
!                                                         SetTotalMaterial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-12
! summary:  Set total number of materials
!
! this subroutine allocates materials in obj

PURE SUBROUTINE ElemData_SetTotalMaterial(obj, n)
  TYPE(ElemData_), INTENT(INOUT) :: obj
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

END SUBROUTINE ElemData_SetTotalMaterial

!----------------------------------------------------------------------------
!                                                           ElemDataInitiate
!----------------------------------------------------------------------------

PURE SUBROUTINE ElemData_Set(obj, globalElemNum, localElemNum, &
        elementType, globalNodes, globalElements, boundaryData, globalEdges, &
             globalFaces, name, isActive, meshID, medium, material, materials)
  ! obj%elementData(ii)%globalElemNum = elemNumber(ii)
  ! obj%elementData(ii)%localElemNum = ii
  ! obj%elementData(ii)%globalNodes = connectivity(:, ii)
  TYPE(ElemData_), INTENT(INOUT) :: obj
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
  IF (PRESENT(name)) obj%name = name
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

END SUBROUTINE ElemData_Set

!----------------------------------------------------------------------------
!                                                          ElemData_Pointer
!----------------------------------------------------------------------------

FUNCTION ElemData_Pointer() RESULT(ans)
  CLASS(ElemData_), POINTER :: ans
  ALLOCATE (ElemData_ :: ans)
END FUNCTION ElemData_Pointer

!----------------------------------------------------------------------------
!                                                               ElemData_lt
!----------------------------------------------------------------------------

FUNCTION ElemData_lt(obj, obj2) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  TYPE(ElemData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%globalElemNum .GT. obj2%globalElemNum
END FUNCTION ElemData_lt

!----------------------------------------------------------------------------
!                                                               ElemData_eq
!----------------------------------------------------------------------------

FUNCTION ElemData_eq(obj, obj2) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  TYPE(ElemData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%globalElemNum .EQ. obj2%globalElemNum
END FUNCTION ElemData_eq

!----------------------------------------------------------------------------
!                                                         ElemData_SetID
!----------------------------------------------------------------------------

SUBROUTINE ElemData_SetID(obj, id)
  TYPE(ElemData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: id
  obj%localElemNum = id
END SUBROUTINE ElemData_SetID

!----------------------------------------------------------------------------
!                                                 ElemData_GetGlobalFaceCon
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-12
! summary:  Returns the vertex connectivity of global face of elements

SUBROUTINE ElemData_GetGlobalFaceCon(obj, globalFaceCon, localFaceCon)
  TYPE(ElemData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(INOUT) :: globalFaceCon(:, :)
  INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: localFaceCon(:, :)

INTEGER(I4B) :: tFaces, tNodes, localFaces0(4_I4B, PARAM_REFELEM_MAX_FACES), &
                  faceElemType(PARAM_REFELEM_MAX_FACES), &
                  tFaceNodes(PARAM_REFELEM_MAX_FACES), &
                  iface, face_temp(4), aint

  CALL RefElemGetGeoParam(elemType=obj%name, &
                          tFaces=tFaces, tNodes=tNodes, faceCon=localFaces0, &
                          faceOpt=1_I4B, faceElemType=faceElemType, &
                          tFaceNodes=tFaceNodes)

  DO iface = 1, tFaces
    aint = tFaceNodes(iface)
    face_temp(1:aint) = obj%globalNodes(localFaces0(1:aint, iface))

    CALL FaceShapeMetaData_Quadrangle(face=face_temp(1:aint), &
                                   sorted_face=globalFaceCon(1:aint, iface), &
                                      localFaces=localFaceCon(1:aint, iface))
  END DO

END SUBROUTINE ElemData_GetGlobalFaceCon

!----------------------------------------------------------------------------
!                                                   ElemData_GetConnectivity
!----------------------------------------------------------------------------

SUBROUTINE ElemData_GetConnectivity(obj, con, tsize, opt)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(INOUT) :: con(:)
  INTEGER(I4B), INTENT(OUT) :: tsize
  CHARACTER(*), INTENT(IN), OPTIONAL :: opt

  !! internal variable
  CHARACTER(1) :: opt0
  INTEGER(I4B) :: aint, ii, jj

  opt0 = 'V'
  IF (PRESENT(opt)) opt0(1:1) = opt(1:1)

  SELECT CASE (opt0)
  CASE ("V", "v")
    tsize = SafeSize(obj%globalNodes)
    DO ii = 1, tsize; con(ii) = obj%globalNodes(ii); END DO

  CASE ("E", "e")
    tsize = SafeSize(obj%globalEdges)
    DO ii = 1, tsize; con(ii) = obj%globalEdges(ii); END DO

  CASE ("F", "f")
    tsize = SafeSize(obj%globalFaces)
    DO ii = 1, tsize; con(ii) = obj%globalFaces(ii); END DO
  CASE ("C", "c")
    tsize = 1
    con(1) = obj%globalElemNum
  CASE ("A", "a")
    aint = 1
    tsize = SafeSize(obj%globalNodes)
    jj = 0
    DO ii = aint, tsize
      jj = jj + 1
      con(ii) = obj%globalNodes(jj)
    END DO

    aint = tsize + 1
    tsize = tsize + SafeSize(obj%globalEdges)
    jj = 0
    DO ii = aint, tsize
      jj = jj + 1
      con(ii) = obj%globalEdges(jj)
    END DO

    aint = tsize + 1
    tsize = tsize + SafeSize(obj%globalFaces)
    jj = 0
    DO ii = aint, tsize
      jj = jj + 1
      con(ii) = obj%globalFaces(jj)
    END DO

    aint = tsize + 1
    tsize = tsize + 1
    con(aint) = obj%globalElemNum
  END SELECT

END SUBROUTINE ElemData_GetConnectivity

!----------------------------------------------------------------------------
!                                                 ElemData_GetTotalEntities
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-15
! summary: Returns total number of vertex, edge, faces, and cells in element

FUNCTION ElemData_GetTotalEntities(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(in) :: obj
  INTEGER(I4B) :: ans(4)
  ans = 0
  ans(1) = SafeSize(obj%globalNodes)
  ans(2) = SafeSize(obj%globalEdges)
  ans(3) = SafeSize(obj%globalFaces)
  ans(4) = 1
END FUNCTION ElemData_GetTotalEntities

!----------------------------------------------------------------------------
!                                                     ElemData_GetEdge
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get the global vertex number

FUNCTION ElemData_GetVertex(obj, ii) RESULT(ans)
  TYPE(ElemData_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(in) :: ii
  INTEGER(I4B) :: ans
  ans = obj%globalNodes(ii)
END FUNCTION ElemData_GetVertex

!----------------------------------------------------------------------------
!                                                     ElemData_GetEdge
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get the global edge

FUNCTION ElemData_GetEdge(obj, ii) RESULT(ans)
  TYPE(ElemData_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(in) :: ii
  INTEGER(I4B) :: ans
  ans = obj%globalEdges(ii)
END FUNCTION ElemData_GetEdge

!----------------------------------------------------------------------------
!                                                     ElemData_GetFace
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get the global face

FUNCTION ElemData_GetFace(obj, ii) RESULT(ans)
  TYPE(ElemData_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(in) :: ii
  INTEGER(I4B) :: ans
  ans = obj%globalFaces(ii)
END FUNCTION ElemData_GetFace

!----------------------------------------------------------------------------
!                                                     ElemData_GetCell
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get the global element number

FUNCTION ElemData_GetCell(obj, islocal) RESULT(ans)
  TYPE(ElemData_), INTENT(in) :: obj
  LOGICAL(LGT), INTENT(in) :: islocal
  INTEGER(I4B) :: ans

  IF (islocal) THEN
    ans = obj%localElemNum
  ELSE
    ans = obj%globalElemNum
  END IF
END FUNCTION ElemData_GetCell

!----------------------------------------------------------------------------
!                                                   ElemData_GetTotalEdgeDOF
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get the total number of edge dof on the edge of an element
!
!# Introduction
!
! All dofs are internal to the edge

FUNCTION ElemData_GetTotalEdgeDOF(obj, ii, order, baseContinuity, &
                                  baseInterpolation) RESULT(ans)
  TYPE(ElemData_), INTENT(in) :: obj
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
END FUNCTION ElemData_GetTotalEdgeDOF

!----------------------------------------------------------------------------
!                                                   ElemData_GetTotalFaceDOF
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get the total number of edge dof on the edge of an element
!
!# Introduction
!
! All dofs are internal to face

FUNCTION ElemData_GetTotalFaceDOF(obj, ii, order, baseContinuity, &
                                  baseInterpolation) RESULT(ans)
  TYPE(ElemData_), INTENT(in) :: obj
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
  CALL GetFaceElemType(elemType=obj%name, faceElemType=faceElemType)

  !! Get the
  ans = GetTotalInDOF(order=order, elemType=faceElemType(ii), &
           baseContinuity=baseContinuity, baseInterpolation=baseInterpolation)

  ans = MAX(ans, 0_I4B)

END FUNCTION ElemData_GetTotalFaceDOF

!----------------------------------------------------------------------------
!                                                   ElemData_GetTotalCellDOF
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-20
! summary: Get the total number of edge dof on the edge of an element
!
!# Introduction
!
! All dofs are internal to cell

FUNCTION ElemData_GetTotalCellDOF(obj, order, baseContinuity, &
                                  baseInterpolation) RESULT(ans)
  TYPE(ElemData_), INTENT(in) :: obj
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
END FUNCTION ElemData_GetTotalCellDOF

!----------------------------------------------------------------------------
!                                              ElemData_GetElementToElements
!----------------------------------------------------------------------------

SUBROUTINE ElemData_GetElementToElements1(obj, ans, tsize)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  !! Element to element, it should be allocated by user before calling
  INTEGER(I4B), INTENT(OUT) :: tsize
  !! The size of data written to ans

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%globalElements) / 3

  DO ii = 1, tsize
    ans(ii) = obj%globalElements((ii - 1) * 3 + 1)
  END DO

END SUBROUTINE ElemData_GetElementToElements1

!----------------------------------------------------------------------------
!                                              ElemData_GetElementToElements
!----------------------------------------------------------------------------

SUBROUTINE ElemData_GetElementToElements2(obj, ans, nrow, ncol, &
                                          includeBoundaryElement)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
  !! Element to element, it should be allocated by user before calling
  INTEGER(I4B), INTENT(OUT) :: nrow
  !! Number of rows written to ans
  INTEGER(I4B), INTENT(OUT) :: ncol
  !! Number of columns written to ans
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: includeBoundaryElement

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

END SUBROUTINE ElemData_GetElementToElements2

!----------------------------------------------------------------------------
!                                               ElemData_GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Get the end points of the edge

SUBROUTINE ElemData_GetEdgeConnectivity(obj, ans, tsize, ii)
  TYPE(ElemData_), INTENT(IN) :: obj
  !! Element data object
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  !! edge connectivity, node numbers are global
  INTEGER(I4B), INTENT(OUT) :: tsize
  !! total data written to ans
  INTEGER(I4B), INTENT(IN) :: ii
  !! Edge number (local)

  INTEGER(I4B) :: ncol, jj, con(PARAM_MAX_NNE, PARAM_REFELEM_MAX_EDGES)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ElemData_GetEdgeConnectivity()"
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
END SUBROUTINE ElemData_GetEdgeConnectivity

!----------------------------------------------------------------------------
!                                                                  isActive
!----------------------------------------------------------------------------

PURE FUNCTION ElemData_isActive(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  LOGICAL(LGT) :: ans
  ans = obj%isActive
END FUNCTION ElemData_isActive

!----------------------------------------------------------------------------
!                                                              globalElemNum
!----------------------------------------------------------------------------

PURE FUNCTION ElemData_globalElemNum(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%globalElemNum
END FUNCTION ElemData_globalElemNum

!----------------------------------------------------------------------------
!                                                              localElemNum
!----------------------------------------------------------------------------

PURE FUNCTION ElemData_localElemNum(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%localElemNum
END FUNCTION ElemData_localElemNum

!----------------------------------------------------------------------------
!                                                              localElemNum
!----------------------------------------------------------------------------

PURE FUNCTION ElemData_elementType(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%elementType
END FUNCTION ElemData_elementType

!----------------------------------------------------------------------------
!                                                              localElemNum
!----------------------------------------------------------------------------

PURE FUNCTION ElemData_name(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%name
END FUNCTION ElemData_name

!----------------------------------------------------------------------------
!                                                              localElemNum
!----------------------------------------------------------------------------

PURE FUNCTION ElemData_meshid(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%meshid
END FUNCTION ElemData_meshid

!----------------------------------------------------------------------------
!                                                                GetMaterial
!----------------------------------------------------------------------------

PURE FUNCTION ElemData_GetTotalMaterial(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%material)
END FUNCTION ElemData_GetTotalMaterial

!----------------------------------------------------------------------------
!                                                       GetTotalGlobalNodes
!----------------------------------------------------------------------------

PURE FUNCTION ElemData_GetTotalGlobalNodes(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%globalNodes)
END FUNCTION ElemData_GetTotalGlobalNodes

!----------------------------------------------------------------------------
!                                                       GetTotalGlobalEdges
!----------------------------------------------------------------------------

PURE FUNCTION ElemData_GetTotalGlobalEdges(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%globalEdges)
END FUNCTION ElemData_GetTotalGlobalEdges

!----------------------------------------------------------------------------
!                                                       GetTotalEdgeOrient
!----------------------------------------------------------------------------

PURE FUNCTION ElemData_GetTotalEdgeOrient(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%edgeOrient)
END FUNCTION ElemData_GetTotalEdgeOrient

!----------------------------------------------------------------------------
!                                                       GetTotalEdgeOrient
!----------------------------------------------------------------------------

PURE FUNCTION ElemData_GetTotalGlobalFaces(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%globalFaces)
END FUNCTION ElemData_GetTotalGlobalFaces

!----------------------------------------------------------------------------
!                                                       GetTotalEdgeOrient
!----------------------------------------------------------------------------

PURE FUNCTION ElemData_GetTotalFaceOrient(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%faceOrient)
END FUNCTION ElemData_GetTotalFaceOrient

!----------------------------------------------------------------------------
!                                                     GetTotalGlobalElements
!----------------------------------------------------------------------------

PURE FUNCTION ElemData_GetTotalGlobalElements(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%globalElements)
END FUNCTION ElemData_GetTotalGlobalElements

!----------------------------------------------------------------------------
!                                                     GetTotalBoundaryData
!----------------------------------------------------------------------------

PURE FUNCTION ElemData_GetTotalBoundaryData(obj) RESULT(ans)
  TYPE(ElemData_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = SIZE(obj%boundaryData)
END FUNCTION ElemData_GetTotalBoundaryData

!----------------------------------------------------------------------------
!                                                               GetMaterial
!----------------------------------------------------------------------------

PURE SUBROUTINE ElemData_GetMaterial(obj, ans, tsize)
  TYPE(ElemData_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%material)

  DO ii = 1, tsize
    ans(ii) = obj%material(ii)
  END DO

END SUBROUTINE ElemData_GetMaterial

!----------------------------------------------------------------------------
!                                                            GetGlobalNodes
!----------------------------------------------------------------------------

PURE SUBROUTINE ElemData_GetGlobalNodes(obj, ans, tsize)
  TYPE(ElemData_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%globalNodes)

  DO ii = 1, tsize
    ans(ii) = obj%globalNodes(ii)
  END DO

END SUBROUTINE ElemData_GetGlobalNodes

!----------------------------------------------------------------------------
!                                                            GetGlobalNodes
!----------------------------------------------------------------------------

PURE SUBROUTINE ElemData_GetGlobalEdges(obj, ans, tsize)
  TYPE(ElemData_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%globalNodes)

  DO ii = 1, tsize
    ans(ii) = obj%globalNodes(ii)
  END DO

END SUBROUTINE ElemData_GetGlobalEdges

!----------------------------------------------------------------------------
!                                                            GetGlobalNodes
!----------------------------------------------------------------------------

PURE SUBROUTINE ElemData_GetEdgeOrient(obj, ans, tsize)
  TYPE(ElemData_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%edgeOrient)

  DO ii = 1, tsize
    ans(ii) = obj%edgeOrient(ii)
  END DO

END SUBROUTINE ElemData_GetEdgeOrient

!----------------------------------------------------------------------------
!                                                            GetGlobalNodes
!----------------------------------------------------------------------------

PURE SUBROUTINE ElemData_GetGlobalFaces(obj, ans, tsize)
  TYPE(ElemData_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%globalFaces)

  DO ii = 1, tsize
    ans(ii) = obj%globalFaces(ii)
  END DO

END SUBROUTINE ElemData_GetGlobalFaces

!----------------------------------------------------------------------------
!                                                            GetGlobalNodes
!----------------------------------------------------------------------------

PURE SUBROUTINE ElemData_GetFaceOrient(obj, ans, nrow, ncol)
  TYPE(ElemData_), INTENT(in) :: obj
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

END SUBROUTINE ElemData_GetFaceOrient

!----------------------------------------------------------------------------
!                                                          GetGlobalElements
!----------------------------------------------------------------------------

PURE SUBROUTINE ElemData_GetGlobalElements(obj, ans, tsize)
  TYPE(ElemData_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%globalElements)

  DO ii = 1, tsize
    ans(ii) = obj%globalElements(ii)
  END DO

END SUBROUTINE ElemData_GetGlobalElements

!----------------------------------------------------------------------------
!                                                          GetBoundaryData
!----------------------------------------------------------------------------

PURE SUBROUTINE ElemData_GetBoundaryData(obj, ans, tsize)
  TYPE(ElemData_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  INTEGER(I4B) :: ii

  tsize = SIZE(obj%boundaryData)

  DO ii = 1, tsize
    ans(ii) = obj%boundaryData(ii)
  END DO

END SUBROUTINE ElemData_GetBoundaryData

!----------------------------------------------------------------------------
!                                                     GetGlobalNodesPointer
!----------------------------------------------------------------------------

FUNCTION ElemData_GetGlobalNodesPointer(obj) RESULT(ans)
  TYPE(ElemData_), TARGET, INTENT(IN) :: obj
  INTEGER(I4B), POINTER :: ans(:)
  ans => obj%globalNodes
END FUNCTION ElemData_GetGlobalNodesPointer

END MODULE ElemData_Class
