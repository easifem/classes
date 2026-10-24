! This program is a part of EASIFEM library
! Copyright (C) (Since 2000)  Vikas Sharma, Ph.D
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

MODULE AbstractMeshUtility
USE GlobalData, ONLY: DFP, I4B, LGT, stdout
USE Display_Method, ONLY: Display, ToString
USE ReallocateUtility, ONLY: Reallocate
USE CPUTime_Class, ONLY: CPUTime_
USE BaseType, ONLY: TypeRefelemOpt
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE ElemData_Class, ONLY: ElemData_
USE ElemData_Class, ONLY: ElemDataPointer_
USE ElemData_Class, ONLY: TypeElem
USE ElemData_Class, ONLY: DOMAIN_BOUNDARY_ELEMENT
USE ElemData_Class, ONLY: INTERNAL_ELEMENT
USE ElemData_Class, ONLY: ElemData_GetGlobalFaces
USE FacetData_Class, ONLY: FacetData_

IMPLICIT NONE
PRIVATE

PUBLIC :: InitiateElementToElements3D
PUBLIC :: InitiateElementToElements2D
PUBLIC :: InitiateElementToElements1D
PUBLIC :: GetFacetDataFromElemData

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractMeshUtility.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                  GetFacetDataFromElemData
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-14
! summary:  Get facet data from the element data

SUBROUTINE GetFacetDataFromElemData(elementData, facetData, masks, nsd)
  TYPE(ElemData_), INTENT(IN) :: elementData
  TYPE(FacetData_), INTENT(INOUT) :: facetData(:)
  LOGICAL(LGT), INTENT(INOUT) :: masks(:)
  INTEGER(I4B), INTENT(IN) :: nsd

  INTEGER(I4B) :: faces(32), tsize

  CALL ElemData_GetGlobalFaces(obj=elementData, ans=faces, tsize=tsize)
  ! now lets find the facets located on the DOMAIN_BOUNDARY_ELEMENT
  CALL Help_GetFacetDataFromElemData(elementData, facetData, masks, &
                                     faces(1:tsize))
END SUBROUTINE GetFacetDataFromElemData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_GetFacetDataFromElemData(elementData, facetData, masks, faces)
  TYPE(ElemData_), INTENT(IN) :: elementData
  TYPE(FacetData_), INTENT(INOUT) :: facetData(:)
  LOGICAL(LGT), INTENT(INOUT) :: masks(:)
  INTEGER(I4B), INTENT(IN), OPTIONAL :: faces(:)

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Help_GetFacetDataFromElemData()"
#endif
  INTEGER(I4B) :: ii, iface, tsize, jj

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! now lets find the facets located on the DOMAIN_BOUNDARY_ELEMENT
  tsize = SIZE(elementData%boundaryData)
  loop1: DO ii = 1, tsize

    ! this is local face number in master cell
    jj = elementData%boundaryData(ii)

    ! this is global facet number in the mesh
    iface = faces(jj)

    ! do not count two times
    IF (masks(iface)) CYCLE loop1

    ! we have found the facet element so mark it.
    masks(iface) = math%yes

    ! all elements found in this loop are located inside the domain
    facetData(iface)%elementType = DOMAIN_BOUNDARY_ELEMENT
    facetData(iface)%masterCellNumber = elementData%globalElemNum
    facetData(iface)%masterLocalFacetID = jj

    facetData(iface)%slaveCellNumber = 0_I4B
    facetData(iface)%slaveLocalFacetID = 0_I4B

  END DO loop1

  tsize = SIZE(elementData%globalElements)

  loop2: DO ii = 1, tsize, 3

    ! this is local face number in master cell
    jj = elementData%globalElements(ii + 1)

    ! this is global facet number in the mesh
    iface = faces(jj)

    ! do not count two times
    IF (masks(iface)) CYCLE loop2

    ! we have found the facet element so mark it.
    masks(iface) = math%yes

    ! all elements found in this loop are located inside the domain
    facetData(iface)%elementType = INTERNAL_ELEMENT

    facetData(iface)%slaveCellNumber = elementData%globalElements(ii)
    facetData(iface)%slaveLocalFacetID = elementData%globalElements(ii + 2)

    facetData(iface)%masterCellNumber = elementData%globalElemNum
    facetData(iface)%masterLocalFacetID = jj

  END DO loop2

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END SUBROUTINE Help_GetFacetDataFromElemData

!----------------------------------------------------------------------------
!                                               InitiateElementToElements3D
!----------------------------------------------------------------------------

SUBROUTINE InitiateElementToElements3D(elementData, tFaceInMesh, showTime)
  TYPE(ElemDataPointer_), INTENT(INOUT) :: elementData(:)
  INTEGER(I4B), INTENT(IN) :: tFaceInMesh
  LOGICAL(LGT), INTENT(IN) :: showTime

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateElementToElements3D()"
  LOGICAL(LGT) :: problem, isok1, isok2, isbndy
  INTEGER(I4B) :: telems, iel, aint, bint, tfaces, ii, jj, &
    temp1(3 * TypeRefelemOpt%maxFaces), cint, &
    bndyflag(TypeRefelemOpt%maxFaces)
  INTEGER(I4B), ALLOCATABLE :: face2elem(:, :)
  LOGICAL(LGT), ALLOCATABLE :: amask(:)
  TYPE(CPUTime_) :: TypeCPUTime

  IF (showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  problem = tFaceInMesh .EQ. 0
  IF (problem) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[INTERNAL ERROR] :: Total number of faces are zero.')
    RETURN
  END IF
#endif

  telems = SIZE(elementData)

  CALL Reallocate(face2elem, 4, tFaceInMesh)
  CALL Reallocate(amask, tFaceInMesh)
  amask = math%no

  loop1: DO iel = 1, telems

    problem = .NOT. elementData(iel)%ptr%isActive
    IF (problem) CYCLE loop1

    tfaces = SIZE(elementData(iel)%ptr%globalFaces)
    DO ii = 1, tfaces
      aint = ABS(elementData(iel)%ptr%globalFaces(ii))
      IF (amask(aint)) THEN
        face2elem(2, aint) = iel
        face2elem(4, aint) = ii
        amask(aint) = math%no
      ELSE
        face2elem(1, aint) = iel
        face2elem(3, aint) = ii
        amask(aint) = math%yes
      END IF
    END DO

  END DO loop1

  loop2: DO iel = 1, telems

    problem = .NOT. elementData(iel)%ptr%isActive
    IF (problem) CYCLE loop2

    tfaces = SIZE(elementData(iel)%ptr%globalFaces)
    jj = 0
    temp1 = 0
    bndyflag = 0
    DO ii = 1, tfaces
      aint = ABS(elementData(iel)%ptr%globalFaces(ii))
      bint = face2elem(1, aint)
      isok1 = bint .NE. iel
      isok2 = bint .NE. 0

      IF (amask(aint)) bndyflag(ii) = 1_I4B

      IF (isok1 .AND. isok2) THEN
        jj = jj + 1
        temp1(1 + (jj - 1) * 3) = elementData(bint)%ptr%globalElemNum
        temp1(2 + (jj - 1) * 3) = face2elem(4, aint)
        temp1(3 + (jj - 1) * 3) = face2elem(3, aint)

      ELSE
        cint = face2elem(2, aint)
        IF (cint .NE. 0) THEN
          jj = jj + 1
          temp1(1 + (jj - 1) * 3) = &
            elementData(cint)%ptr%globalElemNum
          temp1(2 + (jj - 1) * 3) = face2elem(3, aint)
          temp1(3 + (jj - 1) * 3) = face2elem(4, aint)
        END IF
      END IF
    END DO

    aint = jj * 3
    CALL Reallocate(elementData(iel)%ptr%globalElements, aint)
    elementData(iel)%ptr%globalElements = temp1(1:aint)

    aint = tfaces - jj
    CALL Reallocate(elementData(iel)%ptr%boundaryData, aint)
    isbndy = jj .NE. tfaces

    IF (isbndy) THEN
      elementData(iel)%ptr%elementType = TypeElem%domainBoundary
      jj = 0
      DO ii = 1, tfaces
        IF (bndyflag(ii) .NE. 0) THEN
          jj = jj + 1
          elementData(iel)%ptr%boundaryData(jj) = ii
        END IF
      END DO
    ELSE
      elementData(iel)%ptr%elementType = TypeElem%internal
    END IF

  END DO loop2

  IF (ALLOCATED(face2elem)) DEALLOCATE(face2elem)
  IF (ALLOCATED(amask)) DEALLOCATE(amask)

  IF (showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName // " : " // myName // &
                 " : time : " // &
                 ToString(TypeCPUTime%GetTime()), unitno=stdout)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END SUBROUTINE InitiateElementToElements3D

!----------------------------------------------------------------------------
!                                                InitiateElementToElements2D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-19
! summary: This routine uses the edge data in 2d to form element to element

SUBROUTINE InitiateElementToElements2D(elementData, tEdgeInMesh, showTime)
  TYPE(ElemDataPointer_), INTENT(INOUT) :: elementData(:)
  INTEGER(I4B), INTENT(IN) :: tEdgeInMesh
  LOGICAL(LGT), INTENT(IN) :: showTime

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "InitiateElementToElements2D()"
  TYPE(CPUTime_) :: TypeCPUTime
#endif
  LOGICAL(LGT) :: problem, isok1, isok2, isbndy
  INTEGER(I4B) :: telems, iel, aint, bint, tedges, ii, jj, temp1(3 * 4), &
    cint, bndyflag(4)
  INTEGER(I4B), ALLOCATABLE :: edge2elem(:, :)
  LOGICAL(LGT), ALLOCATABLE :: amask(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  IF (showTime) CALL TypeCPUTime%SetStartTime()
#endif

#ifdef DEBUG_VER
  problem = tEdgeInMesh .EQ. 0
  IF (problem) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[INTERNAL ERROR] :: Total number of edges are zero.')
    RETURN
  END IF
#endif

  telems = SIZE(elementData)

  CALL Reallocate(edge2elem, 4, tEdgeInMesh)
  CALL Reallocate(amask, tEdgeInMesh)
  amask = math%no

  elemloop: DO iel = 1, telems

    problem = .NOT. elementData(iel)%ptr%isActive
    IF (problem) CYCLE elemloop

    tedges = SIZE(elementData(iel)%ptr%globalEdges)
    DO ii = 1, tedges
      aint = ABS(elementData(iel)%ptr%globalEdges(ii))
      IF (amask(aint)) THEN
        edge2elem(2, aint) = iel
        edge2elem(4, aint) = ii
        amask(aint) = math%no
      ELSE
        edge2elem(1, aint) = iel
        edge2elem(3, aint) = ii
        amask(aint) = math%yes
      END IF
    END DO

  END DO elemloop

  elemloop2: DO iel = 1, telems

    problem = .NOT. elementData(iel)%ptr%isActive
    IF (problem) CYCLE elemloop2

    tedges = SIZE(elementData(iel)%ptr%globalEdges)
    jj = 0
    temp1 = 0
    bndyflag = 0

    edgeloop2: DO ii = 1, tedges
      aint = ABS(elementData(iel)%ptr%globalEdges(ii))
      bint = edge2elem(1, aint)
      isok1 = bint .NE. iel
      isok2 = bint .NE. 0

      IF (amask(aint)) bndyflag(ii) = 1_I4B

      IF (isok1 .AND. isok2) THEN
        jj = jj + 1
        temp1(1 + (jj - 1) * 3) = elementData(bint)%ptr%globalElemNum
        temp1(2 + (jj - 1) * 3) = edge2elem(4, aint)
        temp1(3 + (jj - 1) * 3) = edge2elem(3, aint)

      ELSE
        cint = edge2elem(2, aint)
        IF (cint .NE. 0) THEN
          jj = jj + 1
          temp1(1 + (jj - 1) * 3) = elementData(cint)%ptr%globalElemNum
          temp1(2 + (jj - 1) * 3) = edge2elem(3, aint)
          temp1(3 + (jj - 1) * 3) = edge2elem(4, aint)
        END IF
      END IF

    END DO edgeloop2

    aint = jj * 3
    CALL Reallocate(elementData(iel)%ptr%globalElements, aint)
    elementData(iel)%ptr%globalElements = temp1(1:aint)

    aint = tedges - jj
    CALL Reallocate(elementData(iel)%ptr%boundaryData, aint)
    isbndy = jj .NE. tedges

    IF (isbndy) THEN
      elementData(iel)%ptr%elementType = TypeElem%domainBoundary
      jj = 0
      DO ii = 1, tedges
        IF (bndyflag(ii) .NE. 0) THEN
          jj = jj + 1
          elementData(iel)%ptr%boundaryData(jj) = ii
        END IF
      END DO
    ELSE
      elementData(iel)%ptr%elementType = TypeElem%internal
    END IF

  END DO elemloop2

  IF (ALLOCATED(amask)) DEALLOCATE(amask)
  IF (ALLOCATED(edge2elem)) DEALLOCATE(edge2elem)

#ifdef DEBUG_VER
  IF (showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName // " : " // myName // &
                 " : time : " // &
                 ToString(TypeCPUTime%GetTime()), unitno=stdout)
  END IF
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END SUBROUTINE InitiateElementToElements2D

!----------------------------------------------------------------------------
!                                               InitiateElementToElements1D
!----------------------------------------------------------------------------

SUBROUTINE InitiateElementToElements1D(elementData, tNodesInMesh, &
                                       showTime, local_nptrs)
  TYPE(ElemDataPointer_), INTENT(INOUT) :: elementData(:)
  INTEGER(I4B), INTENT(IN) :: tNodesInMesh
  LOGICAL(LGT), INTENT(IN) :: showTime
  INTEGER(I4B), INTENT(IN) :: local_nptrs(:)

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "InitiateElementToElements1D()"
  TYPE(CPUTime_) :: TypeCPUTime
#endif

  LOGICAL(LGT) :: problem, isok1, isok2, isbndy
  INTEGER(I4B) :: telems, iel, aint, bint, tNodes, ii, jj, temp1(3 * 2), &
    cint, bndyflag(2)
  INTEGER(I4B), ALLOCATABLE :: node2elem(:, :)
  LOGICAL(LGT), ALLOCATABLE :: amask(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  IF (showTime) CALL TypeCPUTime%SetStartTime()
#endif

#ifdef DEBUG_VER
  problem = tNodesInMesh .EQ. 0
  IF (problem) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      '[INTERNAL ERROR] :: Total number of nodes are zero.')
    RETURN
  END IF
#endif

  telems = SIZE(elementData)

  CALL Reallocate(node2elem, 4, tNodesInMesh)
  CALL Reallocate(amask, tNodesInMesh)
  amask = math%no

  elemloop: DO iel = 1, telems

    problem = .NOT. elementData(iel)%ptr%isActive
    IF (problem) CYCLE elemloop

    DO ii = 1, 2
      aint = elementData(iel)%ptr%globalNodes(ii)
      aint = local_nptrs(aint)
      IF (amask(aint)) THEN
        node2elem(2, aint) = iel
        node2elem(4, aint) = ii
        amask(aint) = math%no
      ELSE
        node2elem(1, aint) = iel
        node2elem(3, aint) = ii
        amask(aint) = math%yes
      END IF
    END DO

  END DO elemloop

  tNodes = 2
  elemloop2: DO iel = 1, telems

    problem = .NOT. elementData(iel)%ptr%isActive
    IF (problem) CYCLE elemloop2

    jj = 0
    temp1 = 0
    bndyflag = 0
    DO ii = 1, 2
      aint = elementData(iel)%ptr%globalNodes(ii)
      aint = local_nptrs(aint)
      bint = node2elem(1, aint)
      isok1 = bint .NE. iel
      isok2 = bint .NE. 0

      IF (amask(aint)) bndyflag(ii) = 1_I4B

      IF (isok1 .AND. isok2) THEN
        jj = jj + 1
        temp1(1 + (jj - 1) * 3) = elementData(bint)%ptr%globalElemNum
        temp1(2 + (jj - 1) * 3) = node2elem(4, aint)
        temp1(3 + (jj - 1) * 3) = node2elem(3, aint)

      ELSE
        cint = node2elem(2, aint)
        IF (cint .NE. 0) THEN
          jj = jj + 1
          temp1(1 + (jj - 1) * 3) = elementData(cint)%ptr%globalElemNum
          temp1(2 + (jj - 1) * 3) = node2elem(3, aint)
          temp1(3 + (jj - 1) * 3) = node2elem(4, aint)
        END IF
      END IF
    END DO

    aint = jj * 3
    CALL Reallocate(elementData(iel)%ptr%globalElements, aint)
    elementData(iel)%ptr%globalElements = temp1(1:aint)

    aint = tNodes - jj
    CALL Reallocate(elementData(iel)%ptr%boundaryData, aint)
    isbndy = jj .NE. tNodes

    IF (isbndy) THEN
      elementData(iel)%ptr%elementType = TypeElem%domainBoundary
      jj = 0
      DO ii = 1, tNodes
        IF (bndyflag(ii) .NE. 0) THEN
          jj = jj + 1
          elementData(iel)%ptr%boundaryData(jj) = ii
        END IF
      END DO
    ELSE
      elementData(iel)%ptr%elementType = TypeElem%internal
    END IF

  END DO elemloop2

  IF (ALLOCATED(amask)) DEALLOCATE(amask)
  IF (ALLOCATED(node2elem)) DEALLOCATE(node2elem)

#ifdef DEBUG_VER
  IF (showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName // " : " // myName // &
                 " : time : " // &
                 ToString(TypeCPUTime%GetTime()), unitno=stdout)
  END IF
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END SUBROUTINE InitiateElementToElements1D

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END MODULE AbstractMeshUtility
