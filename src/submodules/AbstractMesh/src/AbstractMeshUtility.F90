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
USE GlobalData
USE Display_Method
USE ReallocateUtility
USE CPUTime_Class
USE ExceptionHandler_Class, ONLY: e
USE ElemData_Class
USE FacetData_Class
USE ReferenceElement_Method

IMPLICIT NONE
PRIVATE

PUBLIC :: InitiateElementToElements3D
PUBLIC :: InitiateElementToElements2D
PUBLIC :: InitiateElementToElements1D
PUBLIC :: GetFacetDataFromElemData

CHARACTER(*), PARAMETER :: modName = "AbstractMeshUtility"

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

  ! now lets find the facets located on the DOMAIN_BOUNDARY_ELEMENT
  IF (nsd .EQ. 3) THEN
    CALL Help_GetFacetDataFromElemData(elementData, facetData, masks,  &
      & elementData%globalFaces)

  ELSE
    CALL Help_GetFacetDataFromElemData(elementData, facetData, masks,  &
      & elementData%globalEdges)

  END IF

END SUBROUTINE GetFacetDataFromElemData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_GetFacetDataFromElemData(elementData, facetData, masks, faces)
  TYPE(ElemData_), INTENT(IN) :: elementData
  TYPE(FacetData_), INTENT(INOUT) :: facetData(:)
  LOGICAL(LGT), INTENT(INOUT) :: masks(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: faces(:)

  ! internal variables
  INTEGER(I4B) :: ii, iface, tsize, jj

  ! now lets find the facets located on the DOMAIN_BOUNDARY_ELEMENT

  tsize = SIZE(elementData%boundaryData)
  DO ii = 1, tsize

    ! this is local face number in master cell
    jj = elementData%boundaryData(ii)

    ! this is global facet number in the mesh
    iface = faces(jj)

    ! do not count two times
    IF (masks(iface)) CYCLE

    ! we have found the facet element so mark it.
    masks(iface) = .TRUE.

    ! all elements found in this loop are located inside the domain
    facetData(iface)%elementType = DOMAIN_BOUNDARY_ELEMENT
    facetData(iface)%masterCellNumber = elementData%globalElemNum
    facetData(iface)%masterLocalFacetID = jj

    facetData(iface)%slaveCellNumber = 0_I4B
    facetData(iface)%slaveLocalFacetID = 0_I4B

  END DO

  tsize = SIZE(elementData%globalElements)

  DO ii = 1, tsize, 3

    ! this is local face number in master cell
    jj = elementData%globalElements(ii + 1)

    ! this is global facet number in the mesh
    iface = faces(jj)

    ! do not count two times
    IF (masks(iface)) CYCLE

    ! we have found the facet element so mark it.
    masks(iface) = .TRUE.

    ! all elements found in this loop are located inside the domain
    facetData(iface)%elementType = INTERNAL_ELEMENT

    facetData(iface)%slaveCellNumber = elementData%globalElements(ii)
    facetData(iface)%slaveLocalFacetID = elementData%globalElements(ii + 2)

    facetData(iface)%masterCellNumber = elementData%globalElemNum
    facetData(iface)%masterLocalFacetID = jj

  END DO

END SUBROUTINE Help_GetFacetDataFromElemData

!----------------------------------------------------------------------------
!                                               InitiateElementToElements3D
!----------------------------------------------------------------------------

SUBROUTINE InitiateElementToElements3D(elementData, tFaceInMesh, showTime)
  TYPE(ElemData_), INTENT(INOUT) :: elementData(:)
  INTEGER(I4B), INTENT(IN) :: tFaceInMesh
  LOGICAL(LGT), INTENT(IN) :: showTime

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateElementToElements3D()"
  LOGICAL(LGT) :: problem, isok1, isok2, isbndy
  INTEGER(I4B) :: telems, iel, aint, bint, tfaces, ii, jj, &
    & temp1(3 * REFELEM_MAX_FACES), cint, bndyflag(REFELEM_MAX_FACES)
  INTEGER(I4B), ALLOCATABLE :: face2elem(:, :)
  LOGICAL(LGT), ALLOCATABLE :: amask(:)
  TYPE(CPUTime_) :: TypeCPUTime

  IF (showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

#ifdef DEBUG_VER
  problem = tFaceInMesh .EQ. 0
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: Total number of faces are zero.')
    RETURN
  END IF
#endif

  telems = SIZE(elementData)

  CALL Reallocate(face2elem, 4, tFaceInMesh)
  CALL Reallocate(amask, tFaceInMesh)
  amask = .FALSE.

  DO iel = 1, telems

    problem = .NOT. elementData(iel)%isActive
    IF (problem) CYCLE

#ifdef DEBUG_VER
    problem = .NOT. ALLOCATED(elementData(iel)%globalFaces)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: local element number = '//tostring(iel)//  &
        & " does not have globalFaces data allocated.")
      RETURN
    END IF
#endif

    tfaces = SIZE(elementData(iel)%globalFaces)
    DO ii = 1, tfaces
      aint = ABS(elementData(iel)%globalFaces(ii))
      IF (amask(aint)) THEN
        face2elem(2, aint) = iel
        face2elem(4, aint) = ii
        amask(aint) = .FALSE.
      ELSE
        face2elem(1, aint) = iel
        face2elem(3, aint) = ii
        amask(aint) = .TRUE.
      END IF
    END DO

  END DO

  DO iel = 1, telems

    problem = .NOT. elementData(iel)%isActive
    IF (problem) CYCLE

    tfaces = SIZE(elementData(iel)%globalFaces)
    jj = 0
    temp1 = 0
    bndyflag = 0
    DO ii = 1, tfaces
      aint = ABS(elementData(iel)%globalFaces(ii))
      bint = face2elem(1, aint)
      isok1 = bint .NE. iel
      isok2 = bint .NE. 0

      IF (amask(aint)) bndyflag(ii) = 1_I4B

      IF (isok1 .AND. isok2) THEN
        jj = jj + 1
        temp1(1 + (jj - 1) * 3) = elementData(bint)%globalElemNum
        temp1(2 + (jj - 1) * 3) = face2elem(4, aint)
        temp1(3 + (jj - 1) * 3) = face2elem(3, aint)

      ELSE
        cint = face2elem(2, aint)
        IF (cint .NE. 0) THEN
          jj = jj + 1
          temp1(1 + (jj - 1) * 3) =  &
            & elementData(cint)%globalElemNum
          temp1(2 + (jj - 1) * 3) = face2elem(3, aint)
          temp1(3 + (jj - 1) * 3) = face2elem(4, aint)
        END IF
      END IF
    END DO

#ifdef DEBUG_VER
    IF (jj .EQ. 0) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: jj = 0 found, somethign is wrong')
      RETURN
    END IF
#endif

    aint = jj * 3
    CALL Reallocate(elementData(iel)%globalElements, aint)
    elementData(iel)%globalElements = temp1(1:aint)

    aint = tfaces - jj
    CALL Reallocate(elementData(iel)%boundaryData, aint)
    isbndy = jj .NE. tfaces

    IF (isbndy) THEN
      elementData(iel)%elementType = TypeElem%domainBoundary
      jj = 0
      DO ii = 1, tfaces
        IF (bndyflag(ii) .NE. 0) THEN
          jj = jj + 1
          elementData(iel)%boundaryData(jj) = ii
        END IF
      END DO
    ELSE
      elementData(iel)%elementType = TypeElem%internal
    END IF

  END DO

  IF (ALLOCATED(face2elem)) DEALLOCATE (face2elem)
  IF (ALLOCATED(amask)) DEALLOCATE (amask)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

  IF (showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName//  &
      & " : time : "//  &
      & tostring(TypeCPUTime%GetTime()), unitno=stdout)
  END IF

END SUBROUTINE InitiateElementToElements3D

!----------------------------------------------------------------------------
!                                                InitiateElementToElements2D
!----------------------------------------------------------------------------

SUBROUTINE InitiateElementToElements2D(elementData, tEdgeInMesh, showTime)
  TYPE(ElemData_), INTENT(INOUT) :: elementData(:)
  INTEGER(I4B), INTENT(IN) :: tEdgeInMesh
  LOGICAL(LGT), INTENT(IN) :: showTime

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "InitiateElementToElements2D()_old"
  LOGICAL(LGT) :: problem, isok1, isok2, isbndy
  INTEGER(I4B) :: telems, iel, aint, bint, tedges, ii, jj, temp1(3 * 4), &
    & cint, bndyflag(4)
  INTEGER(I4B), ALLOCATABLE :: edge2elem(:, :)
  LOGICAL(LGT), ALLOCATABLE :: amask(:)
  TYPE(CPUTime_) :: TypeCPUTime

  IF (showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

#ifdef DEBUG_VER
  problem = tEdgeInMesh .EQ. 0
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: Total number of edges are zero.')
    RETURN
  END IF
#endif

  telems = SIZE(elementData)

  CALL Reallocate(edge2elem, 4, tEdgeInMesh)
  CALL Reallocate(amask, tEdgeInMesh)
  amask = .FALSE.

  DO iel = 1, telems

    problem = .NOT. elementData(iel)%isActive
    IF (problem) CYCLE

#ifdef DEBUG_VER
    problem = .NOT. ALLOCATED(elementData(iel)%globalEdges)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: local element number = '//tostring(iel)//  &
        & " does not have globalEdges data allocated.")
      RETURN
    END IF
#endif

    tedges = SIZE(elementData(iel)%globalEdges)
    DO ii = 1, tedges
      aint = ABS(elementData(iel)%globalEdges(ii))
      IF (amask(aint)) THEN
        edge2elem(2, aint) = iel
        edge2elem(4, aint) = ii
        amask(aint) = .FALSE.
      ELSE
        edge2elem(1, aint) = iel
        edge2elem(3, aint) = ii
        amask(aint) = .TRUE.
      END IF
    END DO

  END DO

  DO iel = 1, telems

    problem = .NOT. elementData(iel)%isActive
    IF (problem) CYCLE

    tedges = SIZE(elementData(iel)%globalEdges)
    jj = 0
    temp1 = 0
    bndyflag = 0
    DO ii = 1, tedges
      aint = ABS(elementData(iel)%globalEdges(ii))
      bint = edge2elem(1, aint)
      isok1 = bint .NE. iel
      isok2 = bint .NE. 0

      IF (amask(aint)) bndyflag(ii) = 1_I4B

      IF (isok1 .AND. isok2) THEN
        jj = jj + 1
        temp1(1 + (jj - 1) * 3) = elementData(bint)%globalElemNum
        temp1(2 + (jj - 1) * 3) = edge2elem(4, aint)
        temp1(3 + (jj - 1) * 3) = edge2elem(3, aint)

      ELSE
        cint = edge2elem(2, aint)
        IF (cint .NE. 0) THEN
          jj = jj + 1
          temp1(1 + (jj - 1) * 3) = elementData(cint)%globalElemNum
          temp1(2 + (jj - 1) * 3) = edge2elem(3, aint)
          temp1(3 + (jj - 1) * 3) = edge2elem(4, aint)
        END IF
      END IF
    END DO

#ifdef DEBUG_VER
    IF (jj .EQ. 0) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: jj = 0 found, somethign is wrong')
      RETURN
    END IF
#endif

    aint = jj * 3
    CALL Reallocate(elementData(iel)%globalElements, aint)
    elementData(iel)%globalElements = temp1(1:aint)

    aint = tedges - jj
    CALL Reallocate(elementData(iel)%boundaryData, aint)
    isbndy = jj .NE. tedges

    IF (isbndy) THEN
      elementData(iel)%elementType = TypeElem%domainBoundary
      jj = 0
      DO ii = 1, tedges
        IF (bndyflag(ii) .NE. 0) THEN
          jj = jj + 1
          elementData(iel)%boundaryData(jj) = ii
        END IF
      END DO
    ELSE
      elementData(iel)%elementType = TypeElem%internal
    END IF

  END DO

  IF (ALLOCATED(amask)) DEALLOCATE (amask)
  IF (ALLOCATED(edge2elem)) DEALLOCATE (edge2elem)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

  IF (showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName//  &
      & " : time : "//  &
      & tostring(TypeCPUTime%GetTime()), unitno=stdout)
  END IF

END SUBROUTINE InitiateElementToElements2D

!----------------------------------------------------------------------------
!                                               InitiateElementToElements1D
!----------------------------------------------------------------------------

SUBROUTINE InitiateElementToElements1D(elementData, tNodesInMesh,  &
  & showTime, local_nptrs)
  TYPE(ElemData_), INTENT(INOUT) :: elementData(:)
  INTEGER(I4B), INTENT(IN) :: tNodesInMesh
  LOGICAL(LGT), INTENT(IN) :: showTime
  INTEGER(I4B), INTENT(IN) :: local_nptrs(:)

!   ! internal variables
  CHARACTER(*), PARAMETER :: myName = "InitiateElementToElements1D()"
  LOGICAL(LGT) :: problem, isok1, isok2, isbndy
  INTEGER(I4B) :: telems, iel, aint, bint, tNodes, ii, jj, temp1(3 * 2), &
    & cint, bndyflag(2)
  INTEGER(I4B), ALLOCATABLE :: node2elem(:, :)
  LOGICAL(LGT), ALLOCATABLE :: amask(:)
  TYPE(CPUTime_) :: TypeCPUTime

  IF (showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

#ifdef DEBUG_VER
  problem = tNodesInMesh .EQ. 0
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: Total number of nodes are zero.')
    RETURN
  END IF
#endif

  telems = SIZE(elementData)

  CALL Reallocate(node2elem, 4, tNodesInMesh)
  CALL Reallocate(amask, tNodesInMesh)
  amask = .FALSE.

  DO iel = 1, telems

    problem = .NOT. elementData(iel)%isActive
    IF (problem) CYCLE

#ifdef DEBUG_VER
    problem = .NOT. ALLOCATED(elementData(iel)%globalNodes)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: local element number = '//tostring(iel)//  &
        & " does not have globalNodes data allocated.")
      RETURN
    END IF
#endif

    DO ii = 1, 2
      aint = elementData(iel)%globalNodes(ii)
      aint = local_nptrs(aint)
      IF (amask(aint)) THEN
        node2elem(2, aint) = iel
        node2elem(4, aint) = ii
        amask(aint) = .FALSE.
      ELSE
        node2elem(1, aint) = iel
        node2elem(3, aint) = ii
        amask(aint) = .TRUE.
      END IF
    END DO

  END DO

  tNodes = 2
  DO iel = 1, telems

    problem = .NOT. elementData(iel)%isActive
    IF (problem) CYCLE

    jj = 0
    temp1 = 0
    bndyflag = 0
    DO ii = 1, 2
      aint = elementData(iel)%globalNodes(ii)
      aint = local_nptrs(aint)
      bint = node2elem(1, aint)
      isok1 = bint .NE. iel
      isok2 = bint .NE. 0

      IF (amask(aint)) bndyflag(ii) = 1_I4B

      IF (isok1 .AND. isok2) THEN
        jj = jj + 1
        temp1(1 + (jj - 1) * 3) = elementData(bint)%globalElemNum
        temp1(2 + (jj - 1) * 3) = node2elem(4, aint)
        temp1(3 + (jj - 1) * 3) = node2elem(3, aint)

      ELSE
        cint = node2elem(2, aint)
        IF (cint .NE. 0) THEN
          jj = jj + 1
          temp1(1 + (jj - 1) * 3) = elementData(cint)%globalElemNum
          temp1(2 + (jj - 1) * 3) = node2elem(3, aint)
          temp1(3 + (jj - 1) * 3) = node2elem(4, aint)
        END IF
      END IF
    END DO

#ifdef DEBUG_VER
    IF (jj .EQ. 0) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: jj = 0 found, somethign is wrong')
      RETURN
    END IF
#endif

    aint = jj * 3
    CALL Reallocate(elementData(iel)%globalElements, aint)
    elementData(iel)%globalElements = temp1(1:aint)

    aint = tNodes - jj
    CALL Reallocate(elementData(iel)%boundaryData, aint)
    isbndy = jj .NE. tNodes

    IF (isbndy) THEN
      elementData(iel)%elementType = TypeElem%domainBoundary
      jj = 0
      DO ii = 1, tNodes
        IF (bndyflag(ii) .NE. 0) THEN
          jj = jj + 1
          elementData(iel)%boundaryData(jj) = ii
        END IF
      END DO
    ELSE
      elementData(iel)%elementType = TypeElem%internal
    END IF

  END DO

  IF (ALLOCATED(amask)) DEALLOCATE (amask)
  IF (ALLOCATED(node2elem)) DEALLOCATE (node2elem)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

  IF (showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName//" : "//myName//  &
      & " : time : "//  &
      & tostring(TypeCPUTime%GetTime()), unitno=stdout)
  END IF

END SUBROUTINE InitiateElementToElements1D

END MODULE AbstractMeshUtility
