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

SUBMODULE(Mesh_Class) ElementDataMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                 InitiateElementToElements
!----------------------------------------------------------------------------

! nodeToElements

MODULE PROCEDURE obj_InitiateElementToElements
CHARACTER(*), PARAMETER :: myName = "obj_InitiateElementToElements()"
INTEGER(I4B) :: i, j, r, iel1, tFace, iFace1, NNS1, pt1, &
  & iel2, iFace2, NNS2, localElem1, temp4(4)
INTEGER(I4B), ALLOCATABLE :: global_nptrs1(:),   &
  & global_nptrsFace1(:), n2e1(:), global_nptrs2(:), &
  & global_nptrsFace2(:), intvec_temp_1(:), intvec_temp_2(:)
LOGICAL(LGT) :: Found, problem, isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

problem = obj%elemType .EQ. 0 .OR. obj%elemType .EQ. Point1
IF (problem) RETURN

IF (.NOT. ASSOCIATED(obj%refelem)) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Mesh_::obj%refelem not ASSOCIATED.")
  RETURN
END IF

problem = obj%isElementToElementsInitiated
IF (problem) THEN
  CALL e%RaiseWarning(modName//"::"//myName//" - "// &
    & "Element to element information is already initiated.")
  RETURN
END IF

isok = ALLOCATED(obj%facetElements)
IF (.NOT. isok) THEN
  IF (obj%xidim .GT. 0) THEN
    temp4 = TotalEntities(obj%refelem%name)
    ALLOCATE (obj%facetElements(temp4(obj%xidim)))
    CALL GetFacetElements(refelem=obj%refelem, ans=obj%facetElements)
  END IF
END IF

tFace = SIZE(obj%facetElements)
! Total number of facet elements

obj%isElementToElementsInitiated = .TRUE.

isok = obj%isNodeToElementsInitiated
IF (.NOT. isok) CALL obj%InitiateNodeToElements()

DO localElem1 = 1, obj%tElements
  iel1 = obj%GetGlobalElemNumber(localElem1)
  global_nptrs1 = obj%GetConnectivity(iel1)

  ! Getting node numbers of element iel1
  DO iFace1 = 1, tFace
    FOUND = .FALSE.
    intvec_temp_2 = GetConnectivity(obj%facetElements(iFace1))
    problem = SIZE(intvec_temp_2) .EQ. 0

    IF (problem) THEN
      CALL Display(obj%facetElements(iFace1), "iFace1: ")
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: Error in getting facet elements.')
      RETURN
    END IF

    global_nptrsFace1 = global_nptrs1(intvec_temp_2)
    ! Getting global node number in face iFace1
    NNS1 = SIZE(global_nptrsFace1)
    ! number of nodes in iFace1
    pt1 = global_nptrsFace1(1)
    ! select a point on facet iFace1
    n2e1 = obj%GetNodeToElements(GlobalNode=pt1)
    ! Get elements connected to the node pt1
    DO iel2 = 1, SIZE(n2e1)
      IF (iel1 .EQ. n2e1(iel2)) CYCLE
      global_nptrs2 = obj%GetConnectivity(n2e1(iel2))

      DO iFace2 = 1, tFace
        ! Getting total number of nodes in iFace2
        intvec_temp_1 = GetConnectivity(obj%facetElements(iFace2))
        global_nptrsFace2 = global_nptrs2(intvec_temp_1)
        NNS2 = SIZE(global_nptrsFace2)
        r = 0

        DO i = 1, NNS2
          DO j = 1, NNS1
            isok = global_nptrsFace2(i) .EQ. global_nptrsFace1(j)
            IF (isok) r = r + 1
          END DO
        END DO

        isok = r .EQ. NNS1
        IF (isok) THEN
          CALL APPEND(obj%elementData(localElem1)%globalElements, &
            & [n2e1(iel2), iFace1, iFace2])
          FOUND = .TRUE.
          EXIT
        END IF

      END DO
      IF (FOUND) EXIT
    END DO
  END DO

  isok = INT(SIZE(obj%elementData(localElem1)%globalElements) / 3) &
    & .NE. tFace
  IF (isok) THEN
    obj%elementData(localElem1)%elementType = BOUNDARY_ELEMENT
  END IF
END DO

IF (ALLOCATED(global_nptrs1)) DEALLOCATE (global_nptrs1)
IF (ALLOCATED(global_nptrs2)) DEALLOCATE (global_nptrs2)
IF (ALLOCATED(global_nptrsFace1)) DEALLOCATE (global_nptrsFace1)
IF (ALLOCATED(global_nptrsFace2)) DEALLOCATE (global_nptrsFace2)
IF (ALLOCATED(n2e1)) DEALLOCATE (n2e1)
END PROCEDURE obj_InitiateElementToElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ElementDataMethods
