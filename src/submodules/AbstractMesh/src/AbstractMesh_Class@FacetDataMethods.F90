! ThIs program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! ThIs program is free software: you can redistribute it and/or modify
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

SUBMODULE(AbstractMesh_Class) FacetDataMethods
USE ReallocateUtility, ONLY: Reallocate
USE AbstractMeshUtility, ONLY: GetFacetDataFromElemData
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     InitiateFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetElements
CHARACTER(*), PARAMETER :: myName = "obj_InitiateFacetElements()"
INTEGER(I4B) :: iel, tface, telements
LOGICAL(LGT) :: problem
LOGICAL(LGT), ALLOCATABLE :: masks(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

problem = obj%xidim .EQ. 0
IF (problem) RETURN

problem = obj%isFacetDataInitiated
IF (problem) RETURN

problem = .NOT. obj%isElementToElements()
IF (problem) CALL obj%InitiateElementToElements()

problem = .NOT. obj%IsBoundaryData()
IF (problem) CALL obj%InitiateBoundaryData()

tface = obj%tfaces

problem = tface .EQ. 0
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[ERROR] :: Facet elements not found in the mesh')
  RETURN
END IF

! facetData
ALLOCATE (obj%facetData(tface))
CALL Reallocate(masks, tface)
masks = .FALSE.

obj%isFacetDataInitiated = .TRUE.

telements = obj%GetTotalElements()
DO iel = 1, telements
  problem = .NOT. obj%isElementActive(globalElement=iel, islocal=.TRUE.)
  IF (problem) CYCLE
  CALL GetFacetDataFromElemData(elementData=obj%elementData(iel)%ptr, &
                            facetData=obj%facetData, masks=masks, nsd=obj%nsd)

END DO

IF (ALLOCATED(masks)) DEALLOCATE (masks)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_InitiateFacetElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE FacetDataMethods
