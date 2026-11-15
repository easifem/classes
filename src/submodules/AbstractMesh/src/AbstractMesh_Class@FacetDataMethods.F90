! This program is a part of EASIFEM library
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
USE AbstractMeshUtility, ONLY: GetFacetDataFromElemData
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractMesh_Class@FacetDataMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                     InitiateFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateFacetElements()"
#endif
INTEGER(I4B) :: iel, tface, telements
LOGICAL(LGT) :: isok
LOGICAL(LGT), ALLOCATABLE :: masks(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! point and curve meshes have no facet elements
isok = obj%xidim .LE. 1
IF (isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = obj%isFacetDataInitiated
IF (isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = .NOT. obj%isElementToElements()
IF (isok) CALL obj%InitiateElementToElements()

isok = .NOT. obj%IsBoundaryData()
IF (isok) CALL obj%InitiateBoundaryData()

tface = obj%tfaces

#ifdef DEBUG_VER
isok = tface .NE. 0
CALL AssertError1(isok, myName, "facet elements not found in mesh.")
#endif

! facetData
ALLOCATE (obj%facetData(tface))
CALL Reallocate(masks, tface)
masks = .FALSE.

obj%isFacetDataInitiated = .TRUE.

telements = obj%GetTotalElements()
elemloop: DO iel = 1, telements
  isok = .NOT. obj%isElementActive(globalElement=iel, islocal=.TRUE.)
  IF (isok) CYCLE elemloop
  CALL GetFacetDataFromElemData(elementData=obj%elementData(iel)%ptr, &
                                facetData=obj%facetData, masks=masks, &
                                nsd=obj%nsd)
END DO elemloop

IF (ALLOCATED(masks)) DEALLOCATE (masks)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_InitiateFacetElements

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE FacetDataMethods
