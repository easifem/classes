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

SUBMODULE(DomainConnectivity_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: &
  modName = "Domain_Connectivity_Class@ContructorMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_Deallocate
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "dc_Deallocate()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  obj%isInitiated = .FALSE.
  obj%isFacetToCell = .FALSE.
  obj%isNodeToNode = .FALSE.
  obj%isCellToCell = .FALSE.
  IF (ALLOCATED(obj%facetToCell)) DEALLOCATE(obj%facetToCell)
  IF (ALLOCATED(obj%nodeToNode)) DEALLOCATE(obj%nodeToNode)
  IF (ALLOCATED(obj%cellToCell)) DEALLOCATE(obj%cellToCell)
  IF (ALLOCATED(obj%elemToElem)) DEALLOCATE(obj%elemToElem)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE dc_Deallocate

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_Deallocate2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "dc_Deallocate2()"
#endif

  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (ALLOCATED(obj)) THEN
    DO ii = 1, SIZE(obj)
      CALL obj(ii)%DEALLOCATE()
    END DO
    DEALLOCATE(obj)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE dc_Deallocate2

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_Deallocate3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "dc_Deallocate3()"
#endif

  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (ALLOCATED(obj)) THEN
    DO ii = 1, SIZE(obj)
      IF (ASSOCIATED(obj(ii)%ptr)) THEN
        CALL obj(ii)%ptr%DEALLOCATE()
        obj(ii)%ptr => NULL()
      END IF
    END DO
    DEALLOCATE(obj)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE dc_Deallocate3

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_Final
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "dc_Final()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL obj%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE dc_Final

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
