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

SUBMODULE(DomainConnectivity_Class) IOMethods
USE BaseMethod
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "DomainConnectivity_Class@IOMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                      DisplayFacetToCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_DisplayFacetToCellData
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "dc_DisplayFacetToCellData()"
#endif

  INTEGER(I4B) :: ii, tsize
  TYPE(String) :: astr
  !!
  !! main
  !!

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (.NOT. ALLOCATED(obj%facetToCell)) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF
  !! Display the message
  CALL Display(msg, unitno=unitno)
  !!
  tsize = SIZE(obj%facetToCell)
  astr = "facetID, globalCell(master), localFacetID(master), dim(master), &
  & entityNum(master), globalCell(slave), localFacetID(slave), dim(slave),&
  & entityNum(slave) "
  CALL Display(astr%chars(), unitno=unitno)
  DO ii = 1, tsize
    astr = TOSTRING(obj%facetToCell(ii)%facetID) // ", " // &
      TOSTRING(obj%facetToCell(ii)%GlobalCellData(:, 1)) // ", " // &
      TOSTRING(obj%facetToCell(ii)%GlobalCellData(:, 2))
    CALL Display(astr%chars(), unitno=unitno)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE dc_DisplayFacetToCellData

END SUBMODULE IOMethods
