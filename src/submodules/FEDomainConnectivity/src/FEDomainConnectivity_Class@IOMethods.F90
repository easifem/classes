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

SUBMODULE(FEDomainConnectivity_Class) IOMethods
USE Display_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                  DisplayFacetToCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayFacetToCellData
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: abool
CHARACTER(:), ALLOCATABLE :: astr

abool = ALLOCATED(obj%facetToCell)
CALL Display(abool, "FEDomainConnectivity_::obj%facetToCell ALLOCATED: ",  &
  & unitno=unitno)
IF (.NOT. abool) RETURN

CALL Display(msg, unitno=unitno)

tsize = SIZE(obj%facetToCell)
astr = "facetID, globalCell(master), localFacetID(master), dim(master), &
  & entityNum(master), globalCell(slave), localFacetID(slave), dim(slave),&
  & entityNum(slave) "

CALL Display(astr, unitno=unitno)

DO ii = 1, tsize
  astr = Tostring(obj%facetToCell(ii)%facetID)//", "// &
    & Tostring(obj%facetToCell(ii)%globalCellData(:, 1))//", "// &
    & Tostring(obj%facetToCell(ii)%globalCellData(:, 2))
  CALL Display(astr, unitno=unitno)
END DO

END PROCEDURE obj_DisplayFacetToCellData

END SUBMODULE IOMethods
