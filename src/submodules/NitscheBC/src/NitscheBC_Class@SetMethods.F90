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

SUBMODULE(NitscheBC_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_SetCellData
INTEGER(I4B) :: tsize
INTEGER(I4B) :: tmeshID
INTEGER(I4B) :: tcell
INTEGER(I4B) :: a
INTEGER(I4B) :: b
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: maxCellEntity
LOGICAL(LGT) :: isVar
CHARACTER(*), PARAMETER :: myName = "bc_SetCellData"
CLASS(DomainConnectivity_), POINTER :: domCon
INTEGER(I4B), ALLOCATABLE :: dimTag(:, :)
INTEGER(I4B), ALLOCATABLE :: intvec(:)

tmeshID = SIZE(meshID)
tsize = SIZE(localID)

CALL obj%GetQuery(isSelectionByMeshID=isVar)
IF (.NOT. isVar) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & "NitscheBC_::obj has isSelectionByMeshID = .FALSE. ")
END IF

IF (SIZE(tFacetElements) .NE. SIZE(meshID)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Size of tFacetElements and meshID are not same')
END IF

IF (ANY(meshID .GT. tsize)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some of meshID are greater than size of localID')
END IF

tcell = SUM(tFacetElements)
CALL Reallocate(obj%cellElem, tcell)
CALL Reallocate(obj%localFacetID, tcell)
CALL Reallocate(dimTag, tcell, 2)

b = 0
domCon => NULL()
DO ii = 1, tmeshID
  jj = localID(meshID(ii))
  domCon => domConList(jj)%ptr
  a = b + 1
  b = b + tFacetElements(ii)
  obj%cellElem(a:b) = domCon%masterCellNumber()
  obj%localFacetID(a:b) = domCon%masterFacetLocalID()
  dimTag(a:b, :) = domCon%masterDimTag(isTranspose=.TRUE.)
END DO

CALL QuickSort( &
  & vect1=dimTag(:, 2), &
  & vect2=obj%cellElem, &
  & vect3=obj%localFacetID, &
  & low=1, &
  & high=tcell)

maxCellEntity = MAXVAL(dimTag(:, 2))

CALL Reallocate(obj%cellEntity, maxCellEntity + 1)
CALL Reallocate(intvec, maxCellEntity)

DO ii = 1, tcell
  intvec(dimTag(ii, 2)) = intvec(dimTag(ii, 2)) + 1
END DO

obj%cellEntity(1) = 1

DO ii = 2, maxCellEntity + 1
  obj%cellEntity(ii) = obj%cellEntity(ii - 1) + intvec(ii - 1)
END DO

domCon => NULL()
IF (ALLOCATED(intvec)) DEALLOCATE (intvec)
IF (ALLOCATED(dimTag)) DEALLOCATE (dimTag)

END PROCEDURE bc_SetCellData

END SUBMODULE SetMethods
