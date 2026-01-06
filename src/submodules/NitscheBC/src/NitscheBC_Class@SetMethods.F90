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

SUBMODULE(NitscheBC_Class) SetMethods
USE Display_Method, ONLY: ToString
USE BaseType, ONLY: math => TypeMathOpt
USE DomainConnectivity_Class, ONLY: DomainConnectivity_
USE ReallocateUtility, ONLY: Reallocate
USE SortUtility, ONLY: QuickSort

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCellData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetCellData()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: tsize, tMeshID, tcell, a, b, ii, jj, maxCellEntity
LOGICAL(LGT) :: isVar
INTEGER(I4B), ALLOCATABLE :: dimTag(:, :)
INTEGER(I4B), ALLOCATABLE :: intvec(:)
CLASS(DomainConnectivity_), POINTER :: domCon

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tmeshID = SIZE(meshID)
tsize = SIZE(localID)

#ifdef DEBUG_VER
CALL obj%GetParam(isSelectionByMeshID=isok)
CALL AssertError1(isok, myName, &
                  "NitscheBC_::obj has isSelectionByMeshID = .FALSE. ")
#endif

#ifdef DEBUG_VER
a = SIZE(tFacetElements)
b = SIZE(meshID)
isok = a .EQ. b
CALL AssertError1(isok, myName, &
                 "Size of tFacetElements ("//ToString(a)//") and meshID ("// &
                  ToString(b)//") are not same")
#endif

#ifdef DEBUG_VER
isok = ALL(meshID .LE. tsize)
CALL AssertError1(isok, myName, &
                  "Some of meshID are greater than size of localID")
#endif

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
  dimTag(a:b, :) = domCon%masterDimTag(isTranspose=math%yes)
END DO

CALL QuickSort( &
  vect1=dimTag(:, 2), vect2=obj%cellElem, vect3=obj%localFacetID, &
  low=1, high=tcell)

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetCellData

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
