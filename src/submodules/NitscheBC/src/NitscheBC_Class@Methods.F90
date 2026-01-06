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

SUBMODULE(NitscheBC_Class) Methods
USE AbstractBC_Class, ONLY: AbstractBCDeallocate
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE DomainConnectivity_Class, ONLY: DomainConnectivity_
USE InputUtility, ONLY: Input
USE ReallocateUtility, ONLY: Reallocate
USE SortUtility, ONLY: QuickSort

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractBCDeallocate(obj=obj)

isok = ALLOCATED(obj%cellElem)
IF (isok) DEALLOCATE (obj%cellElem)

isok = ALLOCATED(obj%localFacetID)
IF (isok) DEALLOCATE (obj%localFacetID)

isok = ALLOCATED(obj%cellEntity)
IF (isok) DEALLOCATE (obj%cellEntity)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                       Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate_Vector()"
#endif
#include "../../include/deallocate_vector.F90"
END PROCEDURE obj_Deallocate_Vector

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate_Ptr_Vector()"
#endif
#include "../../include/deallocate_vector_ptr.F90"
END PROCEDURE obj_Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                         GetMinCellEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMinCellEntity
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj%cellEntity)
ans = 0
IF (isok) ans = LBOUND(obj%cellEntity, 1)
END PROCEDURE obj_GetMinCellEntity

!----------------------------------------------------------------------------
!                                                           GetMaxCellEntity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxCellEntity
LOGICAL(LGT) :: isok
isok = ALLOCATED(obj%cellEntity)
ans = 0
IF (isok) ans = UBOUND(obj%cellEntity, 1) - 1
END PROCEDURE obj_GetMaxCellEntity

!----------------------------------------------------------------------------
!                                                        IsCellEntityPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsCellEntityPresent
INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok, abool

isok = ALLOCATED(obj%cellEntity)
ans = .FALSE.

IF (isok) THEN
  tsize = SIZE(obj%cellEntity)
  abool = entityNum .LT. tsize
  IF (abool) THEN
    ii = obj%cellEntity(entityNum + 1) - obj%cellEntity(entityNum)
    ans = ii .GT. 0
  END IF
END IF
END PROCEDURE obj_IsCellEntityPresent

!----------------------------------------------------------------------------
!                                                             GetStartIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetStartIndex
ans = obj%cellEntity(entityNum)
END PROCEDURE obj_GetStartIndex

!----------------------------------------------------------------------------
!                                                                GetEndIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetEndIndex
ans = obj%cellEntity(entityNum + 1) - 1
END PROCEDURE obj_GetEndIndex

!----------------------------------------------------------------------------
!                                                               GetCellElem
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellElem
ans = obj%cellElem(entityNum)
END PROCEDURE obj_GetCellElem

!----------------------------------------------------------------------------
!                                                            GetLocalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalFacetID
ans = obj%localFacetID(entityNum)
END PROCEDURE obj_GetLocalFacetID

!----------------------------------------------------------------------------
!                                                        GetNitscheBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNitscheBCPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNitscheBCPointer"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: bcNo0, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(bc)

bcNo0 = Input(default=tsize, option=bcNo)

#ifdef DEBUG_VER
isok = bcNo0 .LE. tsize
CALL AssertError1(isok, myName, &
        "bcNo0="//ToString(bcNo0)//" is out of bound tsize="//ToString(tsize))
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(bc(bcNo0)%ptr)
CALL AssertError1(isok, myName, &
                  "bc("//ToString(bcNo0)//")%ptr is not ASSOCIATED")
#endif

ans => bc(bcNo0)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNitscheBCPointer

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display_Vector()"
#endif
#include "../../include/display_vector.F90"
END PROCEDURE obj_Display_Vector

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display_Ptr_Vector()"
#endif

#include "../../include/display_vector_ptr.F90"
END PROCEDURE obj_Display_Ptr_Vector

!----------------------------------------------------------------------------
!                                                                SetCellData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCellData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetCellData()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: tsize, tMeshID, tcell, a, b, ii, jj, maxCellEntity
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
!                                                           Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
