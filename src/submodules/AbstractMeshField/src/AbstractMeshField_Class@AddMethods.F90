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

SUBMODULE(AbstractMeshField_Class) AddMethods
USE FEVariable_Method, ONLY: FEVariable_Size => Size
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             MasterAdd
!----------------------------------------------------------------------------

SUBROUTINE MasterAdd(val, indxVal, add_val, indx, tsize, scale)
  REAL(DFP), INTENT(INOUT) :: val(:)
  INTEGER(I4B), INTENT(INOUT) :: indxVal(:)
  REAL(DFP), INTENT(IN) :: add_val(:)
  INTEGER(I4B), INTENT(IN) :: indx
  INTEGER(I4B), INTENT(IN) :: tsize
  REAL(DFP), INTENT(IN) :: scale

  INTEGER(I4B) :: ii

  indxVal(indx + 1) = indxVal(indx) + tsize

  DO ii = indxVal(indx), indxVal(indx + 1) - 1
    val(ii) = val(ii) + scale * add_val(ii - indxVal(indx) + 1)
  END DO

END SUBROUTINE MasterAdd

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Add1()"
#endif

INTEGER(I4B) :: iel, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

iel = 1
isok = obj%fieldType .EQ. typefield%constant
IF (.NOT. isok) iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                                  islocal=islocal)

tsize = FEVariable_Size(fevar)

CALL MasterAdd(val=obj%val, indxVal=obj%indxVal, add_val=fevar%val, &
               scale=scale, indx=iel, tsize=tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Add1

!----------------------------------------------------------------------------
!                                                                        Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Add2()"
#endif

INTEGER(I4B) :: iel, telem, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

telem = obj%mesh%GetTotalElements()
tsize = FEVariable_Size(fevar)

DO iel = 1, telem
  CALL MasterAdd(val=obj%val, indxVal=obj%indxVal, add_val=fevar%val, &
                 scale=scale, indx=iel, tsize=tsize)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Add2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE AddMethods
