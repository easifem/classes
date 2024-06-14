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

SUBMODULE(AbstractMeshField_Class) SetMethods
USE Display_Method, ONLY: ToString

USE FEVariable_Method, ONLY: FEVariable_Deallocate => DEALLOCATE, &
                             FEVariable_SIZE => Size

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             MasterSet
!----------------------------------------------------------------------------

SUBROUTINE MasterSet(val, indxVal, set_val, indx, tsize)
  REAL(DFP), INTENT(INOUT) :: val(:)
  INTEGER(I4B), INTENT(INOUT) :: indxVal(:)
  REAL(DFP), INTENT(IN) :: set_val(:)
  INTEGER(I4B), INTENT(IN) :: indx
  INTEGER(I4B), INTENT(IN) :: tsize

  indxVal(indx + 1) = indxVal(indx) + tsize
  val(indxVal(indx):indxVal(indx + 1) - 1) = set_val(1:tsize)

END SUBROUTINE MasterSet

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
INTEGER(I4B) :: iel, tsize

IF (obj%fieldType .EQ. TypeField%Constant) THEN
  iel = 1
ELSE
  iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                    islocal=islocal)
END IF

tsize = FEVariable_SIZE(fevar)

CALL MasterSet(val=obj%val, indxVal=obj%indxVal, set_val=fevar%val, indx=iel, &
               tsize=tsize)

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
INTEGER(I4B) :: iel, telem, nns, nsd, tsize
LOGICAL(LGT) :: bool1
REAL(DFP), ALLOCATABLE :: xij(:, :)
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
CLASS(AbstractMesh_), POINTER :: mesh
TYPE(FEVariable_) :: fevar

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

bool1 = obj%fieldType .EQ. TypeField%Constant
IF (bool1) THEN
  CALL func%Get(fevar=fevar)
  CALL obj%Set(fevar=fevar, globalElement=1, islocal=.TRUE.)
  RETURN
END IF

mesh => obj%mesh
telem = mesh%GetTotalElements()

nns = mesh%GetMaxNNE()
CALL Reallocate(nptrs, nns)

nsd = mesh%GetNSD()
CALL Reallocate(xij, nsd, nns)

!$OMP PARALLEL DO PRIVATE(iel, tsize, nptrs, xij, fevar)
DO iel = 1, telem
  CALL mesh%GetConnectivity_(globalElement=iel, islocal=.TRUE., &
                             ans=nptrs, tsize=tsize)

  CALL mesh%GetNodeCoord(nodeCoord=xij(1:nsd, 1:tsize), &
                         globalNode=nptrs(1:tsize), islocal=.FALSE.)

  CALL func%Get(fevar=fevar, xij=xij(1:nsd, 1:tsize), times=times)

  CALL obj%Set(fevar=fevar, globalElement=iel, islocal=.TRUE.)
END DO
!$OMP END PARALLEL DO

IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
CALL FEVariable_Deallocate(fevar)

mesh => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                               Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
LOGICAL(LGT) :: isok
CLASS(UserFunction_), POINTER :: func

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif DEBUG_VER

#ifdef DEBUG_VER
isok = material%IsMaterialPresent(name)
CALL AssertError1(isok, myName, &
                  'material name = '//name//" not found.")
#endif

#ifdef DEBUG_VER
func => NULL()
#endif

func => material%GetMaterialPointer(name)

#ifdef DEBUG_VER
isok = ASSOCIATED(func)
CALL AssertError1(isok, myName, &
                  'material pointer not found.')
#endif

CALL obj%Set(func=func, times=times)

func => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
INTEGER(I4B) :: iel, telem, tsize

IF (obj%fieldType .EQ. TypeField%Constant) THEN
  telem = 1
ELSE
  telem = obj%mesh%GetTotalElements()
END IF

tsize = FEVariable_Size(fevar)

DO iel = 1, telem
  CALL MasterSet(val=obj%val, set_val=fevar%val, indx=iel, &
                 indxVal=obj%indxVal, tsize=tsize)
END DO

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
