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

USE FEVariable_Method, ONLY: FEVariable_Deallocate => DEALLOCATE

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             MasterSet
!----------------------------------------------------------------------------

SUBROUTINE MasterSet(val, set_val, indx)
  REAL(DFP), INTENT(INOUT) :: val(:, :)
  REAL(DFP), INTENT(IN) :: set_val(:)
  INTEGER(I4B), INTENT(IN) :: indx

  INTEGER(I4B) :: ii, tsize

  tsize = MIN(SIZE(val, 1), SIZE(set_val))

  DO CONCURRENT(ii=1:tsize)
    val(ii, indx) = set_val(ii)
  END DO

END SUBROUTINE MasterSet

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
INTEGER(I4B) :: size1, size2
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
#endif DEBUG_VER

INTEGER(I4B) :: iel

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')

size1 = SIZE(obj%val, 1)
size2 = SIZE(fevar%val)
isok = size1 .EQ. size2
CALL AssertError1(isok, myName, &
                  'the size of obj%val is '//tostring(size1)// &
                  ' size of fevar%val is '//tostring(size2))

#endif

IF (obj%fieldType .EQ. TypeField%Constant) THEN
  iel = 1
ELSE
  iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                    islocal=islocal)
END IF

CALL MasterSet(obj%val, fevar%val, iel)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

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

DO iel = 1, telem
  CALL mesh%GetConnectivity_(globalElement=iel, islocal=.TRUE., &
                             ans=nptrs, tsize=tsize)

  CALL mesh%GetNodeCoord(nodeCoord=xij(1:nsd, 1:tsize), &
                         globalNode=nptrs(1:tsize), islocal=.FALSE.)

  CALL func%Get(fevar=fevar, xij=xij(1:nsd, 1:tsize), times=times)

  CALL obj%Set(fevar=fevar, globalElement=iel, islocal=.TRUE.)
END DO

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
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
INTEGER(I4B) :: size1, size2
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
#endif

INTEGER(I4B) :: iel, telem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')

size1 = SIZE(obj%val, 1)
size2 = SIZE(fevar%val)
isok = size1 .EQ. size2
CALL AssertError1(isok, myName, &
                  'the size of obj%val is '//tostring(size1)// &
                  ' size of fevar%val is '//tostring(size2))
#endif

IF (obj%fieldType .EQ. TypeField%Constant) THEN
  telem = 1
ELSE
  telem = obj%mesh%GetTotalElements()
END IF

DO iel = 1, telem
  CALL MasterSet(obj%val, fevar%val, iel)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
