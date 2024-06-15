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

SUBMODULE(AbstractMeshField_Class) InsertMethods
USE GlobalData, ONLY: Constant, Space, Time, SpaceTime, &
                      Scalar, Vector, Matrix

USE Display_Method, ONLY: ToString

USE FEVariable_Method, ONLY: FEVariable_Deallocate => DEALLOCATE, &
                             FEVariable_SIZE => Size, &
                             FEVariable_Shape => Shape

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             MasterInsert
!----------------------------------------------------------------------------

SUBROUTINE MasterInsert(val, indxVal, set_val, indx, tsize, ss, &
                        indxShape, s, tshape)
  REAL(DFP), INTENT(INOUT) :: val(:)
  INTEGER(I4B), INTENT(INOUT) :: indxVal(:)
  REAL(DFP), INTENT(IN) :: set_val(:)
  INTEGER(I4B), INTENT(IN) :: indx
  INTEGER(I4B), INTENT(IN) :: tsize
  INTEGER(I4B), INTENT(INOUT) :: ss(:)
  INTEGER(I4B), INTENT(INOUT) :: indxShape(:)
  INTEGER(I4B), INTENT(IN) :: s(:)
  INTEGER(I4B), INTENT(IN) :: tshape

  indxVal(indx + 1) = indxVal(indx) + tsize
  val(indxVal(indx):indxVal(indx + 1) - 1) = set_val(1:tsize)

  indxShape(indx + 1) = indxShape(indx) + tshape
  ss(indxShape(indx):indxShape(indx + 1) - 1) = s(1:tshape)

END SUBROUTINE MasterInsert

!----------------------------------------------------------------------------
!                                                                        Insert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Insert1
INTEGER(I4B) :: iel, tsize, tshape, s(MAX_RANK_FEVARIABLE)

IF (obj%fieldType .EQ. TypeField%Constant) THEN
  iel = 1
ELSE
  iel = obj%mesh%GetLocalElemNumber(globalElement=globalElement, &
                                    islocal=islocal)
END IF

tsize = FEVariable_SIZE(fevar)

tshape = GetTotalRow(rank=obj%rank, varType=obj%varType)

s(1:tshape) = FEVariable_Shape(fevar)

CALL MasterInsert(val=obj%val, indxVal=obj%indxVal, set_val=fevar%val, indx=iel, &
                  tsize=tsize, ss=obj%ss, indxShape=obj%indxShape, &
                  s=s, tshape=tshape)

END PROCEDURE obj_Insert1

!----------------------------------------------------------------------------
!                                                                       Insert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Insert2
CHARACTER(*), PARAMETER :: myName = "obj_Insert2()"
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
  CALL obj%Insert(fevar=fevar, globalElement=1, islocal=.TRUE.)
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

  CALL obj%Insert(fevar=fevar, globalElement=iel, islocal=.TRUE.)
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
END PROCEDURE obj_Insert2

!----------------------------------------------------------------------------
!                                                               Insert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Insert3
CHARACTER(*), PARAMETER :: myName = "obj_Insert3()"
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

CALL obj%Insert(func=func, times=times)

func => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_Insert3

!----------------------------------------------------------------------------
!                                                                       Insert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Insert4
INTEGER(I4B) :: iel, telem

IF (obj%fieldType .EQ. TypeField%Constant) THEN
  telem = 1
ELSE
  telem = obj%mesh%GetTotalElements()
END IF

DO iel = 1, telem
  CALL obj%Insert(fevar=fevar, globalElement=iel, islocal=.TRUE.)
END DO

END PROCEDURE obj_Insert4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"
#include "./include/GetTotalRow.F90"

END SUBMODULE InsertMethods
