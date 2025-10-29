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
USE Display_Method, ONLY: ToString, Display
USE FEVariable_Method, ONLY: FEVariable_Deallocate => DEALLOCATE, &
                             FEVariable_SIZE => Size, &
                             FEVariable_GetShape => GetShape
USE ReallocateUtility, ONLY: Reallocate
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt

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
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Insert1()"
#endif

INTEGER(I4B) :: iel, tsize, tshape, s(fevaropt%maxRank)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

iel = 1
isok = obj%fieldType .EQ. TypeField%Constant
IF (.NOT. isok) iel = obj%mesh%GetLocalElemNumber( &
                      globalElement=globalElement, islocal=islocal)

tsize = FEVariable_SIZE(fevar)
CALL FEVariable_GetShape(obj=fevar, ans=s, tsize=tshape)

CALL MasterInsert(val=obj%val, indxVal=obj%indxVal, set_val=fevar%val, &
                  indx=iel, tsize=tsize, ss=obj%ss, indxShape=obj%indxShape, &
                  s=s, tshape=tshape)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Insert1

!----------------------------------------------------------------------------
!                                                                       Insert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Insert2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Insert2()"
#endif

INTEGER(I4B) :: iel, telem, nns, nsd, tsize, nrow, ncol
LOGICAL(LGT) :: bool1
REAL(DFP), ALLOCATABLE :: xij(:, :)
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
CLASS(AbstractMesh_), POINTER :: mesh
TYPE(FEVariable_) :: fevar

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

bool1 = obj%fieldType .EQ. typefield%Constant
IF (bool1) THEN
  CALL func%Get(fevar=fevar)
  CALL obj%Insert(fevar=fevar, globalElement=1, islocal=.TRUE.)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

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

  CALL mesh%GetNodeCoord(nodeCoord=xij(1:nsd, 1:tsize), nrow=nrow, &
                        ncol=ncol, globalNode=nptrs(1:tsize), islocal=.FALSE.)

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
#endif
END PROCEDURE obj_Insert2

!----------------------------------------------------------------------------
!                                                               Insert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Insert3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Insert3()"
#endif

LOGICAL(LGT) :: isok
CLASS(UserFunction_), POINTER :: func

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

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
CALL AssertError1(isok, myName, 'material pointer not found.')
#endif

CALL obj%Insert(func=func, times=times)

func => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Insert3

!----------------------------------------------------------------------------
!                                                                       Insert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Insert4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Insert4()"
#endif
INTEGER(I4B) :: iel, telem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%fieldType .EQ. TypeField%Constant) THEN
  telem = 1
ELSE
  telem = obj%mesh%GetTotalElements()
END IF

DO iel = 1, telem
  CALL obj%Insert(fevar=fevar, globalElement=iel, islocal=.TRUE.)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Insert4

!----------------------------------------------------------------------------
!                                                                       Insert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Insert5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Insert5()"
#endif

LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.

INTEGER(I4B) :: iel, nns, nsd, tsize, nrow, ncol

LOGICAL(LGT) :: isok
REAL(DFP), ALLOCATABLE :: xij(:, :)
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
TYPE(FEVariable_) :: fevar
CLASS(AbstractMesh_), POINTER :: mesh

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = obj%fieldType .EQ. typefield%Constant
IF (isok) THEN
  CALL func%Get(fevar=fevar)
  CALL obj%Insert(fevar=fevar, globalElement=1, islocal=.TRUE.)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

mesh => obj%mesh

nns = mesh%GetMaxNNE()
nsd = mesh%GetNSD()

ALLOCATE (nptrs(nns), xij(nsd, nns))

iel = mesh%GetLocalElemNumber(globalElement=globalElement, islocal=islocal)

CALL mesh%GetConnectivity_(globalElement=iel, islocal=yes, ans=nptrs, &
                           tsize=tsize)

CALL mesh%GetNodeCoord(nodeCoord=xij(1:nsd, 1:tsize), nrow=nrow, &
                       ncol=ncol, globalNode=nptrs(1:tsize), islocal=no)

CALL func%Get(fevar=fevar, xij=xij(1:nsd, 1:tsize), times=times)

CALL obj%Insert(fevar=fevar, globalElement=iel, islocal=yes)

CALL FEVariable_Deallocate(fevar)
DEALLOCATE (xij, nptrs)

mesh => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Insert5

!----------------------------------------------------------------------------
!                                                                        Insert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Insert6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Insert6()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: tsize, ii
TYPE(UserFunctionPointer_), ALLOCATABLE :: func(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(material)
ALLOCATE (func(tsize))

DO ii = 1, tsize

! Check if material(ii)%ptr is associated or not
#ifdef DEBUG_VER
  isok = ASSOCIATED(material(ii)%ptr)
  CALL AssertError1(isok, myName, &
                    'material('//ToString(ii)//')%ptr is not associated')
#endif

! Check if material(ii)%ptr has the name or not
#ifdef DEBUG_VER
  isok = material(ii)%ptr%IsMaterialPresent(name)
  CALL AssertError1(isok, myName, &
                    'material name = '//name//' not found in material('// &
                    ToString(ii)//')')
#endif

  func(ii)%ptr => material(ii)%ptr%GetMaterialPointer(name)

END DO

CALL obj%Insert(medium=medium, func=func, times=times)

! Clean up func
DO ii = 1, tsize
  func(ii)%ptr => NULL()
END DO
DEALLOCATE (func)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Insert6

!----------------------------------------------------------------------------
!                                                                      Insert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Insert7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Insert7()"
LOGICAL(LGT) :: isok
#endif

LOGICAL(LGT), PARAMETER :: yes = .TRUE.
INTEGER(I4B) :: tsize, ii, telements, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(func)

! Check if func(ii)%ptr is associated or not
#ifdef DEBUG_VER
DO ii = 1, tsize
  isok = ASSOCIATED(func(ii)%ptr)
  CALL AssertError1(isok, myName, &
                    'func('//ToString(ii)//')%ptr is not associated')
END DO
#endif

telements = obj%mesh%GetTotalElements()

DO ii = 1, telements
  jj = obj%mesh%GetMaterial(globalElement=ii, islocal=yes, &
                            medium=medium)

  ! Check if material index is out of bound or not
#ifdef DEBUG_VER
  isok = (jj .LE. tsize) .AND. (jj .NE. 0)
  CALL AssertError1(isok, myName, &
                    'material index '//ToString(jj)// &
                    ' is out of bounds for func size '// &
                    ToString(tsize)//' for local element = '// &
                    ToString(ii))
#endif

  CALL obj%Insert(func=func(jj)%ptr, globalElement=ii, islocal=yes, &
                  times=times)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Insert7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE InsertMethods
