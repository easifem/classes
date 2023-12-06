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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
INTEGER(I4B) :: iel, telem

#ifdef DEBUG_VER
LOGICAL(LGT) :: problem
INTEGER(I4B) :: size1, size2
#endif DEBUG_VER

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

#ifdef DEBUG_VER

size1 = SIZE(obj%val, 1)
size2 = SIZE(fevar%val)
problem = size1 .NE. size2
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR]  the size of obj%val is '//tostring(size1)//  &
    & ' size of fevar%val is '//tostring(size2))
END IF

#endif DEBUG_VER

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  obj%val(:, 1) = fevar%val(:)
  RETURN
END IF

IF (PRESENT(globalElement)) THEN
  iel = obj%mesh%GetLocalElemNumber(globalElement)
  obj%val(:, iel) = fevar%val(:)
  RETURN
END IF

telem = obj%mesh%GetTotalElements()
DO iel = 1, telem
  obj%val(:, iel) = fevar%val(:)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
INTEGER(I4B) :: iel, telem, minelem, maxelem, nns, nsd
LOGICAL(LGT) :: bool1
REAL(DFP), ALLOCATABLE :: xij(:, :)
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
CLASS(ReferenceElement_), POINTER :: refelem
CLASS(Mesh_), POINTER :: mesh
TYPE(FEVariable_) :: fevar

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

bool1 = obj%fieldType .EQ. FIELD_TYPE_CONSTANT
IF (bool1) THEN
  CALL func%Get(fevar=fevar)
  obj%val(:, 1) = fevar%val(:)
  RETURN
END IF

mesh => obj%mesh
telem = mesh%GetTotalElements()

refelem => NULL()
refelem => mesh%GetRefElemPointer()
nns = .NNE.refelem
CALL Reallocate(nptrs, nns)
nsd = dom%GetNSD()
CALL Reallocate(xij, nsd, nns)

minelem = mesh%GetMinElemNumber()
maxelem = mesh%GetMaxElemNumber()

DO iel = minelem, maxelem
  bool1 = mesh%IsElementPresent(iel)
  IF (.NOT. bool1) CYCLE

  nptrs = mesh%GetConnectivity(globalElement=iel)
  CALL dom%GetNodeCoord(nodeCoord=xij, globalNode=nptrs)
  CALL func%Get(fevar=fevar, xij=xij, timeVec=timeVec)
  CALL obj%Set(fevar=fevar, globalElement=iel)
END DO

IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
CALL DEALLOCATE (fevar)
refelem => NULL()
mesh => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
