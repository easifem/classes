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

SUBMODULE(FiniteElement_Class) ConstructorMethods
USE ExceptionHandler_Class, ONLY: e
USE BaseMethod, ONLY: Reallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                        InitiateLagrangeFE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateLagrangeFE
TYPE(ParameterList_) :: param
CHARACTER(*), PARAMETER :: myName = "obj_InitiateLagrangeFE()"
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

problem = baseInterpolation(1:8) .NE. "Lagrange"

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ARG ERROR] :: This routine is valid for baseInterpolation = '//  &
    & 'LagrangePolynomial or LagrangeInterpolation '//  &
    & ' given value of baseInterpolation is '//TRIM(baseInterpolation))
  RETURN
END IF

CALL param%Initiate()
CALL SetFiniteElementParam( &
  & param=param,  &
  & prefix=obj%GetPrefix(), &
  & nsd=nsd,  &
  & elemType=elemType,  &
  & baseContinuity=baseContinuity,  &
  & baseInterpolation=baseInterpolation,  &
  & ipType=ipType,  &
  & basisType=[basisType],  &
  & alpha=[alpha],  &
  & beta=[beta],  &
  & lambda=[lambda],  &
  & order=order)
CALL obj%Initiate(param)
CALL param%DEALLOCATE()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_InitiateLagrangeFE

!----------------------------------------------------------------------------
!                                                 FiniteElementInitiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
TYPE(ParameterList_), POINTER :: sublist
INTEGER(I4B) :: ierr, ii, tsize
INTEGER(I4B), ALLOCATABLE :: elemType(:), order(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

sublist => NULL()

IF (.NOT. param%isSubList(key=myprefix)) RETURN

ierr = param%GetSubList(key=myprefix, sublist=sublist)

IF (ierr .NE. 0) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: some error occured while getting'//  &
    & ' the sublist from param.')
  RETURN
END IF

tsize = dom%GetTotalMesh(dim=dim)
CALL Reallocate(elemType, tsize, order, tsize)
elemType = dom%GetElemType(dim=dim)
order = dom%GetOrder(dim=dim)
tsize = SIZE(elemType)

CALL FiniteElementDeallocate(obj)
ALLOCATE (obj(tsize))

DO ii = 1, tsize
  ierr = sublist%Set(key=myprefix//"/elemType", VALUE=elemType(ii))
  ierr = sublist%Set(key=myprefix//"/order", VALUE=order(ii))
  ALLOCATE (FiniteElement_ :: obj(ii)%ptr)
  CALL obj(ii)%ptr%Initiate(param=param)
END DO

sublist => NULL()
IF (ALLOCATED(elemType)) DEALLOCATE (elemType)
IF (ALLOCATED(order)) DEALLOCATE (order)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Vector
CHARACTER(*), PARAMETER :: myName = "Deallocate_Vector()"
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    CALL obj(ii)%DEALLOCATE()
  END DO
  DEALLOCATE (obj)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE Deallocate_Vector

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Ptr_Vector
CHARACTER(*), PARAMETER :: myName = "Deallocate_Ptr_Vector()"
INTEGER(I4B) :: ii
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
