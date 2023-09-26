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

SUBMODULE(FiniteElement_Class) Methods
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_Initiate
CALL AbstractFEInitiate(obj=obj, param=param, prefix=myprefix)
END PROCEDURE fe_Initiate

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_CheckEssentialParam
CALL AbstractFECheckEssentialParam(obj=obj, param=param, prefix=myprefix)
END PROCEDURE fe_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                     SetFiniteElementParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetFiniteElementParam
CALL SetAbstractFEParam( &
& param=param,  &
& prefix=myprefix,  &
& nsd=nsd,  &
& elemType=elemType,  &
& baseContinuity=baseContinuity,  &
& baseInterpolation=baseInterpolation,  &
& ipType=ipType,  &
& basisType=basisType,  &
& alpha=alpha,  &
& beta=beta,  &
& lambda=lambda,  &
& order=order,  &
& anisoOrder=anisoOrder,  &
& edgeOrder=edgeOrder,  &
& faceOrder=faceOrder,  &
& cellOrder=cellOrder)
END PROCEDURE SetFiniteElementParam

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    CALL obj(ii)%DEALLOCATE()
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE Deallocate_Vector

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Ptr_Vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_Initiate1
CHARACTER(*), PARAMETER :: myName = "fe_Initiate1()"
TYPE(ParameterList_), POINTER :: sublist
INTEGER(I4B) :: ierr, ii, tElemType
INTEGER(I4B), ALLOCATABLE :: elemType(:)

sublist => NULL()

IF (.NOT. param%isSubList(key=myPrefix)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[ARGUMENT ERROR] :: '//myprefix//' should be a sublist')
END IF

ierr = param%GetSubList(key=myprefix, sublist=sublist)

elemType = dom%GetElemType(dim=-1_I4B)
tElemType = SIZE(elemType)

CALL DEALLOCATE (obj)
ALLOCATE(obj(tElemType) )

DO ii = 1, SIZE(elemType)
  ierr = sublist%Set(key=myprefix//"/elemType", VALUE=elemType(ii))
  ALLOCATE(FiniteElement_::obj(ii)%ptr)
  CALL obj(ii)%ptr%Initiate(param=param)
END DO

sublist => NULL()
IF (ALLOCATED(elemType)) DEALLOCATE (elemType)
END PROCEDURE fe_Initiate1

END SUBMODULE Methods
