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

SUBMODULE(MixedFiniteElement_Class) Methods
USE ExceptionHandler_Class, ONLY: e
USE FiniteElement_Class, ONLY: Deallocate_FE => DEALLOCATE
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_Initiate
CHARACTER(*), PARAMETER :: myName = "fe_Initiate()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP] :: The method is under development.')
! CALL AbstractFEInitiate(obj=obj, param=param, prefix=myprefix)
END PROCEDURE fe_Initiate

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_Deallocate
CHARACTER(*), PARAMETER :: myName = "fe_Deallocate()"
CALL Deallocate_FE(obj)
CALL DEALLOCATE (obj%fe)
END PROCEDURE fe_Deallocate

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "fe_CheckEssentialParam()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP] :: The method is under development')
! CALL AbstractFECheckEssentialParam(obj=obj, param=param, prefix=myprefix)
END PROCEDURE fe_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                     SetFiniteElementParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetMixedFiniteElementParam
CHARACTER(*), PARAMETER :: myName = "SetMixedFiniteElementParam()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP] :: The method is under development.')
! CALL SetAbstractFEParam( &
! & param=param,  &
! & prefix=myprefix,  &
! & nsd=nsd,  &
! & elemType=elemType,  &
! & baseContinuity=baseContinuity,  &
! & baseInterpolation=baseInterpolation,  &
! & ipType=ipType,  &
! & basisType=basisType,  &
! & alpha=alpha,  &
! & beta=beta,  &
! & lambda=lambda,  &
! & order=order,  &
! & anisoOrder=anisoOrder,  &
! & edgeOrder=edgeOrder,  &
! & faceOrder=faceOrder,  &
! & cellOrder=cellOrder)
END PROCEDURE SetMixedFiniteElementParam

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

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP] :: This method is under development.')

! TYPE(ParameterList_), POINTER :: sublist
! INTEGER(I4B) :: ierr, ii, tElemType
! INTEGER(I4B), ALLOCATABLE :: elemType(:)
!
! sublist => NULL()
!
! IF (.NOT. param%isSubList(key=myPrefix)) THEN
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!     & '[ARGUMENT ERROR] :: '//myprefix//' should be a sublist')
! END IF
!
! ierr = param%GetSubList(key=myprefix, sublist=sublist)
!
! elemType = dom%GetElemType(dim=-1_I4B)
! tElemType = SIZE(elemType)
!
! CALL DEALLOCATE (obj)
! ALLOCATE(obj(tElemType) )
!
! DO ii = 1, SIZE(elemType)
!   ierr = sublist%Set(key=myprefix//"/elemType", VALUE=elemType(ii))
!   ALLOCATE(FiniteElement_::obj(ii)%ptr)
!   CALL obj(ii)%ptr%Initiate(param=param)
! END DO
!
! sublist => NULL()
! IF (ALLOCATED(elemType)) DEALLOCATE (elemType)
END PROCEDURE fe_Initiate1

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_Initiate2
CHARACTER(*), PARAMETER :: myName = "fe_Initiate2()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP] :: This method is under development.')
END PROCEDURE fe_Initiate2

END SUBMODULE Methods
