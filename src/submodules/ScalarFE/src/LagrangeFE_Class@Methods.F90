! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(LagrangeFE_Class) Methods
USE BaseType, ONLY: TypeFeVariableOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                          LagrangeFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_LagrangeFEPointer1
ALLOCATE (ans)
END PROCEDURE obj_LagrangeFEPointer1

!----------------------------------------------------------------------------
!                                                         LagrangeFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_LagrangeFEPointer2
ALLOCATE (ans)
CALL ans%Initiate(elemType=elemType, fetype=TypeFeVariableOpt%scalar, &
                  nsd=nsd, baseContinuity=baseContinuity, &
           baseInterpolation="LAGRANGE", ipType=ipType, basisType=basisType, &
    alpha=alpha, beta=beta, lambda=lambda, order=order, anisoOrder=anisoOrder)
END PROCEDURE obj_LagrangeFEPointer2

!----------------------------------------------------------------------------
!                                                                GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!                                                   FiniteElementDeallocate
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
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
