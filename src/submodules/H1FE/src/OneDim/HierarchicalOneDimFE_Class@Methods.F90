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

SUBMODULE(HierarchicalOneDimFE_Class) Methods
USE BaseType, ONLY: TypeFeVariableOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                HierarchicalOneDimFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_HierarchicalOneDimFEPointer1
ALLOCATE (ans)
END PROCEDURE obj_HierarchicalOneDimFEPointer1

!----------------------------------------------------------------------------
!                                                HierarchicalOneDimFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_HierarchicalOneDimFEPointer2
ALLOCATE (ans)
CALL ans%Initiate(fetype=TypeFeVariableOpt%scalar, &
                  baseContinuity=baseContinuity, &
                  baseInterpolation="Hierarchical", &
                  order=order)
END PROCEDURE obj_HierarchicalOneDimFEPointer2

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
