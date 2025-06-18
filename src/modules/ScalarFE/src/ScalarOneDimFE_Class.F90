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

MODULE ScalarOneDimFE_Class
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_

IMPLICIT NONE

PRIVATE

PUBLIC :: ScalarOneDimFE_
PUBLIC :: ScalarOneDimFEPointer_
PUBLIC :: FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                        AbstractRefElement_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-13
! summary: Finite element class
!
!{!pages/docs-api/FiniteElement/FiniteElement_.md!}

TYPE, ABSTRACT, EXTENDS(AbstractOneDimFE_) :: ScalarOneDimFE_
END TYPE ScalarOneDimFE_

!----------------------------------------------------------------------------
!                                                      FiniteElementPointer_
!----------------------------------------------------------------------------

TYPE :: ScalarOneDimFEPointer_
  CLASS(ScalarOneDimFE_), POINTER :: ptr => NULL()
END TYPE ScalarOneDimFEPointer_

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(ScalarOneDimFEPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

END MODULE ScalarOneDimFE_Class
