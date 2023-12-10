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
!

MODULE MixedFiniteElement_Class
USE GlobalData
USE AbstractRefElement_Class
USE AbstractFE_Class
USE FiniteElement_Class
USE FPL, ONLY: ParameterList_
USE Domain_Class, ONLY: Domain_, DomainPointer_
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
PUBLIC :: MixedFiniteElement_
PUBLIC :: MixedFiniteElementPointer_
PUBLIC :: MixedFiniteElementDeallocate
CHARACTER(*), PARAMETER :: modName = "MixedFiniteElement_Class"
CHARACTER(*), PARAMETER :: myprefix = "MixedFiniteElement"

!----------------------------------------------------------------------------
!                                                        AbstractRefElement_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-13
! summary: Finite element class
!
!{!pages/docs-api/MixedFiniteElement/MixedFiniteElement_.md!}

TYPE, EXTENDS(AbstractFE_) :: MixedFiniteElement_
  PRIVATE
  TYPE(FiniteElementPointer_), ALLOCATABLE :: fe(:)
END TYPE MixedFiniteElement_

!----------------------------------------------------------------------------
!                                                      FiniteElementPointer_
!----------------------------------------------------------------------------

TYPE :: MixedFiniteElementPointer_
  CLASS(MixedFiniteElement_), POINTER :: ptr => NULL()
END TYPE MixedFiniteElementPointer_

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-25
! summary:  Deallocate a vector of FiniteElement

INTERFACE MixedFiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(MixedFiniteElement_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE MixedFiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE MixedFiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(MixedFiniteElementPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE MixedFiniteElementDeallocate

END MODULE MixedFiniteElement_Class
