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

MODULE FiniteElement_Class
USE GlobalData
USE AbstractRefElement_Class
USE AbstractFE_Class
USE FPL, ONLY: ParameterList_
USE Domain_Class, ONLY: Domain_
IMPLICIT NONE
PRIVATE
PUBLIC :: FiniteElement_
PUBLIC :: FiniteElementPointer_
PUBLIC :: SetFiniteElementParam
PUBLIC :: FiniteElementDeallocate
PUBLIC :: FiniteElementInitiate
CHARACTER(*), PARAMETER :: modName = "FiniteElement_Class"
CHARACTER(*), PARAMETER :: myprefix = "FiniteElement"

!----------------------------------------------------------------------------
!                                                        AbstractRefElement_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-13
! summary: Finite element class
!
!{!pages/docs-api/FiniteElement/FiniteElement_.md!}

TYPE, EXTENDS(AbstractFE_) :: FiniteElement_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
END TYPE FiniteElement_

!----------------------------------------------------------------------------
!                                                      FiniteElementPointer_
!----------------------------------------------------------------------------

TYPE :: FiniteElementPointer_
  CLASS(FiniteElement_), POINTER :: ptr => NULL()
END TYPE FiniteElementPointer_

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Initiate vector of FiniteElement pointers

INTERFACE FiniteElementInitiate
  MODULE SUBROUTINE obj_Initiate1(obj, param, dom, dim)
    TYPE(FiniteElementPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(Domain_), INTENT(IN) :: dom
    INTEGER(I4B), INTENT(IN) :: dim
  END SUBROUTINE obj_Initiate1
END INTERFACE FiniteElementInitiate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-25
! summary:  Deallocate a vector of FiniteElement

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Vector(obj)
    TYPE(FiniteElement_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE FiniteElementDeallocate
  MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
    TYPE(FiniteElementPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE Deallocate_Ptr_Vector
END INTERFACE FiniteElementDeallocate

!----------------------------------------------------------------------------
!                                                     GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-11
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(FiniteElement_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

END MODULE FiniteElement_Class
