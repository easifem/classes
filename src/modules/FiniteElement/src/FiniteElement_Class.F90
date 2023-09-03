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

MODULE FiniteElement_Class
USE GlobalData
USE AbstractRefElement_Class
USE AbstractFE_Class
USE FPL, ONLY: ParameterList_
IMPLICIT NONE
PRIVATE
PUBLIC :: FiniteElement_
PUBLIC :: FiniteElementPointer_
PUBLIC :: SetFiniteElementParam
CHARACTER(*), PARAMETER :: modName = "FiniteElement_Class"

!----------------------------------------------------------------------------
!                                                        AbstractRefElement_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-13
! summary: Finite element class
!
!{!pages/docs-api/FiniteElement/FiniteElement_.md!}

TYPE, EXTENDS(AbstractFE_) :: FiniteElement_
! CONTAINS
!   PROCEDURE, PUBLIC, PASS(obj) :: Initiate => fe_Initiate
!   !! Constructor method for finite element
!   PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
!     & fe_checkEssentialParam
END TYPE FiniteElement_

!----------------------------------------------------------------------------
!                                                      FiniteElementPointer_
!----------------------------------------------------------------------------

TYPE :: FiniteElementPointer_
  CLASS(FiniteElement_), POINTER :: ptr => NULL()
END TYPE FiniteElementPointer_

!----------------------------------------------------------------------------
!                                               checkEssentialParam@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-08-11
! summary: This routine check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE fe_checkEssentialParam(obj, param)
    CLASS(FiniteElement_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE fe_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Initiates an instance of the finite element

INTERFACE
  MODULE SUBROUTINE fe_Initiate(obj, param)
    CLASS(FiniteElement_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE fe_Initiate
END INTERFACE

END MODULE FiniteElement_Class
