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

MODULE LagrangeFE_Class
USE GlobalData
USE AbstractFE_Class
USE PolynomialFactory
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "LagrangeFE_Class"

!----------------------------------------------------------------------------
!                                                              LagrangeFE_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Lagrange finite element class if defined

TYPE, EXTENDS(AbstractScalarFE_) :: LagrangeFE_
  TYPE(LagrangeSpace1D_) :: oneD
  TYPE(LagrangeSpace2D_) :: twoD
  TYPE(LagrangeSpace3D_) :: threeD
CONTAINS
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => fe_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => fe_Deallocate
  FINAL :: fe_final
  PROCEDURE, PUBLIC, PASS(obj) :: Display => fe_Display
END TYPE LagrangeFE_

PUBLIC :: LagrangeFE_

!----------------------------------------------------------------------------
!                                                         LagrangeFEPointer_
!----------------------------------------------------------------------------

TYPE :: LagrangeFEPointer_
  CLASS(LagrangeFE_), POINTER :: ptr => NULL()
END TYPE LagrangeFEPointer_

PUBLIC :: LagrangeFEPointer_

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Initiate Lagrange finite element

INTERFACE
  MODULE SUBROUTINE fe_Initiate(obj, elemType, order, ipType)
    CLASS(LagrangeFE_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
  END SUBROUTINE fe_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary:         Deallocate the data

INTERFACE
  MODULE SUBROUTINE fe_Deallocate(obj)
    CLASS(LagrangeFE_), INTENT(INOUT) :: obj
  END SUBROUTINE fe_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Final@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary:         Final the data

INTERFACE
  MODULE SUBROUTINE fe_Final(obj)
    TYPE(LagrangeFE_), INTENT(INOUT) :: obj
  END SUBROUTINE fe_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE fe_Display(obj, msg, unitno)
    CLASS(LagrangeFE_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE fe_Display
END INTERFACE

END MODULE LagrangeFE_Class
