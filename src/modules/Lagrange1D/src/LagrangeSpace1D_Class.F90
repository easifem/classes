
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

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Space of Lagrange polynomial is defined

MODULE LagrangeSpace1D_Class
USE String_Class, ONLY: String
USE GlobalData
USE AbstractPolynomialSpace1D_Class
USE Lagrange1D_Class, ONLY: Lagrange1D_
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                          LagrangeSpace1D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Lagrange space are defined
!
!{!pages/LagrangeSpace1D_.md!}

TYPE, EXTENDS(AbstractPolynomialSpace1D_) :: LagrangeSpace1D_
  PRIVATE
  TYPE(Lagrange1D_), ALLOCATABLE :: x(:)
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => func_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => func_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => func_Display
  FINAL :: func_Final
END TYPE LagrangeSpace1D_

PUBLIC :: LagrangeSpace1D_

!----------------------------------------------------------------------------
!                                                    LagrangeSpace1DPointer_
!----------------------------------------------------------------------------

TYPE :: LagrangeSpace1DPointer_
  CLASS(LagrangeSpace1D_), POINTER :: ptr => NULL()
END TYPE LagrangeSpace1DPointer_

PUBLIC :: LagrangeSpace1DPointer_

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Initiate the Lagrangee space

INTERFACE
  MODULE SUBROUTINE func_Initiate(obj, x, order, varname)
    CLASS(LagrangeSpace1D_), INTENT(INOUT) :: obj
    !!
    REAL(DFP), INTENT(IN) :: x(:)
    !! points, order = size(x) - 1
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    CHARACTER(LEN=*), INTENT(IN) :: varname
    !! variable varname
  END SUBROUTINE func_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                         LagrangeSpace1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Constructor for lagrange basis of first kind

INTERFACE
  MODULE FUNCTION LagrangeSpace1D1(x, order, varname) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:)
    !! points, order = size(x) - 1
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    CHARACTER(LEN=*), INTENT(IN) :: varname
    !! variable varname
    TYPE(LagrangeSpace1D_) :: ans
    !! Lagrange basis in 1D
  END FUNCTION LagrangeSpace1D1
END INTERFACE

INTERFACE LagrangeSpace1D
  MODULE PROCEDURE LagrangeSpace1D1
END INTERFACE LagrangeSpace1D

PUBLIC :: LagrangeSpace1D

!----------------------------------------------------------------------------
!                                 LagrangeSpace1D_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary: Constructor for Lagrange basis of first kind

INTERFACE
  MODULE FUNCTION LagrangeSpace1D_Pointer1(x, order, varname) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:)
    !! points, order = size(x) - 1
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    CHARACTER(LEN=*), INTENT(IN) :: varname
    !! variable varname
    CLASS(LagrangeSpace1D_), POINTER :: ans
    !! Lagrange basis in 1D
  END FUNCTION LagrangeSpace1D_Pointer1
END INTERFACE

INTERFACE LagrangeSpace1D_Pointer
  MODULE PROCEDURE LagrangeSpace1D_Pointer1
END INTERFACE LagrangeSpace1D_Pointer

PUBLIC :: LagrangeSpace1D_Pointer

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary:         Deallocate the object

INTERFACE
  MODULE SUBROUTINE func_Deallocate(obj)
    CLASS(LagrangeSpace1D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary:         Finalizer for chebyshev space

INTERFACE
  MODULE SUBROUTINE func_Final(obj)
    TYPE(LagrangeSpace1D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE func_Display(obj, msg, unitno)
    CLASS(LagrangeSpace1D_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE func_Display
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LagrangeSpace1D_Class
