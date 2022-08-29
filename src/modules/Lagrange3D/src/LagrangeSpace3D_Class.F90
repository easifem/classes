
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

MODULE LagrangeSpace3D_Class
USE String_Class, ONLY: String
USE GlobalData
USE AbstractPolynomialSpace3D_Class
USE Lagrange3D_Class, ONLY: Lagrange3D_
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                          LagrangeSpace3D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Lagrange space are defined
!
!{!pages/LagrangeSpace3D_.md!}

TYPE, EXTENDS(AbstractPolynomialSpace3D_) :: LagrangeSpace3D_
  PRIVATE
  TYPE(Lagrange3D_), ALLOCATABLE :: x(:)
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => func_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => func_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => func_Display
  FINAL :: func_Final
END TYPE LagrangeSpace3D_

PUBLIC :: LagrangeSpace3D_

!----------------------------------------------------------------------------
!                                                    LagrangeSpace3DPointer_
!----------------------------------------------------------------------------

TYPE :: LagrangeSpace3DPointer_
  CLASS(LagrangeSpace3D_), POINTER :: ptr => NULL()
END TYPE LagrangeSpace3DPointer_

PUBLIC :: LagrangeSpace3DPointer_

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Initiate the Lagrangee space

INTERFACE
  MODULE SUBROUTINE func_Initiate(obj, x, order, varname1, varname2, &
    & varname3, elemType)
    CLASS(LagrangeSpace3D_), INTENT(INOUT) :: obj
    !!
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
    !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname2
    !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname3
    !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
  END SUBROUTINE func_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                    LagrangeSpace3D@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Constructor for lagrange basis of first kind

INTERFACE
  MODULE FUNCTION LagrangeSpace3D1(x, order, varname1, &
      & varname2, varname3, elemType) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points, order = size(x) - 1
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
    !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname2
    !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname3
    !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    TYPE(LagrangeSpace3D_) :: ans
    !! Lagrange basis in 1D
  END FUNCTION LagrangeSpace3D1
END INTERFACE

INTERFACE LagrangeSpace3D
  MODULE PROCEDURE LagrangeSpace3D1
END INTERFACE LagrangeSpace3D

PUBLIC :: LagrangeSpace3D

!----------------------------------------------------------------------------
!                                           LagrangeSpace3D_Pointer@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary: Constructor for Lagrange basis of first kind

INTERFACE
  MODULE FUNCTION LagrangeSpace3D_Pointer1(x, order, varname1, &
    & varname2, varname3, elemType) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points, order = size(x) - 1
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
    !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname2
    !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname3
    !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    CLASS(LagrangeSpace3D_), POINTER :: ans
    !! Lagrange basis in 1D
  END FUNCTION LagrangeSpace3D_Pointer1
END INTERFACE

INTERFACE LagrangeSpace3D_Pointer
  MODULE PROCEDURE LagrangeSpace3D_Pointer1
END INTERFACE LagrangeSpace3D_Pointer

PUBLIC :: LagrangeSpace3D_Pointer

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary: Deallocate the object

INTERFACE
  MODULE SUBROUTINE func_Deallocate(obj)
    CLASS(LagrangeSpace3D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Final@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary: Finalizer for chebyshev space

INTERFACE
  MODULE SUBROUTINE func_Final(obj)
    TYPE(LagrangeSpace3D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE func_Display(obj, msg, unitno)
    CLASS(LagrangeSpace3D_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE func_Display
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LagrangeSpace3D_Class
