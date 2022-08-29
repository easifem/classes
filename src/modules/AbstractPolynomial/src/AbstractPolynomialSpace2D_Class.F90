
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
! summary: abstract space of polynomial is defined

MODULE AbstractPolynomialSpace2D_Class
USE String_Class, ONLY: String
USE GlobalData
USE AbstractBasis_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                 AbstractPolynomialSpace2D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: abstract space of polynomials are defined
!
!{!pages/AbstractPolynomialSpace2D_.md!}

TYPE, ABSTRACT :: AbstractPolynomialSpace2D_
  PRIVATE
  INTEGER(I4B) :: n = 0_I4B
  !! order of space
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Deallocate => func_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => func_Display
  PROCEDURE, PUBLIC, PASS(obj) :: setParam => func_setParam
END TYPE AbstractPolynomialSpace2D_

PUBLIC :: AbstractPolynomialSpace2D_

!----------------------------------------------------------------------------
!                                           AbstractPolynomialSpace2DPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractPolynomialSpace2DPointer_
  CLASS(AbstractPolynomialSpace2D_), POINTER :: ptr => NULL()
END TYPE AbstractPolynomialSpace2DPointer_

PUBLIC :: AbstractPolynomialSpace2DPointer_

!----------------------------------------------------------------------------
!                                            Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary:         Deallocate the object

INTERFACE
  MODULE SUBROUTINE func_Deallocate(obj)
    CLASS(AbstractPolynomialSpace2D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Deallocate
END INTERFACE

INTERFACE AbstractPolynomialSpace2DDeallocate
  MODULE PROCEDURE func_Deallocate
END INTERFACE AbstractPolynomialSpace2DDeallocate

PUBLIC :: AbstractPolynomialSpace2DDeallocate

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE func_Display(obj, msg, unitno)
    CLASS(AbstractPolynomialSpace2D_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE func_Display
END INTERFACE

INTERFACE AbstractPolynomialSpace2DDisplay
  MODULE PROCEDURE func_Display
END INTERFACE AbstractPolynomialSpace2DDisplay

PUBLIC :: AbstractPolynomialSpace2DDisplay

!----------------------------------------------------------------------------
!                                                       setParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Aug 2022
! summary:         Set the parameters

INTERFACE
  MODULE PURE SUBROUTINE func_SetParam(obj, n)
    CLASS(AbstractPolynomialSpace2D_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n
  END SUBROUTINE func_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractPolynomialSpace2D_Class
