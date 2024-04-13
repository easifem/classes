
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

MODULE AbstractPolynomialSpace1D_Class
USE String_Class, ONLY: String
USE GlobalData
USE AbstractBasis_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                 AbstractPolynomialSpace1D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: abstract space of polynomials are defined
!
!{!pages/AbstractPolynomialSpace1D_.md!}

TYPE, ABSTRACT :: AbstractPolynomialSpace1D_
  PRIVATE
  INTEGER(I4B) :: n = 0_I4B
  !! order of space
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => func_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => func_Display
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => func_SetParam
END TYPE AbstractPolynomialSpace1D_

PUBLIC :: AbstractPolynomialSpace1D_

!----------------------------------------------------------------------------
!                                           AbstractPolynomialSpace1DPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractPolynomialSpace1DPointer_
  CLASS(AbstractPolynomialSpace1D_), POINTER :: ptr => NULL()
END TYPE AbstractPolynomialSpace1DPointer_

PUBLIC :: AbstractPolynomialSpace1DPointer_

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary:         Deallocate the object

INTERFACE
  MODULE SUBROUTINE func_Deallocate(obj)
    CLASS(AbstractPolynomialSpace1D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_Deallocate
END INTERFACE

INTERFACE AbstractPolynomialSpace1DDeallocate
  MODULE PROCEDURE func_Deallocate
END INTERFACE AbstractPolynomialSpace1DDeallocate

PUBLIC :: AbstractPolynomialSpace1DDeallocate

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE func_Display(obj, msg, unitno)
    CLASS(AbstractPolynomialSpace1D_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE func_Display
END INTERFACE

INTERFACE AbstractPolynomialSpace1DDisplay
  MODULE PROCEDURE func_Display
END INTERFACE AbstractPolynomialSpace1DDisplay

PUBLIC :: AbstractPolynomialSpace1DDisplay

!----------------------------------------------------------------------------
!                                                           SetParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Aug 2022
! summary:         Set the parameters

INTERFACE
  MODULE PURE SUBROUTINE func_SetParam(obj, n)
    CLASS(AbstractPolynomialSpace1D_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: n
  END SUBROUTINE func_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractPolynomialSpace1D_Class
