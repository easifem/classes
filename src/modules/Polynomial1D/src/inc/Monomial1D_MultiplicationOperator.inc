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

INTERFACE OPERATOR(*)
  MODULE PROCEDURE &
    ! & func_Multiplication_mono_mono, &
    & func_Multiplication_mono_Int8, &
    & func_Multiplication_mono_Int16, &
    & func_Multiplication_mono_Int32, &
    & func_Multiplication_mono_Int64, &
    & func_Multiplication_mono_Real32, &
    & func_Multiplication_mono_Real64, &
    & func_Multiplication_Int8_mono, &
    & func_Multiplication_Int16_mono, &
    & func_Multiplication_Int32_mono, &
    & func_Multiplication_Int64_mono, &
    & func_Multiplication_Real32_mono, &
    & func_Multiplication_Real64_mono
END INTERFACE OPERATOR(*)

PUBLIC :: OPERATOR(*)

!----------------------------------------------------------------------------
!                                            Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Multiplication two Monomial1D

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Multiplication_mono_mono(obj1, obj2) &
      & RESULT(Ans)
    CLASS(Monomial1D_), INTENT(IN) :: obj1
    CLASS(Monomial1D_), INTENT(IN) :: obj2
    TYPE(Polynomial1D_) :: ans
  END FUNCTION func_Multiplication_mono_mono
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Multiplication two Monomial1D

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Multiplication_mono_Int8(obj1, obj2) &
    & RESULT(ans)
    CLASS(Monomial1D_), INTENT(IN) :: obj1
    INTEGER(Int8), INTENT(IN) :: obj2
    TYPE(Polynomial1D_) :: ans
  END FUNCTION func_Multiplication_mono_Int8
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Multiplication_mono_Int16(obj1, obj2) &
    & RESULT(ans)
    CLASS(Monomial1D_), INTENT(IN) :: obj1
    INTEGER(Int16), INTENT(IN) :: obj2
    TYPE(Polynomial1D_) :: ans
  END FUNCTION func_Multiplication_mono_Int16
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Multiplication_mono_Int32(obj1, obj2) &
    & RESULT(ans)
    CLASS(Monomial1D_), INTENT(IN) :: obj1
    INTEGER(Int32), INTENT(IN) :: obj2
    TYPE(Polynomial1D_) :: ans
  END FUNCTION func_Multiplication_mono_Int32
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Multiplication_mono_Int64(obj1, obj2) &
    & RESULT(ans)
    CLASS(Monomial1D_), INTENT(IN) :: obj1
    INTEGER(Int64), INTENT(IN) :: obj2
    TYPE(Polynomial1D_) :: ans
  END FUNCTION func_Multiplication_mono_Int64
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Multiplication_mono_Real32(obj1, obj2) &
    & RESULT(ans)
    CLASS(Monomial1D_), INTENT(IN) :: obj1
    REAL(Real32), INTENT(IN) :: obj2
    TYPE(Polynomial1D_) :: ans
  END FUNCTION func_Multiplication_mono_Real32
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Multiplication_mono_Real64(obj1, obj2) &
    & RESULT(ans)
    CLASS(Monomial1D_), INTENT(IN) :: obj1
    REAL(Real64), INTENT(IN) :: obj2
    TYPE(Polynomial1D_) :: ans
  END FUNCTION func_Multiplication_mono_Real64
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Multiplication_Int8_mono(obj1, obj2) &
    & RESULT(ans)
    INTEGER(Int8), INTENT(IN) :: obj1
    CLASS(Monomial1D_), INTENT(IN) :: obj2
    TYPE(Polynomial1D_) :: ans
  END FUNCTION func_Multiplication_Int8_mono
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Multiplication_Int16_mono(obj1, obj2) &
    & RESULT(ans)
    INTEGER(Int16), INTENT(IN) :: obj1
    CLASS(Monomial1D_), INTENT(IN) :: obj2
    TYPE(Polynomial1D_) :: ans
  END FUNCTION func_Multiplication_Int16_mono
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Multiplication_Int32_mono(obj1, obj2) &
    & RESULT(ans)
    INTEGER(Int32), INTENT(IN) :: obj1
    CLASS(Monomial1D_), INTENT(IN) :: obj2
    TYPE(Polynomial1D_) :: ans
  END FUNCTION func_Multiplication_Int32_mono
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Multiplication_Int64_mono(obj1, obj2) &
    & RESULT(ans)
    INTEGER(Int64), INTENT(IN) :: obj1
    CLASS(Monomial1D_), INTENT(IN) :: obj2
    TYPE(Polynomial1D_) :: ans
  END FUNCTION func_Multiplication_Int64_mono
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Multiplication_Real32_mono(obj1, obj2) &
    & RESULT(ans)
    REAL(Real32), INTENT(IN) :: obj1
    CLASS(Monomial1D_), INTENT(IN) :: obj2
    TYPE(Polynomial1D_) :: ans
  END FUNCTION func_Multiplication_Real32_mono
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE ELEMENTAL FUNCTION func_Multiplication_Real64_mono(obj1, obj2) &
    & RESULT(ans)
    REAL(Real64), INTENT(IN) :: obj1
    CLASS(Monomial1D_), INTENT(IN) :: obj2
    TYPE(Polynomial1D_) :: ans
  END FUNCTION func_Multiplication_Real64_mono
END INTERFACE
