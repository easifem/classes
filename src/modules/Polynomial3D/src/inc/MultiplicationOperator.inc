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
  MODULE PROCEDURE func_Multiplication_mono_mono, &
    & func_Multiplication_mono_obj, &
    & func_Multiplication_mono_Int8, func_Multiplication_mono_Int16, &
    & func_Multiplication_mono_Int32, func_Multiplication_mono_Int64, &
    & func_Multiplication_mono_Real32, func_Multiplication_mono_Real64, &
    & func_Multiplication_Int8_mono, func_Multiplication_Int16_mono, &
    & func_Multiplication_Int32_mono, func_Multiplication_Int64_mono, &
    & func_Multiplication_Real32_mono, func_Multiplication_Real64_mono
END INTERFACE OPERATOR(*)

PUBLIC :: OPERATOR(*)

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: polynomial = monomial * monomial

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_obj( obj1, obj2 )&
  & RESULT( ans )
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj1
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_obj_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: polynomial = monomial * monomial

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_mono( obj1, obj2 ) &
  & RESULT( ans )
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj1
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_obj_mono
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: polynomial = monomial * monomial

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_mono_obj( obj1, obj2 ) &
  & RESULT( ans )
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj1
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_mono_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: polynomial = monomial * monomial

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_mono_mono( obj1, obj2 ) &
  & RESULT( ans )
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj1
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_mono_mono
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: polynomial = monomial * Int8

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_mono_Int8( obj1, obj2 ) &
  & RESULT( ans )
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj1
  INTEGER( Int8 ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_mono_Int8
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: polynomial = monomial * Int16

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_mono_Int16( obj1, obj2 )  &
  & RESULT( ans )
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj1
  INTEGER( Int16 ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_mono_Int16
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: polynomial = monomial * Int32

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_mono_Int32( obj1, obj2 ) &
  & RESULT( ans )
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj1
  INTEGER( Int32 ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_mono_Int32
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: polynomial = monomial * Int64

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_mono_Int64( obj1, obj2 ) &
  & RESULT( ans )
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj1
  INTEGER( Int64 ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_mono_Int64
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: polynomial = monomial * Real32

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_mono_Real32( obj1, obj2 ) &
  & RESULT( ans )
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj1
  REAL( Real32 ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_mono_Real32
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: polynomial = monomial * Real64

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_mono_Real64( obj1, obj2 ) &
  & RESULT( ans )
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj1
  REAL( Real64 ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_mono_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: polynomial = Int8 * monomial

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Int8_mono( obj1, obj2 ) &
  & RESULT( ans )
  INTEGER( Int8 ), INTENT( IN ) :: obj1
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_Int8_mono
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: polynomial = Int16 * monomial

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Int16_mono( obj1, obj2 ) &
  & RESULT( ans )
  INTEGER( Int16 ), INTENT( IN ) :: obj1
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_Int16_mono
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: polynomial = Int32 * monomial

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Int32_mono( obj1, obj2 ) &
  & RESULT( ans )
  INTEGER( Int32 ), INTENT( IN ) :: obj1
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_Int32_mono
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: polynomial = Int64 * monomial

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Int64_mono( obj1, obj2 ) &
  & RESULT( ans )
  INTEGER( Int64 ), INTENT( IN ) :: obj1
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_Int64_mono
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: polynomial = Real32 * monomial

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Real32_mono( obj1, obj2 ) &
  & RESULT( ans )
  REAL( Real32 ), INTENT( IN ) :: obj1
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_Real32_mono
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: polynomial = Real64 * monomial

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Real64_mono( obj1, obj2 ) &
  & RESULT( ans )
  REAL( Real64 ), INTENT( IN ) :: obj1
  CLASS( Monomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_Real64_mono
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Multiplication two Monomial1D

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_Int8( obj1, obj2 ) &
  & RESULT( Ans )
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj1
  INTEGER( Int8 ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_obj_Int8
END INTERFACE

!----------------------------------------------------------------------------
!                                              Multiplication@OperatorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_Int16( obj1, obj2 ) &
  & RESULT( Ans )
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj1
  INTEGER( Int16 ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_obj_Int16
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_Int32( obj1, obj2 ) &
  & RESULT( Ans )
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj1
  INTEGER( Int32 ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_obj_Int32
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_Int64( obj1, obj2 ) &
  & RESULT( Ans )
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj1
  INTEGER( Int64 ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_obj_Int64
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_Real32( obj1, obj2 ) &
  & RESULT( Ans )
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj1
  REAL( Real32 ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_obj_Real32
END INTERFACE

!----------------------------------------------------------------------------
!                                            Multiplication@OperatorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_Real64( obj1, obj2 ) &
  & RESULT( Ans )
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj1
  REAL( Real64 ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_obj_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Multiplication two Monomial1D

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Int8_obj( obj1, obj2 ) &
  & RESULT( Ans )
  INTEGER( Int8 ), INTENT( IN ) :: obj1
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_Int8_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Multiplication@OperatorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Int16_obj( obj1, obj2 ) &
  & RESULT( Ans )
  INTEGER( Int16 ), INTENT( IN ) :: obj1
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_Int16_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Int32_obj( obj1, obj2 ) &
  & RESULT( Ans )
  INTEGER( Int32 ), INTENT( IN ) :: obj1
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_Int32_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Int64_obj( obj1, obj2 ) &
  & RESULT( Ans )
  INTEGER( Int64 ), INTENT( IN ) :: obj1
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_Int64_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Real32_obj( obj1, obj2 ) &
  & RESULT( Ans )
  REAL( Real32 ), INTENT( IN ) :: obj1
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_Real32_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Real64_obj( obj1, obj2 ) &
  & RESULT( Ans )
  REAL( Real64 ), INTENT( IN ) :: obj1
  CLASS( Polynomial3D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial3D_ ) :: ans
END FUNCTION func_Multiplication_Real64_obj
END INTERFACE
