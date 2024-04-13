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

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Multiplication two Polynomial1D

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_mono( obj1, obj2 ) &
  & RESULT( ans )
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj1
  CLASS( Monomial1D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_obj_mono
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Multiplication two Polynomial1D

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_mono_obj( obj1, obj2 ) &
  & RESULT( ans )
  CLASS( Monomial1D_ ), INTENT( IN ) :: obj1
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_mono_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Multiplication two Polynomial1D

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_obj( obj1, obj2 ) &
  & RESULT( ans )
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj1
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_obj_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                             Multiplication@OperatorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Multiplication two Polynomial1D

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_Int8( obj1, obj2 ) RESULT( ans )
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj1
  INTEGER( Int8 ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_obj_Int8
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_Int16( obj1, obj2 ) RESULT( ans )
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj1
  INTEGER( Int16 ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_obj_Int16
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_Int32( obj1, obj2 ) RESULT( ans )
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj1
  INTEGER( Int32 ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_obj_Int32
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_Int64( obj1, obj2 ) RESULT( ans )
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj1
  INTEGER( Int64 ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_obj_Int64
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_Real32( obj1, obj2 ) &
  & RESULT( ans )
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj1
  REAL( Real32 ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_obj_Real32
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_obj_Real64( obj1, obj2 )  &
  & RESULT( ans )
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj1
  REAL( Real64 ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_obj_Real64
END INTERFACE

!----------------------------------------------------------------------------
!                                            Multiplication@OperatorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Int8_obj( obj1, obj2 ) RESULT( ans )
  INTEGER( Int8 ), INTENT( IN ) :: obj1
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_Int8_obj
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Int16_obj( obj1, obj2 ) RESULT( ans )
  INTEGER( Int16 ), INTENT( IN ) :: obj1
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_Int16_obj
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Int32_obj( obj1, obj2 ) RESULT( ans )
  INTEGER( Int32 ), INTENT( IN ) :: obj1
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_Int32_obj
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Int64_obj( obj1, obj2 ) RESULT( ans )
  INTEGER( Int64 ), INTENT( IN ) :: obj1
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_Int64_obj
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Real32_obj( obj1, obj2 ) &
  & RESULT( ans )
  REAL( Real32 ), INTENT( IN ) :: obj1
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_Real32_obj
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION func_Multiplication_Real64_obj( obj1, obj2 ) &
  & RESULT( ans )
  REAL( Real64 ), INTENT( IN ) :: obj1
  CLASS( Polynomial1D_ ), INTENT( IN ) :: obj2
  TYPE( Polynomial1D_ ) :: ans
END FUNCTION func_Multiplication_Real64_obj
END INTERFACE
