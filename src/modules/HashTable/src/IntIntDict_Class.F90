! This program is a part of EASIFEM library
! Copyright (C) (Since 2020)  Vikas Sharma, Ph.D
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
! date:
! summary:  Module for HashTable for Integers

! Define the module for the key type.
! Override the hash_value and == operator interface.

MODULE IntIntDictUtility
USE GlobalData, ONLY: I4B, LGT
IMPLICIT NONE

INTERFACE hash_value
  MODULE PROCEDURE int_hash_value
END INTERFACE

CONTAINS

FUNCTION int_hash_value(int) RESULT(hash)
  INTEGER(I4B), INTENT(in) :: int
  INTEGER(I4B) :: hash
  hash = int
END FUNCTION int_hash_value

END MODULE IntIntDictUtility

!----------------------------------------------------------------------------
!                                                          IntIntDict_Class
!----------------------------------------------------------------------------

#define FHASH_MODULE_NAME IntIntDict_Class
#define FHASH_TYPE_NAME IntIntDict_
#define FHASH_TYPE_ITERATOR_NAME IntIntDictIterator_

! Define the macros needed by fhash and include fhash.f90
#define KEY_USE USE IntIntDictUtility
!! This is the name of the module where hash_value function for key is
!! defined

#define KEY_TYPE INTEGER(I4B)
!! The data type for key

! #define VALUE_USE use GlobalData, ONLY: I4B, DFP, LGT
!! This is the name of the module where value is defined

#define VALUE_TYPE INTEGER(I4B)
!! This is the data type for value

#define VALUE_TYPE_INIT 0
!! Initial value of data type

#ifndef __GFORTRAN__
#define VALUE_POINTER
#endif

#ifdef VALUE_TYPE_INIT
#define CHECK_ITERATOR_VALUE
#endif

#include "./fhash.inc"
