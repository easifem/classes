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
!                                                       ReadLine@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Read a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_read_Line(obj, val, iostat, iomsg, &
  & ignoreComment, ignoreBlank, commentSymbol, separator )
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(OUT) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
  CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: separator
  END SUBROUTINE txt_read_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ReadLine@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Read a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_read_Lines(obj, val, iostat, iomsg, &
  & ignoreComment, ignoreBlank, commentSymbol, separator )
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: val( : )
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
  CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: separator
  END SUBROUTINE txt_read_Lines
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ReadLine@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Read a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_read_Char(obj, val, iostat, iomsg, &
  & ignoreComment, ignoreBlank, commentSymbol, separator )
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    CHARACTER( LEN = * ), INTENT(OUT) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
  CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: separator
  END SUBROUTINE txt_read_Char
END INTERFACE

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a scalar integer

#define __SUBROUTINE_NAME__ txt_read_Int8
#define __DATA_TYPE__ INTEGER( Int8 )
#include "./ReadIntScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_read_Int16
#define __DATA_TYPE__ INTEGER( Int16 )
#include "./ReadIntScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_read_Int32
#define __DATA_TYPE__ INTEGER( Int32 )
#include "./ReadIntScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_read_Int64
#define __DATA_TYPE__ INTEGER( Int64 )
#include "./ReadIntScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!                                                          read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a scalar real value

#define __SUBROUTINE_NAME__ txt_read_Real32
#define __DATA_TYPE__ REAL( Real32 )
#include "./ReadRealScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_read_Real64
#define __DATA_TYPE__ REAL( Real64 )
#include "./ReadRealScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read an integer vector

#define __SUBROUTINE_NAME__ txt_read_vec_Int8
#define __DATA_TYPE__ INTEGER( Int8 )
#include "./ReadVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_read_vec_Int16
#define __DATA_TYPE__ INTEGER( Int16 )
#include "./ReadVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_read_vec_Int32
#define __DATA_TYPE__ INTEGER( Int32 )
#include "./ReadVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_read_vec_Int64
#define __DATA_TYPE__ INTEGER( Int64 )
#include "./ReadVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a intvector

#define __SUBROUTINE_NAME__ txt_read_IntVector
#define __DATA_TYPE__ TYPE( IntVector_ )
#include "./ReadRealScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a vector of intvector

#define __SUBROUTINE_NAME__ txt_read_vec_IntVector
#define __DATA_TYPE__ TYPE( IntVector_ )
#include "./ReadVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!                                                          read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a real value vector

#define __SUBROUTINE_NAME__ txt_read_vec_Real32
#define __DATA_TYPE__ REAL( Real32 )
#include "./ReadVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_read_vec_Real64
#define __DATA_TYPE__ REAL( Real64 )
#include "./ReadVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__


!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a realvector

#define __SUBROUTINE_NAME__ txt_read_RealVector
#define __DATA_TYPE__ TYPE( RealVector_ )
#include "./ReadRealScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a vector of realvector

#define __SUBROUTINE_NAME__ txt_read_vec_RealVector
#define __DATA_TYPE__ TYPE( RealVector_ )
#include "./ReadVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read an integer matrix

#define __SUBROUTINE_NAME__ txt_read_mat_Int8
#define __DATA_TYPE__ INTEGER( Int8 )
#include "./ReadMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_read_mat_Int16
#define __DATA_TYPE__ INTEGER( Int16 )
#include "./ReadMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_read_mat_Int32
#define __DATA_TYPE__ INTEGER( Int32 )
#include "./ReadMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_read_mat_Int64
#define __DATA_TYPE__ INTEGER( Int64 )
#include "./ReadMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Read a real value vector

#define __SUBROUTINE_NAME__ txt_read_mat_Real32
#define __DATA_TYPE__ REAL( Real32 )
#include "./ReadMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_read_mat_Real64
#define __DATA_TYPE__ REAL( Real64 )
#include "./ReadMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

