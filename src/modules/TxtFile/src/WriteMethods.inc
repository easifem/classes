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
!                                       ConvertMarkdownToSource@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-07
! update: 2021-11-07
! summary: Reads a markdown file and converts it into the source file

INTERFACE
  MODULE SUBROUTINE txt_convertMarkdownToSource(obj, outfile, lang)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(TxtFile_), INTENT(INOUT) :: outfile
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: lang
  END SUBROUTINE txt_convertMarkdownToSource
END INTERFACE

!----------------------------------------------------------------------------
!                                                       WriteLine@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Write a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_write_Blank(obj)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    !! YES or NO
  END SUBROUTINE txt_write_Blank
END INTERFACE

!----------------------------------------------------------------------------
!                                                       WriteLine@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Write a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_write_Line(obj, val, iostat, iomsg, advance)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    !! YES or NO
  END SUBROUTINE txt_write_Line
END INTERFACE

INTERFACE TxtFileWrite
  MODULE PROCEDURE txt_write_Line
END INTERFACE TxtFileWrite

PUBLIC :: TxtFileWrite

!----------------------------------------------------------------------------
!                                                       WriteLine@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Write a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_write_Lines(obj, val, iostat, iomsg, advance)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: val(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    !! YES, NO
  END SUBROUTINE txt_write_Lines
END INTERFACE

INTERFACE TxtFileWrite
  MODULE PROCEDURE txt_write_Lines
END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!                                                       WriteLine@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Write a single line (record)

INTERFACE
  MODULE SUBROUTINE txt_write_Char(obj, val, iostat, iomsg, &
    &  advance)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: val
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: iostat
    CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: advance
    !! YES, NO
  END SUBROUTINE txt_write_Char
END INTERFACE

INTERFACE TxtFileWrite
  MODULE PROCEDURE txt_write_Char
END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!                                                           Write@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write an integer

#define __SUBROUTINE_NAME__ txt_write_Int8
#define __DATA_TYPE__ INTEGER( Int8 )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_Int16
#define __DATA_TYPE__ INTEGER( Int16 )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_Int32
#define __DATA_TYPE__ INTEGER( Int32 )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_Int64
#define __DATA_TYPE__ INTEGER( Int64 )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

INTERFACE TxtFileWrite
  MODULE PROCEDURE txt_write_Int8, txt_write_Int16, txt_write_Int32, &
    & txt_write_Int64
END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write a real value

#define __SUBROUTINE_NAME__ txt_write_Real32
#define __DATA_TYPE__ REAL( Real32 )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_Real64
#define __DATA_TYPE__ REAL( Real64 )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

INTERFACE TxtFileWrite
  MODULE PROCEDURE txt_write_Real32, txt_write_Real64
END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!                                                           Write@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write an integer vector

#define __SUBROUTINE_NAME__ txt_write_vec_Int8
#define __DATA_TYPE__ INTEGER( Int8 )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_vec_Int16
#define __DATA_TYPE__ INTEGER( Int16 )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_vec_Int32
#define __DATA_TYPE__ INTEGER( Int32 )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_vec_Int64
#define __DATA_TYPE__ INTEGER( Int64 )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

INTERFACE TxtFileWrite
  MODULE PROCEDURE txt_write_vec_Int8, &
    & txt_write_vec_Int16, &
    & txt_write_vec_Int32, &
    & txt_write_vec_Int64
END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!                                                         write@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write a intvector

#define __SUBROUTINE_NAME__ txt_write_IntVector
#define __DATA_TYPE__ TYPE( IntVector_ )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

INTERFACE TxtFileWrite
  MODULE PROCEDURE txt_write_IntVector
END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write a vector of intvector

#define __SUBROUTINE_NAME__ txt_write_vec_IntVector
#define __DATA_TYPE__ TYPE( IntVector_ )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

INTERFACE TxtFileWrite
  MODULE PROCEDURE txt_write_vec_IntVector
END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write a real value vector

#define __SUBROUTINE_NAME__ txt_write_vec_Real32
#define __DATA_TYPE__ REAL( Real32 )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_vec_Real64
#define __DATA_TYPE__ REAL( Real64 )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

INTERFACE TxtFileWrite
  MODULE PROCEDURE txt_write_vec_Real32, txt_write_vec_Real64
END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!                                                         write@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write a RealVector

#define __SUBROUTINE_NAME__ txt_write_RealVector
#define __DATA_TYPE__ TYPE( RealVector_ )
#include "./WriteScalar.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

INTERFACE TxtFileWrite
  MODULE PROCEDURE txt_write_RealVector
END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!                                                           read@ReadMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write a vector of RealVector

#define __SUBROUTINE_NAME__ txt_write_vec_RealVector
#define __DATA_TYPE__ TYPE( RealVector_ )
#include "./WriteVector.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

INTERFACE TxtFileWrite
  MODULE PROCEDURE txt_write_vec_RealVector
END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!                                                           Write@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write an integer matrix

#define __SUBROUTINE_NAME__ txt_write_mat_Int8
#define __DATA_TYPE__ INTEGER( Int8 )
#include "./WriteMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_mat_Int16
#define __DATA_TYPE__ INTEGER( Int16 )
#include "./WriteMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_mat_Int32
#define __DATA_TYPE__ INTEGER( Int32 )
#include "./WriteMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_mat_Int64
#define __DATA_TYPE__ INTEGER( Int64 )
#include "./WriteMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

INTERFACE TxtFileWrite
  MODULE PROCEDURE txt_write_mat_Int8, &
    & txt_write_mat_Int16, &
    & txt_write_mat_Int32, &
    & txt_write_mat_Int64
END INTERFACE TxtFileWrite

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 July 2022
! summary: Write a real value vector

#define __SUBROUTINE_NAME__ txt_write_mat_Real32
#define __DATA_TYPE__ REAL( Real32 )
#include "./WriteMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

#define __SUBROUTINE_NAME__ txt_write_mat_Real64
#define __DATA_TYPE__ REAL( Real64 )
#include "./WriteMatrix.inc"
#undef __SUBROUTINE_NAME__
#undef __DATA_TYPE__

INTERFACE TxtFileWrite
  MODULE PROCEDURE txt_write_mat_Real32, &
    & txt_write_mat_Real64
END INTERFACE TxtFileWrite

