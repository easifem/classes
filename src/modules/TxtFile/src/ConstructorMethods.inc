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
!                                            addSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Add surrogate to exception handler of the module

INTERFACE
  MODULE SUBROUTINE txt_addSurrogate(obj, UserObj)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    TYPE(ExceptionHandler_), INTENT(IN) :: UserObj
  END SUBROUTINE txt_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Initiate the txt file

INTERFACE
  MODULE SUBROUTINE txt_initiate(obj, filename, unit, status, access, form, &
    & position, action, pad, recl, comment, separator, delimiter)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unit
    !! User specified unit number, it should  not be `stdout, stdin, stderr`
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: status
    !! OLD, NEW, SCRATCH, REPLACE, UNKNOWN
    !! If UNKNOWN then we use REPLACE
    !! Default is REPLACE
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: access
    !! DIRECT, SEQUENTIAL, STREAM
    !! Default is SEQUENTIAL
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: form
    !! FORMATTED, UNFORMATTED
    !! Default is FORMATTED
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: position
    !! REWIND, APPEND, ASIS
    !! Default is ASIS
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: action
    !! READ, WRITE, READWRITE
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: pad
    !! YES, NO
    !! Default is YES
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: recl
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: comment
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: delimiter
  END SUBROUTINE txt_initiate
END INTERFACE

INTERFACE TxtFileInitiate
  MODULE PROCEDURE txt_initiate
END INTERFACE TxtFileInitiate

PUBLIC :: TxtFileInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Deallocate the data

INTERFACE
  MODULE SUBROUTINE txt_Deallocate(obj, Delete)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: Delete
  END SUBROUTINE txt_Deallocate
END INTERFACE

INTERFACE TxtFileDeallocate
  MODULE PROCEDURE txt_Deallocate
END INTERFACE TxtFileDeallocate

PUBLIC :: TxtFileDeallocate

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_final( obj )
  TYPE(TxtFile_), INTENT( INOUT ) :: obj
END SUBROUTINE txt_final
END INTERFACE