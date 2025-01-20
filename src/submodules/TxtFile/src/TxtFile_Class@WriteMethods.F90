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

SUBMODULE(TxtFile_Class) WriteMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                    convertMarkdownToSource
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_convertMarkdownToSource
  CHARACTER(LEN=*), PARAMETER :: myName = "txt_convertMarkdownToSource"
  TYPE(String) :: aline, lang0
  INTEGER(I4B) :: iostat
  INTEGER(I4B) :: inUnit
  INTEGER(I4B) :: outUnit
  INTEGER(I4B) :: tagcount
  INTEGER( I4B ) :: ii
  LOGICAL(LGT) :: insideCodeBlock
  CHARACTER(len=100) :: iomsg
  !!
  !! check
  !!
  IF (.NOT. obj%isInitiated() .OR. .NOT. obj%isOpen()) &
    & CALL e%raiseError(modName//"::"//myName//" - "// &
    & 'Markdown file is not initiated.')
  !!
  !! check
  !!
  IF (.NOT. outfile%isInitiated() .OR. .NOT. outfile%isOpen()) &
    & CALL e%raiseError(modName//"::"//myName//" - "// &
    & 'output file is not initiated')
  !!
  !! check
  !!
  IF (.NOT. obj%isRead()) &
    & CALL e%raiseError(modName//"::"//myName//" - "// &
    & 'Markdown file does not have read access')
  !!
  !! check
  !!
  IF (.NOT. outfile%isWrite()) &
    & CALL e%raiseError(modName//"::"//myName//" - "// &
    & 'Source file does not have write access')
  !!
  IF( PRESENT( lang ) ) THEN
    lang0 = TRIM(lang)
  ELSE
    lang0 = "fortran"
  END IF
  !!
  !! program starts
  !!
  inUnit = obj%getUnitNo()
  outUnit = outfile%getUnitNo()
  insideCodeBlock = .FALSE.
  tagcount = 0
  !!
  DO
    !!
    CALL obj%readLine(val=aline, iostat=iostat, iomsg=iomsg)
    !!
    IF (obj%isEOF()) EXIT
    !!
    IF (aline%len_trim() .NE. 0) THEN
      !!
      IF (aline%slice(1, 3) .EQ. '```') THEN
        !!
        tagcount = tagcount + 1
        !!
        IF (MOD(tagcount, 2) .EQ. 0) THEN
          insideCodeBlock = .FALSE.
        ELSE
          ii = aline%index(lang0)
          IF( ii .EQ. 0 ) THEN
            insideCodeBlock = .FALSE.
          ELSE
            insideCodeBlock = .TRUE.
            aline = ""
          END IF
        END IF
        !!
      END IF
      !!
      IF (insideCodeBlock) THEN
        WRITE (outUnit, "(a)") aline%chars()
      END IF
      !!
    END IF
    !
  END DO
  !!
END PROCEDURE txt_convertMarkdownToSource

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_Blank
  CHARACTER(LEN=*), PARAMETER :: myName = 'txt_write_Blank'
  INTEGER( I4B ) :: ioerr
  !!
  IF (obj%isOpen() .AND. obj%isWrite() ) THEN
    !!
    WRITE( &
      & UNIT=obj%getUnitNo(), &
      & FMT='(A)', &
      & ADVANCE="YES", &
      & IOSTAT=ioerr ) ""
  ELSE
    CALL e%raiseError(modName //'::'//myName// ' - '// &
      & 'Either file is cloased or it does not have write access')
    !!
  END IF
END PROCEDURE txt_write_Blank

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_Line
  CHARACTER(LEN=*), PARAMETER :: myName = 'txt_write_Line'
  INTEGER( I4B ) :: ioerr
  CHARACTER( LEN = 3 ) :: adv0
  !!
  IF (obj%isOpen() .AND. obj%isWrite() ) THEN
    !!
    IF( PRESENT( advance ) ) THEN
      adv0 = TRIM( UpperCase( advance ) )
    ELSE
      adv0 = "YES"
    END IF
    !!
    IF( TRIM(adv0) .EQ. "YES" ) THEN
      !!
      WRITE( &
        & UNIT=obj%getUnitNo(), &
        & FMT='(A)', &
        & ADVANCE="YES", &
        & IOSTAT=ioerr, &
        & IOMSG=iomsg ) val%chars()
      !!
    ELSE
      !!
      WRITE( &
        & UNIT=obj%getUnitNo(), &
        & FMT='(A)', &
        & ADVANCE="NO", &
        & IOSTAT=ioerr, &
        & IOMSG=iomsg ) val%chars() // TRIM(obj%separator)
      !!
    END IF
    !!
    IF( PRESENT( iostat ) ) iostat = ioerr
    !!
  ELSE
    !!
    CALL e%raiseError(modName //'::'//myName// ' - '// &
      & 'Either file is cloased or it does not have write access')
    !!
  END IF
END PROCEDURE txt_write_Line

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_Lines
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_Lines"
  CHARACTER( LEN = 3 ) :: adv0
  INTEGER( I4B ) :: ii, unit, ioerr
  !!
  IF (obj%isOpen() .AND. obj%isWrite() ) THEN
    !!
    IF( PRESENT( advance ) ) THEN
      adv0 = UpperCase(advance)
    ELSE
      adv0 = "YES"
    END IF
    !!
    unit = obj%getUnitNo()
    !!
    DO ii = 1, SIZE(val)
      WRITE( &
        & UNIT=unit, &
        & FMT='(A)', &
        & ADVANCE=adv0, &
        & IOSTAT=ioerr, &
        & IOMSG=iomsg ) val(ii)%chars()
    END DO
    !!
    IF( PRESENT( iostat ) ) iostat = ioerr
    !!
  ELSE
    CALL e%raiseError(modName //'::'//myName// ' - '// &
      & 'Either file is cloased or it does not have write access')
    !!
  END IF
END PROCEDURE txt_write_Lines

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_Char
  TYPE(String) :: aline
  aline = val
  CALL obj%writeLine( &
    & val=aline, &
    & iostat=iostat, &
    & iomsg=iomsg, &
    & advance=advance )
  aline=""
END PROCEDURE txt_write_Char

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_Int8
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_Int8"
#include "./Write_Scalar.inc"
END PROCEDURE txt_write_Int8

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_Int16
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_Int16"
#include "./Write_Scalar.inc"
END PROCEDURE txt_write_Int16

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_Int32
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_Int32"
#include "./Write_Scalar.inc"
END PROCEDURE txt_write_Int32

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_Int64
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_Int64"
#include "./Write_Scalar.inc"
END PROCEDURE txt_write_Int64

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_Real32
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_Real32"
#include "./Write_Scalar.inc"
END PROCEDURE txt_write_Real32

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_Real64
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_Real64"
#include "./Write_Scalar.inc"
END PROCEDURE txt_write_Real64

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_vec_Int8
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_vec_Int8"
#include "./Write_Vector.inc"
END PROCEDURE txt_write_vec_Int8

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_vec_Int16
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_vec_Int16"
#include "./Write_Vector.inc"
END PROCEDURE txt_write_vec_Int16

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_vec_Int32
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_vec_Int32"
#include "./Write_Vector.inc"
END PROCEDURE txt_write_vec_Int32

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_vec_Int64
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_vec_Int64"
#include "./Write_Vector.inc"
END PROCEDURE txt_write_vec_Int64

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_IntVector
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_IntVector"
  IF( isInitiated( val ) ) THEN
    CALL obj%write( &
      & val = val%val, &
      & iostat=iostat, &
      & iomsg=iomsg, &
      & advance=advance, &
      & orient=orient )
  END IF
END PROCEDURE txt_write_IntVector

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_vec_IntVector
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_vec_IntVector"
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( val )
    CALL obj%write( &
      & val = val(ii)%val, &
      & iostat=iostat, &
      & iomsg=iomsg, &
      & advance=advance, &
      & orient="ROW" )
  END DO
END PROCEDURE txt_write_vec_IntVector

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_vec_Real32
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_vec_Real32"
#include "./Write_Vector.inc"
END PROCEDURE txt_write_vec_Real32

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_vec_Real64
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_vec_Real64"
#include "./Write_Vector.inc"
END PROCEDURE txt_write_vec_Real64

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_RealVector
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_RealVector"
  IF( isInitiated( val ) ) THEN
    CALL obj%write( &
      & val = val%val, &
      & iostat=iostat, &
      & iomsg=iomsg, &
      & advance=advance, &
      & orient=orient )
  END IF
END PROCEDURE txt_write_RealVector

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_vec_RealVector
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_vec_RealVector"
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( val )
    CALL obj%write( &
      & val = val(ii)%val, &
      & iostat=iostat, &
      & iomsg=iomsg, &
      & advance=advance, &
      & orient="ROW" )
  END DO
END PROCEDURE txt_write_vec_RealVector

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_mat_Int8
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_mat_Int8"
#include "./Write_Matrix.inc"
END PROCEDURE txt_write_mat_Int8

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_mat_Int16
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_mat_Int16"
#include "./Write_Matrix.inc"
END PROCEDURE txt_write_mat_Int16

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_mat_Int32
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_mat_Int32"
#include "./Write_Matrix.inc"
END PROCEDURE txt_write_mat_Int32

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_mat_Int64
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_mat_Int64"
#include "./Write_Matrix.inc"
END PROCEDURE txt_write_mat_Int64

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_mat_Real32
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_mat_Real32"
#include "./Write_Matrix.inc"
END PROCEDURE txt_write_mat_Real32

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_write_mat_Real64
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_write_mat_Real64"
#include "./Write_Matrix.inc"
END PROCEDURE txt_write_mat_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE WriteMethods
