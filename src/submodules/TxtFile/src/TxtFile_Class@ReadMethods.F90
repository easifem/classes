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

SUBMODULE(TxtFile_Class) ReadMethods
USE ISO_FORTRAN_ENV, ONLY: IOSTAT_EOR, IOSTAT_END
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 readLine
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_Line
CHARACTER(*), PARAMETER :: myName = 'txt_read_Line'
CHARACTER(maxStrLen) :: buffer
INTEGER(I4B) :: buffer_size, eioerr, ioerr

ioerr = 0
val = ""

IF (obj%isOpen() .AND. .NOT. obj%isEOF()) THEN

  DO WHILE (ioerr .NE. IOSTAT_EOR .AND. ioerr .NE. IOSTAT_END)

    ! Repeatedly read chunks of current input
    ! file line into buffer
    READ ( &
      & UNIT=obj%getUnitNo(), &
      & FMT='(a)', &
      & SIZE=buffer_size, &
      & ADVANCE='NO', &
      & IOSTAT=ioerr) buffer

    IF (ioerr .EQ. IOSTAT_END) THEN
      ! end of file
      CALL obj%setEOFstat(.TRUE.)
      ! Done reading line. Append last buffer to line.

    ELSEIF (ioerr .EQ. IOSTAT_EOR) THEN
      val = val//TRIM(buffer)

      IF (obj%echostat) THEN

        WRITE (UNIT=obj%echounit, FMT='(a)', IOSTAT=eioerr) &
          & TRIM(val%chars())

        IF (eioerr .NE. 0) THEN
          CALL e%raiseError(modName//'::'//myName//" - "// &
            &' - Error echoing line to UNIT='//tostring(obj%echounit)//&
            & ' (IOSTAT='//tostring(eioerr)//')!')
        END IF

      END IF

      val = TRIM(val)

    ELSEIF (ioerr .LT. IOSTAT_EOR) THEN

      ! Error reading line from input file

      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Error reading one line from input file (IOSTAT='// &
        & tostring(ioerr)//')!')

    ELSE

      ! Still reading current line. Append buffer to line

      val = val//buffer

    END IF
  END DO
END IF
IF (PRESENT(iostat)) iostat = ioerr
IF (PRESENT(iomsg)) iomsg = tostring(ioerr)
END PROCEDURE txt_read_Line

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_Lines
CHARACTER(*), PARAMETER :: myName = "txt_read_Lines"
INTEGER(I4B) :: nn, ioerr, unitno, jj
TYPE(String) :: aline
!
nn = obj%getTotalRecords(ignoreComment=ignoreComment, &
  & ignoreBlank=ignoreBlank, &
  & commentSymbol=commentSymbol)
!
IF (ALLOCATED(val)) DEALLOCATE (val)
ALLOCATE (val(nn))
!
ioerr = 0
jj = 0
!
IF (obj%isOpen() .AND. .NOT. obj%isEOF()) THEN
  unitno = obj%getUnitNo()
  !
  DO
    !
    CALL obj%readLine(val=aline, iostat=ioerr, iomsg=iomsg)
    !
    IF (obj%isEOF()) EXIT
    !
    IF (obj%isValidRecord(aline=aline, &
      & ignoreComment=ignoreComment, &
      & ignoreBlank=ignoreBlank, &
      & commentSymbol=commentSymbol)) THEN
      !
      jj = jj + 1
      !
      val(jj) = aline
      !
    END IF
    !
  END DO
  !
  aline = ""
  !
END IF
!
IF (ioerr .LT. IOSTAT_EOR) THEN
  !
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & ' - Error reading a scalar from the file (IOSTAT='// &
    & tostring(iostat)//')!')
  !
END IF
!
IF (PRESENT(iostat)) iostat = ioerr
!
END PROCEDURE txt_read_Lines

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_Char
TYPE(String) :: aline
CALL obj%readLine(val=aline, iostat=iostat, iomsg=iomsg)
val = aline%chars()
aline = ""
END PROCEDURE txt_read_Char

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_boolean
CHARACTER(*), PARAMETER :: myName = "txt_read_Int8"
LOGICAL( LGT ) :: val_kind
#include "./Read_BooleanScalar.F90"
END PROCEDURE txt_read_boolean

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_Int8
CHARACTER(*), PARAMETER :: myName = "txt_read_Int8"
INTEGER(INT8) :: val_kind
#include "./Read_IntScalar.F90"
END PROCEDURE txt_read_Int8

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_Int16
CHARACTER(*), PARAMETER :: myName = "txt_read_Int16"
INTEGER(INT16) :: val_kind
#include "./Read_IntScalar.F90"
END PROCEDURE txt_read_Int16

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_Int32
CHARACTER(*), PARAMETER :: myName = "txt_read_Int32"
INTEGER(INT32) :: val_kind
#include "./Read_IntScalar.F90"
END PROCEDURE txt_read_Int32

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_Int64
CHARACTER(*), PARAMETER :: myName = "txt_read_Int64"
INTEGER(INT64) :: val_kind
#include "./Read_IntScalar.F90"
END PROCEDURE txt_read_Int64

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_Real32
CHARACTER(*), PARAMETER :: myName = "txt_read_Real32"
REAL(REAL32) :: val_kind
#include "./Read_RealScalar.F90"
END PROCEDURE txt_read_Real32

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_Real64
CHARACTER(*), PARAMETER :: myName = "txt_read_Real64"
REAL(REAL64) :: val_kind
#include "./Read_RealScalar.F90"
END PROCEDURE txt_read_Real64

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_vec_boolean
CHARACTER(*), PARAMETER :: myName = "txt_read_vec_boolean"
LOGICAL( LGT ) :: val_kind
#include "./Read_Vector_Boolean.F90"
END PROCEDURE txt_read_vec_boolean

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_vec_Int8
CHARACTER(*), PARAMETER :: myName = "txt_read_vec_Int8"
INTEGER(INT8) :: val_kind
TYPE(IntVector_), ALLOCATABLE :: vals(:)
#include "./Read_Vector.F90"
END PROCEDURE txt_read_vec_Int8

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_vec_Int16
CHARACTER(*), PARAMETER :: myName = "txt_read_vec_Int16"
INTEGER(INT16) :: val_kind
TYPE(IntVector_), ALLOCATABLE :: vals(:)
#include "./Read_Vector.F90"
END PROCEDURE txt_read_vec_Int16

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_vec_Int32
CHARACTER(*), PARAMETER :: myName = "txt_read_vec_Int32"
INTEGER(INT32) :: val_kind
TYPE(IntVector_), ALLOCATABLE :: vals(:)
#include "./Read_Vector.F90"
END PROCEDURE txt_read_vec_Int32

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_vec_Int64
CHARACTER(*), PARAMETER :: myName = "txt_read_vec_Int64"
INTEGER(INT64) :: val_kind
TYPE(IntVector_), ALLOCATABLE :: vals(:)
#include "./Read_Vector.F90"
END PROCEDURE txt_read_vec_Int64

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_IntVector
CHARACTER(*), PARAMETER :: myName = "txt_read_IntVector"
INTEGER(I4B) :: val_kind0
TYPE(IntVector_) :: val_kind
TYPE(IntVector_), ALLOCATABLE :: vals(:)
#include "./Read_IntVector_RealVector.F90"
END PROCEDURE txt_read_IntVector

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_vec_IntVector
CHARACTER(*), PARAMETER :: myName = "txt_read_vec_IntVector"
INTEGER(I4B) :: val_kind
#include "./Read_Vector_IntVector_RealVector.F90"
END PROCEDURE txt_read_vec_IntVector

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_vec_Real32
CHARACTER(*), PARAMETER :: myName = "txt_read_vec_Real32"
REAL(REAL32) :: val_kind
TYPE(RealVector_), ALLOCATABLE :: vals(:)
#include "./Read_Vector.F90"
END PROCEDURE txt_read_vec_Real32

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_vec_Real64
CHARACTER(*), PARAMETER :: myName = "txt_read_vec_Real64"
REAL(REAL64) :: val_kind
TYPE(RealVector_), ALLOCATABLE :: vals(:)
#include "./Read_Vector.F90"
END PROCEDURE txt_read_vec_Real64

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_RealVector
CHARACTER(*), PARAMETER :: myName = "txt_read_RealVector"
REAL(DFP) :: val_kind0
TYPE(RealVector_) :: val_kind
TYPE(RealVector_), ALLOCATABLE :: vals(:)
#include "./Read_IntVector_RealVector.F90"
END PROCEDURE txt_read_RealVector

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_vec_RealVector
CHARACTER(*), PARAMETER :: myName = "txt_read_vec_RealVector"
REAL(DFP) :: val_kind
#include "./Read_Vector_IntVector_RealVector.F90"
END PROCEDURE txt_read_vec_RealVector

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_mat_Int8
CHARACTER(*), PARAMETER :: myName = "txt_read_mat_Int8"
INTEGER(INT8) :: val_kind
TYPE(IntVector_), ALLOCATABLE :: vals(:)
#include "./Read_Matrix.F90"
END PROCEDURE txt_read_mat_Int8

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_mat_Int16
CHARACTER(*), PARAMETER :: myName = "txt_read_mat_Int16"
INTEGER(INT16) :: val_kind
TYPE(IntVector_), ALLOCATABLE :: vals(:)
#include "./Read_Matrix.F90"
END PROCEDURE txt_read_mat_Int16

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_mat_Int32
CHARACTER(*), PARAMETER :: myName = "txt_read_mat_Int32"
INTEGER(INT32) :: val_kind
TYPE(IntVector_), ALLOCATABLE :: vals(:)
#include "./Read_Matrix.F90"
END PROCEDURE txt_read_mat_Int32

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_mat_Int64
CHARACTER(*), PARAMETER :: myName = "txt_read_mat_Int64"
INTEGER(INT64) :: val_kind
TYPE(IntVector_), ALLOCATABLE :: vals(:)
#include "./Read_Matrix.F90"
END PROCEDURE txt_read_mat_Int64

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_mat_Real32
CHARACTER(*), PARAMETER :: myName = "txt_read_mat_Real32"
REAL(REAL32) :: val_kind
TYPE(RealVector_), ALLOCATABLE :: vals(:)
#include "./Read_Matrix.F90"
END PROCEDURE txt_read_mat_Real32

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_read_mat_Real64
CHARACTER(*), PARAMETER :: myName = "txt_read_mat_Real64"
REAL(REAL64) :: val_kind
TYPE(RealVector_), ALLOCATABLE :: vals(:)
#include "./Read_Matrix.F90"
END PROCEDURE txt_read_mat_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ReadMethods
