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

SUBMODULE(CSVFile_Class) WriteMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_Line
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO")
END PROCEDURE obj_write_Line

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_Lines
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO")
END PROCEDURE obj_write_Lines

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_Char
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO")
END PROCEDURE obj_write_Char

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_Int8
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_Int8

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_Int16
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_Int16

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_Int32
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_Int32

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_Int64
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_Int64

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_Real32
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_Real32

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_Real64
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_Real64

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_vec_Int8
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_vec_Int8

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_vec_Int16
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_vec_Int16

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_vec_Int32
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_vec_Int32

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_vec_Int64
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_vec_Int64

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_IntVector
IF (isInitiated(val)) THEN
  CALL obj%WRITE( &
    & val=val%val, &
    & iostat=iostat, &
    & iomsg=iomsg, &
    & orient="ROW")
END IF
END PROCEDURE obj_write_IntVector

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_vec_IntVector
INTEGER(I4B) :: ii
DO ii = 1, SIZE(val)
    !!
  CALL obj%WRITE( &
    & val=val(ii)%val, &
    & iostat=iostat, &
    & iomsg=iomsg, &
    & orient="ROW")
    !!
  CALL obj%nextRow()
    !!
END DO
END PROCEDURE obj_write_vec_IntVector

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_vec_Real32
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_vec_Real32

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_vec_Real64
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_vec_Real64

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_RealVector
IF (isInitiated(val)) THEN
  CALL obj%WRITE( &
    & val=val%val, &
    & iostat=iostat, &
    & iomsg=iomsg, &
    & orient="ROW")
END IF
END PROCEDURE obj_write_RealVector

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_vec_RealVector
INTEGER(I4B) :: ii
DO ii = 1, SIZE(val)
  CALL obj%WRITE( &
    & val=val(ii)%val, &
    & iostat=iostat, &
    & iomsg=iomsg, &
    & orient="ROW")
  CALL obj%nextRow()
END DO
END PROCEDURE obj_write_vec_RealVector

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_mat_Int8
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_mat_Int8

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_mat_Int16
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_mat_Int16

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_mat_Int32
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_mat_Int32

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_mat_Int64
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_mat_Int64

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_mat_Real32
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_mat_Real32

!----------------------------------------------------------------------------
!                                                                 write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_write_mat_Real64
CALL TxtFileWrite(obj=obj, val=val, iostat=iostat, iomsg=iomsg, &
  & advance="NO", orient="ROW")
END PROCEDURE obj_write_mat_Real64

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE WriteMethods
