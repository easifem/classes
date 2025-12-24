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

SUBMODULE(CSVFile_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 getnrow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getnrows
ans = obj%nrows
END PROCEDURE obj_getnrows

!----------------------------------------------------------------------------
!                                                                 getncol
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getncols
ans = obj%ncols
END PROCEDURE obj_getncols

!----------------------------------------------------------------------------
!                                                               getChunkSize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getChunkSize
ans = obj%chunk_size
END PROCEDURE obj_getChunkSize

!----------------------------------------------------------------------------
!                                                           GetVariableTypes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getDataTypes
IF (ALLOCATED(obj%DATA)) THEN
  ALLOCATE (dataType(obj%ncols))
  dataType = GetDataType(obj%DATA(1, :))
END IF
END PROCEDURE obj_getDataTypes

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getValue
IF (ALLOCATED(obj%DATA)) THEN
  SELECT TYPE (val)
  TYPE IS (INTEGER(INT8))
    val = obj%DATA(irow, icol)%to_number(kind=1_INT8)
  TYPE IS (INTEGER(INT16))
    val = obj%DATA(irow, icol)%to_number(kind=1_INT16)
  TYPE IS (INTEGER(INT32))
    val = obj%DATA(irow, icol)%to_number(kind=1_INT32)
  TYPE IS (INTEGER(INT64))
    val = obj%DATA(irow, icol)%to_number(kind=1_INT64)
  TYPE IS (REAL(REAL32))
    val = obj%DATA(irow, icol)%to_number(kind=1.0_REAL32)
  TYPE IS (REAL(REAL64))
    val = obj%DATA(irow, icol)%to_number(kind=1.0_REAL64)
  TYPE IS (LOGICAL(LGT))
    val = obj%DATA(irow, icol)%to_logical()
  TYPE IS (CHARACTER(LEN=*))
    IF (obj%DATA(irow, icol)%is_allocated()) THEN
      val = obj%DATA(irow, icol)%chars()
    ELSE
      val = ""
    END IF
  TYPE IS (String)
    val = obj%DATA(irow, icol)
  END SELECT
END IF
END PROCEDURE obj_getValue

!----------------------------------------------------------------------------
!                                                                 getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getColumn
INTEGER(I4B) :: ii
  !! counter
IF (obj%ncols .GE. icol .AND. icol .GT. 0) THEN
  DO ii = 1, obj%nrows
    CALL obj%getValue(irow=ii, icol=icol, val=val(ii))
  END DO
END IF
  !!
END PROCEDURE obj_getColumn

!----------------------------------------------------------------------------
!                                                             getRealColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRealColumn_real32
  !!
IF (ALLOCATED(obj%DATA)) THEN
  CALL Reallocate(val, obj%nrows)
  CALL obj%getColumn(icol=icol, val=val)
ELSE
  CALL Reallocate(val, 0_I4B)
END IF
  !!
END PROCEDURE obj_GetRealColumn_real32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRealColumn_real64
  !!
IF (ALLOCATED(obj%DATA)) THEN
  CALL Reallocate(val, obj%nrows)
  CALL obj%getColumn(icol=icol, val=val)
ELSE
  CALL Reallocate(val, 0_I4B)
END IF
  !!
END PROCEDURE obj_GetRealColumn_real64

!----------------------------------------------------------------------------
!                                                             getRealColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getRealVectorColumn
  !!
IF (ALLOCATED(obj%DATA)) THEN
  CALL ALLOCATE (val, obj%nrows)
  CALL obj%getColumn(icol=icol, val=val%val)
ELSE
  CALL ALLOCATE (val, 0_I4B)
END IF
  !!
END PROCEDURE obj_getRealVectorColumn

!----------------------------------------------------------------------------
!                                                              getIntColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getIntColumn_int8
  !!
IF (ALLOCATED(obj%DATA)) THEN
  CALL Reallocate(val, obj%nrows)
  CALL obj%getColumn(icol=icol, val=val)
ELSE
  CALL Reallocate(val, 0_I4B)
END IF
  !!
END PROCEDURE obj_getIntColumn_int8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getIntColumn_int16
  !!
IF (ALLOCATED(obj%DATA)) THEN
  CALL Reallocate(val, obj%nrows)
  CALL obj%getColumn(icol=icol, val=val)
ELSE
  CALL Reallocate(val, 0_I4B)
END IF
  !!
END PROCEDURE obj_getIntColumn_int16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getIntColumn_int32
  !!
IF (ALLOCATED(obj%DATA)) THEN
  CALL Reallocate(val, obj%nrows)
  CALL obj%getColumn(icol=icol, val=val)
ELSE
  CALL Reallocate(val, 0_I4B)
END IF
  !!
END PROCEDURE obj_getIntColumn_int32

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getIntColumn_int64
  !!
IF (ALLOCATED(obj%DATA)) THEN
  CALL Reallocate(val, obj%nrows)
  CALL obj%getColumn(icol=icol, val=val)
ELSE
  CALL Reallocate(val, 0_I4B)
END IF
  !!
END PROCEDURE obj_getIntColumn_int64

!----------------------------------------------------------------------------
!                                                              getIntColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getIntVectorColumn
  !!
IF (ALLOCATED(obj%DATA)) THEN
  CALL ALLOCATE (val, obj%nrows)
  CALL obj%getColumn(icol=icol, val=val%val)
ELSE
  CALL ALLOCATE (val, 0_I4B)
END IF
  !!
END PROCEDURE obj_getIntVectorColumn

!----------------------------------------------------------------------------
!                                                           getStringColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getStringColumn
  !!
IF (ALLOCATED(obj%DATA)) THEN
  CALL Reallocate(val, obj%nrows)
  CALL obj%getColumn(icol=icol, val=val)
ELSE
  CALL Reallocate(val, 0_I4B)
END IF
  !!
END PROCEDURE obj_getStringColumn

!----------------------------------------------------------------------------
!                                                         getLogicalColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getLogicalColumn
  !!
IF (ALLOCATED(obj%DATA)) THEN
  CALL Reallocate(val, obj%nrows)
  CALL obj%getColumn(icol=icol, val=val)
ELSE
  CALL Reallocate(val, 0_I4B)
END IF
  !!
END PROCEDURE obj_getLogicalColumn

END SUBMODULE GetMethods
