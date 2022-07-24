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

MODULE PROCEDURE txt_getnrows
  ans=obj%nrows
END PROCEDURE txt_getnrows

!----------------------------------------------------------------------------
!                                                                 getncol
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getncols
  ans=obj%ncols
END PROCEDURE txt_getncols

!----------------------------------------------------------------------------
!                                                               getChunkSize
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getChunkSize
  ans=obj%chunk_size
END PROCEDURE txt_getChunkSize

!----------------------------------------------------------------------------
!                                                           GetVariableTypes
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getDataTypes
  IF( ALLOCATED( obj%data ) ) THEN
    ALLOCATE( dataType( obj%ncols ) )
    dataType = GetDataType( obj%data(1, : ) )
  END IF
END PROCEDURE txt_getDataTypes

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getValue
  !!
  IF( ALLOCATED( obj%data ) ) THEN
    !!
    SELECT TYPE( val )
    !!
    TYPE IS ( INTEGER( I4B ) )
      !!
      val = obj%data( irow, icol )%to_number( kind=1_I4B )
      !!
    TYPE IS ( REAL( DFP )  )
      !!
      val = obj%data( irow, icol )%to_number( kind=1.0_DFP )
      !!
    TYPE IS ( LOGICAL( LGT ) )
      !!
      val = obj%data( irow, icol )%to_logical( )
      !!
    TYPE IS ( CHARACTER( LEN = * ) )
      !!
      IF( obj%data( irow, icol )%is_allocated( ) ) THEN
        val = obj%data( irow, icol )%chars( )
      ELSE
        val = ""
      END IF
      !!
    TYPE IS ( String )
      !!
      val = obj%data( irow, icol )
      !!
    END SELECT
    !!
  END IF
  !!
END PROCEDURE txt_getValue

!----------------------------------------------------------------------------
!                                                                 getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getColumn
  INTEGER( I4B ) :: ii
  !! counter
  IF( obj%ncols .GE. icol .and. icol .GT. 0 ) THEN
    DO ii = 1, obj%nrows
      CALL obj%getValue( irow=ii, icol=icol, val=val(ii))
    END DO
  END IF
  !!
END PROCEDURE txt_getColumn

!----------------------------------------------------------------------------
!                                                             getRealColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getRealColumn
  !!
  IF( ALLOCATED( obj%data ) ) THEN
    CALL Reallocate( val, obj%nrows )
    CALL obj%getColumn( icol=icol, val=val )
  ELSE
    CALL Reallocate( val, 0_I4B )
  END IF
  !!
END PROCEDURE txt_getRealColumn

!----------------------------------------------------------------------------
!                                                             getRealColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getRealVectorColumn
  !!
  IF( ALLOCATED( obj%data ) ) THEN
    CALL Allocate( val, obj%nrows )
    CALL obj%getColumn( icol=icol, val=val%val )
  ELSE
    CALL Allocate( val, 0_I4B )
  END IF
  !!
END PROCEDURE txt_getRealVectorColumn

!----------------------------------------------------------------------------
!                                                              getIntColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getIntColumn
  !!
  IF( ALLOCATED( obj%data ) ) THEN
    CALL Reallocate( val, obj%nrows )
    CALL obj%getColumn( icol=icol, val=val )
  ELSE
    CALL Reallocate( val, 0_I4B )
  END IF
  !!
END PROCEDURE txt_getIntColumn

!----------------------------------------------------------------------------
!                                                              getIntColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getIntVectorColumn
  !!
  IF( ALLOCATED( obj%data ) ) THEN
    CALL Allocate( val, obj%nrows )
    CALL obj%getColumn( icol=icol, val=val%val )
  ELSE
    CALL Allocate( val, 0_I4B )
  END IF
  !!
END PROCEDURE txt_getIntVectorColumn

!----------------------------------------------------------------------------
!                                                           getStringColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getStringColumn
  !!
  IF( ALLOCATED( obj%data ) ) THEN
    CALL Reallocate( val, obj%nrows )
    CALL obj%getColumn( icol=icol, val=val )
  ELSE
    CALL Reallocate( val, 0_I4B )
  END IF
  !!
END PROCEDURE txt_getStringColumn

!----------------------------------------------------------------------------
!                                                         getLogicalColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getLogicalColumn
  !!
  IF( ALLOCATED( obj%data ) ) THEN
    CALL Reallocate( val, obj%nrows )
    CALL obj%getColumn( icol=icol, val=val )
  ELSE
    CALL Reallocate( val, 0_I4B )
  END IF
  !!
END PROCEDURE txt_getLogicalColumn

END SUBMODULE GetMethods
