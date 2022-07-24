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

SUBMODULE(CSVFile_Class) ReadMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_CSVFileRead
  CHARACTER( LEN = * ), PARAMETER :: myName="txt_CSVFileRead"
  INTEGER( I4B ) :: ioerr, trecords, nrows, ncols, skippedRows, irow, ii, jj
  LOGICAL( LGT ) :: isSkipRows
  TYPE(String) :: aline
  TYPE(String), ALLOCATABLE :: tokens( : )
  CHARACTER( LEN = maxStrLen ) :: iomsg
  !!
  !! check
  !!
  IF (.NOT. obj%isOpen() .OR. obj%isEOF()) THEN
    CALL e%raiseError(modName //'::'//myName// ' - '// &
      & 'Either CSVfile is not opened or end of file!')
    RETURN
  END IF
  !!
  !! get number of records
  !!
  trecords = obj%getTotalRecords()
  !!
  !! get
  !!
  IF( ALLOCATED( obj%skipRows ) ) THEN
    skippedRows = SIZE( obj%skipRows )
    isSkipRows = .TRUE.
  ELSE
    skippedRows = 0
    isSkipRows = .FALSE.
  END IF
  !!
  nrows = trecords - skippedRows - obj%headerIndx
  obj%nrows=nrows
  !!
  irow = 0; ioerr = 0
  !!
  DO ii = 1, trecords
    !!
    CALL obj%readLine( &
      & val=aline, &
      & iostat=ioerr, &
      & iomsg=iomsg )
    !!
    IF( ioerr .GT. 0 ) THEN
      CALL e%raiseError(modName //'::'//myName// ' - '// &
        & 'Error occured while reading line = ' // tostring( ii ) // &
        & " message = "//TRIM(iomsg) )
      RETURN
    END IF
    !!
    IF( isSkipRows ) THEN
      IF( ANY( ii .EQ. obj%skipRows ) ) CYCLE
    END IF
    !!
    CALL aline%split(tokens=tokens, sep=obj%separator)
    !!
    IF( .NOT. ALLOCATED( tokens ) ) CYCLE
    !!
    !!
    !!
    IF( .NOT. ALLOCATED( obj%data ) ) THEN
      ncols = SIZE( tokens )
      obj%ncols=ncols
      ALLOCATE( obj%data( nrows, ncols ) )
      IF( obj%headerIndx .NE. 0 ) ALLOCATE( obj%header( ncols ) )
    END IF
    !!
    !!
    !!
    IF( ii .EQ. obj%headerIndx ) THEN
      !!
      DO jj = 1, obj%ncols
        obj%header( jj ) = tokens( jj )
      END DO
      !!
    ELSE
      !!
      irow = irow + 1
      !!
      DO jj = 1, obj%ncols
        obj%data( irow, jj ) = tokens( jj )
      END DO
      !!
    END IF
    !!
  END DO
  !!
  !!
  !!
  IF( PRESENT( iostat ) ) iostat = ioerr
  IF( ALLOCATED( tokens ) ) DEALLOCATE( tokens )
  !!
END PROCEDURE txt_CSVFileRead

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ReadMethods
