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
INTEGER( I4B ) :: nn, ii, ioerr, unitno, jj
TYPE(String) :: aline
TYPE(String), ALLOCATABLE :: tokens( : )
!!
nn = obj%getTotalRecords( ignoreComment=ignoreComment, &
  & ignoreBlank=ignoreBlank, &
  & commentSymbol=commentSymbol )
!!
ALLOCATE( vals( nn ) )
!!
ioerr=0
jj = 0
!!
IF (obj%isOpen() .AND. .NOT. obj%isEOF()) THEN
  unitno = obj%getUnitNo()
  !!
  DO
    !!
    CALL obj%readLine(val=aline, iostat=ioerr, iomsg=iomsg )
    !!
    IF ( obj%isEOF() ) EXIT
    !!
    IF( obj%isValidRecord( aline=aline, &
      & ignoreComment=ignoreComment, &
      & ignoreBlank=ignoreBlank, &
      & commentSymbol=commentSymbol ) ) THEN
      !!
      CALL aline%split(tokens=tokens, sep=separator)
      !!
      jj = jj + 1
      !!
      vals( jj ) = tokens%to_number( val_kind )
      !!
    END IF
    !!
  END DO
  !!
  aline = ""
  !!
END IF
!!
IF (ioerr .LT. IOSTAT_EOR) THEN
  !!
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & ' - Error reading a scalar from the file (IOSTAT='// &
    & tostring(iostat)//')!')
  !!
END IF
!!
val = GET( obj=vals, DataType=val_kind )
!!
IF( PRESENT( iostat ) ) iostat = ioerr
!!
IF( ALLOCATED( tokens ) ) DEALLOCATE( tokens )
IF( ALLOCATED( vals ) ) DEALLOCATE( vals )
