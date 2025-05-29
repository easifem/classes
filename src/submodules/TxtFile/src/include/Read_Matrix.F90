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

INTEGER(I4B) :: totalRecords(2), ii, ioerr, ii, mm, kk
LOGICAL(LGT) :: isok, abool
TYPE(String) :: aline
TYPE(String), ALLOCATABLE :: tokens(:)

totalRecords = obj%GetTotalDataBounds(ignoreComment=ignoreComment, &
    ignoreBlank=ignoreBlank, commentSymbol=commentSymbol, separator=separator)

CALL Reallocate(val, totalRecords(1), totalRecords(2))

ioerr = 0
ii = 0

abool = obj%IsOpen() .AND. .NOT. obj%IsEOF()

IF (abool) THEN

  DO

    CALL obj%ReadLine(val=aline, iostat=ioerr, iomsg=iomsg)

    IF (obj%IsEOF()) EXIT

    isok = obj%IsValidRecord(aline=aline, ignoreComment=ignoreComment, &
                         ignoreBlank=ignoreBlank, commentSymbol=commentSymbol)

    IF (isok) THEN

      CALL aline%Split(tokens=tokens, sep=separator)
      ii = ii + 1
      mm = SIZE(tokens)

      DO kk = 1, mm
        val(ii, kk) = tokens(kk)%To_number(val_kind)
      END DO

    END IF

  END DO

  aline = ""

END IF

IF (ioerr .LT. IOSTAT_EOR) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'Error reading a scalar from the file (IOSTAT='// &
                    tostring(iostat)//')!')
END IF

IF (PRESENT(iostat)) iostat = ioerr
IF (ALLOCATED(tokens)) DEALLOCATE (tokens)
