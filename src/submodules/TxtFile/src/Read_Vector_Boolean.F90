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

INTEGER(I4B) :: totalRecords, ii, ioerr, jj, totalTokens, kk
TYPE(String) :: aline
TYPE(String), ALLOCATABLE :: tokens(:)
LOGICAL(LGT) :: isok, abool

totalRecords = obj%GetTotalRecords(ignoreComment=ignoreComment, &
                         ignoreBlank=ignoreBlank, commentSymbol=commentSymbol)

CALL Reallocate(val, totalRecords)

ioerr = 0
jj = 0

abool = obj%isOpen() .AND. .NOT. obj%isEOF()

IF (abool) THEN

  DO

    CALL obj%readLine(val=aline, iostat=ioerr, iomsg=iomsg)

    IF (obj%isEOF()) EXIT

    isok = obj%isValidRecord(aline=aline, ignoreComment=ignoreComment, &
                         ignoreBlank=ignoreBlank, commentSymbol=commentSymbol)

    IF (isok) THEN

      CALL aline%split(tokens=tokens, sep=separator)

      totalTokens = SIZE(tokens)

      DO kk = 1, totalTokens
        val(jj + kk) = tokens(kk)%to_number(val_kind)
      END DO

      jj = jj + totalTokens

    END IF

  END DO

  aline = ""

END IF

IF (ioerr .LT. IOSTAT_EOR) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    ' - Error reading a scalar from the file (IOSTAT='// &
                    tostring(iostat)//')!')
END IF

IF (PRESENT(iostat)) iostat = ioerr
IF (ALLOCATED(tokens)) DEALLOCATE (tokens)
