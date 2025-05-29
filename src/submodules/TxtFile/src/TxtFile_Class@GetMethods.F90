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

SUBMODULE(TxtFile_Class) GetMethods

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                               getEchoStat
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_GetEchoStat
ans = obj%echostat
END PROCEDURE txt_GetEchoStat

!----------------------------------------------------------------------------
!                                                               getEchoUnit
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_GetEchoUnit
ans = obj%echounit
END PROCEDURE txt_GetEchoUnit

!----------------------------------------------------------------------------
!                                                            GetTotalRecords
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_GetTotalRecords
TYPE(String) :: aline
INTEGER(I4B) :: iostat
LOGICAL(LGT) :: isok, notok

ans = 0
notok = .NOT. obj%IsInitiated() .OR. .NOT. obj%IsOpen() .OR. &
        .NOT. obj%IsRead()

IF (notok) RETURN

CALL obj%REWIND()

DO

  CALL obj%readLine(val=aline, iostat=iostat)

  IF (obj%IsEOF()) EXIT

  isok = obj%IsValidRecord(aline=aline, ignoreBlank=ignoreBlank, &
                     ignoreComment=ignoreComment, commentSymbol=commentSymbol)

  IF (isok) ans = ans + 1
  
  aline = ""

END DO

CALL obj%REWIND()

END PROCEDURE txt_GetTotalRecords

!----------------------------------------------------------------------------
!                                                             GetTotalData
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_GetTotalData
TYPE(String) :: aline
INTEGER(I4B) :: iostat, totalTokens
LOGICAL(LGT) :: isok, notok
TYPE(String), ALLOCATABLE :: tokens(:)

notok = .NOT. obj%IsInitiated() .OR. .NOT. obj%IsOpen() .OR. &
        .NOT. obj%IsRead()

ans = 0

IF (notok) RETURN

CALL obj%REWIND()

DO

  CALL obj%ReadLine(val=aline, iostat=iostat)

  IF (obj%IsEOF()) EXIT

  isok = obj%IsValidRecord(aline=aline, &
                           ignoreBlank=ignoreBlank, &
                           ignoreComment=ignoreComment, &
                           commentSymbol=commentSymbol)

  IF (isok) THEN
    CALL aline%Split(tokens=tokens, sep=separator)
    totalTokens = SIZE(tokens)
    ans = ans + totalTokens
  END IF

  aline = ""

END DO

CALL obj%REWIND()

IF (ALLOCATED(tokens)) DEALLOCATE (tokens)

END PROCEDURE txt_GetTotalData

!----------------------------------------------------------------------------
!                                                         GetTotalDataBounds
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_GetTotalDataBounds
TYPE(String) :: aline
INTEGER(I4B) :: iostat, totalTokens
LOGICAL(LGT) :: isok, notok
TYPE(String), ALLOCATABLE :: tokens(:)

notok = .NOT. obj%IsInitiated() .OR. .NOT. obj%IsOpen() .OR. &
        .NOT. obj%IsRead()

ans = 0

IF (notok) RETURN

CALL obj%REWIND()

DO

  CALL obj%ReadLine(val=aline, iostat=iostat)

  IF (obj%IsEOF()) EXIT

  isok = obj%IsValidRecord(aline=aline, &
                           ignoreBlank=ignoreBlank, &
                           ignoreComment=ignoreComment, &
                           commentSymbol=commentSymbol)

  IF (isok) THEN
    CALL aline%Split(tokens=tokens, sep=separator)
    totalTokens = SIZE(tokens)
    ans(2) = MAX(ans(2), totalTokens)
    ans(1) = ans(1) + 1
  END IF

  aline = ""

END DO

CALL obj%REWIND()

IF (ALLOCATED(tokens)) DEALLOCATE (tokens)

END PROCEDURE txt_GetTotalDataBounds

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
