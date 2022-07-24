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

SUBMODULE(TxtFile_Class) EnquireMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_isValidRecord
  LOGICAL(LGT) :: ignoreComment0
  LOGICAL(LGT) :: ignoreBlank0
  CHARACTER(LEN=1) :: asymb
  TYPE(String) :: aline0
  !!
  ignoreBlank0 = Input(option=ignoreBlank, default=.FALSE.)
  ignoreComment0 = Input(option=ignoreComment, default=.FALSE.)
  asymb = "#"
  IF (PRESENT(commentSymbol)) asymb = commentSymbol
  ans = .FALSE.
  !!
  IF (aline%LEN_TRIM() .EQ. 0) THEN
    IF (.NOT. ignoreBlank0) ans = .TRUE.
  ELSE
    aline0 = TRIM(ADJUSTL(aline%chars()))
    IF (aline0%slice(1, 1) .EQ. asymb(1:1)) THEN
      IF (.NOT. ignoreComment0) ans = .TRUE.
    ELSE
      ans = .TRUE.
    END IF
  END IF
  !!
  aline0=""
  !!
END PROCEDURE txt_isValidRecord

END SUBMODULE EnquireMethods
