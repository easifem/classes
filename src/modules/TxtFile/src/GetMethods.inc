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

!----------------------------------------------------------------------------
!                                                     GetEchoUnit@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Get the echo units

INTERFACE
  MODULE PURE FUNCTION txt_getEchoUnit(obj) RESULT(ans)
    CLASS(TxtFile_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION txt_getEchoUnit
END INTERFACE

!----------------------------------------------------------------------------
!                                                getTotalRecords@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-09
! update: 2021-11-09
! summary: Returns the total number of records in a file
!
!
!# Introduction
!
! This function returns the total number of records in a file
! If `ignoreComment=.TRUE.`, then the comments are ignored
! If `ignoreComment` is true, then `commentSymbol` should be given

INTERFACE
  MODULE FUNCTION txt_getTotalRecords(obj, ignoreComment, ignoreBlank, &
    & commentSymbol) RESULT(Ans)
    CLASS(TxtFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
    CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
    INTEGER(I4B) :: ans
  END FUNCTION txt_getTotalRecords
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetEchoStat@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Get the echo status

INTERFACE
  MODULE PURE FUNCTION txt_getEchoStat(obj) RESULT(ans)
    CLASS(TxtFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION txt_getEchoStat
END INTERFACE
