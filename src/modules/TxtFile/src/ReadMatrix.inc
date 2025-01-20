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

INTERFACE
MODULE SUBROUTINE __SUBROUTINE_NAME__ ( obj, val, iostat, iomsg, &
  & ignoreComment, ignoreBlank, commentSymbol, separator )
  CLASS( TxtFile_ ), INTENT( INOUT ) :: obj
  __DATA_TYPE__, ALLOCATABLE, INTENT( INOUT ) :: val(:,:)
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: iostat
  CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: iomsg
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreComment
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: ignoreBlank
  CHARACTER(len=1), OPTIONAL, INTENT(IN) :: commentSymbol
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: separator
END SUBROUTINE __SUBROUTINE_NAME__
END INTERFACE