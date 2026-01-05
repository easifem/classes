! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

INTEGER(I4B) :: ii, tsize
CHARACTER(:), ALLOCATABLE :: mymsg
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(obj)

DO ii = 1, tsize
  isok = ASSOCIATED(obj(ii)%ptr)
  mymsg = msg//"("//ToString(ii)//") ASSOCIATED: "
  CALL Display(isok, mymsg, unitNo=unitNo)

  IF (isok) THEN
    mymsg = msg//"("//ToString(ii)//"): "
    CALL obj(ii)%ptr%Display(msg=mymsg, unitNo=unitNo)
  END IF

END DO

mymsg = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
