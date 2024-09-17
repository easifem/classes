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

INTEGER(I4B) :: ii
TYPE(String) :: aline
CHARACTER(1) :: orient0

IF (PRESENT(orient)) THEN
  orient0 = UpperCase(orient(1:1))
ELSE
  orient0 = "C"
END IF

SELECT CASE (orient0)

CASE ("R")
  aline = STR(val, separator=obj%separator)
  CALL obj%writeLine(val=aline, iostat=iostat, iomsg=iomsg, advance=advance)

CASE ("C", "T")

  DO ii = 1, SIZE(val)
    CALL obj%WRITE(val=val(ii), iostat=iostat, iomsg=iomsg, advance=advance)
  END DO

END SELECT
