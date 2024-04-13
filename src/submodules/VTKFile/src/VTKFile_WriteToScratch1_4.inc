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

SELECT TYPE( x )
!> Real64
TYPE IS( REAL(Real64) )
  n_byte = nn*BYReal64
  WRITE(unit=obj%scratch, iostat=iostat) n_byte, 'R8', nn
  WRITE(unit=obj%scratch, iostat=iostat) x
!> Real32
TYPE IS( REAL(Real32) )
  n_byte = nn*BYReal32
  WRITE(unit=obj%scratch, iostat=iostat)n_byte, 'R4', nn
  WRITE(unit=obj%scratch, iostat=iostat)x
!> Int64
#ifdef USE_Int64
TYPE IS( INTEGER(Int64) )
  n_byte = nn*BYInt64
  WRITE(unit=obj%scratch, iostat=iostat)n_byte, 'I8', nn
  WRITE(unit=obj%scratch, iostat=iostat)x
#endif
!> Int32
TYPE IS( INTEGER(Int32) )
  n_byte = nn*BYInt32
  WRITE(unit=obj%scratch, iostat=iostat)n_byte, 'I4', nn
  WRITE(unit=obj%scratch, iostat=iostat)x
!> Int16
TYPE IS( INTEGER(Int16) )
  n_byte = nn*BYInt16
  WRITE(unit=obj%scratch, iostat=iostat)n_byte, 'I2', nn
  WRITE(unit=obj%scratch, iostat=iostat)x
!> Int8
TYPE IS( INTEGER(Int8) )
  n_byte = nn*BYInt8
  WRITE(unit=obj%scratch, iostat=iostat)n_byte, 'I1', nn
  WRITE(unit=obj%scratch, iostat=iostat)x
END SELECT