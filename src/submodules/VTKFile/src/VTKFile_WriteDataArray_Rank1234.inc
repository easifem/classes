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

noc = INPUT( default=SIZE(x,1), option=numberOfComponents )

SELECT CASE( obj%DataFormat )
CASE( VTK_ASCII )
  content = encodeVTKDataArray( x=x, fmt="ASCII" )
CASE( VTK_BINARY )
  content = encodeVTKDataArray( x=x, fmt="BINARY" )
CASE( VTK_APPENDED )
  isOffset = .TRUE.
END SELECT

CALL obj%WriteDataArrayTag( dataType=dataType, &
& numberOfComponents=noc, &
& name=name, isTuples=isTuples, &
& isOffset=isOffset, content=content )

IF( isOffset) THEN
  CALL obj%WriteToScratch( x )
  CALL Obj%UpdateOffset( nByte=nByte )
END IF