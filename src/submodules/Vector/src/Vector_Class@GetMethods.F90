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

SUBMODULE(Vector_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_get1
CHARACTER(LEN=*), PARAMETER :: myName = "vec_get1"
IF (.NOT. obj%isInitiated) &
  & CALL e%raiseInformation(modName//'::'//myName//" - "// &
  & 'Vector object is not initiated')
IF (nodenum .GT. obj%tsize) &
  & CALL e%raiseInformation(modName//'::'//myName//" - "// &
  & 'Out of bound index')
ans = get(obj=obj%realVec, nodenum=nodenum, dataType=1.0_DFP)
END PROCEDURE vec_get1

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_get2
CHARACTER(LEN=*), PARAMETER :: myName = "vec_get2"
IF (.NOT. obj%isInitiated) &
  & CALL e%raiseInformation(modName//'::'//myName//" - "// &
  & 'Vector object is not initiated')
ans = get(obj=obj%realVec, dataType=1.0_DFP)
END PROCEDURE vec_get2

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_get3
CHARACTER(LEN=*), PARAMETER :: myName = "vec_get3"
IF (.NOT. obj%isInitiated) &
  & CALL e%raiseInformation(modName//'::'//myName//" - "// &
  & 'Vector object is not initiated')
IF (ANY(nodenum .GT. obj%tsize)) &
  & CALL e%raiseInformation(modName//'::'//myName//" - "// &
  & 'Out of bound index')
ans = get(obj=obj%realVec, nodenum=nodenum, dataType=1.0_DFP)
END PROCEDURE vec_get3

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_get4
CHARACTER(LEN=*), PARAMETER :: myName = "vec_get4"
IF (.NOT. obj%isInitiated) &
  & CALL e%raiseInformation(modName//'::'//myName//" - "// &
  & 'Vector object is not initiated')
IF (istart .GT. obj%tsize .OR. iend .GT. obj%tsize) &
  & CALL e%raiseInformation(modName//'::'//myName//" - "// &
  & 'Out of bound index')
ans = get(obj=obj%realVec, istart=istart, iend=iend, stride=stride, &
  & dataType=1.0_DFP)
END PROCEDURE vec_get4

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_get5
CHARACTER(LEN=*), PARAMETER :: myName = "vec_get5"
CALL e%raiseInformation(modName//'::'//myName//" - "// &
  & 'Vector object is not initiated')
END PROCEDURE vec_get5

END SUBMODULE GetMethods
