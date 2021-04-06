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

SUBMODULE( RealList_Class ) Method
IMPLICIT NONE
CHARACTER( LEN = * ), PARAMETER :: modName = "RealList_Class.f90"

CONTAINS
#define LIST_NODE_NAME RealListNode_
#define DATA_TYPE_NAME REAL( DFP )
#define LIST_NAME RealList_
#define EXCEPTION eRealListType
#include "../../List/src/TempList_Class@Method.inc"

END SUBMODULE Method