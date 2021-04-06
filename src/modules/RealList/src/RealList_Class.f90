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

MODULE RealList_Class
USE GlobalData
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
IMPLICIT NONE
PRIVATE

TYPE( ExceptionHandler_ ), PUBLIC, SAVE :: eRealListType
#define LIST_NODE_NAME RealListNode_
#define DATA_TYPE_NAME REAL( DFP )
#define LIST_NAME RealList_
#define EXCEPTION eRealListType
#include "../../List/src/TempList_Class.inc"

TYPE( LIST_NODE_NAME ), PARAMETER, PUBLIC :: TypeRealListNode = RealListNode_(data=1.0_DFP)
TYPE( LIST_NAME ), PARAMETER, PUBLIC :: TypeRealList = RealList_()

TYPE RealListNodePointer_
  CLASS( LIST_NODE_NAME ), POINTER :: ptr => NULL()
END TYPE RealListNodePointer_

PUBLIC :: RealListNodePointer_


TYPE RealListPointer_
  CLASS( LIST_NAME ), POINTER :: ptr => NULL( )
END TYPE RealListPointer_

PUBLIC :: RealListPointer_

END MODULE RealList_Class