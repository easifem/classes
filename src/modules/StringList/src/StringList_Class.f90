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

MODULE StringList_Class
USE GlobalData
USE BaseType, ONLY: String, TypeString
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
IMPLICIT NONE
PRIVATE

TYPE( ExceptionHandler_ ), PUBLIC, SAVE :: eStringListType
#define LIST_NODE_NAME StringListNode_
#define DATA_TYPE_NAME TYPE( String )
#define LIST_NAME StringList_
#define EXCEPTION eStringListType
#include "../../List/src/TempList_Class.inc"

TYPE( LIST_NODE_NAME ), PARAMETER, PUBLIC :: TypeStringListNode = StringListNode_(data=TypeString)
TYPE( LIST_NAME ), PARAMETER, PUBLIC :: TypeStringList = StringList_()

TYPE StringListNodePointer_
  CLASS( LIST_NODE_NAME ), POINTER :: ptr => NULL()
END TYPE StringListNodePointer_

PUBLIC :: StringListNodePointer_


TYPE StringListPointer_
  CLASS( LIST_NAME ), POINTER :: ptr => NULL( )
END TYPE StringListPointer_

PUBLIC :: StringListPointer_

END MODULE StringList_Class

