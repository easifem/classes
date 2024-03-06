! This program is a part of EASIFEM library
! Copyright (C) (Since 2020)  Vikas Sharma, Ph.D
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

MODULE EdgeData_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE Display_Method, ONLY: Display, tostring
IMPLICIT NONE
PRIVATE
PUBLIC :: EdgeData_
PUBLIC :: EdgeData_Pointer
PUBLIC :: EdgeData_Deallocate
PUBLIC :: EdgeData_Display
PUBLIC :: EdgeData_lt
PUBLIC :: EdgeData_eq
PUBLIC :: EdgeData_SetID
PUBLIC :: EdgeData_Copy
PUBLIC :: Initiate
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: Display

INTERFACE Initiate
  MODULE PROCEDURE EdgeData_Initiate
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE EdgeData_Initiate
END INTERFACE

INTEGER(I4B), PARAMETER :: INT_SIZE_IN_TREE_DATA = 2

INTERFACE Display
  MODULE PROCEDURE EdgeData_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                              EdgeData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  TreeData stored at each node level

TYPE EdgeData_
  INTEGER(I4B) :: VALUE(INT_SIZE_IN_TREE_DATA)
  INTEGER(I4B) :: id = 0
END TYPE EdgeData_

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-07
! summary:  Copy

SUBROUTINE EdgeData_Copy(obj1, obj2)
  TYPE(EdgeData_), INTENT(INOUT) :: obj1
  TYPE(EdgeData_), INTENT(IN) :: obj2

  obj1%VALUE = obj2%VALUE
  obj1%id = obj2%id
END SUBROUTINE EdgeData_Copy

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Deallocate tree data

SUBROUTINE EdgeData_Deallocate(obj)
  TYPE(EdgeData_), INTENT(INOUT) :: obj
  obj%VALUE = 0
  obj%id = 0
END SUBROUTINE EdgeData_Deallocate

!----------------------------------------------------------------------------
!                                                               Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Display data

SUBROUTINE EdgeData_Display(obj, msg, unitno)
  TYPE(EdgeData_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  CALL Display(obj%VALUE, msg//"("//tostring(obj%id)//"):", unitno=unitno)
END SUBROUTINE EdgeData_Display

!----------------------------------------------------------------------------
!                                                                      lt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Lesser than

FUNCTION EdgeData_lt(obj, obj2) RESULT(ans)
  TYPE(EdgeData_), INTENT(IN) :: obj
  TYPE(EdgeData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%VALUE(1) .LT. obj2%VALUE(1)
END FUNCTION EdgeData_lt

!----------------------------------------------------------------------------
!                                                                      eq
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  equality

FUNCTION EdgeData_eq(obj, obj2) RESULT(ans)
  TYPE(EdgeData_), INTENT(IN) :: obj
  TYPE(EdgeData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = ALL(obj%VALUE .EQ. obj2%VALUE)
END FUNCTION EdgeData_eq

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Initiate the data

SUBROUTINE EdgeData_Initiate(obj, VALUE)
  TYPE(EdgeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: VALUE(INT_SIZE_IN_TREE_DATA)

  ! internal variables
  INTEGER(I4B) :: ii
  DO ii = 1, INT_SIZE_IN_TREE_DATA
    obj%VALUE(ii) = VALUE(ii)
  END DO
END SUBROUTINE EdgeData_Initiate

!----------------------------------------------------------------------------
!                                                       EdgeData_Pointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  EdgeData_Pointer

FUNCTION EdgeData_Pointer(VALUE) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: VALUE(INT_SIZE_IN_TREE_DATA)
  TYPE(EdgeData_), POINTER :: ans
  ALLOCATE (ans)
  CALL EdgeData_Initiate(ans, VALUE)
END FUNCTION EdgeData_Pointer

!----------------------------------------------------------------------------
!                                                                  SetID
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Initiate the data

SUBROUTINE EdgeData_SetID(obj, id)
  TYPE(EdgeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: id
  obj%id = id
END SUBROUTINE EdgeData_SetID

END MODULE EdgeData_Class
