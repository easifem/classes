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

MODULE EdgeTreeData_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE Display_Method, ONLY: Display, tostring
IMPLICIT NONE
PRIVATE
PUBLIC :: EdgeTreeData_
PUBLIC :: EdgeTreeData_Pointer
PUBLIC :: EdgeTreeData_DEALLOCATE
PUBLIC :: EdgeTreeData_Display
PUBLIC :: EdgeTreeData_lt
PUBLIC :: EdgeTreeData_eq
PUBLIC :: EdgeTreeData_SetID
PUBLIC :: Initiate
PUBLIC :: ASSIGNMENT(=)

INTERFACE Initiate
  MODULE PROCEDURE EdgeTreeData_Initiate
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE EdgeTreeData_Initiate
END INTERFACE

INTEGER(I4B), PARAMETER :: INT_SIZE_IN_TREE_DATA = 2

!----------------------------------------------------------------------------
!                                                              EdgeTreeData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  TreeData stored at each node level

TYPE EdgeTreeData_
  INTEGER(I4B) :: VALUE(INT_SIZE_IN_TREE_DATA)
  INTEGER(I4B) :: id = 0
END TYPE EdgeTreeData_

CONTAINS

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Deallocate tree data

SUBROUTINE EdgeTreeData_Deallocate(obj)
  TYPE(EdgeTreeData_), INTENT(INOUT) :: obj
  obj%VALUE = 0
  obj%id = 0
END SUBROUTINE EdgeTreeData_Deallocate

!----------------------------------------------------------------------------
!                                                               Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Display data

SUBROUTINE EdgeTreeData_Display(obj, msg, unitno)
  TYPE(EdgeTreeData_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  CALL Display(obj%VALUE, msg//"("//tostring(obj%id)//"):", unitno=unitno)
END SUBROUTINE EdgeTreeData_Display

!----------------------------------------------------------------------------
!                                                                      lt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Lesser than

FUNCTION EdgeTreeData_lt(obj, obj2) RESULT(ans)
  TYPE(EdgeTreeData_), INTENT(IN) :: obj
  TYPE(EdgeTreeData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%VALUE(1) .LT. obj2%VALUE(1)
END FUNCTION EdgeTreeData_lt

!----------------------------------------------------------------------------
!                                                                      eq
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  equality

FUNCTION EdgeTreeData_eq(obj, obj2) RESULT(ans)
  TYPE(EdgeTreeData_), INTENT(IN) :: obj
  TYPE(EdgeTreeData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = ALL(obj%VALUE .EQ. obj2%VALUE)
END FUNCTION EdgeTreeData_eq

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Initiate the data

SUBROUTINE EdgeTreeData_Initiate(obj, VALUE)
  TYPE(EdgeTreeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: VALUE(INT_SIZE_IN_TREE_DATA)

  ! internal variables
  INTEGER(I4B) :: ii
  DO ii = 1, INT_SIZE_IN_TREE_DATA
    obj%VALUE(ii) = VALUE(ii)
  END DO
END SUBROUTINE EdgeTreeData_Initiate

!----------------------------------------------------------------------------
!                                                       EdgeTreeData_Pointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  EdgeTreeData_Pointer

FUNCTION EdgeTreeData_Pointer(VALUE) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: VALUE(INT_SIZE_IN_TREE_DATA)
  TYPE(EdgeTreeData_), POINTER :: ans
  ALLOCATE (ans)
  CALL EdgeTreeData_Initiate(ans, VALUE)
END FUNCTION EdgeTreeData_Pointer

!----------------------------------------------------------------------------
!                                                                  SetID
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Initiate the data

SUBROUTINE EdgeTreeData_SetID(obj, id)
  TYPE(EdgeTreeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: id
  obj%id = id
END SUBROUTINE EdgeTreeData_SetID

END MODULE EdgeTreeData_Class
