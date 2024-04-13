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

MODULE IntTreeData_Class
USE GlobalData, ONLY: I4B, LGT
USE Display_Method, ONLY: Display
IMPLICIT NONE
PRIVATE
PUBLIC :: IntTreeData_
PUBLIC :: IntTreeData_Pointer
PUBLIC :: IntTreeData_DEALLOCATE
PUBLIC :: IntTreeData_Display
PUBLIC :: IntTreeData_lt
PUBLIC :: IntTreeData_eq

INTERFACE Initiate
  MODULE PROCEDURE IntTreeData_Initiate
END INTERFACE Initiate
PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                              IntTreeData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  TreeData stored at each node level

TYPE IntTreeData_
  INTEGER(I4B) :: VALUE = 0
END TYPE IntTreeData_

CONTAINS

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Deallocate tree data

SUBROUTINE IntTreeData_Deallocate(obj)
  TYPE(IntTreeData_), INTENT(INOUT) :: obj
  obj%VALUE = 0
END SUBROUTINE IntTreeData_Deallocate

!----------------------------------------------------------------------------
!                                                               Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Display data

SUBROUTINE IntTreeData_Display(obj, msg, unitno)
  TYPE(IntTreeData_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  CALL Display(obj%VALUE, msg, unitno=unitno)
END SUBROUTINE IntTreeData_Display

!----------------------------------------------------------------------------
!                                                                      lt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Lesser than

FUNCTION IntTreeData_lt(obj, obj2) RESULT(ans)
  TYPE(IntTreeData_), INTENT(IN) :: obj
  TYPE(IntTreeData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%VALUE .LT. obj2%VALUE
END FUNCTION IntTreeData_lt

!----------------------------------------------------------------------------
!                                                                      eq
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  equality

FUNCTION IntTreeData_eq(obj, obj2) RESULT(ans)
  TYPE(IntTreeData_), INTENT(IN) :: obj
  TYPE(IntTreeData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%VALUE .EQ. obj2%VALUE
END FUNCTION IntTreeData_eq

!----------------------------------------------------------------------------
!                                                               Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Initiate

SUBROUTINE IntTreeData_Initiate(obj, VALUE)
  TYPE(IntTreeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: VALUE
  obj%VALUE = VALUE
END SUBROUTINE IntTreeData_Initiate

!----------------------------------------------------------------------------
!                                                                      eq
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  equality

FUNCTION IntTreeData_Pointer(VALUE) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: VALUE
  TYPE(IntTreeData_), POINTER :: ans
  ALLOCATE (ans)
  CALL IntTreeData_Initiate(ans, VALUE)
END FUNCTION IntTreeData_Pointer

END MODULE IntTreeData_Class
