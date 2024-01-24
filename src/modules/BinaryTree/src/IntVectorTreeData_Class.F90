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
USE GlobalData, ONLY: DFP, I4B, LGT
USE Display_Method, ONLY: Display
IMPLICIT NONE
PRIVATE
PUBLIC :: IntTreeData_

INTEGER(I4B), PARAMETER :: INT_SIZE_IN_TREE_DATA = 1

!----------------------------------------------------------------------------
!                                                              IntTreeData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  TreeData stored at each node level

TYPE IntTreeData_
  PRIVATE
  INTEGER(I4B) :: VALUE(INT_SIZE_IN_TREE_DATA)
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: lt => obj_lt
  PROCEDURE, PUBLIC, PASS(obj) :: gt => obj_gt
  PROCEDURE, PUBLIC, PASS(obj) :: eq => obj_eq

  ! Not so necessary data
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
END TYPE IntTreeData_

CONTAINS

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Deallocate tree data

SUBROUTINE obj_Deallocate(obj)
  CLASS(IntTreeData_), INTENT(INOUT) :: obj
  obj%VALUE = 0.0_DFP
END SUBROUTINE obj_Deallocate

!----------------------------------------------------------------------------
!                                                               Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Initiate

SUBROUTINE obj_Initiate(obj, VALUE)
  CLASS(IntTreeData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: VALUE(INT_SIZE_IN_TREE_DATA)

  ! internal variables
  INTEGER(I4B) :: ii
  DO ii = 1, INT_SIZE_IN_TREE_DATA
    obj%VALUE(ii) = VALUE(ii)
  END DO
END SUBROUTINE obj_Initiate

!----------------------------------------------------------------------------
!                                                               Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Display data

SUBROUTINE obj_Display(obj, msg, unitno)
  CLASS(IntTreeData_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  CALL Display(obj%VALUE, msg//"%value: ", unitno=unitno)
END SUBROUTINE obj_Display

!----------------------------------------------------------------------------
!                                                                      lt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Lesser than

FUNCTION obj_lt(obj, obj2) RESULT(ans)
  CLASS(IntTreeData_), INTENT(IN) :: obj
  CLASS(IntTreeData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%VALUE(1) .LE. obj2%VALUE(1)
END FUNCTION obj_lt

!----------------------------------------------------------------------------
!                                                                      gt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Greater than

FUNCTION obj_gt(obj, obj2) RESULT(ans)
  CLASS(IntTreeData_), INTENT(IN) :: obj
  CLASS(IntTreeData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%VALUE(1) .GT. obj2%VALUE(1)
END FUNCTION obj_gt

!----------------------------------------------------------------------------
!                                                                      eq
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  equality

FUNCTION obj_eq(obj, obj2) RESULT(ans)
  CLASS(IntTreeData_), INTENT(IN) :: obj
  CLASS(IntTreeData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = ALL(obj%VALUE .EQ. obj2%VALUE)
END FUNCTION obj_eq

END MODULE IntTreeData_Class
