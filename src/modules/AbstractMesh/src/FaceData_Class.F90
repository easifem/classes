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

MODULE FaceData_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE Display_Method
IMPLICIT NONE
PRIVATE
PUBLIC :: FaceData_
PUBLIC :: FaceData_Pointer
PUBLIC :: FaceData_Deallocate
PUBLIC :: FaceData_Display
PUBLIC :: FaceData_lt
PUBLIC :: FaceData_eq
PUBLIC :: FaceData_SetID
PUBLIC :: FaceData_Copy
PUBLIC :: Initiate
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: Display

INTERFACE Initiate
  MODULE PROCEDURE FaceData_Initiate
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE FaceData_Initiate
END INTERFACE

INTEGER(I4B), PARAMETER :: INT_SIZE_IN_TREE_DATA = 4_I4B

INTERFACE Display
  MODULE PROCEDURE FaceData_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                              FaceData_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  TreeData stored at each node level

TYPE FaceData_
  INTEGER(I4B) :: VALUE(INT_SIZE_IN_TREE_DATA) = 0_I4B
  INTEGER(I4B) :: id = 0
END TYPE FaceData_

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-07
! summary:  Copy

SUBROUTINE FaceData_Copy(obj1, obj2)
  TYPE(FaceData_), INTENT(INOUT) :: obj1
  TYPE(FaceData_), INTENT(IN) :: obj2

  obj1%VALUE = obj2%VALUE
  obj1%id = obj2%id
END SUBROUTINE FaceData_Copy

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Deallocate tree data

SUBROUTINE FaceData_Deallocate(obj)
  TYPE(FaceData_), INTENT(INOUT) :: obj
  obj%VALUE = 0
  obj%id = 0
END SUBROUTINE FaceData_Deallocate

!----------------------------------------------------------------------------
!                                                               Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Display data

SUBROUTINE FaceData_Display(obj, msg, unitno)
  TYPE(FaceData_), INTENT(IN) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  CALL Display(obj%VALUE, msg//"("//tostring(obj%id)//"):", unitno=unitno)
END SUBROUTINE FaceData_Display

!----------------------------------------------------------------------------
!                                                                      lt
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Lesser than

FUNCTION FaceData_lt(obj, obj2) RESULT(ans)
  TYPE(FaceData_), INTENT(IN) :: obj
  TYPE(FaceData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = obj%VALUE(1) .LT. obj2%VALUE(1)
END FUNCTION FaceData_lt

!----------------------------------------------------------------------------
!                                                                      eq
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  equality

FUNCTION FaceData_eq(obj, obj2) RESULT(ans)
  TYPE(FaceData_), INTENT(IN) :: obj
  TYPE(FaceData_), INTENT(IN) :: obj2
  LOGICAL(LGT) :: ans
  ans = ALL(obj%VALUE .EQ. obj2%VALUE)
END FUNCTION FaceData_eq

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Initiate the data

SUBROUTINE FaceData_Initiate(obj, VALUE)
  TYPE(FaceData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: VALUE(:)

  ! internal variables
  INTEGER(I4B) :: ii
  DO ii = 1, SIZE(VALUE)
    obj%VALUE(ii) = VALUE(ii)
  END DO
END SUBROUTINE FaceData_Initiate

!----------------------------------------------------------------------------
!                                                       FaceData_Pointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  FaceData_Pointer

FUNCTION FaceData_Pointer(VALUE) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: VALUE(:)
  TYPE(FaceData_), POINTER :: ans
  ALLOCATE (ans)
  CALL FaceData_Initiate(ans, VALUE)
END FUNCTION FaceData_Pointer

!----------------------------------------------------------------------------
!                                                                  SetID
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Initiate the data

SUBROUTINE FaceData_SetID(obj, id)
  TYPE(FaceData_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: id
  obj%id = id
END SUBROUTINE FaceData_SetID

END MODULE FaceData_Class
