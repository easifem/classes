! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(FEDOF_Class) SetMethods
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             SetCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCellOrder

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetCellOrder()"
#endif

INTEGER(I4B) :: tsize, ii, jj
LOGICAL(LGT) :: isok
INTEGER(INT8) :: int8_order

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(order)
CALL Reallocate(obj%cellOrder, obj%tCells)

IF (tsize .EQ. 1) THEN

  int8_order = INT(order(1), kind=INT8)
  obj%cellOrder = int8_order

ELSE

  DO ii = 1, tsize
    isok = obj%mesh%IsElementPresent(globalElement=ii, islocal=islocal)
    IF (.NOT. isok) CYCLE
    jj = obj%mesh%GetLocalElemNumber(globalElement=ii, islocal=islocal)
    int8_order = INT(order(ii), kind=INT8)
    obj%cellOrder(jj) = int8_order
  END DO

END IF

obj%maxCellOrder = MAXVAL(obj%cellOrder)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetCellOrder

!----------------------------------------------------------------------------
!                                                               SetEdgeOrder
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
