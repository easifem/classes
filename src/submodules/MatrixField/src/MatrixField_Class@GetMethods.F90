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

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE(MatrixField_Class) GetMethods
USE CSRMatrix_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
ans = SIZE(obj%mat, dim)
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                                      SHAPE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Shape
ans = SHAPE(obj%mat)
END PROCEDURE obj_Shape

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
INTEGER(I4B) :: storageFMT0
INTEGER(I4B) :: nodenum(SIZE(globalNode))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (PRESENT(storageFMT)) THEN
  storageFMT0 = GetStorageFMT(obj%mat, 1)
ELSE
  storageFMT0 = storageFMT
END IF

nodenum = obj%domain%GetLocalNodeNumber(globalNode)
CALL GetValue(obj=obj%mat, VALUE=VALUE, nodenum=nodenum,  &
& storageFMT=storageFMT0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
CHARACTER(*), PARAMETER :: myName = "obj_Get2()"
INTEGER(I4B) :: iNodeNum0, jNodeNum0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (obj%isRectangle) THEN
  iNodeNum0 = obj%domains(1)%ptr%GetLocalNodeNumber(iNodeNum)
  jNodeNum0 = obj%domains(2)%ptr%GetLocalNodeNumber(jNodeNum)
ELSE
  iNodeNum0 = obj%domain%GetLocalNodeNumber(iNodeNum)
  jNodeNum0 = obj%domain%GetLocalNodeNumber(jNodeNum)
END IF

CALL GetValue(obj=obj%mat, VALUE=VALUE, idof=idof, jdof=jdof, &
  & iNodeNum=iNodeNum0, jNodeNum=jNodeNum0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
INTEGER(I4B) :: iNodeNum0(SIZE(iNodeNum)), jNodeNum0(SIZE(jNodeNum))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (obj%isRectangle) THEN
  iNodeNum0 = obj%domains(1)%ptr%GetLocalNodeNumber(iNodeNum)
  jNodeNum0 = obj%domains(2)%ptr%GetLocalNodeNumber(jNodeNum)
ELSE
  iNodeNum0 = obj%domain%GetLocalNodeNumber(iNodeNum)
  jNodeNum0 = obj%domain%GetLocalNodeNumber(jNodeNum)
END IF

CALL GetValue(obj=obj%mat, VALUE=VALUE, ivar=ivar, jvar=jvar, &
  & iNodeNum=iNodeNum0, jNodeNum=jNodeNum0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
CHARACTER(*), PARAMETER :: myName = "obj_Get4()"
INTEGER(I4B) :: iNodeNum0(SIZE(iNodeNum)), jNodeNum0(SIZE(jNodeNum))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (obj%isRectangle) THEN
  iNodeNum0 = obj%domains(1)%ptr%GetLocalNodeNumber(iNodeNum)
  jNodeNum0 = obj%domains(2)%ptr%GetLocalNodeNumber(jNodeNum)
ELSE
  iNodeNum0 = obj%domain%GetLocalNodeNumber(iNodeNum)
  jNodeNum0 = obj%domain%GetLocalNodeNumber(jNodeNum)
END IF

CALL GetValue(obj=obj%mat, VALUE=VALUE, ivar=ivar, jvar=jvar, &
  & iNodeNum=iNodeNum0, jNodeNum=jNodeNum0, idof=idof, jdof=jdof)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
CHARACTER(*), PARAMETER :: myName = "obj_Get5()"
INTEGER(I4B) :: iNodeNum0, jNodeNum0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (obj%isRectangle) THEN
  iNodeNum0 = obj%domains(1)%ptr%GetLocalNodeNumber(iNodeNum)
  jNodeNum0 = obj%domains(2)%ptr%GetLocalNodeNumber(jNodeNum)
ELSE
  iNodeNum0 = obj%domain%GetLocalNodeNumber(iNodeNum)
  jNodeNum0 = obj%domain%GetLocalNodeNumber(jNodeNum)
END IF

CALL GetValue(obj=obj%mat, VALUE=VALUE, ivar=ivar, jvar=jvar, &
  & iNodeNum=iNodeNum0, jNodeNum=jNodeNum0, idof=idof, jdof=jdof)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
CHARACTER(*), PARAMETER :: myName = "obj_Get6()"
INTEGER(I4B) :: iNodeNum0, jNodeNum0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (obj%isRectangle) THEN
  iNodeNum0 = obj%domains(1)%ptr%GetLocalNodeNumber(iNodeNum)
  jNodeNum0 = obj%domains(2)%ptr%GetLocalNodeNumber(jNodeNum)
ELSE
  iNodeNum0 = obj%domain%GetLocalNodeNumber(iNodeNum)
  jNodeNum0 = obj%domain%GetLocalNodeNumber(jNodeNum)
END IF

CALL GetValue(obj=obj%mat, VALUE=VALUE, ivar=ivar, jvar=jvar, &
  & iNodeNum=iNodeNum0, jNodeNum=jNodeNum0, ispacecompo=ispacecompo,  &
  & jspacecompo=jspacecompo, itimecompo=itimecompo, jtimecompo=jtimecompo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                                     Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
CHARACTER(*), PARAMETER :: myName = "obj_Get7()"
INTEGER(I4B) :: iNodeNum0(SIZE(iNodeNum)), jNodeNum0(SIZE(jNodeNum))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

IF (obj%isRectangle) THEN
  iNodeNum0 = obj%domains(1)%ptr%GetLocalNodeNumber(iNodeNum)
  jNodeNum0 = obj%domains(2)%ptr%GetLocalNodeNumber(jNodeNum)
ELSE
  iNodeNum0 = obj%domain%GetLocalNodeNumber(iNodeNum)
  jNodeNum0 = obj%domain%GetLocalNodeNumber(jNodeNum)
END IF

CALL GetValue(obj=obj%mat, VALUE=VALUE, ivar=ivar, jvar=jvar, &
  & iNodeNum=iNodeNum0, jNodeNum=jNodeNum0, ispacecompo=ispacecompo,  &
  & jspacecompo=jspacecompo, itimecompo=itimecompo, jtimecompo=jtimecompo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
