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
USE CSRMatrix_Method, ONLY: CSRMatrix_Size => Size, &
                            CSRMatrix_Shape => Shape, &
                            GetValue
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           IsSubmatInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsSubmatInitiated
ans = obj%isSubmatInit
END PROCEDURE obj_IsSubmatInitiated

!----------------------------------------------------------------------------
!                                                                       SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
ans = CSRMatrix_SIZE(obj%mat, dim)
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                                      SHAPE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Shape
ans = CSRMatrix_SHAPE(obj%mat)
END PROCEDURE obj_Shape

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
#endif

#include "./localNodeError.F90"

CALL GetValue(obj=obj%mat, VALUE=VALUE, nodenum=globalNode, &
              storageFMT=storageFMT, nrow=nrow, ncol=ncol)

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Get2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

CALL GetValue(obj=obj%mat, VALUE=VALUE, idof=idof, jdof=jdof, &
              iNodeNum=iNodeNum, jNodeNum=jNodeNum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

CALL GetValue(obj=obj%mat, VALUE=VALUE, ivar=ivar, jvar=jvar, &
              iNodeNum=iNodeNum, jNodeNum=jNodeNum, nrow=nrow, ncol=ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get4()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

CALL GetValue(obj=obj%mat, VALUE=VALUE, ivar=ivar, jvar=jvar, &
              iNodeNum=iNodeNum, jNodeNum=jNodeNum, idof=idof, jdof=jdof, &
              nrow=nrow, ncol=ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get5()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

CALL GetValue(obj=obj%mat, VALUE=VALUE, ivar=ivar, jvar=jvar, &
              iNodeNum=iNodeNum, jNodeNum=jNodeNum, idof=idof, jdof=jdof)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get6()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

CALL GetValue(obj=obj%mat, VALUE=VALUE, ivar=ivar, jvar=jvar, &
              iNodeNum=iNodeNum, jNodeNum=jNodeNum, ispacecompo=ispacecompo, &
        jspacecompo=jspacecompo, itimecompo=itimecompo, jtimecompo=jtimecompo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                                     Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get7()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#include "./localNodeError.F90"

CALL GetValue(obj=obj%mat, VALUE=VALUE, ivar=ivar, jvar=jvar, &
              iNodeNum=iNodeNum, jNodeNum=jNodeNum, ispacecompo=ispacecompo, &
      jspacecompo=jspacecompo, itimecompo=itimecompo, jtimecompo=jtimecompo, &
              nrow=nrow, ncol=ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
