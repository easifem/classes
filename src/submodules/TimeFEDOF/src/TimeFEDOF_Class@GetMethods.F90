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

SUBMODULE(TimeFEDOF_Class) GetMethods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                               IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                               GetCaseName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCaseName
ans = obj%baseContinuity//obj%baseInterpolation
END PROCEDURE obj_GetCaseName

!----------------------------------------------------------------------------
!                                                                GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalDOF()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%tdof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalDOF

!----------------------------------------------------------------------------
!                                                           GetMeshPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeOptPointer
ans => obj%opt
END PROCEDURE obj_GetTimeOptPointer

!----------------------------------------------------------------------------
!                                                       GetBaseInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBaseInterpolation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBaseInterpolation()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%fe)
CALL AssertError1(isok, myName, 'obj%fe is not associated')
#endif

ans = obj%fe%GetBaseInterpolation()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetBaseInterpolation

!----------------------------------------------------------------------------
!                                                               GetCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCellOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCellOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%cellOrder

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetCellOrder

!----------------------------------------------------------------------------
!                                                              GetFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFEPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => obj%fe

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFEPointer

!----------------------------------------------------------------------------
!                                                    GetMaxTotalConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxTotalConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxTotalConnectivity()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%isMaxConSet) THEN
  obj%maxCon = obj%tdof
  obj%isMaxConSet = .TRUE.
END IF

ans = obj%maxCon

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxTotalConnectivity

!----------------------------------------------------------------------------
!                                                    GetMaxTotalQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxTotalQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxTotalQuadraturePoints()"
#endif

INTEGER(I4B) :: cellOrder

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

IF (.NOT. obj%isMaxQuadPointSet) THEN
  cellOrder = obj%cellOrder * obj%scaleForQuadOrder
  ! CALL obj%fe%SetQuadratureOrder(order=cellOrder)
  ! ans = feptr%GetTotalQuadraturePoints()
  ! obj%maxQuadPoint = obj%tdof
  ! obj%isMaxQuadPointSet = .TRUE.
END IF

ans = obj%maxQuadPoint

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxTotalQuadraturePoints

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
