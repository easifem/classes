! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(GmshPlaneSurface_Class) Methods
USE ExceptionHandler_Class, ONLY: e
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE ReallocateUtility, ONLY: Reallocate
USE SafeSizeUtility, ONLY: SafeSize
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "GmshPlaneSurface_Class@Methods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetOuterloop(outerloop)
CALL obj%SetInnerloops(innerloops)
obj%indx = indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                               SetOuterloop
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOuterloop
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOuterloop()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%outerloop%Copy(outerloop)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetOuterloop

!----------------------------------------------------------------------------
!                                                          AllocateInnerloops
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AllocateInnerloops
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AllocateInnerloops()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%innerloops)
IF (isok) THEN

  IF (tsize .LE. obj%innerloopsCapacity) THEN
    obj%innerloopsSize = tsize
  ELSE
    obj%innerloopsCapacity = tsize
    obj%innerloopsSize = tsize
    DEALLOCATE (obj%innerloops)
    ALLOCATE (obj%innerloops(tsize))
  END IF

ELSE

  obj%innerloopsCapacity = tsize
  obj%innerloopsSize = tsize
  ALLOCATE (obj%innerloops(tsize))

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AllocateInnerloops

!----------------------------------------------------------------------------
!                                                              SetInnerloops
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetInnerloops
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetInnerloops()"
#endif

INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(innerloops)
CALL obj%AllocateInnerloops(tsize=tsize)

DO ii = 1, tsize
  isok = ASSOCIATED(innerloops(ii)%ptr)
  IF (.NOT. isok) CYCLE
  CALL obj%SetInnerloop(innerloop=innerloops(ii)%ptr, loopId=ii)
END DO
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetInnerloops

!----------------------------------------------------------------------------
!                                                                SetInnerloop
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetInnerloop
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetInnerloop()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError3(loopId, obj%innerloopsSize, myName, &
                  "a=loopId, b=obj%innerloopsSize,")
#endif

isok = ASSOCIATED(obj%innerloops(loopId)%ptr)
IF (.NOT. isok) ALLOCATE (obj%innerloops(loopId))
CALL obj%innerloops(loopId)%ptr%Copy(innerloop)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetInnerloop

!----------------------------------------------------------------------------
!                                                               AddInnerloop
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddInnerloop
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddInnerloop()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, loopId, tsize
TYPE(GmshCurveLoopPointer_), ALLOCATABLE :: temp(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = obj%innerloopsSize .LT. obj%innerloopsCapacity
IF (isok) THEN
  ! it means there is enough space to keep innerloop, so
  ! we just set
  obj%innerloopsSize = obj%innerloopsSize + 1
  loopId = obj%innerloopsSize
  CALL obj%SetInnerloop(innerloop=innerloop, loopId=loopId)
ELSE

  ! backup current innerloops in temp
  tsize = obj%innerloopsCapacity
  ALLOCATE (temp(tsize))
  DO ii = 1, tsize
    temp(ii)%ptr => obj%innerloops(ii)%ptr
    obj%innerloops(ii)%ptr => NULL()
  END DO

  ! increase the size of innerloops
  DEALLOCATE (obj%innerloops)
  CALL obj%AllocateInnerloops(tsize=tsize + 1)

  ! copying the original content
  DO ii = 1, tsize
    obj%innerloops(ii)%ptr => temp(ii)%ptr
    temp(ii)%ptr => NULL()
  END DO

  DEALLOCATE (temp)
  loopId = tsize + 1
  CALL obj%SetInnerloop(innerloop=innerloop, loopId=loopId)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddInnerloop

!----------------------------------------------------------------------------
!                                                                    SetIndx
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetIndx
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetIndx()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%indx = indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetIndx

!----------------------------------------------------------------------------
!                                                               GetOuterloop
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOuterloop
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetOuterloop()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL ans%Copy(obj%outerloop)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetOuterloop

!----------------------------------------------------------------------------
!                                                               GetInnerloops
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInnerloops
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetInnerloops()"
#endif

INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%innerloopsSize
ALLOCATE (ans(tsize))

DO ii = 1, tsize
  ans(ii)%ptr => obj%innerloops(ii)%ptr
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetInnerloops

!----------------------------------------------------------------------------
!                                                        GetInnerloopPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInnerloopPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetInnerloopPointer()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%innerloopsSize

#ifdef DEBUG_VER
CALL AssertError3(loopId, tsize, myName, &
                  "a=loopId, b=tsize, ")
#endif

ans => obj%innerloops(loopId)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetInnerloopPointer

!----------------------------------------------------------------------------
!                                                                    GetIndx
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIndx
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetIndx()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetIndx

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%indx, "indx: ", unitno=unitno)
CALL obj%outerloop%Display("outerloop: ", unitno=unitno)

CALL Display(obj%innerloopsSize, "innerloopsSize: ")
CALL Display(obj%innerloopsCapacity, "innerloopsCapacity: ")

DO ii = 1, obj%innerloopsSize
  isok = ASSOCIATED(obj%innerloops(ii)%ptr)
  IF (isok) THEN
    CALL obj%innerloops(ii)%ptr%Display("innerloops("//ToString(ii)//"): ", &
                                        unitno=unitno)
  END IF
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
