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
!

SUBMODULE(BasisOpt_Class) IOMethods
USE Display_Method, ONLY: Display
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Display => Display
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(obj%isInit, msg="isInit: ", unitno=unitno)
IF (.NOT. obj%isInit) RETURN

CALL Display(obj%firstCall, msg="firstCall: ", unitno=unitno)
CALL Display(obj%isIsotropicOrder, msg="isIsotropicOrder: ", unitno=unitno)
CALL Display(obj%isAnisotropicOrder, msg="isAnisotropicOrder: ", &
             unitno=unitno)
CALL Display(obj%isIpType, msg="isIpType: ", unitno=unitno)
CALL Display(obj%isEdgeOrder, msg="isEdgeOrder: ", unitno=unitno)
CALL Display(obj%isFaceOrder, msg="isFaceOrder: ", unitno=unitno)
CALL Display(obj%isCellOrder, msg="isCellOrder: ", unitno=unitno)
CALL Display(obj%isEdgeOrient, msg="isEdgeOrient: ", unitno=unitno)
CALL Display(obj%isFaceOrient, msg="isFaceOrient: ", unitno=unitno)
CALL Display(obj%isCellOrient, msg="isCellOrient: ", unitno=unitno)

CALL Display(obj%tdof, msg="tdof: ", unitno=unitno)
CALL Display(obj%nsd, msg="nsd: ", unitno=unitno)
CALL Display(obj%xidim, msg="xidim: ", unitno=unitno)
CALL Display(obj%topoName, msg="topoName: ", unitno=unitno)
CALL Display(obj%elemType, msg="elemType: ", unitno=unitno)
CALL Display(obj%elemIndx, msg="elemIndx: ", unitno=unitno)
CALL Display(obj%feType, msg="feType: ", unitno=unitno)

CALL Display(obj%tEdgeOrder, msg="tEdgeOrder: ", unitno=unitno)
CALL Display(obj%tFaceOrder, msg="tFaceOrder: ", unitno=unitno)
CALL Display(obj%tCellOrder, msg="tCellOrder: ", unitno=unitno)

CALL Display(obj%transformType, msg="transformType: ", unitno=unitno)
CALL Display(obj%ipType, msg="ipType: ", unitno=unitno)
CALL Display(obj%order, msg="order: ", unitno=unitno)
CALL Display(obj%anisoOrder, msg="anisoOrder: ", unitno=unitno)

IF (obj%isEdgeOrder) THEN
  IF (obj%tEdgeOrder .GT. 0_I4B) THEN
    CALL Display(obj%edgeOrder(:obj%tEdgeOrder), msg="edgeOrder: ", &
                 unitno=unitno)

    CALL Display(obj%edgeOrient(:obj%tEdgeOrder), msg="edgeOrient: ", &
                 unitno=unitno)
  END IF
END IF

IF (obj%isFaceOrder) THEN
  isok = obj%tFaceOrder .GT. 0_I4B
  IF (isok) THEN
    CALL Display(obj%faceOrder(1:3, 1:obj%tFaceOrder), msg="faceOrder: ", &
                 unitno=unitno)
    CALL Display(obj%faceOrient(1:3, 1:obj%tFaceOrder), msg="faceOrient: ", &
                 unitno=unitno)
  END IF
END IF

IF (obj%isCellOrder) THEN
  CALL Display(obj%cellOrder, msg="cellOrder: ", unitno=unitno)
  CALL Display(obj%cellOrient, msg="cellOrient: ", unitno=unitno)
END IF

CALL Display(obj%dofType, msg="dofType: ", unitno=unitno)
CALL Display(obj%basisType, msg="basisType: ", unitno=unitno)
CALL Display(obj%alpha, msg="alpha: ", unitno=unitno)
CALL Display(obj%beta, msg="beta: ", unitno=unitno)
CALL Display(obj%lambda, msg="lambda: ", unitno=unitno)
CALL Display(obj%refElemDomain, msg="refElemDomain: ", unitno=unitno)
CALL Display(obj%baseContinuity, msg="baseContinuity: ", unitno=unitno)
CALL Display(obj%baseInterpolation, msg="baseInterpolation: ", unitno=unitno)
CALL Display(obj%refelemCoord, msg="refelemCoord: ", unitno=unitno)
CALL obj%quadOpt%Display(msg="quadOpt: ", unitno=unitno)
CALL QuadraturePoint_Display(obj=obj%quad, msg="quad: ", &
                             unitno=unitno)
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                    MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MdEncode
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_MdEncode()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_MdEncode

!----------------------------------------------------------------------------
!                                                                    ReactEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReactEncode
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReactEncode()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReactEncode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
