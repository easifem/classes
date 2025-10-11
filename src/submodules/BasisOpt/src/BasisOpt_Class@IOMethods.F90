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
USE GlobalData, ONLY: CHAR_LF
USE Display_Method, ONLY: Display, ToString
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Display => Display
USE ReferenceElement_Method, ONLY: ElementName

USE MdEncode_Method, ONLY: MdEncode, &
                           React_StartTabs, &
                           React_StartTabItem, &
                           React_EndTabItem
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

CALL Display(msg, unitno=unitno)

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
CALL Display(obj%topoType, msg="topoType: ", unitno=unitno)
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

isok = ALLOCATED(obj%coeff)
CALL Display(isok, msg="obj%coeff allocated: ", unitno=unitno)
IF (isok) THEN
  CALL Display(SHAPE(obj%coeff), msg="obj%coeff shape: ", unitno=unitno)
END IF

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
CHARACTER(*), PARAMETER :: myName = "obj_MdEncode"
#endif

INTEGER(I4B), PARAMETER :: jj = 21
TYPE(String) :: rowTitle(jj), colTitle(1), astr(jj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

colTitle(1) = ""
rowTitle(1) = "**nsd**"; astr(1) = ToString(obj%nsd)
rowTitle(2) = "**feType**"; astr(2) = ToString(obj%feType)
rowTitle(3) = "**elemType**"; astr(3) = ElementName(obj%elemType)
rowTitle(4) = "**ipType**"; astr(4) = ToString(obj%ipType)
rowTitle(5) = "**basisType**"; astr(5) = ToString(obj%basisType)
rowTitle(6) = "**alpha**"; astr(6) = ToString(obj%alpha)
rowTitle(7) = "**beta**"; astr(7) = ToString(obj%beta)
rowTitle(8) = "**lambda**"; astr(8) = ToString(obj%lambda)
rowTitle(9) = "**dofType**"; astr(9) = ToString(obj%dofType)
rowTitle(10) = "**transformType**"; astr(10) = ToString(obj%transformType)
rowTitle(11) = "**baseContinuity**"; astr(11) = obj%baseContinuity
rowTitle(12) = "**baseInterpolion**"; astr(12) = obj%baseInterpolation
rowTitle(13) = "**refElemDomain**"; astr(13) = obj%refElemDomain
rowTitle(14) = "**isIsotropicOrder**"; astr(14) = &
 ToString(obj%isIsotropicOrder)
rowTitle(15) = "**isAnisotropicOrder**"; astr(15) = &
 ToString(obj%isAnisotropicOrder)
rowTitle(16) = "**isEdgeOrder**"; astr(16) = ToString(obj%isEdgeOrder)
rowTitle(17) = "**isFaceOrder**"; astr(17) = ToString(obj%isFaceOrder)
rowTitle(18) = "**isCellOrder**"; astr(18) = ToString(obj%isCellOrder)

IF (obj%isEdgeOrder) THEN
  rowTitle(19) = "**edgeOrder**"; astr(19) = ToString(obj%edgeOrder)
ELSE
  rowTitle(19) = "**edgeOrder**"; astr(19) = " "
END IF

! IF (obj%isFaceOrder) THEN
!   rowTitle(20) = "**faceOrder**"; astr(20) = ToString(obj%faceOrder)
! ELSE
rowTitle(20) = "**faceOrder**"; astr(20) = " "
! END IF

IF (obj%iscellOrder) THEN
  rowTitle(21) = "**cellOrder**"; astr(21) = ToString(obj%cellOrder)
ELSE
  rowTitle(21) = "**cellOrder**"; astr(21) = " "
END IF

ans = MdEncode(val=astr(1:21), rh=rowTitle(1:21), ch=colTitle)//char_lf

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
CHARACTER(*), PARAMETER :: myName = "obj_ReactEncode"
#endif

INTEGER(I4B), PARAMETER :: jj = 21
TYPE(String) :: rowTitle(jj), colTitle(1), astr(jj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

colTitle(1) = ""
rowTitle(1) = "**nsd**"
astr(1) = ToString(obj%nsd)

rowTitle(2) = "**feType**"
astr(2) = ToString(obj%feType)

rowTitle(3) = "**elemType**"
astr(3) = ElementName(obj%elemType)

rowTitle(4) = "**ipType**"
astr(4) = ToString(obj%ipType)

rowTitle(5) = "**basisType**"
astr(5) = ToString(obj%basisType)

rowTitle(6) = "**alpha**"
astr(6) = ToString(obj%alpha)

rowTitle(7) = "**beta**"
astr(7) = ToString(obj%beta)

rowTitle(8) = "**lambda**"
astr(8) = ToString(obj%lambda)

rowTitle(9) = "**dofType**"
astr(9) = ToString(obj%dofType)

rowTitle(10) = "**transformType**"
astr(10) = ToString(obj%transformType)

rowTitle(11) = "**baseContinuity**"
astr(11) = obj%baseContinuity

rowTitle(12) = "**baseInterpolation**"
astr(12) = obj%baseInterpolation

rowTitle(13) = "**refElemDomain**"
astr(13) = obj%refElemDomain

rowTitle(14) = "**isIsotropicOrder**"
astr(14) = ToString(obj%isIsotropicOrder)

rowTitle(15) = "**isAnisotropicOrder**"
astr(15) = ToString(obj%isAnisotropicOrder)

rowTitle(16) = "**isEdgeOrder**"
astr(16) = ToString(obj%isEdgeOrder)

rowTitle(17) = "**isFaceOrder**"
astr(17) = ToString(obj%isFaceOrder)

rowTitle(18) = "**isCellOrder**"
astr(18) = ToString(obj%isCellOrder)

IF (obj%isEdgeOrder) THEN
  rowTitle(19) = "**edgeOrder**"
  astr(19) = ToString(obj%edgeOrder)
ELSE
  rowTitle(19) = "**edgeOrder**"
  astr(19) = " "
END IF

! IF (obj%isFaceOrder) THEN
!   rowTitle(20) = "**faceOrder**"
!   astr(20) = ToString(obj%faceOrder)
! ELSE
rowTitle(20) = "**faceOrder**"
astr(20) = " "
! END IF

IF (obj%iscellOrder) THEN
  rowTitle(21) = "**cellOrder**"
  astr(21) = ToString(obj%cellOrder)
ELSE
  rowTitle(21) = "**cellOrder**"
  astr(21) = " "
END IF

ans = React_StartTabs()//char_lf
ans = ans//React_StartTabItem(VALUE="0", label="Finite Element")//char_lf// &
      MdEncode(val=astr(1:21), rh=rowTitle(1:21), ch=colTitle)//char_lf// &
      React_EndTabItem()//char_lf

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ReactEncode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
