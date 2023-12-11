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

SUBMODULE(AbstractFE_Class) IOMethods
USE BaseMethod
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
IF (.NOT. obj%isInitiated) THEN
  CALL Display("Element is Empty", unitno=unitno)
END IF

IF (ASSOCIATED(obj%refelem)) THEN
  CALL obj%refelem%Display( &
    & msg="ReferenceElement: ",  &
    & unitno=unitno, &
    & notFull=notFull)
END IF

CALL Display(obj%nsd, msg="nsd: ", unitno=unitno)
CALL Display(obj%feType, msg="feType: ", unitno=unitno)
CALL Display(obj%elemType, msg="elemType: ", unitno=unitno)
CALL Display(obj%ipType, msg="ipType: ", unitno=unitno)
CALL Display(obj%basisType, msg="basisType: ", unitno=unitno)
CALL Display(obj%alpha, msg="alpha: ", unitno=unitno)
CALL Display(obj%beta, msg="beta: ", unitno=unitno)
CALL Display(obj%lambda, msg="lambda: ", unitno=unitno)
CALL Display(obj%dofType, msg="dofType: ", unitno=unitno)
CALL Display(obj%transformType, msg="transformType: ", unitno=unitno)
CALL Display(obj%baseContinuity0, msg="baseContinuity: ", unitno=unitno)
CALL Display(obj%baseInterpolation0, msg="baseInterpolation: ", unitno=unitno)
CALL Display(obj%refElemDomain, msg="refElemDomain: ", unitno=unitno)

IF (obj%isIsotropicOrder) THEN
  CALL Display("isIsotropicOrder: TRUE", unitno=unitno)
  CALL Display(obj%order, msg="order: ", unitno=unitno)
END IF

IF (obj%isAnisotropicOrder) THEN
  CALL Display("isAnisotropicOrder: TRUE", unitno=unitno)
  CALL Display(obj%anisoOrder, msg="anisoOrder: ", unitno=unitno)
END IF

IF (obj%isEdgeOrder) THEN
  CALL Display("isEdgeOrder: TRUE", unitno=unitno)
  IF (obj%tEdgeOrder .GT. 0_I4B) THEN
    CALL Display( &
      & obj%edgeOrder(:obj%tEdgeOrder), &
      & msg="edgeOrder: ", &
      & unitno=unitno)
  END IF
END IF

IF (obj%isFaceOrder) THEN
  CALL Display("isFaceOrder: TRUE", unitno=unitno)
  IF (obj%tFaceOrder .GT. 0_I4B) THEN
    CALL Display( &
      & obj%faceOrder(:obj%tFaceOrder), &
      & msg="faceOrder: ", &
      & unitno=unitno)
  END IF
END IF

IF (obj%isCellOrder) THEN
  CALL Display("isCellOrder: TRUE", unitno=unitno)
  IF (obj%tCellOrder .GT. 0_I4B) THEN
    CALL Display( &
      & obj%cellOrder(:obj%tCellOrder), &
      & msg="cellOrder: ", &
      & unitno=unitno)
  END IF
END IF

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MdEncode
CHARACTER(*), PARAMETER :: myName = "obj_MdEncode"
INTEGER(I4B), PARAMETER :: jj = 21
TYPE(String) :: rowTitle(jj), colTitle(1), astr(jj)

colTitle(1) = ""
rowTitle(1) = "**nsd**"; astr(1) = tostring(obj%nsd)
rowTitle(2) = "**feType**"; astr(2) = tostring(obj%feType)
rowTitle(3) = "**elemType**"; astr(3) = ElementName(obj%elemType)
rowTitle(4) = "**ipType**"; astr(4) = tostring(obj%ipType)
rowTitle(5) = "**basisType**"; astr(5) = tostring(obj%basisType)
rowTitle(6) = "**alpha**"; astr(6) = tostring(obj%alpha)
rowTitle(7) = "**beta**"; astr(7) = tostring(obj%beta)
rowTitle(8) = "**lambda**"; astr(8) = tostring(obj%lambda)
rowTitle(9) = "**dofType**"; astr(9) = tostring(obj%dofType)
rowTitle(10) = "**transformType**"; astr(10) = tostring(obj%transformType)
rowTitle(11) = "**baseContinuity**"; astr(11) = obj%baseContinuity0%chars()
rowTitle(12) = "**baseInterpolion**"; astr(12) = obj%baseInterpolation0%chars()
rowTitle(13) = "**refElemDomain**"; astr(13) = obj%refElemDomain%chars()
rowTitle(14) = "**isIsotropicOrder**"; astr(14) = tostring(obj%isIsotropicOrder)
rowTitle(15) = "**isAnisotropicOrder**"; astr(15) = tostring(obj%isAnisotropicOrder)
rowTitle(16) = "**isEdgeOrder**"; astr(16) = tostring(obj%isEdgeOrder)
rowTitle(17) = "**isFaceOrder**"; astr(17) = tostring(obj%isFaceOrder)
rowTitle(18) = "**isCellOrder**"; astr(18) = tostring(obj%isCellOrder)
IF (obj%isEdgeOrder) THEN
  rowTitle(19) = "**edgeOrder**"; astr(19) = tostring(obj%edgeOrder)
ELSE
  rowTitle(19) = "**edgeOrder**"; astr(19) = " "
END IF

IF (obj%isFaceOrder) THEN
  rowTitle(20) = "**faceOrder**"; astr(20) = tostring(obj%faceOrder)
ELSE
  rowTitle(20) = "**faceOrder**"; astr(20) = " "
END IF

IF (obj%iscellOrder) THEN
  rowTitle(21) = "**cellOrder**"; astr(21) = tostring(obj%cellOrder)
ELSE
  rowTitle(21) = "**cellOrder**"; astr(21) = " "
END IF

ans = MdEncode( &
  & val=astr(1:21), &
  & rh=rowTitle(1:21), &
  & ch=colTitle)//char_lf//"**Reference Element**"// &
  & char_lf//char_lf//obj%refelem%MdEncode()

END PROCEDURE obj_MdEncode

!----------------------------------------------------------------------------
!                                                                MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReactEncode
CHARACTER(*), PARAMETER :: myName = "obj_ReactEncode"
INTEGER(I4B), PARAMETER :: jj = 21
TYPE(String) :: rowTitle(jj), colTitle(1), astr(jj)

colTitle(1) = ""
rowTitle(1) = "**nsd**"
astr(1) = tostring(obj%nsd)

rowTitle(2) = "**feType**"
astr(2) = tostring(obj%feType)

rowTitle(3) = "**elemType**"
astr(3) = ElementName(obj%elemType)

rowTitle(4) = "**ipType**"
astr(4) = tostring(obj%ipType)

rowTitle(5) = "**basisType**"
astr(5) = tostring(obj%basisType)

rowTitle(6) = "**alpha**"
astr(6) = tostring(obj%alpha)

rowTitle(7) = "**beta**"
astr(7) = tostring(obj%beta)

rowTitle(8) = "**lambda**"
astr(8) = tostring(obj%lambda)

rowTitle(9) = "**dofType**"
astr(9) = tostring(obj%dofType)

rowTitle(10) = "**transformType**"
astr(10) = tostring(obj%transformType)

rowTitle(11) = "**baseContinuity**"
astr(11) = obj%baseContinuity0%chars()

rowTitle(12) = "**baseInterpolation**"
astr(12) = obj%baseInterpolation0%chars()

rowTitle(13) = "**refElemDomain**"
astr(13) = obj%refElemDomain%chars()

rowTitle(14) = "**isIsotropicOrder**"
astr(14) = tostring(obj%isIsotropicOrder)

rowTitle(15) = "**isAnisotropicOrder**"
astr(15) = tostring(obj%isAnisotropicOrder)

rowTitle(16) = "**isEdgeOrder**"
astr(16) = tostring(obj%isEdgeOrder)

rowTitle(17) = "**isFaceOrder**"
astr(17) = tostring(obj%isFaceOrder)

rowTitle(18) = "**isCellOrder**"
astr(18) = tostring(obj%isCellOrder)

IF (obj%isEdgeOrder) THEN
  rowTitle(19) = "**edgeOrder**"
  astr(19) = tostring(obj%edgeOrder)
ELSE
  rowTitle(19) = "**edgeOrder**"
  astr(19) = " "
END IF

IF (obj%isFaceOrder) THEN
  rowTitle(20) = "**faceOrder**"
  astr(20) = tostring(obj%faceOrder)
ELSE
  rowTitle(20) = "**faceOrder**"
  astr(20) = " "
END IF

IF (obj%iscellOrder) THEN
  rowTitle(21) = "**cellOrder**"
  astr(21) = tostring(obj%cellOrder)
ELSE
  rowTitle(21) = "**cellOrder**"
  astr(21) = " "
END IF

ans = React_StartTabs()//char_lf
ans = ans//React_StartTabItem(VALUE="0", label="Finite Element")//char_lf// &
  & MdEncode( &
  & val=astr(1:21), &
  & rh=rowTitle(1:21), &
  & ch=colTitle)//char_lf// &
  & React_EndTabItem()//char_lf// &
  & React_StartTabItem(VALUE="1", label="Reference Element")//char_lf// &
  & char_lf//obj%refelem%ReactEncode()//char_lf// &
  & React_EndTabItem()//char_lf//React_EndTabs()//char_lf
END PROCEDURE obj_ReactEncode

END SUBMODULE IOMethods
