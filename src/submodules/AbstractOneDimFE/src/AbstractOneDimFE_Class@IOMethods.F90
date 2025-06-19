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

SUBMODULE(AbstractOneDimFE_Class) IOMethods
USE Display_Method, ONLY: Display, ToString

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
INTEGER(I4B) :: s(2)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%isInitiated, msg="isInitiated: ", unitno=unitno)
IF (.NOT. obj%isInitiated) RETURN

CALL obj%opt%Display(unitno=unitno, msg="OneDimBasisOpt from opt:")

isok = ALLOCATED(obj%xij)
CALL Display(isok, msg="xij is Allocated: ", unitno=unitno)
IF (isok) THEN
  s = SIZE(obj%xij)
  CALL Display(s, msg="xij size: ", unitno=unitno)
END IF

isok = ALLOCATED(obj%coeff)
CALL Display(isok, msg="coeff is Allocated: ", unitno=unitno)
IF (isok) THEN
  s = SIZE(obj%coeff)
  CALL Display(isok, msg="coeff size: ", unitno=unitno)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MdEncode
CHARACTER(*), PARAMETER :: myName = "obj_MdEncode()"

! INTEGER(I4B), PARAMETER :: jj = 21
! TYPE(String) :: rowTitle(jj), colTitle(1), astr(jj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')

! colTitle(1) = ""
! rowTitle(1) = "**nsd**"; astr(1) = ToString(obj%nsd)
! rowTitle(2) = "**feType**"; astr(2) = ToString(obj%feType)
! rowTitle(3) = "**elemType**"; astr(3) = ElementName(obj%elemType)
! rowTitle(4) = "**ipType**"; astr(4) = ToString(obj%ipType)
! rowTitle(5) = "**basisType**"; astr(5) = ToString(obj%basisType)
! rowTitle(6) = "**alpha**"; astr(6) = ToString(obj%alpha)
! rowTitle(7) = "**beta**"; astr(7) = ToString(obj%beta)
! rowTitle(8) = "**lambda**"; astr(8) = ToString(obj%lambda)
! rowTitle(9) = "**dofType**"; astr(9) = ToString(obj%dofType)
! rowTitle(10) = "**transformType**"; astr(10) = ToString(obj%transformType)
! rowTitle(11) = "**baseContinuity**"; astr(11) = obj%baseContinuity
! rowTitle(12) = "**baseInterpolion**"; astr(12) = obj%baseInterpolation
! rowTitle(13) = "**refElemDomain**"; astr(13) = obj%refElemDomain
! rowTitle(14) = "**isIsotropicOrder**"; astr(14) = ToString(obj%isIsotropicOrder)
! rowTitle(15) = "**isAnisotropicOrder**"; astr(15) = ToString(obj%isAnisotropicOrder)
! rowTitle(16) = "**isEdgeOrder**"; astr(16) = ToString(obj%isEdgeOrder)
! rowTitle(17) = "**isFaceOrder**"; astr(17) = ToString(obj%isFaceOrder)
! rowTitle(18) = "**isCellOrder**"; astr(18) = ToString(obj%isCellOrder)
!
! IF (obj%isEdgeOrder) THEN
!   rowTitle(19) = "**edgeOrder**"; astr(19) = ToString(obj%edgeOrder)
! ELSE
!   rowTitle(19) = "**edgeOrder**"; astr(19) = " "
! END IF
!
! ! IF (obj%isFaceOrder) THEN
! !   rowTitle(20) = "**faceOrder**"; astr(20) = ToString(obj%faceOrder)
! ! ELSE
! rowTitle(20) = "**faceOrder**"; astr(20) = " "
! ! END IF
!
! IF (obj%iscellOrder) THEN
!   rowTitle(21) = "**cellOrder**"; astr(21) = ToString(obj%cellOrder)
! ELSE
!   rowTitle(21) = "**cellOrder**"; astr(21) = " "
! END IF
!
! ans = MdEncode(val=astr(1:21), rh=rowTitle(1:21), ch=colTitle)//char_lf

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_MdEncode

!----------------------------------------------------------------------------
!                                                                MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReactEncode
CHARACTER(*), PARAMETER :: myName = "obj_ReactEncode"

! INTEGER(I4B), PARAMETER :: jj = 21
! TYPE(String) :: rowTitle(jj), colTitle(1), astr(jj)

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')

! colTitle(1) = ""
! rowTitle(1) = "**nsd**"
! astr(1) = ToString(obj%nsd)
!
! rowTitle(2) = "**feType**"
! astr(2) = ToString(obj%feType)
!
! rowTitle(3) = "**elemType**"
! astr(3) = ElementName(obj%elemType)
!
! rowTitle(4) = "**ipType**"
! astr(4) = ToString(obj%ipType)
!
! rowTitle(5) = "**basisType**"
! astr(5) = ToString(obj%basisType)
!
! rowTitle(6) = "**alpha**"
! astr(6) = ToString(obj%alpha)
!
! rowTitle(7) = "**beta**"
! astr(7) = ToString(obj%beta)
!
! rowTitle(8) = "**lambda**"
! astr(8) = ToString(obj%lambda)
!
! rowTitle(9) = "**dofType**"
! astr(9) = ToString(obj%dofType)
!
! rowTitle(10) = "**transformType**"
! astr(10) = ToString(obj%transformType)
!
! rowTitle(11) = "**baseContinuity**"
! astr(11) = obj%baseContinuity
!
! rowTitle(12) = "**baseInterpolation**"
! astr(12) = obj%baseInterpolation
!
! rowTitle(13) = "**refElemDomain**"
! astr(13) = obj%refElemDomain
!
! rowTitle(14) = "**isIsotropicOrder**"
! astr(14) = ToString(obj%isIsotropicOrder)
!
! rowTitle(15) = "**isAnisotropicOrder**"
! astr(15) = ToString(obj%isAnisotropicOrder)
!
! rowTitle(16) = "**isEdgeOrder**"
! astr(16) = ToString(obj%isEdgeOrder)
!
! rowTitle(17) = "**isFaceOrder**"
! astr(17) = ToString(obj%isFaceOrder)
!
! rowTitle(18) = "**isCellOrder**"
! astr(18) = ToString(obj%isCellOrder)
!
! IF (obj%isEdgeOrder) THEN
!   rowTitle(19) = "**edgeOrder**"
!   astr(19) = ToString(obj%edgeOrder)
! ELSE
!   rowTitle(19) = "**edgeOrder**"
!   astr(19) = " "
! END IF
!
! ! IF (obj%isFaceOrder) THEN
! !   rowTitle(20) = "**faceOrder**"
! !   astr(20) = ToString(obj%faceOrder)
! ! ELSE
! rowTitle(20) = "**faceOrder**"
! astr(20) = " "
! ! END IF
!
! IF (obj%iscellOrder) THEN
!   rowTitle(21) = "**cellOrder**"
!   astr(21) = ToString(obj%cellOrder)
! ELSE
!   rowTitle(21) = "**cellOrder**"
!   astr(21) = " "
! END IF
!
! ans = React_StartTabs()//char_lf
! ans = ans//React_StartTabItem(VALUE="0", label="Finite Element")//char_lf// &
!       MdEncode(val=astr(1:21), rh=rowTitle(1:21), ch=colTitle)//char_lf// &
!       React_EndTabItem()//char_lf

END PROCEDURE obj_ReactEncode

END SUBMODULE IOMethods
