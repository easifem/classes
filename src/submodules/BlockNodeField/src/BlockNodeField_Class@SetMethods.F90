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

SUBMODULE(BlockNodeField_Class) SetMethods
USE ArangeUtility, ONLY: Arange
USE DOF_Method, ONLY: GetIndex_
USE DOF_Method, ONLY: GetNodeLoc
USE DOF_Method, ONLY: GetNodeLoc_
USE DOF_Method, ONLY: OPERATOR(.tDOF.)
USE InputUtility, ONLY: Input
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "BlockNodeField_Class@SetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetAll(VALUE=VALUE, scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
#endif
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
tsize = obj%SIZE()
CALL AssertError2(tsize, SIZE(VALUE), myName, &
                  'a=obj%size(), b=size(value)')
#endif

tsize = obj%SIZE()

CALL obj%SetMultiple(VALUE=VALUE, scale=scale, &
                     addContribution=addContribution, &
                     istart=1_I4B, iend=tsize, stride=1_I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
#endif
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#include "./localNodeError.F90"

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, ivar=ivar, idof=idof)

CALL obj%SetSingle(VALUE=VALUE, indx=indx, scale=scale, &
                   addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
#endif
INTEGER(I4B) :: indx(128), ii, tsize, idof(128)
REAL(DFP) :: areal(128)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#include "./localNodeError.F90"

areal = VALUE

tsize = obj%dof .tDOF. ivar
idof(1:tsize) = Arange(1_I4B, tsize)

DO ii = 1, SIZE(globalNode)
  CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode(ii), ivar=ivar, &
                   idof=idof(1:tsize), ans=indx, tsize=tsize)

  CALL obj%SetMultiple(VALUE=areal(1:tsize), indx=indx(1:tsize), &
                       scale=scale, addContribution=addContribution)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
#endif

INTEGER(I4B) :: indx(SIZE(VALUE)), tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE), &
                  SIZE(globalNode) * (obj%dof .tdof. ivar), &
                  myName, &
                  "a=size(value), b=size(globalNode) * (obj%tDOF%ivar)")
#endif

#include "./localNodeError.F90"

CALL GetIndex_(obj=obj%dof, nodenum=globalNode, ivar=ivar, ans=indx, &
               tsize=tsize)

CALL obj%SetMultiple(VALUE=VALUE, indx=indx, scale=scale, &
                     addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set6()"
#endif
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

DO ii = 1, SIZE(globalNode)
  CALL obj%Set(globalNode=globalNode(ii), islocal=islocal, scale=scale, &
               addContribution=addContribution, VALUE=VALUE, &
               ivar=ivar, idof=idof)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set7()"
#endif
INTEGER(I4B) :: indx(SIZE(globalNode)), tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE), SIZE(globalNode), myName, &
                  "a=size(value), b=size(globalNode)")
#endif

#include "./localNodeError.F90"

CALL GetNodeLoc_(obj=obj%dof, ans=indx, tsize=tsize, ivar=ivar, &
                 idof=idof, nodenum=globalNode)

CALL obj%SetMultiple(indx=indx, VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set8()"
#endif
INTEGER(I4B) :: indx(SIZE(globalNode)), tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE), SIZE(globalNode), myName, &
                  "a=size(value), b=size(globalNode)")
#endif

#include "./localNodeError.F90"

CALL GetNodeLoc_(obj=obj%dof, ans=indx, tsize=tsize, ivar=ivar, &
                 spaceCompo=spaceCompo, timeCompo=timeCompo, &
                 nodenum=globalNode)

CALL obj%SetMultiple(indx=indx, VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
#endif
REAL(DFP) :: value0(SIZE(globalNode))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

value0 = VALUE
CALL obj%Set(VALUE=value0, scale=scale, addContribution=addContribution, &
             globalNode=globalNode, islocal=islocal, ivar=ivar, &
             spaceCompo=spaceCompo, &
             timeCompo=timeCompo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set10()"
#endif
INTEGER(I4B) :: indx(SIZE(VALUE)), tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE), &
                  SIZE(globalNode) * SIZE(timeCompo), &
                  myName, &
                  "a=size(value), b=size(globalNode)*size(timeCompo)")
#endif

#include "./localNodeError.F90"

CALL GetNodeLoc_(obj=obj%dof, ivar=ivar, spaceCompo=spaceCompo, &
                 timeCompo=timeCompo, nodenum=globalNode, ans=indx, &
                 tsize=tsize)

CALL obj%SetMultiple(indx=indx, VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set11()"
#endif
REAL(DFP) :: value0(SIZE(globalNode) * SIZE(timeCompo))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

value0 = VALUE
CALL obj%Set(VALUE=value0, globalNode=globalNode, islocal=islocal, &
             ivar=ivar, spaceCompo=spaceCompo, timeCompo=timeCompo, &
             scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set12
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set12()"
#endif
INTEGER(I4B) :: indx(SIZE(VALUE)), tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE), &
                  SIZE(globalNode) * SIZE(spaceCompo), &
                  myName, &
                  "a=size(value), b=size(globalNode)*size(spaceCompo)")
#endif

#include "./localNodeError.F90"

CALL GetNodeLoc_(obj=obj%dof, ivar=ivar, spaceCompo=spaceCompo, &
                 timeCompo=timeCompo, nodenum=globalNode, ans=indx, &
                 tsize=tsize)

CALL obj%SetMultiple(indx=indx, VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set13
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set13()"
#endif
REAL(DFP) :: value0(SIZE(globalNode) * SIZE(spaceCompo))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

value0 = VALUE
CALL obj%Set(VALUE=value0, globalNode=globalNode, islocal=islocal, &
             ivar=ivar, spaceCompo=spaceCompo, timeCompo=timeCompo, &
             scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set14
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set14()"
#endif
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')

#endif

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, ivar=ivar, &
                  spaceCompo=spaceCompo, timeCompo=timeCompo)

CALL obj%SetSingle(VALUE=VALUE, indx=indx, &
                   scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set15
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set15()"
#endif
INTEGER(I4B) :: indx(SIZE(timeCompo)), tsize
REAL(DFP) :: value0(SIZE(timeCompo))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

CALL GetNodeLoc_(obj=obj%dof, ivar=ivar, spaceCompo=spaceCompo, &
                 timeCompo=timeCompo, nodenum=globalNode, ans=indx, &
                 tsize=tsize)

value0 = VALUE

CALL obj%SetMultiple(indx=indx, VALUE=value0, scale=scale, &
                     addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set15

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set16
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set16()"
#endif
INTEGER(I4B) :: indx(SIZE(spaceCompo)), tsize
REAL(DFP) :: value0(SIZE(spaceCompo))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

CALL GetNodeLoc_(obj=obj%dof, ivar=ivar, spaceCompo=spaceCompo, &
                 timeCompo=timeCompo, nodenum=globalNode, ans=indx, &
                 tsize=tsize)

value0 = VALUE

CALL obj%SetMultiple(indx=indx, VALUE=value0, scale=scale, &
                     addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set16

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set17
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set17()"
#endif
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError1(VALUE%IsInitiated(), myName, &
                  'BlockNodeField_::value is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError2(obj%SIZE(), VALUE%SIZE(), myName, &
                  'a=obj%SIZE(), b=VALUE%SIZE()')
#endif

abool = Input(option=addContribution, default=.FALSE.)

! scale is used only when the values are added
areal = 1.0_DFP
IF (abool) areal = Input(option=scale, default=1.0_DFP)

! SetMultiple works between engines (native, LIS, ...)
tsize = obj%SIZE()
CALL obj%SetMultiple(istart=1_I4B, iend=tsize, stride=1_I4B, VALUE=VALUE, &
                     istart_value=1_I4B, iend_value=tsize, &
                     stride_value=1_I4B, scale=areal, addContribution=abool)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set17

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Assign
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Assign()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetAll(VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Assign

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
