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

SUBMODULE(STScalarField_Class) SetMethods
USE InputUtility, ONLY: Input
USE AbstractField_Class, ONLY: TypeField
USE ScalarField_Class, ONLY: ScalarField_
USE RealVector_Method, ONLY: Set, Add, GetPointer
USE Display_Method, ONLY: tostring

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
INTEGER(I4B) :: localNode(1)
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
LOGICAL(LGT) :: isok

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: STScalarField_::obj is not initiated')
  RETURN
END IF

IF (SIZE(VALUE) .NE. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
         '[INTERNAL ERROR] :: Size of value should be equal to obj%timeCompo')
  RETURN
END IF

#endif

abool = Input(option=AddContribution, default=.FALSE.)

IF (obj%fieldType .EQ. TypeField%constant) THEN

  localNode(1) = 1

ELSE

  localNode(1) = obj%fedof%mesh%GetLocalNodeNumber(globalNode=globalNode, &
                                                   islocal=islocal)

#ifdef DEBUG_VER

  isok = obj%tSize .GE. localNode
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: global nodes are out of bound')
    RETURN
  END IF

#endif

END IF

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj=obj%realVec, dofobj=obj%dof, nodenum=localNode, &
           VALUE=VALUE, conversion=[NONE], scale=areal)
  RETURN
END IF

CALL Set(obj=obj%realVec, dofobj=obj%dof, nodenum=localNode, &
         VALUE=VALUE, conversion=[NONE])

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
INTEGER(I4B) :: idof
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

abool = Input(option=AddContribution, default=.FALSE.)

#ifdef DEBUG_VER

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: STScalarField_::obj is not initiated')
  RETURN
END IF

IF (SIZE(VALUE) .NE. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: size(value) should be same as obj%timeCompo')
  RETURN
END IF

#endif

IF (obj%fieldType .EQ. TypeField%constant) THEN

  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj=obj%realVec, dofobj=obj%dof, nodenum=[1], VALUE=VALUE, &
             conversion=[NONE], scale=areal)
    RETURN
  END IF

  CALL Set(obj=obj%realVec, dofobj=obj%dof, nodenum=[1], VALUE=VALUE, &
           conversion=[NONE])

  RETURN
END IF

vecPointer => NULL()

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  DO idof = 1, obj%timeCompo
    vecPointer => GetPointer(obj%realVec, obj%dof, idof)
    vecPointer = vecPointer + scale * VALUE(idof)
  END DO
  vecPointer => NULL()
  RETURN
END IF

DO idof = 1, obj%timeCompo
  vecPointer => GetPointer(obj%realVec, obj%dof, idof)
  vecPointer = VALUE(idof)
END DO

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
INTEGER(I4B) :: idof
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

abool = Input(option=AddContribution, default=.FALSE.)

#ifdef DEBUG_VER

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
     '[INTERNAL ERROR] :: timeCompo should be less or equal to obj%timeCompo')
  RETURN
END IF

#endif

IF (obj%fieldType .EQ. TypeField%constant) THEN

  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj=obj%realVec, dofobj=obj%dof, nodenum=[1], VALUE=[VALUE], &
             idof=timeCompo, scale=areal)
    RETURN
  END IF

  CALL Set(obj=obj%realVec, dofobj=obj%dof, nodenum=[1], VALUE=[VALUE], &
           idof=timeCompo)
  RETURN

END IF

vecPointer => GetPointer(obj%realVec, obj%dof, timeCompo)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  vecPointer = vecPointer + areal * VALUE
  vecPointer => NULL()
  RETURN
END IF

vecPointer = VALUE
vecPointer => NULL()

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
INTEGER(I4B) :: ii, tnodes, aa, jj
REAL(DFP) :: areal
LOGICAL(LGT) :: abool, isok

#ifdef DEBUG_VER

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: STScalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: Not callable for constant STScalar field')
  RETURN
END IF

#endif

tnodes = obj%fedof%GetTotalDOF()

#ifdef DEBUG_VER

isok = (SIZE(VALUE, 1) .NE. obj%timeCompo) .OR. (SIZE(VALUE, 2) .NE. tnodes)
IF (isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: The shape of value should be [ ' &
                    //tostring(obj%timeCompo) &
                    //', ' &
                    //tostring(tnodes) &
                    //' ]')
  RETURN
END IF

#endif

abool = Input(option=AddContribution, default=.FALSE.)
aa = 0

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  !! CALL Add(obj=obj%realvec, dofobj=obj%dof, value=value, scale=areal)
  DO jj = 1, tnodes
    DO ii = 1, obj%timeCompo
      aa = aa + 1
      obj%realVec%val(aa) = obj%realVec%val(aa) + areal * VALUE(ii, jj)
    END DO
  END DO
  RETURN
END IF

!! CALL Set(obj=obj%realvec, dofobj=obj%dof, value=value, scale=areal)

DO jj = 1, tnodes
  DO ii = 1, obj%timeCompo
    aa = aa + 1
    obj%realVec%val(aa) = VALUE(ii, jj)
  END DO
END DO

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set5"
INTEGER(I4B) :: idof
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=AddContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'given timeCompo should be less than or equal to obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'This subroutine is not callable for constant STScalar field')
  RETURN
END IF

IF (SIZE(VALUE) .NE. obj%domain%getTotalNodes()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'Size of value should be equal to the total number of nodes')
  RETURN
END IF

vecPointer => GetPointer(obj%realVec, obj%dof, timeCompo)
IF (abool) THEN
  vecPointer = vecPointer + areal * VALUE
ELSE
  vecPointer = VALUE
END IF

vecPointer => NULL()

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set5"
INTEGER(I4B) :: idof
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=AddContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated .OR. .NOT. VALUE%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'This subroutine is not callable for constant STScalar field')
  RETURN
END IF

SELECT TYPE (VALUE)
TYPE IS (ScalarField_)
  IF (VALUE%domain%getTotalNodes() .NE. obj%domain%getTotalNodes()) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
        & 'Size of value should be equal to the total number of nodes')
    RETURN
  END IF

  IF (VALUE%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
    vecPointer => GetPointer(VALUE%realVec, VALUE%dof, 1)
    CALL obj%Set(VALUE=vecPointer(1), timeCompo=timeCompo, &
      & scale=areal, AddContribution=abool)
    vecPointer => NULL()
    RETURN
  END IF

  vecPointer => GetPointer(obj%realVec, obj%dof, timeCompo)
  IF (abool) THEN
    vecPointer = vecPointer + areal * get(VALUE%realVec, 1.0_DFP)
  ELSE
    vecPointer = get(VALUE%realVec, 1.0_DFP)
  END IF
  vecPointer => NULL()

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & 'No case found for the type of value')
END SELECT

END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
REAL(DFP) :: val(SIZE(VALUE), SIZE(globalNode))
INTEGER(I4B) :: ii
DO ii = 1, SIZE(globalNode)
  val(:, ii) = VALUE(:)
END DO
CALL obj%Set( &
  & VALUE=val, &
  & globalNode=globalNode, &
  & scale=scale, &
  & AddContribution=AddContribution)
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
CHARACTER(*), PARAMETER :: myName = "obj_Set8"
INTEGER(I4B) :: localNode(SIZE(globalNode))
REAL(DFP) :: val(SIZE(VALUE))
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=AddContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'This routine should not be called for constant STScalar field')
  RETURN
END IF

IF (SIZE(VALUE, 1) .NE. obj%timeCompo .OR. &
    & SIZE(VALUE, 2) .NE. SIZE(globalNode)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
      & 'SIZE( value, 1 ) not equal timeCompo or SIZE( value, 2 ) ' &
      & //'not equal to the SIZE(globalNode)')
  RETURN
END IF

localNode = obj%domain%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .GT. obj%tSize)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'Some of the globalNode are out of bound')
  RETURN
END IF

val = RESHAPE(VALUE, [SIZE(VALUE)])

IF (abool) THEN
  CALL Add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=val, &
    & conversion=[NONE], &
    & scale=areal)
ELSE
  CALL Set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=val, &
    & conversion=[NONE])
END IF

END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set9"
INTEGER(I4B) :: idof
INTEGER(I4B) :: localNode(SIZE(globalNode))
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=AddContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'given timeCompo should be less than or equal to obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'This subroutine is not callable for constant STScalar field')
  RETURN
END IF

IF (SIZE(VALUE) .NE. SIZE(globalNode)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'Size of value should be equal to size of globalNode')
  RETURN
END IF

localNode = obj%domain%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .GT. obj%domain%getTotalNodes())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'Some of the global node num are out of bound')
  RETURN
END IF

vecPointer => GetPointer(obj%realVec, obj%dof, timeCompo)
IF (abool) THEN
  vecPointer(localNode) = vecPointer(localNode) + areal * VALUE
ELSE
  vecPointer(localNode) = VALUE
END IF
vecPointer => NULL()

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set9"
INTEGER(I4B) :: idof
INTEGER(I4B) :: localNode
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=AddContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'given timeCompo should be less than or equal to obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'This subroutine is not callable for constant STScalar field')
  RETURN
END IF

localNode = obj%domain%GetLocalNodeNumber(globalNode)

IF (localNode .GT. obj%domain%getTotalNodes()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'The given global node num are out of bound')
  RETURN
END IF

vecPointer => GetPointer(obj%realVec, obj%dof, timeCompo)
IF (abool) THEN
  vecPointer(localNode) = vecPointer(localNode) + areal * VALUE
ELSE
  vecPointer(localNode) = VALUE
END IF
vecPointer => NULL()

END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
CHARACTER(*), PARAMETER :: myName = "obj_Set11"
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%Set(globalNode=globalNode, VALUE=VALUE, &
  & scale=scale, AddContribution=AddContribution)
END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set12
CHARACTER(*), PARAMETER :: myName = "obj_Set12"
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%Set(globalNode=globalNode, VALUE=VALUE, &
  & scale=scale, AddContribution=AddContribution)
END PROCEDURE obj_Set12

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set13
CHARACTER(*), PARAMETER :: myName = "obj_Set13"
INTEGER(I4B) :: localNode(SIZE(globalNode))

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'This routine should not be called for constant STScalar field')
  RETURN
END IF

IF (SIZE(VALUE, 1) .NE. obj%timeCompo .OR. &
  & SIZE(VALUE, 2) .NE. SIZE(globalNode)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
      & 'SIZE( value, 1 ) not equal timeCompo or SIZE( value, 2 ) ' &
      & //'not equal to the SIZE(globalNode)')
  RETURN
END IF

localNode = obj%domain%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .GT. obj%tSize)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
 & 'Some of the globalNode are out of bound')
  RETURN
END IF

SELECT CASE (VALUE%vartype)
CASE (SpaceTime)
  CALL obj%Set( &
    & VALUE=GET(VALUE, TypeFEVariableScalar, TypeFEVariableSpaceTime), &
    & globalNode=globalNode, &
    & scale=scale, &
    & AddContribution=AddContribution)
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'No case found for Value%vartype')
END SELECT

END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set14
IF (PRESENT(AddContribution)) THEN
  CALL Add(obj=obj%realvec, VALUE=VALUE, scale=scale)
ELSE
  CALL Set(obj=obj%realvec, VALUE=VALUE)
END IF
END PROCEDURE obj_Set14

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set15
CHARACTER(*), PARAMETER :: myName = "obj_Set15"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: tsize_value
INTEGER(I4B) :: ii
INTEGER(I4B) :: indx1
INTEGER(I4B) :: indx2
REAL(DFP) :: avar

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'STScalarField_::obj is not initiated')
  RETURN
END IF

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'AbstractNodeField_ ::value is not initiated')
  RETURN
END IF

tsize = obj%dof.tNodes. [ivar, idof]
tsize_value = VALUE%dof.tNodes. [ivar_value, idof_value]
IF (tsize .NE. tsize_value) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'tSize of obj(ivar, idof) is equal to value(ivar_value, idof_value)')
  RETURN
END IF

DO ii = 1, tsize
  indx1 = GetNodeLoc(&
    & obj=VALUE%dof, &
    & nodenum=ii, &
    & ivar=ivar_value, &
    & idof=idof_value)
  CALL VALUE%GetSingle(VALUE=avar, indx=indx1)
  indx2 = GetNodeLoc(&
    & obj=obj%dof, &
    & nodenum=ii, &
    & ivar=ivar, &
    & idof=idof)
  CALL obj%SetSingle(VALUE=avar, indx=indx2, scale=scale, &
    & AddContribution=AddContribution)
END DO

END PROCEDURE obj_Set15

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set16
CHARACTER(*), PARAMETER :: myName = "obj_Set16()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

CALL Set(obj%realVec, VALUE=VALUE%realVec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_Set16

!----------------------------------------------------------------------------
!                                                               SetByFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
LOGICAL(LGT) :: istimes, problem
INTEGER(I4B) :: ttime, nsd, tnodes, ii, globalNode(1), itime
REAL(DFP) :: args(4), VALUE(obj%timeCompo), aval, xij(3, 1)
INTEGER(I4B), PARAMETER :: needed_returnType = Scalar
CLASS(AbstractDomain_), POINTER :: dom

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

istimes = PRESENT(times)
problem = .FALSE.

args = 0.0_DFP
IF (istimes) THEN
  ttime = SIZE(times)
  problem = ttime .NE. obj%timeCompo
END IF

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: size of times should be obj%timeCompo='// &
    & tostring(obj%timeCompo))
  RETURN
END IF

dom => NULL()
dom => obj%domain
problem = .NOT. ASSOCIATED(dom)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: domain is not ASSOCIATED.')
  RETURN
END IF

nsd = dom%GetNSD()
tnodes = dom%GetTotalNodes()

IF (istimes) THEN

  DO ii = 1, tnodes
    globalNode = ii
    CALL dom%GetNodeCoord(globalNode=globalNode, nodeCoord=xij(1:nsd, 1:1))
    args(1:nsd) = xij(1:nsd, 1)

    DO itime = 1, obj%timeCompo
      args(4) = times(itime)
      CALL func%Get(val=VALUE(itime), args=args)
    END DO

    CALL obj%Set(globalNode=globalNode(1), VALUE=VALUE)
  END DO

END IF

IF (.NOT. istimes) THEN
  DO ii = 1, tnodes
    globalNode = ii
    CALL dom%GetNodeCoord(globalNode=globalNode, nodeCoord=xij(1:nsd, 1:1))
    args(1:nsd) = xij(1:nsd, 1)
    CALL func%Get(val=aval, args=args)
    VALUE = aval
    CALL obj%Set(globalNode=globalNode(1), VALUE=VALUE)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_SetByFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
