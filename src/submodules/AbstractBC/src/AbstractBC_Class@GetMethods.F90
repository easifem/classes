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

SUBMODULE(AbstractBC_Class) GetMethods
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: ToString

USE GlobalData, ONLY: CHAR_LF

USE AbstractMesh_Class, ONLY: AbstractMesh_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetMeshID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshID
ans = obj%boundary%GetMeshID(dim=dim)
END PROCEDURE obj_GetMeshID

!----------------------------------------------------------------------------
!                                                           GetMeshIDPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshIDPointer
CALL obj%boundary%GetMeshIDPointer(dim=dim, ans=ans, tsize=tsize)
END PROCEDURE obj_GetMeshIDPointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDOFNo
ans = obj%idof
END PROCEDURE obj_GetDOFNo

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
#endif

INTEGER(I4B) :: ii, jj
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, &
                  'AbstractBC_ object is not initiated, initiate it first.')

isok = ASSOCIATED(obj%dom)
CALL AssertError1(isok, myName, &
                  'AbstractBC_::obj%dom is not associated!')
#endif

CALL obj%boundary%GetNodeNum(dom=obj%dom, ans=nodenum, tsize=nrow)

IF (obj%isUserFunction) THEN
  CALL obj%GetFromUserFunction(nodeNum=nodeNum, nodalValue=nodalValue, &
                               times=times, nrow=nrow, ncol=ncol)
  RETURN
END IF

ncol = 1
IF (PRESENT(times)) ncol = SIZE(times)

#ifdef DEBUG_VER

isok = ALLOCATED(obj%nodalValue)
CALL AssertError1(isok, myname, &
                  'AbstractBC_::obj%nodalValue is not allocated!')

#endif

! get nodal values
SELECT CASE (obj%nodalValueType)

CASE (TypeFEVariableOpt%constant)
  nodalValue(1:nrow, 1:ncol) = obj%nodalValue(1, 1)

CASE (TypeFEVariableOpt%space)

#ifdef DEBUG_VER
  isok = SIZE(obj%nodalValue, 1) .GE. nrow
  CALL AssertError1(isok, myname, &
                    'SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
#endif

  ncol = 1
  DO CONCURRENT(ii=1:nrow)
    nodalValue(ii, 1) = obj%nodalValue(ii, 1)
  END DO

! Time
CASE (TypeFEVariableOpt%time)
  ncol = SIZE(obj%nodalValue, 1)

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(jj, 1)
  END DO

! SpaceTime
CASE (TypeFEVariableOpt%spacetime)

#ifdef DEBUG_VER
  isok = SIZE(obj%nodalValue, 1) .GE. nrow
  CALL AssertError1(isok, myname, &
                    'SIZE(obj%nodalValue, 1) .NE. SIZE(nodeNum)')
#endif

  ncol = SIZE(obj%nodalvalue, 2)

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(ii, jj)
  END DO

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get2()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, &
                  'AbstractBC_ object is not initiated, initiate it first.')

isok = ASSOCIATED(obj%dom)
CALL AssertError1(isok, myName, &
                  'AbstractBC_::obj%dom is not associated!')
#endif

CALL obj%boundary%GetNodeNum(dom=obj%dom, ans=nodenum, tsize=tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                           obj_GetFEVar
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development.')

! If useFunction is true  then
!    if constant
!    if time
!    if space
!    if space-time

! If useFunction is not true then
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                           GetTotalNodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodenum
ans = obj%boundary%GetTotalNodeNum(obj%dom)
END PROCEDURE obj_GetTotalNodenum

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isuseFunction
ans = obj%isUserFunction
END PROCEDURE obj_isuseFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
CALL obj%boundary%GetParam(isSelectionByBox=isSelectionByBox, &
                           isSelectionByMeshID=isSelectionByMeshID, &
                           isSelectionByElemNum=isSelectionByElemNum, &
                           isSelectionByNodeNum=isSelectionByNodeNum)

IF (PRESENT(idof)) idof = obj%idof
IF (PRESENT(isTangent)) isTangent = obj%isTangent
IF (PRESENT(isNormal)) isNormal = obj%isNormal
IF (PRESENT(useFunction)) useFunction = obj%isUserFunction
IF (PRESENT(isUserFunction)) isUserFunction = obj%isUserFunction
IF (PRESENT(nodalValueType)) nodalValueType = obj%nodalValueType
IF (PRESENT(isInitiated)) isInitiated = obj%isInitiated
IF (PRESENT(useExternal)) useExternal = obj%useExternal
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                                GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
CHARACTER(*), PARAMETER :: myName = "obj_GetPrefix()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
          '[WIP ERROR] :: This routine should be implemented by child class.')
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFromUserFunction
CHARACTER(*), PARAMETER :: myName = "obj_GetFromUserFunction()"
INTEGER(I4B) :: ii, kk, retType, tnodes, nsd, tTimes, argType, &
                tsize
REAL(DFP) :: xij(4, 1), ans
LOGICAL(LGT) :: problem
CLASS(AbstractMesh_), POINTER :: meshptr

! get pointer to nodecoord

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER

problem = .NOT. ASSOCIATED(obj%func)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    "[INTERNAL ERROR] :: When nodalValueType is "// &
                    CHAR_LF//"Space and useFunction is specified, "// &
                    CHAR_LF//"then SpaceFunction is needed, "// &
                    CHAR_LF//"but it is not associated")
  RETURN
END IF

#endif

retType = obj%func%GetReturnType()

#ifdef DEBUG_VER

problem = retType .NE. TypeFEVariableOpt%scalar
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
             '[INTERNAL ERROR] :: Return type of user function should be '// &
                    'scalar.')
  RETURN
END IF

problem = (obj%nodalValueType .EQ. TypeFEVariableOpt%time) .AND. &
          (.NOT. PRESENT(times))
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    "[INTERNAL ERROR] :: When `nodalValueType` is Time "// &
                    " and `IsUserFunction` is TRUE, "// &
                    " then `times` is needed in the passing argument,"// &
                    " but it is not present")
  RETURN
END IF

problem = (obj%nodalValueType .EQ. TypeFEVariableOpt%spacetime) .AND. &
          (.NOT. PRESENT(times))
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 "[INTERNAL ERROR] :: When `nodalValueType` is SpaceTime "// &
                    " and `IsUserFunction` is TRUE, "// &
                    " then `times` is needed in the passing argument,"// &
                    " but it is not present")
  RETURN
END IF

#endif

argType = obj%func%GetArgType()

#ifdef DEBUG_VER

problem = argType .NE. obj%nodalValueType
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: argType='//tostring(argType)// &
                    ' in user function is not same '// &
                    'as nodalValueType '//tostring(obj%nodalValueType)// &
                    ' in AbstractBC_')
  RETURN
END IF

#endif

tnodes = SIZE(nodeNum)
nrow = tnodes
ncol = 0
nsd = obj%dom%GetNSD()

SELECT CASE (obj%nodalValueType)

! Constant
CASE (TypeFEVariableOpt%constant)
  ! CALL Reallocate(nodalValue, tnodes, 1)
  ncol = 1
  CALL obj%func%Get(val=ans)
  nodalValue(1:nrow, 1) = ans

! Space
CASE (TypeFEVariableOpt%space)
  ! CALL Reallocate(nodalValue, tnodes, 1)
  ncol = 1
  meshptr => obj%dom%GetMeshPointer(dim=nsd)

  DO ii = 1, tnodes
    CALL meshptr%GetNodeCoord(nodeCoord=xij(:, 1), tsize=tsize, &
                              globalNode=nodeNum(ii), islocal=.FALSE.)

    CALL obj%func%Get(val=ans, args=xij(1:3, 1))

    nodalValue(ii, 1) = ans
  END DO

  meshptr => NULL()

! Time
CASE (TypeFEVariableOpt%time)

  tTimes = SIZE(times)

  ! CALL Reallocate(nodalValue, tnodes, tTimes)
  ncol = tTimes

  DO ii = 1, tTimes
    CALL obj%func%Get(val=ans, args=times(ii:ii))

    nodalValue(1:nrow, ii) = ans
  END DO

! SpaceTime
CASE (TypeFEVariableOpt%spacetime)
  tTimes = SIZE(times)

  ! CALL Reallocate(nodalValue, tnodes, tTimes)
  ncol = tTimes

  meshptr => obj%dom%GetMeshPointer(dim=nsd)

  DO kk = 1, tTimes
    xij(nsd + 1, 1) = times(kk)

    DO ii = 1, tnodes
      CALL meshptr%GetNodeCoord(nodeCoord=xij(:, 1), tsize=tsize, &
                                globalNode=nodeNum(ii), islocal=.FALSE.)

      CALL obj%func%Get(val=ans, args=xij(1:4, 1))

      nodalValue(ii, kk) = ans
    END DO
  END DO

  meshptr => NULL()

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetFromUserFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
