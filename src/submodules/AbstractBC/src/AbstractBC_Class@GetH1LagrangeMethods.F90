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

SUBMODULE(AbstractBC_Class) GetH1LagrangeMethods
USE Display_Method, ONLY: ToString

USE GlobalData, ONLY: CHAR_LF

USE AbstractMesh_Class, ONLY: AbstractMesh_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             CheckError
!----------------------------------------------------------------------------

SUBROUTINE checkerror(obj, myName)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: myName

  LOGICAL(LGT) :: isok

  CALL AssertError1(obj%isInitiated, myName, &
                    'AbstractBC_ object is not initiated, initiate it first.')

  isok = ASSOCIATED(obj%dom)
  CALL AssertError1(isok, myName, &
                    'AbstractBC_::obj%dom is not associated!')
END SUBROUTINE checkerror

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get_H1_Lagrange1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get_H1_Lagrange1()"
#endif

LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.

INTEGER(I4B) :: ii, jj, nsd
LOGICAL(LGT) :: isok
CLASS(AbstractMesh_), POINTER :: mesh

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL checkerror(obj, myName)
#endif

CALL obj%boundary%GetNodeNum(dom=obj%dom, ans=nodenum, tsize=nrow)
nsd = obj%dom%GetNSD()
mesh => obj%dom%GetMeshPointer(dim=nsd)
CALL mesh%GetLocalNodeNumber_(globalNode=nodenum, ans=nodenum, islocal=no)

IF (obj%isUserFunction) THEN
  CALL GetFromUserFunction(obj=obj, nodeNum=nodeNum, nodalValue=nodalValue, &
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
  isok = obj%nrow .GE. nrow
  CALL AssertError1(isok, myname, &
                    'SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
#endif

  ncol = 1
  DO CONCURRENT(ii=1:nrow)
    nodalValue(ii, 1) = obj%nodalValue(ii, 1)
  END DO

! Time
CASE (TypeFEVariableOpt%time)
  ncol = obj%nrow

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(jj, 1)
  END DO

! SpaceTime
CASE (TypeFEVariableOpt%spacetime)

#ifdef DEBUG_VER
  isok = obj%nrow .GE. nrow
  CALL AssertError1(isok, myname, &
                    'SIZE(obj%nodalValue, 1) .NE. SIZE(nodeNum)')
#endif

  ncol = obj%ncol

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(ii, jj)
  END DO

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Get_H1_Lagrange1

!----------------------------------------------------------------------------
!                                                          GetConstantValue
!----------------------------------------------------------------------------

SUBROUTINE GetConstantValue(obj, fedof, nodeNum, nodalValue, nrow, ncol, &
                            times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
  !! Degree of freedom
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! Size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nrow = Size of nodeNum
  !! ncol = 1 or Size of times
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! the size of data written in nodalValue
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  !! times vector is only used when usefunction is true in obj

  !! internal variables
  CHARACTER(*), PARAMETER :: myName = "GetConstantValue()"
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: nsd
  CLASS(AbstractMesh_), POINTER :: mesh

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%nodalValue)
  CALL AssertError1(isok, myname, &
                    'AbstractBC_::obj%nodalValue is not allocated!')
#endif

  CALL obj%boundary%GetNodeNum(dom=obj%dom, ans=nodenum, tsize=nrow)
  nsd = obj%dom%GetNSD()
  mesh => obj%dom%GetMeshPointer(dim=nsd)
  CALL mesh%GetLocalNodeNumber_(globalNode=nodenum, ans=nodenum, &
                                islocal=.FALSE.)
  mesh => NULL()

  ncol = 1
  IF (PRESENT(times)) ncol = SIZE(times)
  nodalValue(1:nrow, 1:ncol) = obj%nodalValue(1, 1)
END SUBROUTINE GetConstantValue

!----------------------------------------------------------------------------
!                                                             GetSpaceValue
!----------------------------------------------------------------------------

SUBROUTINE GetSpaceValue(obj, fedof, nodeNum, nodalValue, nrow, ncol, &
                         times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
  !! Degree of freedom
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! Size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nrow = Size of nodeNum
  !! ncol = 1 or Size of times
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! the size of data written in nodalValue
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  !! times vector is only used when usefunction is true in obj

  !! internal variables
  CHARACTER(*), PARAMETER :: myName = "GetSpaceValue()"
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, nsd
  CLASS(AbstractMesh_), POINTER :: mesh

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%nodalValue)
  CALL AssertError1(isok, myname, &
                    'AbstractBC_::obj%nodalValue is not allocated!')
#endif

  CALL obj%boundary%GetNodeNum(dom=obj%dom, ans=nodenum, tsize=nrow)
  nsd = obj%dom%GetNSD()
  mesh => obj%dom%GetMeshPointer(dim=nsd)
  CALL mesh%GetLocalNodeNumber_(globalNode=nodenum, ans=nodenum, &
                                islocal=.FALSE.)
  mesh => NULL()

#ifdef DEBUG_VER
  isok = obj%nrow .GE. nrow
  CALL AssertError1(isok, myname, &
                    'SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
#endif

  ncol = 1
  DO CONCURRENT(ii=1:nrow)
    nodalValue(ii, 1) = obj%nodalValue(ii, 1)
  END DO

END SUBROUTINE GetSpaceValue

!----------------------------------------------------------------------------
!                                                             GetTimeValue
!----------------------------------------------------------------------------

SUBROUTINE GetTimeValue(obj, fedof, nodeNum, nodalValue, nrow, ncol, &
                        times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
  !! Degree of freedom
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! Size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nrow = Size of nodeNum
  !! ncol = 1 or Size of times
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! the size of data written in nodalValue
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  !! times vector is only used when usefunction is true in obj

  !! internal variables
  CHARACTER(*), PARAMETER :: myname = 'GetTimeValue()'
  INTEGER(I4B) :: ii, jj, nsd
  LOGICAL(LGT) :: isok
  CLASS(AbstractMesh_), POINTER :: mesh

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%nodalValue)
  CALL AssertError1(isok, myname, &
                    'AbstractBC_::obj%nodalValue is not allocated!')
#endif

  CALL obj%boundary%GetNodeNum(dom=obj%dom, ans=nodenum, tsize=nrow)
  nsd = obj%dom%GetNSD()
  mesh => obj%dom%GetMeshPointer(dim=nsd)
  CALL mesh%GetLocalNodeNumber_(globalNode=nodenum, ans=nodenum, &
                                islocal=.FALSE.)
  mesh => NULL()

  ncol = obj%nrow

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(jj, 1)
  END DO
END SUBROUTINE GetTimeValue

!----------------------------------------------------------------------------
!                                                         GetSpaceTimeValue
!----------------------------------------------------------------------------

SUBROUTINE GetSpaceTimeValue(obj, fedof, nodeNum, nodalValue, nrow, ncol, &
                             times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
  !! Degree of freedom
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! Size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nrow = Size of nodeNum
  !! ncol = 1 or Size of times
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! the size of data written in nodalValue
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  !! times vector is only used when usefunction is true in obj

  !! internal variables
  CHARACTER(*), PARAMETER :: myName = 'GetSpaceTimeValue()'
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, jj, nsd
  CLASS(AbstractMesh_), POINTER :: mesh

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%nodalValue)
  CALL AssertError1(isok, myname, &
                    'AbstractBC_::obj%nodalValue is not allocated!')

#endif

  CALL obj%boundary%GetNodeNum(dom=obj%dom, ans=nodenum, tsize=nrow)
  nsd = obj%dom%GetNSD()
  mesh => obj%dom%GetMeshPointer(dim=nsd)
  CALL mesh%GetLocalNodeNumber_(globalNode=nodenum, ans=nodenum, &
                                islocal=.FALSE.)
  mesh => NULL()

#ifdef DEBUG_VER
  isok = obj%nrow .GE. nrow
  CALL AssertError1(isok, myname, &
                    'SIZE(obj%nodalValue, 1) .NE. SIZE(nodeNum)')
#endif

  ncol = obj%ncol

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(ii, jj)
  END DO
END SUBROUTINE GetSpaceTimeValue

!----------------------------------------------------------------------------
!                                                             checkerror_uf
!----------------------------------------------------------------------------

SUBROUTINE checkerror_uf(obj, myName, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: myName
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)

  ! internal variable
  LOGICAL(LGT) :: problem, no_times
  INTEGER(I4B) :: aint

  no_times = .NOT. PRESENT(times)

  problem = .NOT. ASSOCIATED(obj%func)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      "[INTERNAL ERROR] :: When nodalValueType is "// &
                      CHAR_LF//"Space and useFunction is specified, "// &
                      CHAR_LF//"then SpaceFunction is needed, "// &
                      CHAR_LF//"but it is not associated")
    RETURN
  END IF

  aint = obj%func%GetReturnType()

  problem = aint .NE. TypeFEVariableOpt%scalar
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
             '[INTERNAL ERROR] :: Return type of user function should be '// &
                      'scalar.')
    RETURN
  END IF

  problem = (obj%nodalValueType .EQ. TypeFEVariableOpt%time) .AND. no_times
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      "[INTERNAL ERROR] :: When `nodalValueType` is Time "// &
                      " and `IsUserFunction` is TRUE, "// &
                      " then `times` is needed in the passing argument,"// &
                      " but it is not present")
    RETURN
  END IF

  problem = (obj%nodalValueType .EQ. TypeFEVariableOpt%spacetime) .AND. &
            no_times
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                 "[INTERNAL ERROR] :: When `nodalValueType` is SpaceTime "// &
                      " and `IsUserFunction` is TRUE, "// &
                      " then `times` is needed in the passing argument,"// &
                      " but it is not present")
    RETURN
  END IF

  aint = obj%func%GetArgType()

  problem = aint .NE. obj%nodalValueType
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: argType='//tostring(aint)// &
                      ' in user function is not same '// &
                      'as nodalValueType '//tostring(obj%nodalValueType)// &
                      ' in AbstractBC_')
    RETURN
  END IF

END SUBROUTINE checkerror_uf

!----------------------------------------------------------------------------
!                                                     GetFromUserFunction
!----------------------------------------------------------------------------

SUBROUTINE GetFromUserFunction(obj, nodeNum, nodalValue, nrow, ncol, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: nodeNum(:)
  !! node number should be allocated
  !! size of nodenum is nrow
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nodal values
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  !! time value
  !! time values are needed when userfunction is time or space-time
  INTEGER(I4B) :: nrow, ncol
  !! nrow is size of nodenum and size of number of rows in nodalvalue
  !! ncol is colsize of nodalvalue

  ! internal varibles
  CHARACTER(*), PARAMETER :: myName = "obj_GetFromUserFunction()"
  INTEGER(I4B) :: ii, kk, retType, tnodes, nsd, tTimes, argType, &
                  tsize
  REAL(DFP) :: xij(4, 1), ans
  LOGICAL(LGT) :: problem
  CLASS(AbstractMesh_), POINTER :: meshptr
  LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.

! get pointer to nodecoord

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL checkerror_uf(obj, myName, times)
#endif

  retType = obj%func%GetReturnType()
  argType = obj%func%GetArgType()

  tnodes = SIZE(nodeNum)
  nrow = tnodes
  ncol = 0
  nsd = obj%dom%GetNSD()

  SELECT CASE (obj%nodalValueType)

! Constant
  CASE (TypeFEVariableOpt%constant)
    ncol = 1
    CALL obj%func%Get(val=ans)
    nodalValue(1:nrow, 1) = ans

! Space
  CASE (TypeFEVariableOpt%space)
    ncol = 1
    meshptr => obj%dom%GetMeshPointer(dim=nsd)

    DO ii = 1, tnodes
      CALL meshptr%GetNodeCoord(nodeCoord=xij(:, 1), tsize=tsize, &
                                globalNode=nodeNum(ii), islocal=yes)

      CALL obj%func%Get(val=ans, args=xij(1:3, 1))

      nodalValue(ii, 1) = ans
    END DO

    meshptr => NULL()

! Time
  CASE (TypeFEVariableOpt%time)

    tTimes = SIZE(times)

    ncol = tTimes

    DO ii = 1, tTimes
      CALL obj%func%Get(val=ans, args=times(ii:ii))

      nodalValue(1:nrow, ii) = ans
    END DO

! SpaceTime
  CASE (TypeFEVariableOpt%spacetime)
    tTimes = SIZE(times)

    ncol = tTimes

    meshptr => obj%dom%GetMeshPointer(dim=nsd)

    DO kk = 1, tTimes
      xij(nsd + 1, 1) = times(kk)

      DO ii = 1, tnodes
        CALL meshptr%GetNodeCoord(nodeCoord=xij(:, 1), tsize=tsize, &
                                  globalNode=nodeNum(ii), islocal=yes)

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

END SUBROUTINE GetFromUserFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetH1LagrangeMethods
