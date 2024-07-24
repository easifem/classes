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

SUBMODULE(AbstractBC_Class) GetH1HierarchicalMethods
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

MODULE PROCEDURE obj_Get_H1_Hierarchical1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get_H1_Hierarchical1()"
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

IF (obj%isUserFunction) THEN
  CALL GetFromUserFunction(obj=obj, nodeNum=nodeNum, nodalValue=nodalValue, &
                           times=times, nrow=nrow, ncol=ncol)
  RETURN
END IF

SELECT CASE (obj%nodalValueType)

CASE (TypeFEVariableOpt%constant)
  CALL GetConstantValue(obj=obj, fedof=fedof, nodenum=nodenum, &
                     nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, times=times)

CASE (TypeFEVariableOpt%space)

  CALL GetSpaceValue(obj=obj, fedof=fedof, nodenum=nodenum, &
                     nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, times=times)

! Time
CASE (TypeFEVariableOpt%time)

  CALL GetTimeValue(obj=obj, fedof=fedof, nodenum=nodenum, &
                    nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, times=times)

CASE (TypeFEVariableOpt%spacetime)

  CALL GetSpaceTimeValue(obj=obj, fedof=fedof, nodenum=nodenum, &
                     nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, times=times)

CASE DEFAULT
  CALL AssertError1(.FALSE., myname, 'No case found for nodalValueType')

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Get_H1_Hierarchical1

!----------------------------------------------------------------------------
!                                                                GetNodeNum
!----------------------------------------------------------------------------

SUBROUTINE GetNodeNum(obj, fedof, nodenum, nrow, iNodeOnNode, iNodeOnFace, &
                      iNodeOnEdge)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
  !! Degree of freedom
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! Size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
  INTEGER(I4B), INTENT(OUT) :: nrow
  !! the size of data written in nodalValue
  INTEGER(I4B), INTENT(OUT) :: iNodeOnNode
  !! starting point of nodes on nodes
  INTEGER(I4B), INTENT(OUT) :: iNodeOnFace
  !! starting point of nodes on face
  INTEGER(I4B), INTENT(OUT) :: iNodeOnEdge
  !! starting point of nodes on edge

  !! internal variables
  INTEGER(I4B) :: mysize, nsd, localElement, localboundary, ii
  CLASS(AbstractMesh_), POINTER :: mesh

  nrow = 0

  !! INFO: Lets get vertex node, if there are any
  CALL obj%boundary%GetNodeNum(ans=nodenum, tsize=mysize)
  nrow = nrow + mysize
  iNodeOnNode = 1
  iNodeOnFace = nrow + 1

  nsd = obj%dom%GetNSD()
  mesh => obj%dom%GetMeshPointer(dim=nsd)
  IF (nrow .NE. 0) THEN
    CALL mesh%GetLocalNodeNumber_(globalNode=nodenum(1:nrow), ans=nodenum, &
                                  islocal=.FALSE.)
  END IF

  CALL obj%SetElemToLocalBoundary()

  !! INFO: Now we have elemToFace and elemToEdge ready
  !! - If tElemToFace is not zero then we call GetDOF from FEDOF to
  !! get DOF on the face
  !! - If tElemToEdge is not zero then we call GetDOF from FEDOF to get
  !! DOF on the edge
  DO ii = 1, obj%tElemToFace
    localElement = obj%elemToFace(1, ii)
    localboundary = obj%elemToFace(2, ii)
    CALL fedof%GetFaceDOF(globalElement=localElement, &
        localFaceNumber=localboundary, ans=nodenum(nrow + 1:), tsize=mysize, &
                          islocal=.TRUE.)
    nrow = nrow + mysize
  END DO

  iNodeOnEdge = nrow + 1

  DO ii = 1, obj%tElemToEdge
    localElement = obj%elemToEdge(1, ii)
    localboundary = obj%elemToEdge(2, ii)
    CALL fedof%GetEdgeDOF(globalElement=localElement, &
        localEdgeNumber=localboundary, ans=nodenum(nrow + 1:), tsize=mysize, &
                          islocal=.TRUE.)
    nrow = nrow + mysize
  END DO

  mesh => NULL()

END SUBROUTINE GetNodeNum

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
  INTEGER(I4B) :: mysize, nsd, localElement, localboundary, ii, jj, kk
  CLASS(AbstractMesh_), POINTER :: mesh

  nrow = 0; ncol = 1

  IF (PRESENT(times)) ncol = SIZE(times)

  CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodenum, nrow=nrow, &
    iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, iNodeOnEdge=iNodeOnEdge)

  !! INFO: Lets get vertex node, if there are any
  CALL obj%boundary%GetNodeNum(ans=nodenum, tsize=mysize, dom=obj%dom)
  nrow = nrow + mysize

  IF (nrow .NE. 0) THEN
    nsd = obj%dom%GetNSD()
    mesh => obj%dom%GetMeshPointer(dim=nsd)
    CALL mesh%GetLocalNodeNumber_(globalNode=nodenum(1:nrow), ans=nodenum, &
                                  islocal=.FALSE.)

    mesh => NULL()
  END IF

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(1, 1)
  END DO

  CALL obj%SetElemToLocalBoundary()

  !! INFO: Now we have elemToFace and elemToEdge ready
  !! - If tElemToFace is not zero then we call GetDOF from FEDOF to
  !! get DOF on the face
  !! - If tElemToEdge is not zero then we call GetDOF from FEDOF to get
  !! DOF on the edge
  !! In the case of constant boundary condition,
  !! we will assign zero value to the face and edge degree of freedom
  DO ii = 1, obj%tElemToFace
    localElement = obj%elemToFace(1, ii)
    localboundary = obj%elemToFace(2, ii)
    CALL fedof%GetFaceDOF(globalElement=localElement, &
        localFaceNumber=localboundary, ans=nodenum(nrow + 1:), tsize=mysize, &
                          islocal=.TRUE.)

    DO CONCURRENT(kk=1:mysize, jj=1:ncol)
      nodalValue(nrow + kk, jj) = 0.0_DFP
    END DO

    nrow = nrow + mysize
  END DO

  DO ii = 1, obj%tElemToEdge
    localElement = obj%elemToEdge(1, ii)
    localboundary = obj%elemToEdge(2, ii)
    CALL fedof%GetEdgeDOF(globalElement=localElement, &
        localEdgeNumber=localboundary, ans=nodenum(nrow + 1:), tsize=mysize, &
                          islocal=.TRUE.)

    DO CONCURRENT(kk=1:mysize, jj=1:ncol)
      nodalValue(nrow + kk, jj) = 0.0_DFP
    END DO

    nrow = nrow + mysize
  END DO

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
  CHARACTER(*), PARAMETER :: myname = "GetSpaceValue()"
  INTEGER(I4B) :: mysize, nsd, localElement, localboundary, ii, jj, kk
  CLASS(AbstractMesh_), POINTER :: mesh

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')

  nrow = 0
  ncol = 1
  IF (PRESENT(times)) ncol = SIZE(times)

  !! INFO: Lets get vertex node, if there are any
  CALL obj%boundary%GetNodeNum(ans=nodenum, tsize=mysize, dom=obj%dom)
  nrow = nrow + mysize

  IF (nrow .NE. 0) THEN
    nsd = obj%dom%GetNSD()
    mesh => obj%dom%GetMeshPointer(dim=nsd)
    CALL mesh%GetLocalNodeNumber_(globalNode=nodenum(1:nrow), ans=nodenum, &
                                  islocal=.FALSE.)

    mesh => NULL()
  END IF

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(1, 1)
  END DO

  CALL obj%SetElemToLocalBoundary()

  !! INFO: Now we have elemToFace and elemToEdge ready
  !! - If tElemToFace is not zero then we call GetDOF from FEDOF to
  !! get DOF on the face
  !! - If tElemToEdge is not zero then we call GetDOF from FEDOF to get
  !! DOF on the edge
  !! In the case of constant boundary condition,
  !! we will assign zero value to the face and edge degree of freedom
  DO ii = 1, obj%tElemToFace
    localElement = obj%elemToFace(1, ii)
    localboundary = obj%elemToFace(2, ii)
    CALL fedof%GetFaceDOF(globalElement=localElement, &
        localFaceNumber=localboundary, ans=nodenum(nrow + 1:), tsize=mysize, &
                          islocal=.TRUE.)

    DO CONCURRENT(kk=1:mysize, jj=1:ncol)
      nodalValue(nrow + kk, jj) = obj%nodalValue(1, 1)
    END DO

    nrow = nrow + mysize
  END DO

  DO ii = 1, obj%tElemToEdge
    localElement = obj%elemToEdge(1, ii)
    localboundary = obj%elemToEdge(2, ii)
    CALL fedof%GetEdgeDOF(globalElement=localElement, &
        localEdgeNumber=localboundary, ans=nodenum(nrow + 1:), tsize=mysize, &
                          islocal=.TRUE.)

    DO CONCURRENT(kk=1:mysize, jj=1:ncol)
      nodalValue(nrow + kk, jj) = obj%nodalValue(1, 1)
    END DO

    nrow = nrow + mysize
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
END SUBROUTINE GetSpaceTimeValue

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

END SUBROUTINE GetFromUserFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetH1HierarchicalMethods
