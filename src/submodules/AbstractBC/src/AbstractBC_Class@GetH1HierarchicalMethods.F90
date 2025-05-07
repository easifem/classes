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
#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

USE Display_Method, ONLY: ToString

USE GlobalData, ONLY: CHAR_LF

USE AbstractMesh_Class, ONLY: AbstractMesh_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetTotalNodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_H1_Hierarchical_GetTotalNodenum
!! internal variables
INTEGER(I4B) :: mysize, localElement, localboundary, ii

ans = 0
mysize = obj%boundary%GetTotalNodeNum(dom=obj%dom)
ans = ans + mysize

CALL obj%SetElemToLocalBoundary()

DO ii = 1, obj%tElemToFace
  localElement = obj%elemToFace(1, ii)
  localboundary = obj%elemToFace(2, ii)
  mysize = fedof%GetTotalFaceDOF(globalElement=localElement, &
                                localFaceNumber=localboundary, islocal=.TRUE.)
  ans = ans + mysize
END DO

DO ii = 1, obj%tElemToEdge
  localElement = obj%elemToEdge(1, ii)
  localboundary = obj%elemToEdge(2, ii)
  mysize = fedof%GetTotalEdgeDOF(globalElement=localElement, &
                                localEdgeNumber=localboundary, islocal=.TRUE.)
  ans = ans + mysize
END DO

END PROCEDURE obj_H1_Hierarchical_GetTotalNodenum

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

INTEGER(I4B) :: ii, jj, nsd
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL checkerror(obj, myName)

IF (obj%isUserFunction) THEN
  CALL checkerror_uf(obj, myName, times)
END IF
#endif

SELECT CASE (obj%nodalValueType)

CASE (TypeFEVariableOpt%constant)

  IF (obj%isUserFunction) THEN
    CALL GetConstantValue_uf(obj=obj, fedof=fedof, nodenum=nodenum, &
                     nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, times=times)
  ELSE
    CALL GetConstantValue(obj=obj, fedof=fedof, nodenum=nodenum, &
                     nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, times=times)
  END IF

CASE (TypeFEVariableOpt%space)

  IF (obj%isUserFunction) THEN
    CALL GetSpaceValue_uf(obj=obj, fedof=fedof, nodenum=nodenum, &
                     nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, times=times)

  ELSE
    CALL GetSpaceValue(obj=obj, fedof=fedof, nodenum=nodenum, &
                     nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, times=times)
  END IF

! Time
CASE (TypeFEVariableOpt%time)

  IF (obj%isUserFunction) THEN
    CALL GetTimeValue_uf(obj=obj, fedof=fedof, nodenum=nodenum, &
                     nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, times=times)
  ELSE
    CALL GetTimeValue(obj=obj, fedof=fedof, nodenum=nodenum, &
                     nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, times=times)
  END IF

CASE (TypeFEVariableOpt%spacetime)

  IF (obj%isUserFunction) THEN
    CALL GetSpaceTimeValue_uf(obj=obj, fedof=fedof, nodenum=nodenum, &
                     nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, times=times)
  ELSE
    CALL GetSpaceTimeValue(obj=obj, fedof=fedof, nodenum=nodenum, &
                     nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, times=times)
  END IF

CASE DEFAULT
  CALL AssertError1(.FALSE., myname, 'No case found for nodalValueType')

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Get_H1_Hierarchical1

!----------------------------------------------------------------------------
!                                                         GetH1Hierarchical
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get_H1_Hierarchical2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get_H1_Hierarchical2()"
#endif

INTEGER(I4B) :: iNodeOnEdge, iNodeOnFace, iNodeOnNode

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL checkerror(obj, myName)
#endif

CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodenum, nrow=tsize, &
    iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, iNodeOnEdge=iNodeOnEdge)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get_H1_Hierarchical2

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
  !! CALL obj%boundary%GetNodeNum(ans=nodenum, tsize=mysize)
  CALL obj%boundary%GetNodeNum(dom=obj%dom, ans=nodenum, tsize=mysize)
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
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetConstantValue()"
#endif

  INTEGER(I4B) :: ii, jj, kk, iNodeOnNode, iNodeOnFace, iNodeOnEdge

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nrow = 0; ncol = 1
  IF (PRESENT(times)) ncol = SIZE(times)

  CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodenum, nrow=nrow, &
    iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, iNodeOnEdge=iNodeOnEdge)

  DO CONCURRENT(ii=1:iNodeOnFace - 1, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(1, 1)
  END DO

  DO CONCURRENT(ii=iNodeOnFace:nrow, jj=1:ncol)
    nodalValue(ii, jj) = 0.0_DFP
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE GetConstantValue

!----------------------------------------------------------------------------
!                                                        GetConsantValue_uf
!----------------------------------------------------------------------------

SUBROUTINE GetConstantValue_uf(obj, fedof, nodeNum, nodalValue, nrow, ncol, &
                               times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Abstract boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
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

  !! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetConstantValue_uf()"
#endif

  REAL(DFP) :: ans
  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL obj%func%Get(val=ans)

  nrow = 0; ncol = 1
  IF (PRESENT(times)) ncol = SIZE(times)

  CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodenum, nrow=nrow, &
    iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, iNodeOnEdge=iNodeOnEdge)

  DO CONCURRENT(ii=1:iNodeOnFace - 1, jj=1:ncol)
    nodalValue(ii, jj) = ans
  END DO

  DO CONCURRENT(ii=iNodeOnFace:nrow, jj=1:ncol)
    nodalValue(ii, jj) = 0.0_DFP
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE GetConstantValue_uf

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
  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge

  nrow = 0; ncol = 1

  IF (PRESENT(times)) ncol = SIZE(times)

  CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodenum, nrow=nrow, &
    iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, iNodeOnEdge=iNodeOnEdge)

  DO CONCURRENT(ii=1:iNodeOnFace - 1, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(ii, 1)
  END DO

  DO CONCURRENT(ii=iNodeOnFace:nrow, jj=1:ncol)
    nodalValue(ii, jj) = 0.0_DFP
  END DO

END SUBROUTINE GetSpaceValue

!----------------------------------------------------------------------------
!                                                          GetSpaceValue_uf
!----------------------------------------------------------------------------

SUBROUTINE GetSpaceValue_uf(obj, fedof, nodeNum, nodalValue, nrow, ncol, &
                            times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !!
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
  !! fedof
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
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

  CHARACTER(*), PARAMETER :: myName = "GetSpaceValue_uf()"

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')

END SUBROUTINE GetSpaceValue_uf

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
  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge

  nrow = 0
  ncol = obj%nrow

  CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodenum, nrow=nrow, &
    iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, iNodeOnEdge=iNodeOnEdge)

  DO CONCURRENT(ii=1:iNodeOnFace - 1, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(jj, 1)
  END DO

  DO CONCURRENT(ii=iNodeOnFace:nrow, jj=1:ncol)
    nodalValue(ii, jj) = 0.0_DFP
  END DO
END SUBROUTINE GetTimeValue

!----------------------------------------------------------------------------
!                                                           GetTimeValue_uf
!----------------------------------------------------------------------------

SUBROUTINE GetTimeValue_uf(obj, fedof, nodeNum, nodalValue, nrow, ncol, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! obj
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
  !! fedof
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
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

  !! internal variables
  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge
  REAL(DFP) :: ans

  nrow = 0
  ncol = obj%nrow

  CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodenum, nrow=nrow, &
    iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, iNodeOnEdge=iNodeOnEdge)

  DO jj = 1, ncol
    CALL obj%func%Get(val=ans, args=times(jj:jj))

    DO CONCURRENT(ii=1:iNodeOnFace - 1, jj=1:ncol)
      nodalValue(ii, jj) = ans
    END DO

    DO CONCURRENT(ii=iNodeOnFace:nrow)
      nodalValue(ii, jj) = 0.0_DFP
    END DO
  END DO

END SUBROUTINE GetTimeValue_uf

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
  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge

  nrow = 0
  ncol = obj%ncol

  CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodenum, nrow=nrow, &
    iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, iNodeOnEdge=iNodeOnEdge)

  DO CONCURRENT(ii=1:iNodeOnFace - 1, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(ii, jj)
  END DO

  DO CONCURRENT(ii=iNodeOnFace:nrow, jj=1:ncol)
    nodalValue(ii, jj) = 0.0_DFP
  END DO
END SUBROUTINE GetSpaceTimeValue

!----------------------------------------------------------------------------
!                                                       GetSpaceTimeValue_uf
!----------------------------------------------------------------------------

SUBROUTINE GetSpaceTimeValue_uf(obj, fedof, nodeNum, nodalValue, nrow, ncol, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! obj
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
  !! fedof
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
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

  CHARACTER(*), PARAMETER :: myName = "GetSpaceTimeValue_uf()"
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')
END SUBROUTINE GetSpaceTimeValue_uf

!----------------------------------------------------------------------------
!                                                             CheckError_UF
!----------------------------------------------------------------------------

SUBROUTINE checkerror_uf(obj, myName, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: myName
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)

  ! internal variable
  LOGICAL(LGT) :: problem, isok
  INTEGER(I4B) :: aint
  CHARACTER(:), ALLOCATABLE :: msg

  isok = ASSOCIATED(obj%func)
  msg = " When nodalValueType is "//CHAR_LF// &
        "Space and useFunction is specified, "//CHAR_LF// &
        "then SpaceFunction is needed, "//CHAR_LF// &
        "but it is not associated"

  CALL AssertError1(isok, myName, msg)

  aint = obj%func%GetReturnType()

  isok = aint .EQ. TypeFEVariableOpt%scalar
  msg = "Return type of user function should be scalar."
  CALL AssertError1(isok, myName, msg)

  SELECT CASE (obj%nodalValueType)
  CASE (TypeFEVariableOpt%time, TypeFEVariableOpt%spacetime)
    isok = PRESENT(times)
    msg = "When `nodalValueType` is Time "//CHAR_LF// &
          " and `IsUserFunction` is TRUE, "//CHAR_LF// &
          " then `times` is needed in the passing argument,"// &
          " but it is not present"
    CALL AssertError1(isok, myName, msg)
  END SELECT

  aint = obj%func%GetArgType()
  isok = aint .EQ. obj%nodalValueType
  msg = "argType="//tostring(aint)// &
        " in user function is not same "// &
        "as nodalValueType "//tostring(obj%nodalValueType)// &
        " in AbstractBC_"
  CALL AssertError1(isok, myName, msg)

END SUBROUTINE checkerror_uf

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetH1HierarchicalMethods
