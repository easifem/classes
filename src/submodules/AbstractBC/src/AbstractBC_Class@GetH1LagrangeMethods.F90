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

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetTotalNodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodeNumH1Lagrange
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodeNumH1Lagrange()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, localFaceNumber, localEdgeNumber, localCellNumber, &
                mysize
LOGICAL(LGT), PARAMETER :: yes = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%dom)
CALL AssertError1(isok, myName, &
                  'AbstractBC_::obj%dom is not associated!')
#endif

ans = obj%boundary%GetTotalNodeNum(dom=obj%dom)
CALL obj%SetElemToLocalBoundary()

DO ii = 1, obj%tElemToFace
  CALL obj%GetElemToFace(indx=ii, localCellNumber=localCellNumber, &
                         localFaceNumber=localFaceNumber)
  mysize = fedof%GetTotalFaceDOF(globalElement=localCellNumber, &
                                 localFaceNumber=localFaceNumber, islocal=yes)
  ans = ans + mysize
END DO

DO ii = 1, obj%tElemToEdge
  CALL obj%GetElemToEdge(indx=ii, localCellNumber=localCellNumber, &
                         localEdgeNumber=localEdgeNumber)

  mysize = fedof%GetTotalEdgeDOF(globalElement=localCellNumber, &
                                 localEdgeNumber=localEdgeNumber, islocal=yes)

  ans = ans + mysize
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalNodeNumH1Lagrange

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetH1Lagrange1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetH1Lagrange1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL CheckError(obj, myName)
#endif

#ifdef DEBUG_VER
IF (obj%isUserFunction) CALL CheckError_uf(obj, myName, times)
#endif

! If obj%isUserFunction is true, then call GetSpaceValue_uf

! get nodal values
SELECT CASE (obj%nodalValueType)

CASE (TypeFEVariableOpt%constant)

  IF (obj%isUserFunction) THEN
    CALL GetConstantValue_uf(obj=obj, fedof=fedof, nodeNum=nodeNum, &
                             nodalValue=nodalValue, nrow=nrow, ncol=ncol, &
                             times=times)

  ELSE
    CALL GetConstantValue(obj=obj, fedof=fedof, nodeNum=nodeNum, &
                          nodalValue=nodalValue, nrow=nrow, ncol=ncol, &
                          times=times)
  END IF

CASE (TypeFEVariableOpt%space)

  IF (obj%isUserFunction) THEN
    CALL GetSpaceValue_uf(obj=obj, fedof=fedof, nodeNum=nodeNum, &
                          nodalValue=nodalValue, nrow=nrow, ncol=ncol, &
                          times=times)

  ELSE
    CALL GetSpaceValue(obj=obj, fedof=fedof, nodeNum=nodeNum, &
                       nodalValue=nodalValue, nrow=nrow, ncol=ncol, &
                       times=times)
  END IF

CASE (TypeFEVariableOpt%time)

  IF (obj%isUserFunction) THEN
    CALL GetTimeValue_uf(obj=obj, fedof=fedof, nodeNum=nodeNum, &
                         nodalValue=nodalValue, nrow=nrow, ncol=ncol, &
                         times=times)

  ELSE
    CALL GetTimeValue(obj=obj, fedof=fedof, nodeNum=nodeNum, &
                      nodalValue=nodalValue, nrow=nrow, ncol=ncol)

  END IF

CASE (TypeFEVariableOpt%spacetime)

  IF (obj%isUserFunction) THEN
    CALL GetSpaceTimeValue_uf(obj=obj, fedof=fedof, nodeNum=nodeNum, &
                              nodalValue=nodalValue, nrow=nrow, ncol=ncol, &
                              times=times)

  ELSE
    CALL GetSpaceTimeValue(obj=obj, fedof=fedof, nodeNum=nodeNum, &
                           nodalValue=nodalValue, nrow=nrow, ncol=ncol)
  END IF

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
          'No case found for nodalValueType = '//ToString(obj%nodalValueType))
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_GetH1Lagrange1

!----------------------------------------------------------------------------
!                                                             GetH1Lagrange
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetH1Lagrange2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetH1Lagrange2()"
#endif

INTEGER(I4B) :: iNodeOnNode, iNodeOnFace, iNodeOnEdge

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL CheckError(obj, myName)
#endif

CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodenum, nrow=tsize, &
                iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
                iNodeOnEdge=iNodeOnEdge)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetH1Lagrange2

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
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetNodeNum()"
#endif

  INTEGER(I4B) :: mysize, nsd, localCellNumber, localFaceNumber, ii, &
                  localEdgeNumber
  CLASS(AbstractMesh_), POINTER :: mesh
  LOGICAL(LGT) :: isok
  LOGICAL(LGT), PARAMETER :: no = .FALSE., yes = .TRUE.

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = ASSOCIATED(obj%dom)
  CALL AssertError1(isok, myName, &
                    'AbstractBC_::obj%dom is not associated!')
#endif

  nrow = 0

  ! info: Lets get vertex node, if there are any
  CALL obj%boundary%GetNodeNum(dom=obj%dom, ans=nodenum, tsize=mysize)
  nrow = nrow + mysize
  iNodeOnNode = 1
  iNodeOnFace = nrow + 1

  nsd = obj%dom%GetNSD()
  mesh => obj%dom%GetMeshPointer(dim=nsd)
  isok = nrow .NE. 0
  IF (isok) CALL mesh%GetLocalNodeNumber_(globalNode=nodenum(1:nrow), &
                                          ans=nodenum, islocal=no)

  CALL obj%SetElemToLocalBoundary()
  ! info: Now we have elemToFace and elemToEdge ready
  ! - If tElemToFace is not zero then we call GetDOF from FEDOF to
  ! get DOF on the face
  ! - If tElemToEdge is not zero then we call GetDOF from FEDOF to get
  ! DOF on the edge

  DO ii = 1, obj%tElemToFace
    CALL obj%GetElemToFace(indx=ii, localFaceNumber=localFaceNumber, &
                           localCellNumber=localCellNumber)

    CALL fedof%GetFaceDOF( &
      globalElement=localCellNumber, localFaceNumber=localFaceNumber, &
      ans=nodenum(nrow + 1:), tsize=mysize, islocal=yes)

    nrow = nrow + mysize
  END DO

  iNodeOnEdge = nrow + 1
  DO ii = 1, obj%tElemToEdge
    CALL obj%GetElemToEdge(indx=ii, localEdgeNumber=localEdgeNumber, &
                           localCellNumber=localCellNumber)

    CALL fedof%GetEdgeDOF( &
      globalElement=localCellNumber, localEdgeNumber=localEdgeNumber, &
      ans=nodenum(nrow + 1:), tsize=mysize, islocal=yes)

    nrow = nrow + mysize
  END DO

  mesh => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

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
  LOGICAL(LGT) :: isok
#endif

  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%nodalValue)
  CALL AssertError1(isok, myname, &
                    'AbstractBC_::obj%nodalValue is not allocated!')
#endif

  nrow = 0
  CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodeNum, nrow=nrow, &
                  iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
                  iNodeOnEdge=iNodeOnEdge)

  ncol = 1
  IF (PRESENT(times)) ncol = SIZE(times)

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(1, 1)
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
                  iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
                  iNodeOnEdge=iNodeOnEdge)

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    nodalValue(ii, jj) = ans
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
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetSpaceValue()"
#endif
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%nodalValue)
  CALL AssertError1(isok, myname, &
                    'AbstractBC_::obj%nodalValue is not allocated!')
#endif

  CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodeNum, nrow=nrow, &
                  iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
                  iNodeOnEdge=iNodeOnEdge)

#ifdef DEBUG_VER
  isok = obj%nrow .GE. nrow
  CALL AssertError1(isok, myname, &
                    'SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
#endif

  ncol = 1
  IF (PRESENT(times)) ncol = SIZE(times)

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(ii, 1)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

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

  !! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetSpaceValue_uf()"
#endif

  REAL(DFP) :: ans, xij(4)
  INTEGER(I4B) :: ii, jj, nsd, tsize, iNodeOnNode, iNodeOnFace, iNodeOnEdge, &
                  nargs
  CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nrow = 0
  CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodeNum, nrow=nrow, &
                  iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
                  iNodeOnEdge=iNodeOnEdge)

  nsd = obj%dom%GetNSD()
  meshptr => obj%dom%GetMeshPointer(dim=nsd)

  nargs = obj%func%GetNumArgs()
  xij = 0.0_DFP

  ncol = 1
  IF (PRESENT(times)) ncol = SIZE(times)

  DO ii = 1, nrow
    CALL meshptr%GetNodeCoord(nodeCoord=xij, tsize=tsize, &
                              globalNode=nodeNum(ii), islocal=.TRUE.)

    CALL obj%func%Get(val=ans, args=xij(1:nargs))

    DO jj = 1, ncol
      nodalValue(ii, jj) = ans
    END DO
  END DO

  meshptr => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetSpaceValue_uf

!----------------------------------------------------------------------------
!                                                             GetTimeValue
!----------------------------------------------------------------------------

SUBROUTINE GetTimeValue(obj, fedof, nodeNum, nodalValue, nrow, ncol)
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

  !! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myname = 'GetTimeValue()'
  LOGICAL(LGT) :: isok
#endif

  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%nodalValue)
  CALL AssertError1(isok, myname, &
                    'AbstractBC_::obj%nodalValue is not allocated!')
#endif

  CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodeNum, nrow=nrow, &
                  iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
                  iNodeOnEdge=iNodeOnEdge)

  ncol = obj%nrow

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(jj, 1)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

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

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetTimeValue_uf()"
#endif

  REAL(DFP) :: ans
  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nrow = 0
  CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodeNum, nrow=nrow, &
                  iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
                  iNodeOnEdge=iNodeOnEdge)

  ncol = 1
  IF (PRESENT(times)) ncol = SIZE(times)

  DO jj = 1, ncol
    CALL obj%func%Get(val=ans, args=times(jj:jj))
    DO ii = 1, nrow
      nodalValue(ii, jj) = ans
    END DO
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetTimeValue_uf

!----------------------------------------------------------------------------
!                                                         GetSpaceTimeValue
!----------------------------------------------------------------------------

SUBROUTINE GetSpaceTimeValue(obj, fedof, nodeNum, nodalValue, nrow, ncol)
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

  !! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'GetSpaceTimeValue()'
#endif

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%nodalValue)
  CALL AssertError1(isok, myname, &
                    'AbstractBC_::obj%nodalValue is not allocated!')

#endif

  CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodeNum, nrow=nrow, &
                  iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
                  iNodeOnEdge=iNodeOnEdge)

#ifdef DEBUG_VER
  isok = obj%nrow .GE. nrow
  CALL AssertError1(isok, myname, &
                    'SIZE(obj%nodalValue, 1) .NE. SIZE(nodeNum)')
#endif

  ncol = obj%ncol

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(ii, jj)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetSpaceTimeValue

!----------------------------------------------------------------------------
!                                                       GetSpaceTimeValue_uf
!----------------------------------------------------------------------------

SUBROUTINE GetSpaceTimeValue_uf(obj, fedof, nodeNum, nodalValue, nrow, ncol, &
                                times)
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

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetSpaceTimeValue_uf()"
#endif

  INTEGER(I4B) :: ii, jj, nsd, tsize, iNodeOnNode, iNodeOnFace, iNodeOnEdge, &
                  nargs
  REAL(DFP) :: xij(4), ans
  CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetNodeNum(obj=obj, fedof=fedof, nodenum=nodeNum, nrow=nrow, &
                  iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
                  iNodeOnEdge=iNodeOnEdge)

  nargs = obj%func%GetNumArgs()
  ncol = SIZE(times)
  nsd = obj%dom%GetNSD()
  meshptr => obj%dom%GetMeshPointer(dim=nsd)

  DO jj = 1, ncol
    xij(nargs) = times(jj)

    DO ii = 1, nrow
      CALL meshptr%GetNodeCoord(nodeCoord=xij, tsize=tsize, &
                                globalNode=nodeNum(ii), islocal=.TRUE.)

      CALL obj%func%Get(val=ans, args=xij(1:nargs))

      nodalValue(ii, jj) = ans
    END DO

  END DO

  meshptr => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetSpaceTimeValue_uf

!----------------------------------------------------------------------------
!                                                             CheckError
!----------------------------------------------------------------------------

SUBROUTINE CheckError(obj, myName)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: myName

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName0 = "CheckError()"
  LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName0//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER

  CALL AssertError1(obj%isInit, myName, &
                    'AbstractBC_ object is not initiated, initiate it first.')

  isok = ASSOCIATED(obj%dom)
  CALL AssertError1(isok, myName, &
                    'AbstractBC_::obj%dom is not associated!')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName0//' - '// &
                          '[END] ')
#endif
END SUBROUTINE CheckError

!----------------------------------------------------------------------------
!                                                             CheckError_uf
!----------------------------------------------------------------------------

SUBROUTINE CheckError_uf(obj, myName, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: myName
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)

  ! internal variable
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName0 = "CheckError_uf()"
  LOGICAL(LGT) :: no_times, isok
  INTEGER(I4B) :: aint
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName0//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = ASSOCIATED(obj%func)
  CALL AssertError1(isok, myName, &
       "When nodalValueType is Space and useFunction is specified, then &
       &SpaceFunction is needed, but it is not associated")
#endif

#ifdef DEBUG_VER
  aint = obj%func%GetReturnType()
  isok = aint .EQ. TypeFEVariableOpt%scalar
  CALL AssertError1(isok, myName, &
                    'Return type of user function should be scalar.')
#endif

#ifdef DEBUG_VER
  isok = .TRUE.; no_times = .NOT. PRESENT(times)
  IF (no_times) isok = (obj%nodalValueType .NE. TypeFEVariableOpt%time)
  CALL AssertError1(isok, myName, &
       'When `nodalValueType` is Time and `IsUserFunction` is TRUE, &
       &then `times` is needed in the passing argument, but it is not &
       &present')
#endif

#ifdef DEBUG_VER
  isok = .TRUE.
  IF (no_times) isok = (obj%nodalValueType .NE. TypeFEVariableOpt%spaceTime)
  CALL AssertError1(isok, myName, &
       'When `nodalValueType` is SpaceTime and `IsUserFunction` is TRUE, &
       &then `times` is needed in the passing argument but it is not present')
#endif

#ifdef DEBUG_VER
  aint = obj%func%GetArgType()
  isok = aint .EQ. obj%nodalValueType
  CALL AssertError1(isok, myName, &
       'argType='//ToString(aint)//' in user function is not same &
       &as nodalValueType'//ToString(obj%nodalValueType)//' in AbstractBC_')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName0//' - '// &
                          '[END] ')
#endif
END SUBROUTINE CheckError_uf

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetH1LagrangeMethods
