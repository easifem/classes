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
USE GlobalData, ONLY: CHAR_LF
USE Display_Method, ONLY: Display, ToString
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: QuadraturePoint_, ElemShapeData_
USE AbstractFE_Class, ONLY: AbstractFE_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             CheckError
!----------------------------------------------------------------------------

SUBROUTINE Checkerror(obj, myName)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: myName

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName0 = "checkerror()"
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
END SUBROUTINE Checkerror

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetH1Hierarchical
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetH1Hierarchical()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL Checkerror(obj, myName)

IF (obj%isUserFunction) THEN
  CALL Checkerror_uf(obj, myName, times)
END IF
#endif

SELECT CASE (obj%nodalValueType)

CASE (TypeFEVariableOpt%constant)

  IF (obj%isUserFunction) THEN
    CALL GetConstantValue_uf(obj=obj, fedof=fedof, nodenum=nodenum, &
                             nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, &
                             times=times)
  ELSE
    CALL GetConstantValue(obj=obj, fedof=fedof, nodenum=nodenum, &
                          nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, &
                          times=times)
  END IF

CASE (TypeFEVariableOpt%space)

  IF (obj%isUserFunction) THEN
    CALL GetSpaceValue_uf(obj=obj, fedof=fedof, nodenum=nodenum, &
                          nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, &
                          times=times)

  ELSE
    CALL GetSpaceValue(obj=obj, fedof=fedof, nodenum=nodenum, &
                       nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, &
                       times=times)
  END IF

! Time
CASE (TypeFEVariableOpt%time)

  IF (obj%isUserFunction) THEN
    CALL GetTimeValue_uf(obj=obj, fedof=fedof, nodenum=nodenum, &
                         nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, &
                         times=times)
  ELSE
    CALL GetTimeValue(obj=obj, fedof=fedof, nodenum=nodenum, &
                      nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, &
                      times=times)
  END IF

CASE (TypeFEVariableOpt%spaceTime)

  IF (obj%isUserFunction) THEN
    CALL GetSpaceTimeValue_uf(obj=obj, fedof=fedof, nodenum=nodenum, &
                              nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, &
                              times=times)
  ELSE
    CALL GetSpaceTimeValue(obj=obj, fedof=fedof, nodenum=nodenum, &
                           nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, &
                           times=times)
  END IF

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myname, &
             'No case found for nodalValueType'//ToString(obj%nodalValueType))
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_GetH1Hierarchical

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

  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nrow = 0; ncol = 1
  IF (PRESENT(times)) ncol = SIZE(times)

  CALL obj%Get(fedof=fedof, nodenum=nodenum, tsize=nrow, &
               iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
               iNodeOnEdge=iNodeOnEdge)

  DO CONCURRENT(ii=iNodeOnNode:iNodeOnFace - 1, jj=1:ncol)
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
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL obj%func%Get(val=ans)

  nrow = 0; ncol = 1
  isok = PRESENT(times)
  IF (isok) ncol = SIZE(times)

  CALL obj%Get(fedof=fedof, nodenum=nodenum, tsize=nrow, &
               iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
               iNodeOnEdge=iNodeOnEdge)

  DO CONCURRENT(ii=iNodeOnNode:iNodeOnFace - 1, jj=1:ncol)
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
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetSpaceValue()"
#endif

  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nrow = 0; ncol = 1
  isok = PRESENT(times)
  IF (isok) ncol = SIZE(times)

  CALL obj%Get(fedof=fedof, nodenum=nodenum, tsize=nrow, &
               iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
               iNodeOnEdge=iNodeOnEdge)

  DO CONCURRENT(ii=iNodeOnNode:iNodeOnFace - 1, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(ii, 1)
  END DO

  DO CONCURRENT(ii=iNodeOnFace:nrow, jj=1:ncol)
    nodalValue(ii, jj) = 0.0_DFP
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
  !! Abstract boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof
  !! fedof
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! node number should be allocated
  !! size of nodeNum is nrow
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nodal values
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  !! time value
  !! time values are needed when userfunction is time or space-time
  INTEGER(I4B) :: nrow, ncol
  !! nrow is size of nodeNum and size of number of rows in nodalvalue
  !! ncol is colsize of nodalvalue

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetSpaceValue_uf()"
#endif

  LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.

  INTEGER(I4B) :: iel, localFaceNumber, localCellNumber, mysize, nargs, &
                  iNodeOnNode, iNodeOnFace, iNodeOnEdge, ii, jj, indx(1), &
                  localEdgeNumber, elemCoord_i, elemCoord_j, ipiv(10)
  REAL(DFP) :: xij(4), ans, elemCoord(3, 8), massMat(10, 10), funcValue(50)

  LOGICAL(LGT) :: isok
  CLASS(AbstractMesh_), POINTER :: cellMesh
  CLASS(AbstractFE_), POINTER :: feptr
  TYPE(ElemShapeData_) :: elemsd, facetElemsd, geoElemsd, geoFacetElemsd
  TYPE(QuadraturePoint_) :: quad, facetQuad

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nrow = 0; ncol = 1
  isok = PRESENT(times); IF (isok) ncol = SIZE(times)

  ! Vertex nodes and values
  CALL obj%boundary%GetnodeNum(dom=obj%dom, ans=nodeNum, tsize=mysize)
  nrow = nrow + mysize
  iNodeOnNode = 1
  iNodeOnFace = nrow + 1

  DO ii = 1, nrow
    CALL fedof%GetVertexDOF(globalNode=nodeNum(ii), ans=indx, islocal=no, &
                            tsize=jj)
    nodeNum(ii) = indx(1)
  END DO

  cellMesh => obj%dom%GetMeshPointer()
  nargs = obj%func%GetNumArgs()

#ifdef DEBUG_VER
  isok = (nargs .LE. 4)
  CALL AssertError1(isok, myName, &
    "Number of arguments in user function should be less than or equal to 4.")
#endif

  xij = 0.0_DFP

  DO ii = 1, nrow
    CALL cellMesh%GetNodeCoord(nodeCoord=xij, tsize=jj, &
                               globalNode=nodeNum(ii), islocal=yes)

    CALL obj%func%Get(val=ans, args=xij(1:nargs))

    DO jj = 1, ncol
      nodalValue(ii, jj) = ans
    END DO
  END DO

  ! Set SetElemToLocalBoundary if it is not set
  CALL obj%SetElemToLocalBoundary()

  ! Face nodes and values
  DO iel = 1, obj%tElemToFace
    CALL obj%GetElemToFace(indx=iel, localFaceNumber=localFaceNumber, &
                           localCellNumber=localCellNumber)

    CALL fedof%GetFaceDOF( &
      globalElement=localCellNumber, localFaceNumber=localFaceNumber, &
      ans=nodeNum(nrow + 1:), tsize=mysize, islocal=yes)

    CALL fedof%SetFE(globalElement=localCellNumber, islocal=yes)
    feptr => fedof%GetFEPointer(globalElement=localCellNumber, islocal=yes)

    CALL feptr%GetFacetQuadraturePoints(quad=quad, facetQuad=facetQuad, &
                                        localFaceNumber=localFaceNumber)

    CALL feptr%GetLocalFacetElemShapeData( &
      elemsd=elemsd, facetElemsd=facetElemsd, quad=quad, &
      facetQuad=facetQuad, localFaceNumber=localFaceNumber)

    ! DEBUG: we need geoFacetElemsd
    CALL feptr%SetOrder(order=1)
    CALL feptr%GetLocalFacetElemShapeData( &
      elemsd=geoElemsd, facetElemsd=geoFacetElemsd, quad=quad, &
      facetQuad=facetQuad, localFaceNumber=localFaceNumber)

    CALL cellMesh%GetNodeCoord(nodeCoord=elemCoord, nrow=elemCoord_i, &
                               ncol=elemCoord_j, islocal=yes, &
                               globalElement=localCellNumber)

    CALL feptr%GetGlobalFacetElemShapeData( &
      elemsd=elemsd, facetElemsd=facetElemsd, &
      localFaceNumber=localFaceNumber, geoElemsd=geoElemsd, &
      geoFacetElemsd=geoFacetElemsd, xij=elemCoord)

    ! DEBUG: the below is needed because we changed feptr order
    CALL fedof%SetFE(globalElement=localCellNumber, islocal=yes)

    ! WIP:

    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'WIP')

    CALL feptr%GetFacetDOFValueFromUserFunction( &
      elemsd=elemsd, facetElemsd=facetElemsd, xij=elemCoord, &
      localFaceNumber=localFaceNumber, func=obj%func, ans=nodalValue(:, 1), &
      tsize=jj, massMat=massMat, ipiv=ipiv, funcValue=funcValue, &
      onlyFaceBubble=yes)

    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'start')

    CALL Display(nodalValue(1:jj, 1), msg="nodalValue: ")

    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'stop')
    STOP

    nrow = nrow + mysize
  END DO

  ! Edge nodes
  iNodeOnEdge = nrow + 1
  DO ii = 1, obj%tElemToEdge
    CALL obj%GetElemToEdge(indx=ii, localEdgeNumber=localEdgeNumber, &
                           localCellNumber=localCellNumber)

    CALL fedof%GetEdgeDOF( &
      globalElement=localCellNumber, localEdgeNumber=localEdgeNumber, &
      ans=nodenum(nrow + 1:), tsize=mysize, islocal=yes)

    nrow = nrow + mysize
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
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
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetTimeValue()"
#endif

  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nrow = 0
  ncol = obj%nrow

  CALL obj%Get(fedof=fedof, nodenum=nodenum, tsize=nrow, &
               iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
               iNodeOnEdge=iNodeOnEdge)

  DO CONCURRENT(ii=1:iNodeOnFace - 1, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(jj, 1)
  END DO

  DO CONCURRENT(ii=iNodeOnFace:nrow, jj=1:ncol)
    nodalValue(ii, jj) = 0.0_DFP
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

  !! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetTimeValue_uf()"
#endif

  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge
  REAL(DFP) :: ans

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nrow = 0
  ncol = obj%nrow

  CALL obj%Get(fedof=fedof, nodenum=nodenum, tsize=nrow, &
               iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
               iNodeOnEdge=iNodeOnEdge)

  DO jj = 1, ncol
    CALL obj%func%Get(val=ans, args=times(jj:jj))

    DO CONCURRENT(ii=1:iNodeOnFace - 1, jj=1:ncol)
      nodalValue(ii, jj) = ans
    END DO

    DO CONCURRENT(ii=iNodeOnFace:nrow)
      nodalValue(ii, jj) = 0.0_DFP
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
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetSpaceTimeValue()"
#endif

  INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nrow = 0
  ncol = obj%ncol

  CALL obj%Get(fedof=fedof, nodenum=nodenum, tsize=nrow, &
               iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
               iNodeOnEdge=iNodeOnEdge)

  DO CONCURRENT(ii=1:iNodeOnFace - 1, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(ii, jj)
  END DO

  DO CONCURRENT(ii=iNodeOnFace:nrow, jj=1:ncol)
    nodalValue(ii, jj) = 0.0_DFP
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

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetSpaceTimeValue_uf()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetSpaceTimeValue_uf

!----------------------------------------------------------------------------
!                                                             CheckError_UF
!----------------------------------------------------------------------------

SUBROUTINE checkerror_uf(obj, myName, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: myName
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)

  ! internal variable
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName0 = "checkerror_uf()"
  LOGICAL(LGT) :: isok, bool1
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
                    "Return type of user function should be scalar.")
#endif

#ifdef DEBUG_VER
  bool1 = obj%nodalValueType .EQ. TypeFEVariableOpt%time
  isok = .TRUE.
  IF (bool1) isok = PRESENT(times)
  CALL AssertError1(isok, myName, &
       "When `nodalValueType` is Time `IsUserFunction` is TRUE, then `times`&
       &is needed in the passing argument, but it is not present")
#endif

#ifdef DEBUG_VER
  bool1 = obj%nodalValueType .EQ. TypeFEVariableOpt%spaceTime
  isok = .TRUE.
  IF (bool1) isok = PRESENT(times)
  CALL AssertError1(isok, myName, &
       "When `nodalValueType` is spaceTime `IsUserFunction` is TRUE, then &
       &`times` is needed in the passing argument, but it is not present")
#endif

#ifdef DEBUG_VER
  aint = obj%func%GetArgType()
  isok = aint .EQ. obj%nodalValueType
  CALL AssertError1(isok, myName, &
       "argType="//ToString(aint)//" in user function is not same as &
       &nodalValueType "//ToString(obj%nodalValueType)//" in AbstractBC_")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName0//' - '// &
                          '[END] ')
#endif
END SUBROUTINE checkerror_uf

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetH1HierarchicalMethods
