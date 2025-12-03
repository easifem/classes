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

SUBMODULE(AbstractBC_Class) GetValueMethods
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: ToString, Display
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE BaseType, ONLY: ElemShapeData_, QuadraturePoint_
USE BaseType, ONLY: math => TypeMathOpt
USE ElemShapeData_Method, ONLY: ElemShapeData_Deallocate => DEALLOCATE
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Deallocate => DEALLOCATE
USE InputUtility, ONLY: Input

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                              GetNodeNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeNumber()"
#endif

INTEGER(I4B) :: mysize, localCellNumber, localFaceNumber, ii, &
                localEdgeNumber, jj, indx(1)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%dom)
CALL AssertError1(isok, myName, &
                  'AbstractBC_::obj%dom is not associated!')
#endif

! Vertex nodes
tsize = 0
CALL obj%boundary%GetNodeNum(dom=obj%dom, ans=nodenum, tsize=mysize)
tsize = tsize + mysize

iNodeOnNode = 1
iNodeOnFace = tsize + 1

DO ii = 1, tsize
  CALL fedof%GetVertexDOF(globalNode=nodenum(ii), ans=indx, islocal=math%no, &
                          tsize=jj)
  nodenum(ii) = indx(1)
END DO

CALL obj%SetElemToLocalBoundary()

! Face nodes
DO ii = 1, obj%tElemToFace
  CALL obj%GetElemToFace(indx=ii, localFaceNumber=localFaceNumber, &
                         localCellNumber=localCellNumber)

  CALL fedof%GetFaceDOF( &
    globalElement=localCellNumber, localFaceNumber=localFaceNumber, &
    ans=nodenum(tsize + 1:), tsize=mysize, islocal=math%yes)

  tsize = tsize + mysize
END DO

! Edge nodes
iNodeOnEdge = tsize + 1
DO ii = 1, obj%tElemToEdge
  CALL obj%GetElemToEdge(indx=ii, localEdgeNumber=localEdgeNumber, &
                         localCellNumber=localCellNumber)

  CALL fedof%GetEdgeDOF( &
    globalElement=localCellNumber, localEdgeNumber=localEdgeNumber, &
    ans=nodenum(tsize + 1:), tsize=mysize, islocal=math%yes)

  tsize = tsize + mysize
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeNumber

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
isok = obj%isInit
CALL AssertError1(isok, myName, &
                  'AbstractBC_ object is not initiated, initiate it first.')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%dom)
CALL AssertError1(isok, myName, &
                  'AbstractBC_::obj%dom is not associated!')
#endif

IF (obj%isUserFunction) THEN

#ifdef DEBUG_VER
  CALL Get1CheckErrorUserFunction(obj=obj, myName=myName, times=times)
#endif

  CALL Get1WithUserFunction( &
    obj=obj, fedof=fedof, geofedof=geofedof, nodenum=nodenum, &
    nodalValue=nodalValue, nrow=nrow, ncol=ncol, times=times)

ELSE

  CALL Get1WithoutUserFunction( &
    obj=obj, fedof=fedof, geofedof=geofedof, nodenum=nodenum, &
    nodalValue=nodalValue, nrow=nrow, ncol=ncol)

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                        Get1WithUserFunction
!----------------------------------------------------------------------------

SUBROUTINE Get1WithUserFunction(obj, fedof, geofedof, nodeNum, nodalValue, &
                                nrow, ncol, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Abstract boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
  !! fedof
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! node number should be allocated
  !! size of nodeNum is nrow
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nodal values
  INTEGER(I4B) :: nrow, ncol
  !! nrow is size of nodeNum and size of number of rows in nodalvalue
  !! ncol is colsize of nodalvalue
  REAL(DFP), OPTIONAL, INTENT(IN) :: times
  !! time value
  !! time values are needed when userfunction is time or space-time

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Get1WithUserFunction()"
#endif

  INTEGER(I4B) :: iel, localFaceNumber, localCellNumber, mysize, &
                  iNodeOnNode, iNodeOnFace, iNodeOnEdge, ii, jj, indx(1), &
                  localEdgeNumber, xij_i, xij_j, max_fedof_con, &
                  max_fedof_quad, max_geofedof_con

  REAL(DFP) :: args(4), times0
  REAL(DFP), ALLOCATABLE :: massMat(:, :), funcValue(:), ans(:), xij(:, :)
  INTEGER(I4B), ALLOCATABLE :: ipiv(:)

  LOGICAL(LGT) :: isok
  CLASS(AbstractMesh_), POINTER :: cellMesh
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  TYPE(ElemShapeData_) :: elemsd, facetElemsd, geoElemsd, geoFacetElemsd
  TYPE(QuadraturePoint_) :: quad, facetQuad

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nrow = 0
  ncol = 1
  max_geofedof_con = geofedof%GetMaxTotalConnectivity()
  max_fedof_con = fedof%GetMaxTotalConnectivity()
  max_fedof_quad = fedof%GetMaxTotalQuadraturePoints()

  times0 = Input(default=math%zero, option=times)

  CALL Reallocate(massMat, max_fedof_con, max_fedof_con)
  CALL Reallocate(funcValue, max_fedof_quad)
  CALL Reallocate(ipiv, max_fedof_con)
  CALL Reallocate(ans, max_fedof_con)
  CALL Reallocate(xij, 3, max_geofedof_con)

  ! Vertex nodes and values
  CALL obj%boundary%GetnodeNum(dom=obj%dom, ans=nodeNum, tsize=mysize)
  nrow = nrow + mysize
  iNodeOnNode = 1
  iNodeOnFace = nrow + 1

  DO ii = 1, nrow
    CALL fedof%GetVertexDOF( &
      globalNode=nodeNum(ii), ans=indx, islocal=math%no, tsize=jj)
    nodeNum(ii) = indx(1)
  END DO

  cellMesh => obj%dom%GetMeshPointer()

  args = 0.0_DFP
  args(4) = times0
  DO ii = 1, nrow
    CALL cellMesh%GetNodeCoord( &
      nodeCoord=args, tsize=jj, globalNode=nodeNum(ii), islocal=math%yes)

    CALL obj%func%Get(val=ans(1), args=args)
    nodalValue(ii, 1) = ans(1)

  END DO

  CALL obj%SetElemToLocalBoundary()

  ! Face nodes and values
  DO iel = 1, obj%tElemToFace

    CALL obj%GetElemToFace(indx=iel, localFaceNumber=localFaceNumber, &
                           localCellNumber=localCellNumber)

    CALL fedof%SetFE(globalElement=localCellNumber, islocal=math%yes)
    feptr => fedof%GetFEPointer(globalElement=localCellNumber, &
                                islocal=math%yes)

    CALL geofedof%SetFE(globalElement=localCellNumber, islocal=math%yes)
    geofeptr => geofedof%GetFEPointer(globalElement=localCellNumber, &
                                      islocal=math%yes)

    CALL cellMesh%GetNodeCoord( &
      nodeCoord=xij, nrow=xij_i, ncol=xij_j, globalElement=localCellNumber, &
      islocal=math%yes)

    CALL feptr%GetGlobalFacetElemShapeData2( &
      geofeptr=geofeptr, elemsd=elemsd, facetElemsd=facetElemsd, &
      geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd, &
      localFaceNumber=localFaceNumber, quad=quad, facetQuad=facetQuad, &
      xij=xij)

    CALL feptr%GetFacetDOFValue( &
      elemsd=elemsd, facetElemsd=facetElemsd, xij=xij, times=times0, &
      localFaceNumber=localFaceNumber, func=obj%func, ans=ans, tsize=jj, &
      massMat=massMat, ipiv=ipiv, funcValue=funcValue, &
      onlyFaceBubble=math%yes)

    nodalValue(nrow + 1:nrow + jj, 1) = ans(1:jj)

    CALL fedof%GetFaceDOF( &
      globalElement=localCellNumber, localFaceNumber=localFaceNumber, &
      ans=nodeNum(nrow + 1:), tsize=mysize, islocal=math%yes)

    nrow = nrow + mysize
  END DO

  ! Edge nodes
  iNodeOnEdge = nrow + 1
  DO ii = 1, obj%tElemToEdge
    CALL obj%GetElemToEdge(indx=ii, localEdgeNumber=localEdgeNumber, &
                           localCellNumber=localCellNumber)

    CALL fedof%GetEdgeDOF( &
      globalElement=localCellNumber, localEdgeNumber=localEdgeNumber, &
      ans=nodenum(nrow + 1:), tsize=mysize, islocal=math%yes)

    nrow = nrow + mysize
  END DO

#ifdef DEBUG_VER
  isok = obj%tElemToEdge .EQ. 0
  CALL AssertError1(isok, myName, &
                   "Edge DOF extraction is not implemented for userfunction.")
#endif

  IF (ALLOCATED(massMat)) DEALLOCATE (massMat)
  IF (ALLOCATED(funcValue)) DEALLOCATE (funcValue)
  IF (ALLOCATED(ans)) DEALLOCATE (ans)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(ipiv)) DEALLOCATE (ipiv)

  CALL ElemShapeData_Deallocate(elemsd)
  CALL ElemShapeData_Deallocate(geoElemsd)
  CALL ElemShapeData_Deallocate(facetElemsd)
  CALL ElemShapeData_Deallocate(geoFacetElemsd)
  CALL QuadraturePoint_Deallocate(quad)
  CALL QuadraturePoint_Deallocate(facetQuad)

  NULLIFY (feptr, geofeptr, cellMesh)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Get1WithUserFunction

!----------------------------------------------------------------------------
!                                                     Get1WithoutUserFunction
!----------------------------------------------------------------------------

SUBROUTINE Get1WithoutUserFunction(obj, fedof, geofedof, nodeNum, nodalValue, &
                                   nrow, ncol)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Abstract boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
    !! Degree of freedom for variable and geometry
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
    !! size of nodeNum can be obtained from obj%GetTotalNodeNum
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
    !! Nodal values of boundary value
    !! nrow = size of nodeNum
    !! ncol = 1 or size of times
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in nodalValue

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Get1WithoutUserFunction()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  SELECT CASE (obj%nodalValueType)

  CASE (TypeFEVariableOpt%constant)

    CALL Get1ConstantValue( &
      obj=obj, fedof=fedof, geofedof=geofedof, nodenum=nodenum, &
      nodalvalue=nodalvalue, nrow=nrow, ncol=ncol)

  CASE (TypeFEVariableOpt%space)

    CALL Get1SpaceValue( &
      obj=obj, fedof=fedof, geofedof=geofedof, nodenum=nodenum, &
      nodalvalue=nodalvalue, nrow=nrow, ncol=ncol)

  CASE (TypeFEVariableOpt%time)
    CALL Get1TimeValue( &
      obj=obj, fedof=fedof, geofedof=geofedof, nodenum=nodenum, &
      nodalvalue=nodalvalue, nrow=nrow, ncol=ncol)

  CASE (TypeFEVariableOpt%spaceTime)
    CALL Get1SpaceTimeValue( &
      obj=obj, fedof=fedof, geofedof=geofedof, nodenum=nodenum, &
      nodalvalue=nodalvalue, nrow=nrow, ncol=ncol)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(math%no, myname, &
             'No case found for nodalValueType'//ToString(obj%nodalValueType))
#endif

  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Get1WithoutUserFunction

!----------------------------------------------------------------------------
!                                                          Get1ConstantValue
!----------------------------------------------------------------------------

SUBROUTINE Get1ConstantValue(obj, fedof, geofedof, nodeNum, nodalValue, &
                             nrow, ncol)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
  !! Degree of freedom
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! Size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nrow = Size of nodeNum
  !! ncol = 1 or Size of times
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! the size of data written in nodalValue

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Get1ConstantValue()"
#endif

  INTEGER(I4B) :: iel, localFaceNumber, localCellNumber, mysize, &
                  iNodeOnNode, iNodeOnFace, iNodeOnEdge, ii, jj, indx(5), &
                  localEdgeNumber

  REAL(DFP) :: elemCoord(3, 8), constValue
  REAL(DFP), ALLOCATABLE :: massMat(:, :), ans(:), funcValue(:)
  INTEGER(I4B), ALLOCATABLE :: ipiv(:)
  LOGICAL(LGT) :: isok
  CLASS(AbstractMesh_), POINTER :: cellMesh
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  TYPE(ElemShapeData_) :: elemsd, facetElemsd, geoElemsd, geoFacetElemsd
  TYPE(QuadraturePoint_) :: quad, facetQuad

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  indx(1) = fedof%GetMaxTotalConnectivity()
  indx(2) = fedof%GetMaxTotalQuadraturePoints()
  CALL Reallocate(massMat, indx(1), indx(1))
  CALL Reallocate(ipiv, indx(1))
  CALL Reallocate(ans, indx(1))
  CALL Reallocate(funcValue, indx(2))

  nrow = 0
  ncol = 1

  constValue = obj%nodalValue(1, 1)

  ! Vertex nodes and values
  CALL obj%boundary%GetNodeNum(dom=obj%dom, ans=nodeNum, tsize=mysize)
  nrow = nrow + mysize
  iNodeOnNode = 1
  iNodeOnFace = nrow + 1

  DO ii = 1, nrow
    CALL fedof%GetVertexDOF( &
      globalNode=nodeNum(ii), ans=indx, islocal=math%no, tsize=jj)
    nodeNum(ii) = indx(1)
    nodalValue(ii, 1) = constValue
  END DO

  cellMesh => obj%dom%GetMeshPointer()

  CALL obj%SetElemToLocalBoundary()

  ! Face nodes and values
  DO iel = 1, obj%tElemToFace
    CALL obj%GetElemToFace( &
      indx=iel, localFaceNumber=localFaceNumber, &
      localCellNumber=localCellNumber)

    CALL fedof%SetFE(globalElement=localCellNumber, islocal=math%yes)
    feptr => fedof%GetFEPointer( &
             globalElement=localCellNumber, islocal=math%yes)

    CALL geofedof%SetFE(globalElement=localCellNumber, islocal=math%yes)
    geofeptr => geofedof%GetFEPointer( &
                globalElement=localCellNumber, islocal=math%yes)

    CALL cellMesh%GetNodeCoord( &
      nodeCoord=elemCoord, nrow=indx(1), ncol=indx(2), &
      islocal=math%yes, globalElement=localCellNumber)

    CALL feptr%GetGlobalFacetElemShapeData2( &
      geofeptr=geofeptr, elemsd=elemsd, facetElemsd=facetElemsd, &
      geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd, &
      localFaceNumber=localFaceNumber, quad=quad, facetQuad=facetQuad, &
      xij=elemCoord)

    CALL feptr%GetFacetDOFValue( &
      elemsd=elemsd, facetElemsd=facetElemsd, xij=elemCoord, &
      localFaceNumber=localFaceNumber, ans=ans, tsize=jj, massMat=massMat, &
      ipiv=ipiv, funcValue=funcValue, onlyFaceBubble=math%yes)

    DO ii = 1, jj
      nodalValue(nrow + ii, 1) = constValue * ans(ii)
    END DO

    CALL fedof%GetFaceDOF( &
      globalElement=localCellNumber, localFaceNumber=localFaceNumber, &
      ans=nodeNum(nrow + 1:), tsize=mysize, islocal=math%yes)

    nrow = nrow + mysize
  END DO

  ! Edge nodes
  iNodeOnEdge = nrow + 1
  DO ii = 1, obj%tElemToEdge
    CALL obj%GetElemToEdge(indx=ii, localEdgeNumber=localEdgeNumber, &
                           localCellNumber=localCellNumber)

    CALL fedof%GetEdgeDOF( &
      globalElement=localCellNumber, localEdgeNumber=localEdgeNumber, &
      ans=nodenum(nrow + 1:), tsize=mysize, islocal=math%yes)

    nrow = nrow + mysize
  END DO

#ifdef DEBUG_VER
  isok = obj%tElemToEdge .EQ. 0
  CALL AssertError1(isok, myName, &
                   "Edge DOF extraction is not implemented for userfunction.")
#endif

  DEALLOCATE (massMat, ans, ipiv)
  NULLIFY (cellMesh, feptr, geofeptr)
  CALL ElemShapeData_Deallocate(elemsd)
  CALL ElemShapeData_Deallocate(facetElemsd)
  CALL ElemShapeData_Deallocate(geoElemsd)
  CALL ElemShapeData_Deallocate(geoFacetElemsd)
  CALL QuadraturePoint_Deallocate(quad)
  CALL QuadraturePoint_Deallocate(facetQuad)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Get1ConstantValue

!----------------------------------------------------------------------------
!                                                             Get1SpaceValue
!----------------------------------------------------------------------------

SUBROUTINE Get1SpaceValue( &
  obj, fedof, geofedof, nodeNum, nodalValue, nrow, ncol)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
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
  CHARACTER(*), PARAMETER :: myName = "Get1SpaceValue()"
#endif

  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, iNodeOnNode, iNodeOnFace, iNodeOnEdge

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%nodalValue)
  CALL AssertError1(isok, myname, &
                    'AbstractBC_::obj%nodalValue is not allocated!')
#endif

  CALL obj%GetNodeNumber( &
    fedof=fedof, nodenum=nodeNum, tsize=nrow, iNodeOnNode=iNodeOnNode, &
    iNodeOnFace=iNodeOnFace, iNodeOnEdge=iNodeOnEdge)

#ifdef DEBUG_VER
  isok = obj%nrow .GE. nrow
  CALL AssertError1(isok, myname, &
                    'SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
#endif

  ncol = 1

  DO CONCURRENT(ii=1:nrow)
    nodalValue(ii, 1) = obj%nodalValue(ii, 1)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Get1SpaceValue

!----------------------------------------------------------------------------
!                                                               Get1TimeValue
!----------------------------------------------------------------------------

SUBROUTINE Get1TimeValue( &
  obj, fedof, geofedof, nodeNum, nodalValue, nrow, ncol)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
  !! Degree of freedom
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! Size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nrow = Size of nodeNum
  !! ncol = 1 or Size of times
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! the size of data written in nodalValue

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Get1TimeValue()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')
#endif

!   INTEGER(I4B) :: iel, localFaceNumber, localCellNumber, mysize, &
!                   iNodeOnNode, iNodeOnFace, iNodeOnEdge, ii, jj, indx(1), &
!                   localEdgeNumber, elemCoord_i, elemCoord_j, max_fedof_con, &
!                   itimes
!
!   REAL(DFP) :: elemCoord(3, 8)
!   REAL(DFP), ALLOCATABLE :: massMat(:, :), ans(:)
!   INTEGER(I4B), ALLOCATABLE :: ipiv(:)
!
!   LOGICAL(LGT) :: isok
!   CLASS(AbstractMesh_), POINTER :: cellMesh
!   CLASS(AbstractFE_), POINTER :: feptr, geofeptr
!   TYPE(ElemShapeData_) :: elemsd, facetElemsd, geoElemsd, geoFacetElemsd
!   TYPE(QuadraturePoint_) :: quad, facetQuad
!
! #ifdef DEBUG_VER
!   CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                           '[START] ')
! #endif
!
!   nrow = 0; ncol = obj%nrow
!
!   ! Vertex nodes and values
!   CALL obj%boundary%GetnodeNum(dom=obj%dom, ans=nodeNum, tsize=mysize)
!   nrow = nrow + mysize
!   iNodeOnNode = 1
!   iNodeOnFace = nrow + 1
!
!   DO ii = 1, nrow
!     CALL fedof%GetVertexDOF( &
!       globalNode=nodeNum(ii), ans=indx, islocal=math%no, tsize=jj)
!
!     nodeNum(ii) = indx(1)
!
!     DO itimes = 1, ncol
!       nodalValue(ii, itimes) = obj%nodalValue(itimes, 1)
!     END DO
!   END DO
!
!   cellMesh => obj%dom%GetMeshPointer()
!
!   CALL obj%SetElemToLocalBoundary()
!
!   max_fedof_con = fedof%GetMaxTotalConnectivity()
!   CALL Reallocate(massMat, max_fedof_con, max_fedof_con)
!   CALL Reallocate(ipiv, max_fedof_con)
!   CALL Reallocate(ans, max_fedof_con)
!
!   ! Face nodes and values
!   DO iel = 1, obj%tElemToFace
!
!     CALL obj%GetElemToFace(indx=iel, localFaceNumber=localFaceNumber, &
!                            localCellNumber=localCellNumber)
!
!     CALL fedof%SetFE(globalElement=localCellNumber, islocal=math%yes)
!     feptr => fedof%GetFEPointer( &
!              globalElement=localCellNumber, islocal=math%yes)
!
!     CALL geofedof%SetFE(globalElement=localCellNumber, islocal=math%yes)
!     geofeptr => geofedof%GetFEPointer(globalElement=localCellNumber, &
!                                       islocal=math%yes)
!
!     CALL cellMesh%GetNodeCoord( &
!       nodeCoord=elemCoord, nrow=elemCoord_i, ncol=elemCoord_j, &
!       islocal=math%yes, globalElement=localCellNumber)
!
!     CALL feptr%GetGlobalFacetElemShapeData2( &
!       geofeptr=geofeptr, elemsd=elemsd, facetElemsd=facetElemsd, &
!       geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd, &
!       localFaceNumber=localFaceNumber, quad=quad, facetQuad=facetQuad, &
!       xij=elemCoord)
!
!     CALL feptr%GetFacetDOFValue( &
!       elemsd=elemsd, facetElemsd=facetElemsd, xij=elemCoord, &
!       localFaceNumber=localFaceNumber, ans=ans, &
!       tsize=jj, massMat=massMat, ipiv=ipiv, onlyFaceBubble=math%yes, &
!       tVertices=geoFacetElemsd%nns)
!
!     DO itimes = 1, ncol
!       nodalValue(nrow + 1:nrow + jj, itimes) = &
!         obj%nodalValue(itimes, 1) * ans(1:jj)
!     END DO
!
!     CALL fedof%GetFaceDOF( &
!       globalElement=localCellNumber, localFaceNumber=localFaceNumber, &
!       ans=nodeNum(nrow + 1:), tsize=mysize, islocal=math%yes)
!
!     nrow = nrow + mysize
!   END DO
!
!   ! Edge nodes
!   iNodeOnEdge = nrow + 1
!   DO ii = 1, obj%tElemToEdge
!     CALL obj%GetElemToEdge(indx=ii, localEdgeNumber=localEdgeNumber, &
!                            localCellNumber=localCellNumber)
!
!     CALL fedof%GetEdgeDOF( &
!       globalElement=localCellNumber, localEdgeNumber=localEdgeNumber, &
!       ans=nodenum(nrow + 1:), tsize=mysize, islocal=math%yes)
!
!     nrow = nrow + mysize
!   END DO
!
! #ifdef DEBUG_VER
!   isok = obj%tElemToEdge .EQ. 0
!   CALL AssertError1(isok, myName, &
!                    "Edge DOF extraction is not implemented for userfunction.")
! #endif
!
!   DEALLOCATE (massMat, ans, ipiv)
!   NULLIFY (cellMesh, feptr, geofeptr)
!   CALL ElemShapeData_Deallocate(elemsd)
!   CALL ElemShapeData_Deallocate(facetElemsd)
!   CALL ElemShapeData_Deallocate(geoElemsd)
!   CALL ElemShapeData_Deallocate(geoFacetElemsd)
!   CALL QuadraturePoint_Deallocate(quad)
!   CALL QuadraturePoint_Deallocate(facetQuad)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Get1TimeValue

!----------------------------------------------------------------------------
!                                                         Get1SpaceTimeValue
!----------------------------------------------------------------------------

SUBROUTINE Get1SpaceTimeValue(obj, fedof, geofedof, nodeNum, nodalValue, &
                              nrow, ncol)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
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
  CHARACTER(*), PARAMETER :: myName = 'Get1SpaceTimeValue()'
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

  CALL obj%GetNodeNumber( &
    fedof=fedof, nodenum=nodeNum, tsize=nrow, iNodeOnNode=iNodeOnNode, &
    iNodeOnFace=iNodeOnFace, iNodeOnEdge=iNodeOnEdge)

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
END SUBROUTINE Get1SpaceTimeValue

!----------------------------------------------------------------------------
!                                                  Get1CheckErrorUserFunction
!----------------------------------------------------------------------------

SUBROUTINE Get1CheckErrorUserFunction(obj, myName, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: myName
  REAL(DFP), OPTIONAL, INTENT(IN) :: times

  ! internal variable
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName0 = "Get1CheckErrorUserFunction()"
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
  aint = obj%func%GetNumArgs()
  isok = aint .EQ. 4_I4B
  CALL AssertError1(isok, myName, &
                    "numArgs in userFunction ="//ToString(aint)// &
                    " should be 4 (x, y, z, t)")
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
END SUBROUTINE Get1CheckErrorUserFunction

!----------------------------------------------------------------------------
!                                                                       Get
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
isok = obj%isInit
CALL AssertError1(isok, myName, &
                  'AbstractBC_ object is not initiated, initiate it first.')
#endif

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%dom)
CALL AssertError1(isok, myName, &
                  'AbstractBC_::obj%dom is not associated!')
#endif

IF (obj%isUserFunction) THEN

#ifdef DEBUG_VER
  CALL Get2CheckErrorUserFunction(obj=obj, myName=myName)
#endif

  CALL Get2WithUserFunction( &
    obj=obj, fedof=fedof, geofedof=geofedof, timefedof=timefedof, &
    nodenum=nodenum, nodalValue=nodalValue, nrow=nrow, ncol=ncol, &
    times=times)

ELSE

  CALL Get2WithoutUserFunction( &
    obj=obj, fedof=fedof, geofedof=geofedof, timefedof=timefedof, &
    nodenum=nodenum, nodalValue=nodalValue, nrow=nrow, ncol=ncol, times=times)

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                  Get2CheckErrorUserFunction
!----------------------------------------------------------------------------

SUBROUTINE Get2CheckErrorUserFunction(obj, myName)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: myName

  ! internal variable
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName0 = "Get2CheckErrorUserFunction()"
  LOGICAL(LGT) :: isok
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
  aint = obj%func%GetNumArgs()
  isok = aint .EQ. 4_I4B
  CALL AssertError1(isok, myName, &
                    "numArgs in userFunction ="//ToString(aint)// &
                    " should be 4 (x, y, z, t)")
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
END SUBROUTINE Get2CheckErrorUserFunction

!----------------------------------------------------------------------------
!                                                        Get2WithUserFunction
!----------------------------------------------------------------------------

SUBROUTINE Get2WithUserFunction( &
  obj, fedof, geofedof, timefedof, nodeNum, nodalValue, nrow, ncol, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Abstract boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
  !! fedof
  CLASS(TimeFEDOF_), INTENT(INOUT) :: timefedof
  !! Time degree of freedom
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! node number should be allocated
  !! size of nodeNum is nrow
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nodal values
  REAL(DFP), INTENT(IN) :: times(:)
  !! time value
  !! time values are needed when userfunction is time or space-time
  INTEGER(I4B) :: nrow, ncol
  !! nrow is size of nodeNum and size of number of rows in nodalvalue
  !! ncol is colsize of nodalvalue

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Get2WithUserFunction()"
#endif

  INTEGER(I4B) :: iel, localFaceNumber, localCellNumber, mysize, &
                  iNodeOnNode, iNodeOnFace, iNodeOnEdge, ii, jj, indx(5), &
                  localEdgeNumber, itimes, tTimes
  REAL(DFP) :: args(4)
  REAL(DFP), ALLOCATABLE :: massMat(:, :), funcValue(:, :), ans(:, :), &
                            xij(:, :), temp(:)
  INTEGER(I4B), ALLOCATABLE :: ipiv(:)
  LOGICAL(LGT) :: isok
  CLASS(AbstractMesh_), POINTER :: cellMesh
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  CLASS(AbstractOneDimFE_), POINTER :: timefeptr
  TYPE(ElemShapeData_) :: elemsd, facetElemsd, geoElemsd, geoFacetElemsd, &
                          timeElemsd, timeGeoElemsd
  TYPE(QuadraturePoint_) :: quad, facetQuad, timeQuad

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nrow = 0
  ncol = timefedof%GetTotalDOF()
  tTimes = SIZE(times)
  indx(1) = geofedof%GetMaxTotalConnectivity() ! max_geofedof_con
  CALL Reallocate(xij, 3, indx(1))

  indx(2) = fedof%GetMaxTotalConnectivity() ! max_fedof_con
  indx(3) = timefedof%GetMaxTotalConnectivity() ! max_timefedof_con
  CALL Reallocate(ans, indx(2), indx(3))
  ii = indx(2) * indx(3)
  CALL Reallocate(massMat, ii, ii)
  CALL Reallocate(ipiv, ii)
  CALL Reallocate(temp, ii)

  indx(4) = fedof%GetMaxTotalQuadraturePoints() ! max_fedof_quad
  indx(5) = timefedof%GetMaxTotalQuadraturePoints() ! max_timefedof_quad
  ii = MAXVAL(indx(4:5))
  jj = indx(5)
  CALL Reallocate(funcValue, ii, jj)

  ! Vertex nodes and values
  CALL obj%boundary%GetnodeNum(dom=obj%dom, ans=nodeNum, tsize=mysize)
  nrow = nrow + mysize
  iNodeOnNode = 1
  iNodeOnFace = nrow + 1

  DO ii = 1, nrow
    CALL fedof%GetVertexDOF( &
      globalNode=nodeNum(ii), ans=indx, islocal=math%no, tsize=jj)
    nodeNum(ii) = indx(1)
  END DO

  cellMesh => obj%dom%GetMeshPointer()

  CALL timefedof%SetFE()
  timefeptr => timefedof%GetFEPointer()

  CALL timefeptr%GetGlobalTimeElemShapeData( &
    elemsd=timeElemsd, times=times, geoElemsd=timeGeoElemsd, quad=timeQuad)

  args = 0.0_DFP
  DO ii = 1, nrow
    CALL cellMesh%GetNodeCoord( &
      nodeCoord=args, tsize=jj, globalNode=nodeNum(ii), islocal=math%yes)

    DO itimes = 1, tTimes
      args(4) = times(itimes)
      CALL obj%func%Get(val=ans(1, 1), args=args)
      nodalValue(ii, itimes) = ans(1, 1)
    END DO

    CALL timefeptr%GetTimeDOFValue( &
      elemsd=timeElemsd, x=args, nsd=3_I4B, times=times, func=obj%func, &
      ans=ans(:, 1), tsize=jj, massMat=massMat, ipiv=ipiv, &
      funcValue=funcValue(:, 1), onlyFaceBubble=math%yes)

    DO itimes = 1, jj
      nodalValue(ii, tTimes + itimes) = ans(itimes, 1)
    END DO

  END DO

  CALL obj%SetElemToLocalBoundary()

  ! Face nodes and values
  DO iel = 1, obj%tElemToFace

    CALL obj%GetElemToFace(indx=iel, localFaceNumber=localFaceNumber, &
                           localCellNumber=localCellNumber)

    CALL fedof%SetFE(globalElement=localCellNumber, islocal=math%yes)
    feptr => fedof%GetFEPointer( &
             globalElement=localCellNumber, islocal=math%yes)

    CALL geofedof%SetFE(globalElement=localCellNumber, islocal=math%yes)
    geofeptr => geofedof%GetFEPointer(globalElement=localCellNumber, &
                                      islocal=math%yes)

    CALL cellMesh%GetNodeCoord( &
      nodeCoord=xij, nrow=indx(1), ncol=indx(2), &
      globalElement=localCellNumber, islocal=math%yes)

    CALL feptr%GetGlobalFacetElemShapeData2( &
      geofeptr=geofeptr, elemsd=elemsd, facetElemsd=facetElemsd, &
      geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd, &
      localFaceNumber=localFaceNumber, quad=quad, facetQuad=facetQuad, &
      xij=xij)

    CALL feptr%GetFacetDOFValue( &
      elemsd=elemsd, facetElemsd=facetElemsd, timeElemsd=timeElemsd, &
      xij=xij, times=times, localFaceNumber=localFaceNumber, func=obj%func, &
      ans=ans, nrowStart=indx(1), nrowEnd=indx(2), ncolStart=indx(3), &
      ncolEnd=indx(4), massMat=massMat, ipiv=ipiv, funcValue=funcValue, &
      temp=temp, onlyFaceBubble=math%yes)

    DO jj = indx(3), indx(4)
      DO ii = indx(1), indx(2)
        indx(5) = ii - indx(1) + 1
        nodalValue(nrow + indx(5), jj) = ans(ii, jj)
      END DO
    END DO

    CALL fedof%GetFaceDOF( &
      globalElement=localCellNumber, localFaceNumber=localFaceNumber, &
      ans=nodeNum(nrow + 1:), tsize=mysize, islocal=math%yes)

    nrow = nrow + mysize
  END DO

  ! Edge nodes
  iNodeOnEdge = nrow + 1
  DO ii = 1, obj%tElemToEdge
    CALL obj%GetElemToEdge(indx=ii, localEdgeNumber=localEdgeNumber, &
                           localCellNumber=localCellNumber)

    CALL fedof%GetEdgeDOF( &
      globalElement=localCellNumber, localEdgeNumber=localEdgeNumber, &
      ans=nodenum(nrow + 1:), tsize=mysize, islocal=math%yes)

    nrow = nrow + mysize
  END DO

#ifdef DEBUG_VER
  isok = obj%tElemToEdge .EQ. 0
  CALL AssertError1(isok, myName, &
                   "Edge DOF extraction is not implemented for userfunction.")
#endif

  DEALLOCATE (massMat, funcValue, ans, xij, temp, ipiv)

  CALL ElemShapeData_Deallocate(elemsd)
  CALL ElemShapeData_Deallocate(geoElemsd)
  CALL ElemShapeData_Deallocate(facetElemsd)
  CALL ElemShapeData_Deallocate(geoFacetElemsd)
  CALL ElemShapeData_Deallocate(timeElemsd)
  CALL ElemShapeData_Deallocate(timeGeoElemsd)
  CALL QuadraturePoint_Deallocate(quad)
  CALL QuadraturePoint_Deallocate(facetQuad)
  CALL QuadraturePoint_Deallocate(timeQuad)

  NULLIFY (feptr, geofeptr, cellMesh)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Get2WithUserFunction

!----------------------------------------------------------------------------
!                                                    Get2WithoutUserFunction
!----------------------------------------------------------------------------

SUBROUTINE Get2WithoutUserFunction( &
  obj, fedof, geofedof, timefedof, nodeNum, nodalValue, nrow, ncol, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
    !! Abstract boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
    !! Degree of freedom for variable and geometry
  CLASS(TimeFEDOF_), INTENT(INOUT) :: timefedof
    !! Time degree of freedom
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
    !! size of nodeNum can be obtained from obj%GetTotalNodeNum
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
    !! Nodal values of boundary value
    !! nrow = size of nodeNum
    !! ncol = 1 or size of times
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols written in nodalValue
  REAL(DFP), INTENT(IN) :: times(:)
    !! times vector is only used when usefunction is true in obj

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Get2WithoutUserFunction()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  SELECT CASE (obj%nodalValueType)

  CASE (TypeFEVariableOpt%constant)

    CALL Get2ConstantValue( &
      obj=obj, fedof=fedof, geofedof=geofedof, timefedof=timefedof, &
      nodenum=nodenum, nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, &
      times=times)

  CASE (TypeFEVariableOpt%time)
    CALL Get2TimeValue( &
      obj=obj, fedof=fedof, geofedof=geofedof, timefedof=timefedof, &
      nodenum=nodenum, nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, &
      times=times)

  CASE (TypeFEVariableOpt%space)

    CALL Get2SpaceValue( &
      obj=obj, fedof=fedof, geofedof=geofedof, timefedof=timefedof, &
      nodenum=nodenum, nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, &
      times=times)

  CASE (TypeFEVariableOpt%spaceTime)

    CALL Get2SpaceTimeValue( &
      obj=obj, fedof=fedof, geofedof=geofedof, timefedof=timefedof, &
      nodenum=nodenum, nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, &
      times=times)

#ifdef DEBUG_VER
  CASE DEFAULT
    CALL AssertError1(math%no, myname, &
             'No case found for nodalValueType'//ToString(obj%nodalValueType))
#endif

  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Get2WithoutUserFunction

!----------------------------------------------------------------------------
!                                                          Get2ConstantValue
!----------------------------------------------------------------------------

SUBROUTINE Get2ConstantValue( &
  obj, fedof, geofedof, timefedof, nodeNum, nodalValue, nrow, ncol, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
  !! Degree of freedom
  CLASS(TimeFEDOF_), INTENT(INOUT) :: timefedof
  !! Time degree of freedom
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! Size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nrow = Size of nodeNum
  !! ncol = 1 or Size of times
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! the size of data written in nodalValue
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  !! time values when nodalValueType is space-time or time

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Get2ConstantValue()"
#endif

  INTEGER(I4B) :: iel, localFaceNumber, localCellNumber, mysize, &
                  iNodeOnNode, iNodeOnFace, iNodeOnEdge, ii, jj, indx(5), &
                  localEdgeNumber, itimes, tTimes
  REAL(DFP) :: constValue
  REAL(DFP), ALLOCATABLE :: massMat(:, :), funcValue(:, :), ans(:, :), &
                            xij(:, :), temp(:)
  INTEGER(I4B), ALLOCATABLE :: ipiv(:)
  LOGICAL(LGT) :: isok
  CLASS(AbstractMesh_), POINTER :: cellMesh
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  CLASS(AbstractOneDimFE_), POINTER :: timefeptr
  TYPE(ElemShapeData_) :: elemsd, facetElemsd, geoElemsd, geoFacetElemsd, &
                          timeElemsd, timeGeoElemsd
  TYPE(QuadraturePoint_) :: quad, facetQuad, timeQuad

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%nodalValue)
  CALL AssertError1(isok, myName, &
                    "AbstractBC_::obj%nodalValue is not allocated!")
#endif

  constValue = obj%nodalValue(1, 1)

  nrow = 0
  ncol = timefedof%GetTotalDOF()

  tTimes = SIZE(times)
  indx(1) = geofedof%GetMaxTotalConnectivity() ! max_geofedof_con
  CALL Reallocate(xij, 3, indx(1))

  indx(2) = fedof%GetMaxTotalConnectivity() ! max_fedof_con
  indx(3) = timefedof%GetMaxTotalConnectivity() ! max_timefedof_con
  CALL Reallocate(ans, indx(2), indx(3))
  ii = indx(2) * indx(3)
  CALL Reallocate(massMat, ii, ii)
  CALL Reallocate(ipiv, ii)
  CALL Reallocate(temp, ii)

  indx(4) = fedof%GetMaxTotalQuadraturePoints() ! max_fedof_quad
  indx(5) = timefedof%GetMaxTotalQuadraturePoints() ! max_timefedof_quad
  ii = MAXVAL(indx(4:5))
  jj = indx(5)
  CALL Reallocate(funcValue, ii, jj)

  ! Vertex nodes and values
  CALL obj%boundary%GetnodeNum(dom=obj%dom, ans=nodeNum, tsize=mysize)
  nrow = nrow + mysize
  iNodeOnNode = 1
  iNodeOnFace = nrow + 1

  DO ii = 1, nrow
    CALL fedof%GetVertexDOF( &
      globalNode=nodeNum(ii), ans=indx, islocal=math%no, tsize=jj)
    nodeNum(ii) = indx(1)
  END DO

  cellMesh => obj%dom%GetMeshPointer()

  CALL timefedof%SetFE()
  timefeptr => timefedof%GetFEPointer()

  CALL timefeptr%GetGlobalTimeElemShapeData( &
    elemsd=timeElemsd, times=times, geoElemsd=timeGeoElemsd, quad=timeQuad)

  DO ii = 1, nrow
    DO itimes = 1, tTimes
      nodalValue(ii, itimes) = constValue
    END DO

    CALL timefeptr%GetTimeDOFValue( &
      elemsd=timeElemsd, times=times, ans=ans(:, 1), &
      tsize=jj, massMat=massMat, ipiv=ipiv, funcValue=funcValue(:, 1), &
      onlyFaceBubble=math%yes)

    DO itimes = 1, jj
      nodalValue(ii, tTimes + itimes) = constValue * ans(itimes, 1)
    END DO

  END DO

  CALL obj%SetElemToLocalBoundary()

  ! Face nodes and values
  DO iel = 1, obj%tElemToFace

    CALL obj%GetElemToFace( &
      indx=iel, localFaceNumber=localFaceNumber, &
      localCellNumber=localCellNumber)

    CALL fedof%SetFE(globalElement=localCellNumber, islocal=math%yes)
    feptr => fedof%GetFEPointer( &
             globalElement=localCellNumber, islocal=math%yes)

    CALL geofedof%SetFE(globalElement=localCellNumber, islocal=math%yes)
    geofeptr => geofedof%GetFEPointer( &
                globalElement=localCellNumber, islocal=math%yes)

    CALL cellMesh%GetNodeCoord( &
      nodeCoord=xij, nrow=indx(1), ncol=indx(2), &
      globalElement=localCellNumber, islocal=math%yes)

    CALL feptr%GetGlobalFacetElemShapeData2( &
      geofeptr=geofeptr, elemsd=elemsd, facetElemsd=facetElemsd, &
      geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd, &
      localFaceNumber=localFaceNumber, quad=quad, facetQuad=facetQuad, &
      xij=xij)

    CALL feptr%GetFacetDOFValue( &
      elemsd=elemsd, facetElemsd=facetElemsd, timeElemsd=timeElemsd, &
      xij=xij, times=times, localFaceNumber=localFaceNumber, &
      ans=ans, nrowStart=indx(1), nrowEnd=indx(2), ncolStart=indx(3), &
      ncolEnd=indx(4), massMat=massMat, ipiv=ipiv, funcValue=funcValue, &
      temp=temp, onlyFaceBubble=math%yes)

    DO jj = indx(3), indx(4)
      DO ii = indx(1), indx(2)
        indx(5) = ii - indx(1) + 1
        nodalValue(nrow + indx(5), jj) = constValue * ans(ii, jj)
      END DO
    END DO

    CALL fedof%GetFaceDOF( &
      globalElement=localCellNumber, localFaceNumber=localFaceNumber, &
      ans=nodeNum(nrow + 1:), tsize=mysize, islocal=math%yes)

    nrow = nrow + mysize
  END DO

  ! Edge nodes
  iNodeOnEdge = nrow + 1
  DO ii = 1, obj%tElemToEdge
    CALL obj%GetElemToEdge(indx=ii, localEdgeNumber=localEdgeNumber, &
                           localCellNumber=localCellNumber)

    CALL fedof%GetEdgeDOF( &
      globalElement=localCellNumber, localEdgeNumber=localEdgeNumber, &
      ans=nodenum(nrow + 1:), tsize=mysize, islocal=math%yes)

    nrow = nrow + mysize
  END DO

#ifdef DEBUG_VER
  isok = obj%tElemToEdge .EQ. 0
  CALL AssertError1(isok, myName, &
                   "Edge DOF extraction is not implemented for userfunction.")
#endif

  DEALLOCATE (massMat, funcValue, ans, xij, temp, ipiv)

  CALL ElemShapeData_Deallocate(elemsd)
  CALL ElemShapeData_Deallocate(geoElemsd)
  CALL ElemShapeData_Deallocate(facetElemsd)
  CALL ElemShapeData_Deallocate(geoFacetElemsd)
  CALL ElemShapeData_Deallocate(timeElemsd)
  CALL ElemShapeData_Deallocate(timeGeoElemsd)
  CALL QuadraturePoint_Deallocate(quad)
  CALL QuadraturePoint_Deallocate(facetQuad)
  CALL QuadraturePoint_Deallocate(timeQuad)

  NULLIFY (feptr, geofeptr, cellMesh)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Get2ConstantValue

!----------------------------------------------------------------------------
!                                                              Get2TimeValue
!----------------------------------------------------------------------------

SUBROUTINE Get2TimeValue( &
  obj, fedof, geofedof, timefedof, nodeNum, nodalValue, nrow, ncol, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
  !! Degree of freedom
  CLASS(TimeFEDOF_), INTENT(INOUT) :: timefedof
  !! time degree of freedom
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! Size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nrow = Size of nodeNum
  !! ncol = 1 or Size of times
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! the size of data written in nodalValue
  REAL(DFP), INTENT(IN) :: times(:)
  !! time element coordinates

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetSpaceValue_uf()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')
#endif

!   LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.
!
!   INTEGER(I4B) :: iel, localFaceNumber, localCellNumber, mysize, &
!                   iNodeOnNode, iNodeOnFace, iNodeOnEdge, ii, jj, indx(1), &
!                   localEdgeNumber, elemCoord_i, elemCoord_j, max_fedof_con, &
!                   itimes
!
!   REAL(DFP) :: elemCoord(3, 8)
!   REAL(DFP), ALLOCATABLE :: massMat(:, :)
!   INTEGER(I4B), ALLOCATABLE :: ipiv(:)
!
!   LOGICAL(LGT) :: isok
!   CLASS(AbstractMesh_), POINTER :: cellMesh
!   CLASS(AbstractFE_), POINTER :: feptr, geofeptr
!   TYPE(ElemShapeData_) :: elemsd, facetElemsd, geoElemsd, geoFacetElemsd
!   TYPE(QuadraturePoint_) :: quad, facetQuad
!
! #ifdef DEBUG_VER
!   CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                           '[START] ')
! #endif
!
!   nrow = 0; ncol = obj%nrow
!
!   ! Vertex nodes and values
!   CALL obj%boundary%GetnodeNum(dom=obj%dom, ans=nodeNum, tsize=mysize)
!   nrow = nrow + mysize
!   iNodeOnNode = 1
!   iNodeOnFace = nrow + 1
!
!   DO ii = 1, nrow
!     CALL fedof%GetVertexDOF(globalNode=nodeNum(ii), ans=indx, islocal=no, &
!                             tsize=jj)
!     nodeNum(ii) = indx(1)
!
!     DO itimes = 1, ncol
!       nodalValue(ii, itimes) = obj%nodalValue(itimes, 1)
!     END DO
!   END DO
!
!   cellMesh => obj%dom%GetMeshPointer()
!
!   CALL obj%SetElemToLocalBoundary()
!
!   max_fedof_con = fedof%GetMaxTotalConnectivity()
!   CALL Reallocate(massMat, max_fedof_con, max_fedof_con)
!   CALL Reallocate(ipiv, max_fedof_con)
!
!   ! Face nodes and values
!   DO iel = 1, obj%tElemToFace
!
!     CALL obj%GetElemToFace(indx=iel, localFaceNumber=localFaceNumber, &
!                            localCellNumber=localCellNumber)
!
!     CALL fedof%SetFE(globalElement=localCellNumber, islocal=yes)
!     feptr => fedof%GetFEPointer(globalElement=localCellNumber, islocal=yes)
!
!     CALL geofedof%SetFE(globalElement=localCellNumber, islocal=yes)
!     geofeptr => geofedof%GetFEPointer(globalElement=localCellNumber, &
!                                       islocal=yes)
!
!     CALL feptr%GetFacetQuadraturePoints(quad=quad, facetQuad=facetQuad, &
!                                         localFaceNumber=localFaceNumber)
!
!     CALL feptr%GetLocalFacetElemShapeData( &
!       elemsd=elemsd, facetElemsd=facetElemsd, quad=quad, &
!       facetQuad=facetQuad, localFaceNumber=localFaceNumber)
!
!     CALL geofeptr%GetLocalFacetElemShapeData( &
!       elemsd=geoElemsd, facetElemsd=geoFacetElemsd, quad=quad, &
!       facetQuad=facetQuad, localFaceNumber=localFaceNumber)
!
!     CALL cellMesh%GetNodeCoord( &
!       nodeCoord=elemCoord, nrow=elemCoord_i, ncol=elemCoord_j, islocal=yes, &
!       globalElement=localCellNumber)
!
!     CALL feptr%GetGlobalFacetElemShapeData( &
!       elemsd=elemsd, facetElemsd=facetElemsd, &
!       localFaceNumber=localFaceNumber, geoElemsd=geoElemsd, &
!       geoFacetElemsd=geoFacetElemsd, xij=elemCoord)
!
!     CALL feptr%GetFacetDOFValue( &
!       elemsd=elemsd, facetElemsd=facetElemsd, xij=elemCoord, &
!       localFaceNumber=localFaceNumber, ans=nodalValue(nrow + 1:, 1), &
!       tsize=jj, massMat=massMat, ipiv=ipiv, onlyFaceBubble=yes, &
!       tVertices=geoFacetElemsd%nns)
!
!     DO itimes = 2, ncol
!       nodalValue(nrow + 1:nrow + jj, itimes) = &
!         obj%nodalValue(itimes, 1) * nodalValue(nrow + 1:nrow + jj, 1)
!     END DO
!
!     nodalValue(nrow + 1:nrow + jj, 1) = &
!       obj%nodalValue(1, 1) * nodalValue(nrow + 1:nrow + jj, 1)
!
!     CALL fedof%GetFaceDOF( &
!       globalElement=localCellNumber, localFaceNumber=localFaceNumber, &
!       ans=nodeNum(nrow + 1:), tsize=mysize, islocal=yes)
!
! #ifdef DEBUG_VER
!     isok = mysize .EQ. jj
!     CALL AssertError1(isok, myName, &
!             "Size mismatch in nodalValue which is equal to "//ToString(jj)// &
!                       ", and nodeNum which is equal to "//ToString(mysize))
! #endif
!
!     nrow = nrow + mysize
!   END DO
!
!   ! Edge nodes
!   iNodeOnEdge = nrow + 1
!   DO ii = 1, obj%tElemToEdge
!     CALL obj%GetElemToEdge(indx=ii, localEdgeNumber=localEdgeNumber, &
!                            localCellNumber=localCellNumber)
!
!     CALL fedof%GetEdgeDOF( &
!       globalElement=localCellNumber, localEdgeNumber=localEdgeNumber, &
!       ans=nodenum(nrow + 1:), tsize=mysize, islocal=yes)
!
!     nrow = nrow + mysize
!   END DO
!
! #ifdef DEBUG_VER
!   isok = obj%tElemToEdge .EQ. 0
!   CALL AssertError1(isok, myName, &
!                    "Edge DOF extraction is not implemented for userfunction.")
! #endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Get2TimeValue

!----------------------------------------------------------------------------
!                                                              Get2SpaceValue
!----------------------------------------------------------------------------

SUBROUTINE Get2SpaceValue(obj, fedof, geofedof, timefedof, nodeNum, &
                          nodalValue, nrow, ncol, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
  !! Degree of freedom
  CLASS(TimeFEDOF_), INTENT(INOUT) :: timefedof
  !! time degree of freedom
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! Size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nrow = Size of nodeNum
  !! ncol = 1 or Size of times
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! the size of data written in nodalValue
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  !! time values, only needed when ncol > 1

  !! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Get2SpaceValue()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')
#endif

!   LOGICAL(LGT) :: isok, istimes
!   INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge
!
! #ifdef DEBUG_VER
!   CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                           '[START] ')
! #endif
!
! #ifdef DEBUG_VER
!   isok = ALLOCATED(obj%nodalValue)
!   CALL AssertError1(isok, myname, &
!                     'AbstractBC_::obj%nodalValue is not allocated!')
! #endif
!
!   CALL obj%GetNodeNumber(fedof=fedof, nodenum=nodeNum, tsize=nrow, &
!                          iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
!                          iNodeOnEdge=iNodeOnEdge)
!
! #ifdef DEBUG_VER
!   isok = obj%nrow .GE. nrow
!   CALL AssertError1(isok, myname, &
!                     'SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
! #endif
!
!   ncol = 1
!   istimes = PRESENT(times)
!   IF (istimes) ncol = SIZE(times)
!
!   DO CONCURRENT(ii=1:nrow, jj=1:ncol)
!     nodalValue(ii, jj) = obj%nodalValue(ii, 1)
!   END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Get2SpaceValue

!----------------------------------------------------------------------------
!                                                         Get2SpaceTimeValue
!----------------------------------------------------------------------------

SUBROUTINE Get2SpaceTimeValue(obj, fedof, geofedof, timefedof, nodeNum, &
                              nodalValue, nrow, ncol, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  !! Boundary condition
  CLASS(FEDOF_), INTENT(INOUT) :: fedof, geofedof
  !! Degree of freedom
  CLASS(TimeFEDOF_), INTENT(INOUT) :: timefedof
  !! Time degree of freedom
  INTEGER(I4B), INTENT(INOUT) :: nodeNum(:)
  !! Size of nodeNum can be obtained from obj%boundary%GetTotalNodeNum
  REAL(DFP), INTENT(INOUT) :: nodalValue(:, :)
  !! nrow = Size of nodeNum
  !! ncol = 1 or Size of times
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  !! the size of data written in nodalValue
  REAL(DFP), INTENT(IN) :: times(:)

  !! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'Get2SpaceTimeValue()'
#endif

#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')
#endif

!   LOGICAL(LGT) :: isok
!   INTEGER(I4B) :: ii, jj, iNodeOnNode, iNodeOnFace, iNodeOnEdge
!
! #ifdef DEBUG_VER
!   CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                           '[START] ')
! #endif
!
! #ifdef DEBUG_VER
!   isok = ALLOCATED(obj%nodalValue)
!   CALL AssertError1(isok, myname, &
!                     'AbstractBC_::obj%nodalValue is not allocated!')
!
! #endif
!
!   CALL obj%GetNodeNumber(fedof=fedof, nodenum=nodeNum, tsize=nrow, &
!                          iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
!                          iNodeOnEdge=iNodeOnEdge)
!
! #ifdef DEBUG_VER
!   isok = obj%nrow .GE. nrow
!   CALL AssertError1(isok, myname, &
!                     'SIZE(obj%nodalValue, 1) .NE. SIZE(nodeNum)')
! #endif
!
!   ncol = obj%ncol
!
!   DO CONCURRENT(ii=1:nrow, jj=1:ncol)
!     nodalValue(ii, jj) = obj%nodalValue(ii, jj)
!   END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Get2SpaceTimeValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetValueMethods
