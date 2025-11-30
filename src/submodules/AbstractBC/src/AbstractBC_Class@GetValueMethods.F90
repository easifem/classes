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
USE BaseType, ONLY: ElemShapeData_, QuadraturePoint_
USE BaseType, ONLY: math => TypeMathOpt
USE ElemShapeData_Method, ONLY: ElemShapeData_Deallocate => DEALLOCATE
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Deallocate => DEALLOCATE

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get()"
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
  CALL CheckError_uf(obj, myName, times)
#endif

  CALL GetWithUserFunction( &
    obj=obj, fedof=fedof, geofedof=geofedof, nodenum=nodenum, &
    nodalValue=nodalValue, nrow=nrow, ncol=ncol, times=times)

ELSE

  CALL GetWithoutUserFunction( &
    obj=obj, fedof=fedof, geofedof=geofedof, nodenum=nodenum, &
    nodalValue=nodalValue, nrow=nrow, ncol=ncol, times=times)

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Get

!----------------------------------------------------------------------------
!                                                      GetWithoutUserFunction
!----------------------------------------------------------------------------

SUBROUTINE GetWithoutUserFunction(obj, fedof, geofedof, nodeNum, nodalValue, &
                                  nrow, ncol, times)
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
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! times vector is only used when usefunction is true in obj

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetWithoutUserFunction()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  SELECT CASE (obj%nodalValueType)

  CASE (TypeFEVariableOpt%constant)

    CALL GetConstantValue( &
      obj=obj, fedof=fedof, geofedof=geofedof, nodenum=nodenum, &
      nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, times=times)

  CASE (TypeFEVariableOpt%space)

    CALL GetSpaceValue( &
      obj=obj, fedof=fedof, geofedof=geofedof, nodenum=nodenum, &
      nodalvalue=nodalvalue, nrow=nrow, ncol=ncol, times=times)

  CASE (TypeFEVariableOpt%time)
    CALL GetTimeValue( &
      obj=obj, fedof=fedof, geofedof=geofedof, nodenum=nodenum, &
      nodalvalue=nodalvalue, nrow=nrow, ncol=ncol)

  CASE (TypeFEVariableOpt%spaceTime)
    CALL GetSpaceTimeValue( &
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
END SUBROUTINE GetWithoutUserFunction

!----------------------------------------------------------------------------
!                                                        GetWithUserFunction
!----------------------------------------------------------------------------

SUBROUTINE GetWithUserFunction(obj, fedof, geofedof, nodeNum, nodalValue, &
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
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  !! time value
  !! time values are needed when userfunction is time or space-time
  INTEGER(I4B) :: nrow, ncol
  !! nrow is size of nodeNum and size of number of rows in nodalvalue
  !! ncol is colsize of nodalvalue

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetWithUserFunction()"
#endif

  LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.

  INTEGER(I4B) :: iel, localFaceNumber, localCellNumber, mysize, &
                  iNodeOnNode, iNodeOnFace, iNodeOnEdge, ii, jj, indx(1), &
                  localEdgeNumber, xij_i, xij_j, max_fedof_con, &
                  max_fedof_quad, itimes, max_geofedof_con

  REAL(DFP) :: args(4)
  REAL(DFP), ALLOCATABLE :: massMat(:, :), funcValue(:), ans(:), times0(:), &
                            xij(:, :)
  INTEGER(I4B), ALLOCATABLE :: ipiv(:)

  LOGICAL(LGT) :: isok, istimes
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

  istimes = PRESENT(times)
  IF (istimes) THEN
    ncol = SIZE(times)
    CALL Reallocate(times0, ncol)
    times0(1:ncol) = times(1:ncol)
  ELSE
    CALL Reallocate(times0, ncol)
  END IF

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
      globalNode=nodeNum(ii), ans=indx, islocal=no, tsize=jj)
    nodeNum(ii) = indx(1)
  END DO

  cellMesh => obj%dom%GetMeshPointer()

  args = 0.0_DFP
  DO ii = 1, nrow
    CALL cellMesh%GetNodeCoord( &
      nodeCoord=args, tsize=jj, globalNode=nodeNum(ii), islocal=yes)

    DO itimes = 1, ncol
      args(4) = times0(itimes)
      CALL obj%func%Get(val=ans(1), args=args)
      nodalValue(ii, itimes) = ans(1)
    END DO

  END DO

  CALL obj%SetElemToLocalBoundary()

  ! Face nodes and values
  DO iel = 1, obj%tElemToFace

    CALL obj%GetElemToFace(indx=iel, localFaceNumber=localFaceNumber, &
                           localCellNumber=localCellNumber)

    CALL fedof%SetFE(globalElement=localCellNumber, islocal=yes)
    feptr => fedof%GetFEPointer(globalElement=localCellNumber, islocal=yes)

    CALL geofedof%SetFE(globalElement=localCellNumber, islocal=yes)
    geofeptr => geofedof%GetFEPointer(globalElement=localCellNumber, &
                                      islocal=yes)

    CALL cellMesh%GetNodeCoord( &
      nodeCoord=xij, nrow=xij_i, ncol=xij_j, globalElement=localCellNumber, &
      islocal=yes)

    CALL feptr%GetGlobalFacetElemShapeData2( &
      geofeptr=geofeptr, elemsd=elemsd, facetElemsd=facetElemsd, &
      geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd, &
      localFaceNumber=localFaceNumber, quad=quad, facetQuad=facetQuad, &
      xij=xij)

    DO itimes = 1, ncol
      CALL feptr%GetFacetDOFValue( &
        elemsd=elemsd, facetElemsd=facetElemsd, xij=xij, &
        times=times0(itimes), localFaceNumber=localFaceNumber, &
        func=obj%func, ans=ans, tsize=jj, &
        massMat=massMat, ipiv=ipiv, funcValue=funcValue, &
        onlyFaceBubble=yes)
      nodalValue(nrow + 1:nrow + jj, itimes) = ans(1:jj)
    END DO

    CALL fedof%GetFaceDOF( &
      globalElement=localCellNumber, localFaceNumber=localFaceNumber, &
      ans=nodeNum(nrow + 1:), tsize=mysize, islocal=yes)

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
  isok = obj%tElemToEdge .EQ. 0
  CALL AssertError1(isok, myName, &
                   "Edge DOF extraction is not implemented for userfunction.")
#endif

  IF (ALLOCATED(massMat)) DEALLOCATE (massMat)
  IF (ALLOCATED(funcValue)) DEALLOCATE (funcValue)
  IF (ALLOCATED(ans)) DEALLOCATE (ans)
  IF (ALLOCATED(times0)) DEALLOCATE (times0)
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
END SUBROUTINE GetWithUserFunction

!----------------------------------------------------------------------------
!                                                          GetConstantValue
!----------------------------------------------------------------------------

SUBROUTINE GetConstantValue(obj, fedof, geofedof, nodeNum, nodalValue, &
                            nrow, ncol, times)
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
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  !! time values when nodalValueType is space-time or time

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetConstantValue()"
#endif

  LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.

  INTEGER(I4B) :: iel, localFaceNumber, localCellNumber, mysize, &
                  iNodeOnNode, iNodeOnFace, iNodeOnEdge, ii, jj, indx(1), &
                  localEdgeNumber, elemCoord_i, elemCoord_j, max_fedof_con, &
                  itimes

  REAL(DFP) :: elemCoord(3, 8), scalarAns
  REAL(DFP), ALLOCATABLE :: massMat(:, :), ans(:)
  INTEGER(I4B), ALLOCATABLE :: ipiv(:)

  LOGICAL(LGT) :: isok, istimes
  CLASS(AbstractMesh_), POINTER :: cellMesh
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  TYPE(ElemShapeData_) :: elemsd, facetElemsd, geoElemsd, geoFacetElemsd
  TYPE(QuadraturePoint_) :: quad, facetQuad

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  max_fedof_con = fedof%GetMaxTotalConnectivity()
  CALL Reallocate(massMat, max_fedof_con, max_fedof_con)
  CALL Reallocate(ipiv, max_fedof_con)
  CALL Reallocate(ans, max_fedof_con)

  nrow = 0
  ncol = 1
  istimes = PRESENT(times)
  IF (istimes) ncol = SIZE(times)

  scalarAns = obj%nodalValue(1, 1)

  ! Vertex nodes and values
  CALL obj%boundary%GetNodeNum(dom=obj%dom, ans=nodeNum, tsize=mysize)
  nrow = nrow + mysize
  iNodeOnNode = 1
  iNodeOnFace = nrow + 1

  DO ii = 1, nrow

    CALL fedof%GetVertexDOF(globalNode=nodeNum(ii), ans=indx, islocal=no, &
                            tsize=jj)
    nodeNum(ii) = indx(1)

    DO itimes = 1, ncol
      nodalValue(ii, itimes) = scalarAns
    END DO

  END DO

  cellMesh => obj%dom%GetMeshPointer()

  CALL obj%SetElemToLocalBoundary()

  ! Face nodes and values
  DO iel = 1, obj%tElemToFace

    CALL obj%GetElemToFace(indx=iel, localFaceNumber=localFaceNumber, &
                           localCellNumber=localCellNumber)

    CALL fedof%SetFE(globalElement=localCellNumber, islocal=yes)
    feptr => fedof%GetFEPointer(globalElement=localCellNumber, islocal=yes)

    CALL geofedof%SetFE(globalElement=localCellNumber, islocal=yes)
    geofeptr => geofedof%GetFEPointer(globalElement=localCellNumber, &
                                      islocal=yes)

    CALL feptr%GetFacetQuadraturePoints(quad=quad, facetQuad=facetQuad, &
                                        localFaceNumber=localFaceNumber)

    CALL feptr%GetLocalFacetElemShapeData( &
      elemsd=elemsd, facetElemsd=facetElemsd, quad=quad, &
      facetQuad=facetQuad, localFaceNumber=localFaceNumber)

    CALL geofeptr%GetLocalFacetElemShapeData( &
      elemsd=geoElemsd, facetElemsd=geoFacetElemsd, quad=quad, &
      facetQuad=facetQuad, localFaceNumber=localFaceNumber)

    CALL cellMesh%GetNodeCoord( &
      nodeCoord=elemCoord, nrow=elemCoord_i, ncol=elemCoord_j, islocal=yes, &
      globalElement=localCellNumber)

    CALL feptr%GetGlobalFacetElemShapeData( &
      elemsd=elemsd, facetElemsd=facetElemsd, &
      localFaceNumber=localFaceNumber, geoElemsd=geoElemsd, &
      geoFacetElemsd=geoFacetElemsd, xij=elemCoord)

    CALL feptr%GetFacetDOFValue( &
      elemsd=elemsd, facetElemsd=facetElemsd, xij=elemCoord, &
      localFaceNumber=localFaceNumber, ans=ans, &
      tsize=jj, massMat=massMat, ipiv=ipiv, onlyFaceBubble=yes, &
      tVertices=geoFacetElemsd%nns)

    DO itimes = 1, ncol
      nodalValue(nrow + 1:nrow + jj, itimes) = scalarAns * ans(1:jj)
    END DO

    CALL fedof%GetFaceDOF( &
      globalElement=localCellNumber, localFaceNumber=localFaceNumber, &
      ans=nodeNum(nrow + 1:), tsize=mysize, islocal=yes)

#ifdef DEBUG_VER
    isok = mysize .EQ. jj
    CALL AssertError1(isok, myName, &
            "Size mismatch in nodalValue which is equal to "//ToString(jj)// &
                      ", and nodeNum which is equal to "//ToString(mysize))
#endif

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
  isok = obj%tElemToEdge .EQ. 0
  CALL AssertError1(isok, myName, &
                   "Edge DOF extraction is not implemented for userfunction.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetConstantValue

!----------------------------------------------------------------------------
!                                                               GetTimeValue
!----------------------------------------------------------------------------

SUBROUTINE GetTimeValue(obj, fedof, geofedof, nodeNum, nodalValue, &
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
  CHARACTER(*), PARAMETER :: myName = "GetSpaceValue_uf()"
#endif

  LOGICAL(LGT), PARAMETER :: yes = .TRUE., no = .FALSE.

  INTEGER(I4B) :: iel, localFaceNumber, localCellNumber, mysize, &
                  iNodeOnNode, iNodeOnFace, iNodeOnEdge, ii, jj, indx(1), &
                  localEdgeNumber, elemCoord_i, elemCoord_j, max_fedof_con, &
                  itimes

  REAL(DFP) :: elemCoord(3, 8)
  REAL(DFP), ALLOCATABLE :: massMat(:, :)
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

  nrow = 0; ncol = obj%nrow

  ! Vertex nodes and values
  CALL obj%boundary%GetnodeNum(dom=obj%dom, ans=nodeNum, tsize=mysize)
  nrow = nrow + mysize
  iNodeOnNode = 1
  iNodeOnFace = nrow + 1

  DO ii = 1, nrow
    CALL fedof%GetVertexDOF(globalNode=nodeNum(ii), ans=indx, islocal=no, &
                            tsize=jj)
    nodeNum(ii) = indx(1)

    DO itimes = 1, ncol
      nodalValue(ii, itimes) = obj%nodalValue(itimes, 1)
    END DO
  END DO

  cellMesh => obj%dom%GetMeshPointer()

  CALL obj%SetElemToLocalBoundary()

  max_fedof_con = fedof%GetMaxTotalConnectivity()
  CALL Reallocate(massMat, max_fedof_con, max_fedof_con)
  CALL Reallocate(ipiv, max_fedof_con)

  ! Face nodes and values
  DO iel = 1, obj%tElemToFace

    CALL obj%GetElemToFace(indx=iel, localFaceNumber=localFaceNumber, &
                           localCellNumber=localCellNumber)

    CALL fedof%SetFE(globalElement=localCellNumber, islocal=yes)
    feptr => fedof%GetFEPointer(globalElement=localCellNumber, islocal=yes)

    CALL geofedof%SetFE(globalElement=localCellNumber, islocal=yes)
    geofeptr => geofedof%GetFEPointer(globalElement=localCellNumber, &
                                      islocal=yes)

    CALL feptr%GetFacetQuadraturePoints(quad=quad, facetQuad=facetQuad, &
                                        localFaceNumber=localFaceNumber)

    CALL feptr%GetLocalFacetElemShapeData( &
      elemsd=elemsd, facetElemsd=facetElemsd, quad=quad, &
      facetQuad=facetQuad, localFaceNumber=localFaceNumber)

    CALL geofeptr%GetLocalFacetElemShapeData( &
      elemsd=geoElemsd, facetElemsd=geoFacetElemsd, quad=quad, &
      facetQuad=facetQuad, localFaceNumber=localFaceNumber)

    CALL cellMesh%GetNodeCoord( &
      nodeCoord=elemCoord, nrow=elemCoord_i, ncol=elemCoord_j, islocal=yes, &
      globalElement=localCellNumber)

    CALL feptr%GetGlobalFacetElemShapeData( &
      elemsd=elemsd, facetElemsd=facetElemsd, &
      localFaceNumber=localFaceNumber, geoElemsd=geoElemsd, &
      geoFacetElemsd=geoFacetElemsd, xij=elemCoord)

    CALL feptr%GetFacetDOFValue( &
      elemsd=elemsd, facetElemsd=facetElemsd, xij=elemCoord, &
      localFaceNumber=localFaceNumber, ans=nodalValue(nrow + 1:, 1), &
      tsize=jj, massMat=massMat, ipiv=ipiv, onlyFaceBubble=yes, &
      tVertices=geoFacetElemsd%nns)

    DO itimes = 2, ncol
      nodalValue(nrow + 1:nrow + jj, itimes) = &
        obj%nodalValue(itimes, 1) * nodalValue(nrow + 1:nrow + jj, 1)
    END DO

    nodalValue(nrow + 1:nrow + jj, 1) = &
      obj%nodalValue(1, 1) * nodalValue(nrow + 1:nrow + jj, 1)

    CALL fedof%GetFaceDOF( &
      globalElement=localCellNumber, localFaceNumber=localFaceNumber, &
      ans=nodeNum(nrow + 1:), tsize=mysize, islocal=yes)

#ifdef DEBUG_VER
    isok = mysize .EQ. jj
    CALL AssertError1(isok, myName, &
            "Size mismatch in nodalValue which is equal to "//ToString(jj)// &
                      ", and nodeNum which is equal to "//ToString(mysize))
#endif

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
  isok = obj%tElemToEdge .EQ. 0
  CALL AssertError1(isok, myName, &
                   "Edge DOF extraction is not implemented for userfunction.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetTimeValue

!----------------------------------------------------------------------------
!                                                              GetSpaceValue
!----------------------------------------------------------------------------

SUBROUTINE GetSpaceValue(obj, fedof, geofedof, nodeNum, nodalValue, nrow, &
                         ncol, times)
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
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  !! time values, only needed when ncol > 1

  !! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetSpaceValue()"
#endif
  LOGICAL(LGT) :: isok, istimes
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

  CALL obj%GetNodeNumber(fedof=fedof, nodenum=nodeNum, tsize=nrow, &
                         iNodeOnNode=iNodeOnNode, iNodeOnFace=iNodeOnFace, &
                         iNodeOnEdge=iNodeOnEdge)

#ifdef DEBUG_VER
  isok = obj%nrow .GE. nrow
  CALL AssertError1(isok, myname, &
                    'SIZE( obj%nodalValue, 1 ) .NE. SIZE( nodeNum )')
#endif

  ncol = 1
  istimes = PRESENT(times)
  IF (istimes) ncol = SIZE(times)

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    nodalValue(ii, jj) = obj%nodalValue(ii, 1)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE GetSpaceValue

!----------------------------------------------------------------------------
!                                                         GetSpaceTimeValue
!----------------------------------------------------------------------------

SUBROUTINE GetSpaceTimeValue(obj, fedof, geofedof, nodeNum, nodalValue, &
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

  CALL obj%GetNodeNumber(fedof=fedof, nodenum=nodeNum, tsize=nrow, &
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
!                                                             CheckError
!----------------------------------------------------------------------------

SUBROUTINE CheckError(obj, myName)
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
  isok = obj%isInit
  CALL AssertError1(obj%isInit, myName, &
                    'AbstractBC_ object is not initiated, initiate it first.')
#endif

#ifdef DEBUG_VER
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
!                                                               CheckError_uf
!----------------------------------------------------------------------------

SUBROUTINE CheckError_uf(obj, myName, times)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: myName
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)

  ! internal variable
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName0 = "CheckError_uf()"
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
END SUBROUTINE CheckError_uf

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
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetValueMethods
