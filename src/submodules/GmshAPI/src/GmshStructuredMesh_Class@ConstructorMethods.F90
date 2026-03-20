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

SUBMODULE(GmshStructuredMesh_Class) ConstructorMethods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                  SetGmshStructuredMeshParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Set parameter

SUBROUTINE SetGmshStructuredMeshParam1( &
  & param,  &
  & filename, &
  & pointsOnAxis1,  &
  & transfinitePointsOnAxis1,  &
  & pointsOnAxis2,  &
  & transfinitePointsOnAxis2,  &
  & pointsOnAxis3,  &
  & transfinitePointsOnAxis3,  &
  & recombineAll,  &
  & meshTypeOnAxis1,  &
  & meshTypeOnAxis2,  &
  & meshTypeOnAxis3,  &
  & coefOnAxis1,  &
  & coefOnAxis2,  &
  & coefOnAxis3)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  !! spatial dimension
  CHARACTER(*), INTENT(IN) :: filename
  !! name of the mesh file to be generated
  REAL(DFP), INTENT(IN) :: pointsOnAxis1(:, :)
  !! points on axis 1
  INTEGER(I4B), INTENT(IN) :: transfinitePointsOnAxis1(:)
  !! transfinitePoints on axis 1
  REAL(DFP), INTENT(IN) :: pointsOnAxis2(:, :)
  !! points on axis 2
  INTEGER(I4B), INTENT(IN) :: transfinitePointsOnAxis2(:)
  !! transfinitePoints on axis 2
  REAL(DFP), OPTIONAL, INTENT(IN) :: pointsOnAxis3(:, :)
  !! points on axis 3
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: transfinitePointsOnAxis3(:)
  !! transfinitePoints on axis 3
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: recombineAll
  !! If true we combine triangle and tetrahedron into quad and hexahedron
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis1(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis2(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis3(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: coefOnAxis1(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: coefOnAxis2(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: coefOnAxis3(:)

  ! internal variables
  INTEGER(I4B) :: tPoints(3), tVolumes, aint, bint, nsd, ii
  REAL(DFP), ALLOCATABLE :: pointsOnAxis3_(:, :)
  REAL(DFP), ALLOCATABLE :: coefOnAxis1_(:)
  REAL(DFP), ALLOCATABLE :: coefOnAxis2_(:)
  REAL(DFP), ALLOCATABLE :: coefOnAxis3_(:)
  INTEGER(I4B), ALLOCATABLE :: meshTypeOnAxis1_(:)
  INTEGER(I4B), ALLOCATABLE :: meshTypeOnAxis2_(:)
  INTEGER(I4B), ALLOCATABLE :: meshTypeOnAxis3_(:)
  INTEGER(I4B), ALLOCATABLE :: transfinitePointsOnAxis3_(:)
  REAL(DFP), PARAMETER :: r2type(1, 1) = 0, r1type(1) = 0.0_DFP
  INTEGER(I4B), PARAMETER :: i1type(1) = 0
  CHARACTER(*), PARAMETER :: myName = "SetGmshStructuredMeshParam1()"
  LOGICAL(LGT) :: recombineAll_, is3present

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

  tPoints = 1
  tPoints(1) = SIZE(pointsOnAxis1, 2)
  tPoints(2) = SIZE(pointsOnAxis2, 2)

  is3present = PRESENT(pointsOnAxis3)
  IF (is3present) THEN
    nsd = 3
    aint = SIZE(pointsOnAxis3, 1)
    IF (aint .NE. 3_I4B) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis3, 1) should be 3'//  &
        & ' but it is '//tostring(aint))
    END IF

    IF (.NOT. PRESENT(transfinitePointsOnAxis3)) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: transfinitePointsOnAxis3 should be present '// &
        & 'when pointsOnAxis3 are present.')
    END IF

    aint = SIZE(pointsOnAxis3, 2)
    bint = SIZE(transfinitePointsOnAxis3)
    IF (aint .NE. bint + 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis3, 2) should be same as '//  &
        & ' number of transfinitePointsOnAxis3 + 1'//  &
        & ' but '//tostring(aint)//'!='//tostring(bint + 1))
    END IF

    tPoints(3) = SIZE(pointsOnAxis3, 2)
    pointsOnAxis3_ = pointsOnAxis3
    transfinitePointsOnAxis3_ = transfinitePointsOnAxis3
  ELSE
    nsd = 2
    aint = SIZE(pointsOnAxis1, 1)
    CALL Reallocate(pointsOnAxis3_, aint, 1_I4B)
    CALL Reallocate(transfinitePointsOnAxis3_, 1_I4B)
    pointsOnAxis3_(1:aint, 1) = pointsOnAxis1(1:aint, 1)
    transfinitePointsOnAxis3_(1) = 1
    tPoints(3) = 1
  END IF

  IF (PRESENT(meshTypeOnAxis1)) THEN
    aint = SIZE(pointsOnAxis1, 2)
    bint = SIZE(meshTypeOnAxis1)
    IF (aint .NE. bint + 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis1, 2) should be same as '//  &
        & ' number of size(meshTypeOnAxis1) + 1'//  &
        & ' but '//tostring(aint)//'!='//tostring(bint + 1))
    END IF
    meshTypeOnAxis1_ = meshTypeOnAxis1
  ELSE
    CALL Reallocate(meshTypeOnAxis1_, tPoints(1) - 1)
    meshTypeOnAxis1_ = Progression
  END IF

  IF (PRESENT(meshTypeOnAxis2)) THEN
    aint = SIZE(pointsOnAxis2, 2)
    bint = SIZE(meshTypeOnAxis2)
    IF (aint .NE. bint + 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis2, 2) should be same as '//  &
        & ' number of size(meshTypeOnAxis2) + 1'//  &
        & ' but '//tostring(aint)//'!='//tostring(bint + 1))
    END IF
    meshTypeOnAxis2_ = meshTypeOnAxis2
  ELSE
    CALL Reallocate(meshTypeOnAxis2_, tPoints(2) - 1)
    meshTypeOnAxis2_ = Progression
  END IF

  IF (PRESENT(meshTypeOnAxis3)) THEN
    aint = SIZE(pointsOnAxis3, 2)
    bint = SIZE(meshTypeOnAxis3)
    IF (aint .NE. bint + 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis3, 2) should be same as '//  &
        & ' number of size(meshTypeOnAxis3) + 1'//  &
        & ' but '//tostring(aint)//'!='//tostring(bint + 1))
    END IF
    meshTypeOnAxis3_ = meshTypeOnAxis3
  ELSE
    CALL Reallocate(meshTypeOnAxis3_, MAX(tPoints(3) - 1, 1))
    DO ii = 1, SIZE(meshTypeOnAxis3_)
      meshTypeOnAxis3_(ii) = Progression
    END DO
  END IF

  IF (PRESENT(coefOnAxis1)) THEN
    aint = SIZE(pointsOnAxis1, 2)
    bint = SIZE(coefOnAxis1)
    IF (aint .NE. bint + 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis1, 2) should be same as '//  &
        & ' number of size(coefOnAxis1) + 1'//  &
        & ' but '//tostring(aint)//'!='//tostring(bint + 1))
    END IF
    coefOnAxis1_ = coefOnAxis1
  ELSE
    CALL Reallocate(coefOnAxis1_, tPoints(1) - 1)
    coefOnAxis1_ = 1.0_DFP
  END IF

  IF (PRESENT(coefOnAxis2)) THEN
    aint = SIZE(pointsOnAxis2, 2)
    bint = SIZE(coefOnAxis2)
    IF (aint .NE. bint + 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis2, 2) should be same as '//  &
        & ' number of size(coefOnAxis2) + 1'//  &
        & ' but '//tostring(aint)//'!='//tostring(bint + 1))
    END IF
    coefOnAxis2_ = coefOnAxis2
  ELSE
    CALL Reallocate(coefOnAxis2_, tPoints(2) - 1)
    coefOnAxis2_ = 1.0_DFP
  END IF

  IF (PRESENT(coefOnAxis3)) THEN
    aint = SIZE(pointsOnAxis3, 2)
    bint = SIZE(coefOnAxis3)
    IF (aint .NE. bint + 1) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: size(pointsOnAxis3, 2) should be same as '//  &
        & ' number of size(coefOnAxis3) + 1'//  &
        & ' but '//tostring(aint)//'!='//tostring(bint + 1))
    END IF
    coefOnAxis3_ = coefOnAxis3
  ELSE
    CALL Reallocate(coefOnAxis3_, MAX(tPoints(3) - 1, 1))
    coefOnAxis3_ = 1.0_DFP
  END IF

  recombineAll_ = input(option=recombineAll, default=.TRUE.)
  CALL Set(obj=param, datatype=.TRUE., prefix=myprefix, key="recombineAll",  &
    & VALUE=recombineAll_)

  CALL Set(obj=param, datatype="char", prefix=myprefix, key="filename",  &
    & VALUE=filename)

  CALL Set(obj=param, datatype=TypeIntI4B, prefix=myprefix, key="nsd",  &
    & VALUE=nsd)

  tVolumes = MAX(tPoints(1) - 1, 1_I4B) * MAX(tPoints(2) - 1, 1_I4B) *   &
    & MAX(tPoints(3) - 1, 1_I4B)

  CALL Set(obj=param, datatype=1_I4B, prefix=myprefix, key="tVolumes",  &
    & VALUE=tVolumes)

  CALL Set(obj=param, datatype=[1_I4B], prefix=myprefix, key="tPoints",  &
    & VALUE=tPoints)

  CALL Set(obj=param, datatype=r2type, prefix=myprefix, &
    &  key="pointsOnAxis1", VALUE=pointsOnAxis1)

  CALL Set(obj=param, datatype=r2type, prefix=myprefix, &
    &  key="pointsOnAxis2", VALUE=pointsOnAxis2)

  CALL Set(obj=param, datatype=r2type, prefix=myprefix, &
    &  key="pointsOnAxis3", VALUE=pointsOnAxis3_)

  CALL Set(obj=param, datatype=i1type, prefix=myprefix, &
    &  key="transfinitePointsOnAxis1", VALUE=transfinitePointsOnAxis1)

  CALL Set(obj=param, datatype=i1type, prefix=myprefix, &
    &  key="transfinitePointsOnAxis2", VALUE=transfinitePointsOnAxis2)

  CALL Set(obj=param, datatype=i1type, prefix=myprefix, &
    &  key="transfinitePointsOnAxis3", VALUE=transfinitePointsOnAxis3_)

  CALL Set(obj=param, datatype=i1type, prefix=myprefix, &
    &  key="meshTypeOnAxis1", VALUE=meshTypeOnAxis1_)

  CALL Set(obj=param, datatype=i1type, prefix=myprefix, &
    &  key="meshTypeOnAxis2", VALUE=meshTypeOnAxis2_)

  CALL Set(obj=param, datatype=i1type, prefix=myprefix, &
    &  key="meshTypeOnAxis3", VALUE=meshTypeOnAxis3_)

  CALL Set(obj=param, datatype=r1type, prefix=myprefix, &
    &  key="coefOnAxis1", VALUE=coefOnAxis1_)

  CALL Set(obj=param, datatype=r1type, prefix=myprefix, &
    &  key="coefOnAxis2", VALUE=coefOnAxis2_)

  CALL Set(obj=param, datatype=r1type, prefix=myprefix, &
    &  key="coefOnAxis3", VALUE=coefOnAxis3_)

  IF (ALLOCATED(pointsOnAxis3_)) DEALLOCATE (pointsOnAxis3_)
  IF (ALLOCATED(coefOnAxis1_)) DEALLOCATE (coefOnAxis1_)
  IF (ALLOCATED(coefOnAxis2_)) DEALLOCATE (coefOnAxis2_)
  IF (ALLOCATED(coefOnAxis3_)) DEALLOCATE (coefOnAxis3_)

  IF (ALLOCATED(meshTypeOnAxis1_)) DEALLOCATE (meshTypeOnAxis1_)
  IF (ALLOCATED(meshTypeOnAxis2_)) DEALLOCATE (meshTypeOnAxis2_)
  IF (ALLOCATED(meshTypeOnAxis3_)) DEALLOCATE (meshTypeOnAxis3_)

  IF (ALLOCATED(transfinitePointsOnAxis3_))  &
    & DEALLOCATE (transfinitePointsOnAxis3_)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE SetGmshStructuredMeshParam1

!----------------------------------------------------------------------------
!                                               SetGmshStructuredMeshParam
!----------------------------------------------------------------------------

SUBROUTINE SetGmshStructuredMeshParam2( &
  & param, filename, pointsOnAxis1, transfinitePointsOnAxis1,  &
  & pointsOnAxis2, transfinitePointsOnAxis2, pointsOnAxis3,  &
  & transfinitePointsOnAxis3, recombineAll, meshTypeOnAxis1,  &
  & meshTypeOnAxis2, meshTypeOnAxis3, coefOnAxis1, coefOnAxis2,  &
  & coefOnAxis3)
  TYPE(ParameterList_), INTENT(INOUT) :: param
  !! spatial dimension
  CHARACTER(*), INTENT(IN) :: filename
  !! name of the mesh file to be generated
  REAL(DFP), INTENT(IN) :: pointsOnAxis1(:)
  !! points on axis 1
  INTEGER(I4B), INTENT(IN) :: transfinitePointsOnAxis1(:)
  !! transfinitePoints on axis 1
  REAL(DFP), INTENT(IN) :: pointsOnAxis2(:)
  !! points on axis 2
  INTEGER(I4B), INTENT(IN) :: transfinitePointsOnAxis2(:)
  !! transfinitePoints on axis 2
  REAL(DFP), OPTIONAL, INTENT(IN) :: pointsOnAxis3(:)
  !! points on axis 3
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: transfinitePointsOnAxis3(:)
  !! transfinitePoints on axis 3
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: recombineAll
  !! If true we combine triangle and tetrahedron into quad and hexahedron
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis1(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis2(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis3(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: coefOnAxis1(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: coefOnAxis2(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: coefOnAxis3(:)

  CHARACTER(*), PARAMETER :: myName = "SetGmshStructuredMeshParam2()"
  LOGICAL(LGT) :: is3present
  INTEGER(I4B) :: tsize
  REAL(DFP), ALLOCATABLE :: p1(:, :), p2(:, :), p3(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif

  is3present = PRESENT(pointsOnAxis3)

  tsize = SIZE(pointsOnAxis1)
  CALL Reallocate(p1, 3, tsize)
  p1(1, :) = pointsOnAxis1

  tsize = SIZE(pointsOnAxis2)
  CALL Reallocate(p2, 3, tsize)
  p2(2, :) = pointsOnAxis2

  IF (is3present) THEN
    tsize = SIZE(pointsOnAxis3)
    CALL Reallocate(p3, 3, tsize)
    p3(3, :) = pointsOnAxis3
  END IF

  IF (is3present) THEN
    CALL SetGmshStructuredMeshParam1(param=param, filename=filename,  &
      & pointsOnAxis1=p1, pointsOnAxis2=p2, pointsOnAxis3=p3, &
      & transfinitePointsOnAxis1=transfinitePointsOnAxis1,  &
      & transfinitePointsOnAxis2=transfinitePointsOnAxis2,  &
      & transfinitePointsOnAxis3=transfinitePointsOnAxis3,  &
      & recombineAll=recombineAll,  &
      & meshTypeOnAxis1=meshTypeOnAxis1,  &
      & meshTypeOnAxis2=meshTypeOnAxis2,  &
      & meshTypeOnAxis3=meshTypeOnAxis3,  &
      & coefOnAxis1=coefOnAxis1,  &
      & coefOnAxis2=coefOnAxis2,  &
      & coefOnAxis3=coefOnAxis3)

  ELSE
    CALL SetGmshStructuredMeshParam1(param=param, filename=filename,  &
      & pointsOnAxis1=p1, pointsOnAxis2=p2,  &
      & transfinitePointsOnAxis1=transfinitePointsOnAxis1,  &
      & transfinitePointsOnAxis2=transfinitePointsOnAxis2,  &
      & recombineAll=recombineAll,  &
      & meshTypeOnAxis1=meshTypeOnAxis1,  &
      & meshTypeOnAxis2=meshTypeOnAxis2,  &
      & coefOnAxis1=coefOnAxis1,  &
      & coefOnAxis2=coefOnAxis2)
  END IF

  IF (ALLOCATED(p1)) DEALLOCATE (p1)
  IF (ALLOCATED(p2)) DEALLOCATE (p2)
  IF (ALLOCATED(p3)) DEALLOCATE (p3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif

END SUBROUTINE SetGmshStructuredMeshParam2

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "mesh_Initiate1()"
#endif
INTEGER(I4B) :: ii, jj, kk, ipoint, iedge, lineLoop(4), tedges(3), &
                isurface, te(3), p(2), tsurfaces(3), iVolume, &
                surfaceLoops(6)
REAL(DFP), ALLOCATABLE :: pointsOnAxis1_(:, :), pointsOnAxis2_(:, :), &
                          pointsOnAxis3_(:, :), coefOnAxis1(:), &
                          coefOnAxis2(:), coefOnAxis3(:), &
                          dummy_real_r2(:, :)
INTEGER(I4B), ALLOCATABLE :: meshTypeOnAxis1(:), meshTypeOnAxis2(:), &
                             meshTypeOnAxis3(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

#ifdef DEBUG_VER
ii = SIZE(pointsOnAxis1, 1)
CALL AssertError2(ii, math%three_i, myName, &
                  "a=dim1 of pointsOnAxis1, b=3")
#endif

#ifdef DEBUG_VER
ii = SIZE(pointsOnAxis1, 2)
jj = 1 + SIZE(transfinitePointsOnAxis1)
CALL AssertError2(ii, jj, myName, &
             "a=dim2 of pointsOnAxis1, &
            &b=1+dim1 of transfinitePointsOnAxis1,")
#endif

#ifdef DEBUG_VER
ii = SIZE(pointsOnAxis2, 1)
CALL AssertError2(ii, math%three_i, myName, &
                  "a=dim1 of pointsOnAxis2, b=3,")
#endif

#ifdef DEBUG_VER
ii = SIZE(pointsOnAxis2, 2)
jj = 1 + SIZE(transfinitePointsOnAxis2)
CALL AssertError2(ii, jj, myName, &
             "a=dim2 of pointsOnAxis2, &
            &b=1 + size of transfinitePointsOnAxis2")
#endif

CALL GetValue(obj=param, prefix=myprefix, key="recombineAll", &
              VALUE=obj%recombineAll)

CALL GetValue(obj=param, prefix=myprefix, key="filename",  &
  & VALUE=obj%filename)

CALL GetValue(obj=param, prefix=myprefix, key="tVolumes",  &
  & VALUE=obj%tVolumes)

CALL GetValue(obj=param, prefix=myprefix, key="tPoints",  &
  & VALUE=obj%tPoints)

CALL GetValue(obj=param, prefix=myprefix, key="nsd",  &
  & VALUE=obj%nsd)

te = obj%tPoints - 1

DO ii = 1, 3
  CALL Initiate(obj%points(ii), 3_I4B, obj%tPoints(ii))
  CALL GetValue(obj=param, prefix=myprefix,  &
    & key="pointsOnAxis"//tostring(ii), VALUE=obj%points(ii))
END DO

ALLOCATE (pointsOnAxis1(3, 0:obj%tPoints(1)))
ALLOCATE (pointsOnAxis2(3, 0:obj%tPoints(2)))
ALLOCATE (pointsOnAxis3(3, 0:obj%tPoints(3)))
pointsOnAxis1 = 0.0_DFP
pointsOnAxis2 = 0.0_DFP
pointsOnAxis3 = 0.0_DFP

pointsOnAxis1(1:3, 1:) = Get(obj%points(1))
pointsOnAxis2(1:3, 1:) = Get(obj%points(2))
pointsOnAxis3(1:3, 1:) = Get(obj%points(3))

ipoint = obj%tPoints(1) * obj%tPoints(2) * obj%tPoints(3)
CALL Reallocate(obj%allPoints, 3_I4B, ipoint)

ipoint = 0
DO kk = 1, obj%tpoints(3)
  DO jj = 1, obj%tPoints(2)
    DO ii = 1, obj%tPoints(1)
      ipoint = ipoint + 1
      ! obj%allPoints(:, ipoint) = [pointsOnAxis1(1, ii),  &
      !   & pointsOnAxis2(2, jj),  &
      !   & pointsOnAxis3(3, kk)]
      obj%allPoints(:, ipoint) = pointsOnAxis1(:, ii - 1) &
        & + pointsOnAxis1(:, ii) - pointsOnAxis1(:, ii - 1) &
        & + pointsOnAxis2(:, jj) - pointsOnAxis2(:, jj - 1) &
        & + pointsOnAxis3(:, kk) - pointsOnAxis3(:, kk - 1)
    END DO
  END DO
END DO

CALL Reallocate(meshTypeOnAxis1, MAX(te(1), 1_I4B))
CALL Reallocate(meshTypeOnAxis2, MAX(te(2), 1_I4B))
CALL Reallocate(meshTypeOnAxis3, MAX(te(3), 1_I4B))

CALL Reallocate(coefOnAxis1, MAX(te(1), 1_I4B))
CALL Reallocate(coefOnAxis2, MAX(te(2), 1_I4B))
CALL Reallocate(coefOnAxis3, MAX(te(3), 1_I4B))

CALL GetValue(obj=param, prefix=myprefix,  &
  & key="meshTypeOnAxis1", VALUE=meshTypeOnAxis1)

CALL GetValue(obj=param, prefix=myprefix,  &
  & key="meshTypeOnAxis2", VALUE=meshTypeOnAxis2)

CALL GetValue(obj=param, prefix=myprefix,  &
  & key="meshTypeOnAxis3", VALUE=meshTypeOnAxis3)

CALL GetValue(obj=param, prefix=myprefix,  &
  & key="coefOnAxis1", VALUE=coefOnAxis1)

CALL GetValue(obj=param, prefix=myprefix,  &
  & key="coefOnAxis2", VALUE=coefOnAxis2)

CALL GetValue(obj=param, prefix=myprefix,  &
  & key="coefOnAxis3", VALUE=coefOnAxis3)

DO ii = 1, 3
  CALL Initiate(obj%transfinitePoints(ii),  &
    & MAX(te(ii), 1_I4B))
  CALL GetValue(obj=param, prefix=myprefix,  &
    & key="transfinitePointsOnAxis"//tostring(ii),  &
    & VALUE=obj%transfinitePoints(ii))
END DO

tedges(1) = te(1) * obj%tPoints(2) * obj%tPoints(3)
tedges(2) = te(2) * obj%tPoints(3) * obj%tPoints(1)
tedges(3) = te(3) * obj%tPoints(2) * obj%tPoints(1)

obj%tEdges1 = tedges(1)
obj%tEdges2 = tedges(2)
obj%tEdges3 = tedges(3)

obj%tEdges = tedges(1) + tedges(2) + tedges(3)

CALL Reallocate(obj%edges, 2, obj%tEdges)
CALL Reallocate(obj%edge_tfp, obj%tEdges)
CALL Reallocate(obj%edge_meshType, obj%tEdges)
CALL Reallocate(obj%edge_coef, obj%tEdges)

! edges parallel to axis 1
iedge = 0
DO kk = 1, obj%tPoints(3)
  DO jj = 1, obj%tPoints(2)
    DO ii = 1, obj%tPoints(1) - 1
      p(1) = obj%GetNodeNumber(ii, jj, kk)
      p(2) = obj%GetNodeNumber(ii + 1, jj, kk)
      iedge = iedge + 1
      obj%edges(:, iedge) = p
      obj%edge_tfp(iedge) = Get(obj%transfinitePoints(1), ii, 1_I4B)
      obj%edge_coef(iedge) = coefOnAxis1(ii)
      obj%edge_meshType(iedge) = MeshTypeName(meshTypeOnAxis1(ii))
    END DO
  END DO
END DO

! edges parallel to axis 2
DO kk = 1, obj%tPoints(3)
  DO jj = 1, obj%tPoints(2) - 1
    DO ii = 1, obj%tPoints(1)
      p(1) = obj%GetNodeNumber(ii, jj, kk)
      p(2) = obj%GetNodeNumber(ii, jj + 1, kk)
      iedge = iedge + 1
      obj%edges(:, iedge) = p
      obj%edge_tfp(iedge) = Get(obj%transfinitePoints(2), jj, 1_I4B)
      obj%edge_coef(iedge) = coefOnAxis2(jj)
      obj%edge_meshType(iedge) = MeshTypeName(meshTypeOnAxis2(jj))
    END DO
  END DO
END DO

! edges parallel to axis 3
DO kk = 1, obj%tPoints(3) - 1
  DO jj = 1, obj%tPoints(2)
    DO ii = 1, obj%tPoints(1)
      p(1) = obj%GetNodeNumber(ii, jj, kk)
      p(2) = obj%GetNodeNumber(ii, jj, kk + 1)
      iedge = iedge + 1
      obj%edges(:, iedge) = p
      obj%edge_tfp(iedge) = Get(obj%transfinitePoints(3), kk, 1_I4B)
      obj%edge_coef(iedge) = coefOnAxis3(kk)
      obj%edge_meshType(iedge) = MeshTypeName(meshTypeOnAxis3(kk))
    END DO
  END DO
END DO

! xy
tsurfaces(1) = te(1) * te(2) * obj%tPoints(3)
! yz
tsurfaces(2) = te(2) * te(3) * obj%tPoints(1)
! xz
tsurfaces(3) = te(1) * te(3) * obj%tPoints(2)

obj%tSurfacesXY = tsurfaces(1)
obj%tSurfacesYZ = tsurfaces(2)
obj%tSurfacesXZ = tsurfaces(3)

obj%tSurfaces = tsurfaces(1) + tsurfaces(2) + tsurfaces(3)

CALL Reallocate(obj%curveLoops, 4, obj%tSurfaces)

isurface = 0
! xy (1-2)
DO kk = 1, obj%tPoints(3)
  DO jj = 1, obj%tPoints(2) - 1
    DO ii = 1, obj%tPoints(1) - 1
      isurface = isurface + 1
      lineLoop(1) = obj%GetEdgeNumberOnAxis1(ii, jj, kk)
      lineLoop(3) = obj%GetEdgeNumberOnAxis1(ii, jj + 1, kk)
      lineLoop(2) = obj%GetEdgeNumberOnAxis2(ii + 1, jj, kk)
      lineLoop(4) = obj%GetEdgeNumberOnAxis2(ii, jj, kk)
      obj%curveLoops(:, isurface) = [lineLoop(1), lineLoop(2), &
        & -lineLoop(3), -lineLoop(4)]
    END DO
  END DO
END DO

! yz (2-3)
DO ii = 1, obj%tPoints(1)
  DO kk = 1, obj%tPoints(3) - 1
    DO jj = 1, obj%tPoints(2) - 1
      isurface = isurface + 1
      lineLoop(1) = obj%GetEdgeNumberOnAxis2(ii, jj, kk)
      lineLoop(3) = obj%GetEdgeNumberOnAxis2(ii, jj, kk + 1)
      lineLoop(2) = obj%GetEdgeNumberOnAxis3(ii, jj + 1, kk)
      lineLoop(4) = obj%GetEdgeNumberOnAxis3(ii, jj, kk)
      obj%curveLoops(:, isurface) = [lineLoop(1), lineLoop(2), &
        & -lineLoop(3), -lineLoop(4)]
    END DO
  END DO
END DO

! xz (1-3)
DO jj = 1, obj%tPoints(2)
  DO kk = 1, obj%tPoints(3) - 1
    DO ii = 1, obj%tPoints(1) - 1
      isurface = isurface + 1

      lineLoop(1) = obj%GetEdgeNumberOnAxis1(ii, jj, kk)
      lineLoop(3) = obj%GetEdgeNumberOnAxis1(ii, jj, kk + 1)
      lineLoop(2) = obj%GetEdgeNumberOnAxis3(ii + 1, jj, kk)
      lineLoop(4) = obj%GetEdgeNumberOnAxis3(ii, jj, kk)

      obj%curveLoops(:, isurface) = [lineLoop(1), lineLoop(2), &
        & -lineLoop(3), -lineLoop(4)]
    END DO
  END DO
END DO

! making volumes
IF (obj%tVolumes .GT. 0_I4B) THEN
  CALL Reallocate(obj%surfaceLoops, 6_I4B, obj%tVolumes)
ELSE
  CALL Reallocate(obj%surfaceLoops, 0_I4B, 0_I4B)
END IF

iVolume = 0_I4B
DO kk = 1, obj%tPoints(3) - 1
  DO jj = 1, obj%tPoints(2) - 1
    DO ii = 1, obj%tPoints(1) - 1
      iVolume = iVolume + 1
      surfaceLoops(1) = obj%GetSurfaceNumberXY(ii, jj, kk)
      surfaceLoops(2) = obj%GetSurfaceNumberXY(ii, jj, kk + 1)

      surfaceLoops(3) = obj%GetSurfaceNumberYZ(ii, jj, kk)
      surfaceLoops(4) = obj%GetSurfaceNumberYZ(ii + 1, jj, kk)

      surfaceLoops(5) = obj%GetSurfaceNumberXZ(ii, jj, kk)
      surfaceLoops(6) = obj%GetSurfaceNumberXZ(ii, jj + 1, kk)
      obj%surfaceLoops(:, iVolume) = surfaceLoops
    END DO
  END DO
END DO

IF (ALLOCATED(pointsOnAxis1)) DEALLOCATE (pointsOnAxis1)
IF (ALLOCATED(pointsOnAxis2)) DEALLOCATE (pointsOnAxis2)
IF (ALLOCATED(pointsOnAxis3)) DEALLOCATE (pointsOnAxis3)

IF (ALLOCATED(coefOnAxis1)) DEALLOCATE (coefOnAxis1)
IF (ALLOCATED(coefOnAxis2)) DEALLOCATE (coefOnAxis2)
IF (ALLOCATED(coefOnAxis3)) DEALLOCATE (coefOnAxis3)

IF (ALLOCATED(meshTypeOnAxis1)) DEALLOCATE (meshTypeOnAxis1)
IF (ALLOCATED(meshTypeOnAxis2)) DEALLOCATE (meshTypeOnAxis2)
IF (ALLOCATED(meshTypeOnAxis3)) DEALLOCATE (meshTypeOnAxis3)

END PROCEDURE mesh_Initiate1

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Deallocate data

SUBROUTINE mesh_Deallocate(obj)
  CLASS(GmshStructuredMesh_), INTENT(INOUT) :: obj
  INTEGER(I4B) :: ii

  DO ii = 1, SIZE(obj%points)
    CALL DEALLOCATE (obj%points(ii))
  END DO

  DO ii = 1, 3
    CALL DEALLOCATE (obj%transfinitePoints(ii))
  END DO
  obj%tPoints = 0

  IF (ALLOCATED(obj%allPoints)) DEALLOCATE (obj%allPoints)

  IF (ALLOCATED(obj%edges)) DEALLOCATE (obj%edges)
  IF (ALLOCATED(obj%edge_tfp)) DEALLOCATE (obj%edge_tfp)
  IF (ALLOCATED(obj%edge_coef)) DEALLOCATE (obj%edge_coef)
  IF (ALLOCATED(obj%edge_meshType)) DEALLOCATE (obj%edge_meshType)
  obj%tEdges = 0

  IF (ALLOCATED(obj%curveLoops)) DEALLOCATE (obj%curveLoops)
  obj%tSurfaces = 0

END SUBROUTINE mesh_Deallocate

!----------------------------------------------------------------------------
!                                                           Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
