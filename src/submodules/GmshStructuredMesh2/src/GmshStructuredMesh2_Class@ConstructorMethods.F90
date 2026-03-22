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

SUBMODULE(GmshStructuredMesh2_Class) ConstructorMethods
USE ReallocateUtility, ONLY: Reallocate
USE InputUtility, ONLY: Input

USE RealMatrix_Method, ONLY: RealMatrixInitiate => Initiate
USE RealMatrix_Method, ONLY: RealMatrixDeallocate => DEALLOCATE
USE RealMatrix_Method, ONLY: RealMatrixGet => Get
USE RealMatrix_Method, ONLY: RealMatrixGetColumn_ => GetColumn_
USE RealMatrix_Method, ONLY: RealMatrixSet => Set

USE IntVector_Method, ONLY: IntVectorInitiate => Initiate
USE IntVector_Method, ONLY: IntVectorDeallocate => DEALLOCATE
USE IntVector_Method, ONLY: IntVectorSet => Set
USE IntVector_Method, ONLY: IntVectorGet => Get

USE RealVector_Method, ONLY: RealVectorInitiate => Initiate
USE RealVector_Method, ONLY: RealVectorDeallocate => DEALLOCATE
USE RealVector_Method, ONLY: RealVectorSet => Set
USE RealVector_Method, ONLY: RealVectorGet => Get

USE String_Class, ONLY: StringReallocate => Reallocate
IMPLICIT NONE

CHARACTER(*), PARAMETER :: &
  modName = "GmshStructuredMesh2_Class@ConstructorMethods.F90"

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "mesh_Initiate()"
#endif

INTEGER(I4B) :: ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

#ifdef DEBUG_VER
ii = SIZE(pointsOnAxis1, 1)
jj = math%three_i
CALL AssertError2(ii, jj, myName, "a=dim1 of pointsOnAxis1, b=3")
#endif

#ifdef DEBUG_VER
ii = SIZE(pointsOnAxis1, 2)
jj = 1 + SIZE(transfinitePointsOnAxis1)
CALL AssertError2( &
  ii, jj, myName, &
  "a=dim2 of pointsOnAxis1, b=1+dim1 of transfinitePointsOnAxis1,")
#endif

#ifdef DEBUG_VER
ii = SIZE(pointsOnAxis2, 1)
jj = math%three_i
CALL AssertError2(ii, jj, myName, "a=dim1 of pointsOnAxis2, b=3,")
#endif

#ifdef DEBUG_VER
ii = SIZE(pointsOnAxis2, 2)
jj = 1 + SIZE(transfinitePointsOnAxis2)
CALL AssertError2( &
  ii, jj, myName, &
  "a=dim2 of pointsOnAxis2, b=1 + size of transfinitePointsOnAxis2")
#endif

obj%tPoints(1) = SIZE(pointsOnAxis1, 2)
obj%tPoints(2) = SIZE(pointsOnAxis2, 2)

CALL SetTransfiniteMeshType(pointsOnAxis=pointsOnAxis1, &
                            ans=obj%transfiniteMeshType(1), &
                            meshTypeOnAxis=meshTypeOnAxis1)

CALL SetTransfiniteMeshType(pointsOnAxis=pointsOnAxis2, &
                            ans=obj%transfiniteMeshType(2), &
                            meshTypeOnAxis=meshTypeOnAxis2)

CALL SetTransfiniteCoeff(pointsOnAxis=pointsOnAxis1, &
                         ans=obj%transfiniteCoeff(1), &
                         coeffOnAxis=coeffOnAxis1)

CALL SetTransfiniteCoeff(pointsOnAxis=pointsOnAxis2, &
                         ans=obj%transfiniteCoeff(2), &
                         coeffOnAxis=coeffOnAxis2)

obj%recombineAll = Input(option=recombineAll, default=math%yes)

obj%filename = TRIM(filename)

CALL RealMatrixInitiate(obj%points(1), pointsOnAxis1)
CALL RealMatrixInitiate(obj%points(2), pointsOnAxis2)

CALL SetAllPoints(obj=obj)

CALL IntVectorInitiate(obj%transfinitePoints(1), transfinitePointsOnAxis1)
CALL IntVectorInitiate(obj%transfinitePoints(2), transfinitePointsOnAxis2)

CALL SetEdgesOnAxis(obj)
CALL SetCurveLoops(obj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%recombineAll = math%yes
obj%filename = ""

CALL RealMatrixDeallocate(obj%points(1))
CALL RealMatrixDeallocate(obj%points(2))
obj%tPoints = 0

isok = ALLOCATED(obj%allPoints)
IF (isok) DEALLOCATE (obj%allPoints)

CALL IntVectorDeallocate(obj%transfinitePoints(1))
CALL IntVectorDeallocate(obj%transfinitePoints(2))

CALL RealVectorDeallocate(obj%transfiniteCoeff(1))
CALL RealVectorDeallocate(obj%transfiniteCoeff(2))

CALL IntVectorDeallocate(obj%transfiniteMeshType(1))
CALL IntVectorDeallocate(obj%transfiniteMeshType(2))

isok = ALLOCATED(obj%edges)
IF (isok) DEALLOCATE (obj%edges)

isok = ALLOCATED(obj%edge_tfp)
IF (isok) DEALLOCATE (obj%edge_tfp)

isok = ALLOCATED(obj%edge_coeff)
IF (isok) DEALLOCATE (obj%edge_coeff)

isok = ALLOCATED(obj%edge_meshType)
IF (isok) DEALLOCATE (obj%edge_meshType)

obj%tEdges1 = 0
obj%tEdges2 = 0
obj%tEdges = 0

isok = ALLOCATED(obj%curveLoops)
IF (isok) DEALLOCATE (obj%curveLoops)

obj%tSurfaces = 0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                               SetAllPoints
!----------------------------------------------------------------------------

SUBROUTINE SetAllPoints(obj)
  TYPE(GmshStructuredMesh2_), INTENT(INOUT) :: obj

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetAllPoints()"
#endif

  INTEGER(I4B) :: tsize, ipoint, ii, jj, poaSize
  INTEGER(I4B), PARAMETER :: nsd = 2
  REAL(DFP) :: poa(3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  tsize = obj%tPoints(1) * obj%tPoints(2)
  CALL Reallocate(obj%allPoints, math%three_i, tsize)

  ipoint = 0
  DO jj = 1, obj%tPoints(2)
    DO ii = 1, obj%tPoints(1)

      ipoint = ipoint + 1

      CALL RealMatrixGetColumn_( &
        obj=obj%points(1), col=ii, ans=poa, tsize=poaSize)

      obj%allPoints(1:nsd, ipoint) = poa(1:nsd)

      CALL RealMatrixGetColumn_( &
        obj=obj%points(2), col=jj, ans=poa, tsize=poaSize)

      obj%allPoints(1:nsd, ipoint) = obj%allPoints(1:nsd, ipoint) &
                                     + poa(1:nsd)

    END DO
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SetAllPoints

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetTransfiniteMeshType(pointsOnAxis, ans, meshTypeOnAxis)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: meshTypeOnAxis(:)
  REAL(DFP), INTENT(IN) :: pointsOnAxis(:, :)
  TYPE(IntVector_), INTENT(INOUT) :: ans

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetMeshTypeOnAxis()"
#endif
  INTEGER(I4B), PARAMETER :: default_meshtype = Progression
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, jj

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  !! Get total number of segaments on axis (result is stored in jj)
  ii = SIZE(pointsOnAxis, 2) - 1
  CALL IntVectorInitiate(ans, ii)
  CALL IntVectorSet(ans, default_meshtype)

  isok = PRESENT(meshTypeOnAxis)

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  jj = SIZE(meshTypeOnAxis)

#ifdef DEBUG_VER
  CALL AssertError2( &
    ii, jj, myName, &
    "a=number of points in pointsOnAxis, b=size of meshTypeOnAxis,")
#endif

  CALL IntVectorSet(ans, meshTypeOnAxis(1:jj))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SetTransfiniteMeshType

!----------------------------------------------------------------------------
!                                                          SetMeshTypeOnAxis1
!----------------------------------------------------------------------------

SUBROUTINE SetTransfiniteCoeff(pointsOnAxis, coeffOnAxis, ans)
  REAL(DFP), OPTIONAL, INTENT(IN) :: coeffOnAxis(:)
  REAL(DFP), INTENT(IN) :: pointsOnAxis(:, :)
  TYPE(RealVector_), INTENT(INOUT) :: ans

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetTransfiniteCoeff()"
#endif
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, jj

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  !! Get total number of segaments on axis (result is stored in jj)
  ii = SIZE(pointsOnAxis, 2) - 1
  CALL RealVectorInitiate(ans, ii)
  CALL RealVectorSet(ans, math%one)
  isok = PRESENT(coeffOnAxis)

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  jj = SIZE(coeffOnAxis)

#ifdef DEBUG_VER
  CALL AssertError2( &
    ii, jj, myName, &
    "a=number of points in pointsOnAxis, b=size of coeffOnAxis,")
#endif

  CALL RealVectorSet(ans, coeffOnAxis(1:jj))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SetTransfiniteCoeff

!----------------------------------------------------------------------------
!                                                           SetEdgesOnAxis1
!----------------------------------------------------------------------------

SUBROUTINE SetEdgesOnAxis(obj)
  TYPE(GmshStructuredMesh2_), INTENT(INOUT) :: obj

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetEdgesOnAxis()"
#endif

  INTEGER(I4B) :: ii, jj, p(2), iedge

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  obj%tEdges1 = (obj%tPoints(1) - 1) * obj%tPoints(2)
  obj%tEdges2 = (obj%tPoints(2) - 1) * obj%tPoints(1)
  obj%tEdges = obj%tEdges1 + obj%tEdges2

  CALL Reallocate(obj%edges, 2, obj%tEdges)
  CALL Reallocate(obj%edge_tfp, obj%tEdges)
  CALL StringReallocate(obj%edge_meshType, obj%tEdges)
  CALL Reallocate(obj%edge_coeff, obj%tEdges)

  iedge = 0
  ! edges parallel to axis 1
  DO jj = 1, obj%tPoints(2)
    DO ii = 1, obj%tPoints(1) - 1
      p(1) = obj%GetNodeNumber(ii, jj)
      p(2) = obj%GetNodeNumber(ii + 1, jj)
      iedge = iedge + 1
      obj%edges(1:2, iedge) = p
      obj%edge_tfp(iedge) = IntVectorGet( &
                            obj%transfinitePoints(1), ii, math%one_i)

      obj%edge_coeff(iedge) = RealVectorGet( &
                              obj%transfiniteCoeff(1), ii, math%one)

      obj%edge_meshType(iedge) = obj%GetMeshTypeName( &
                                 IntVectorGet(obj%transfiniteMeshType(1), &
                                              ii, math%one_i))
    END DO
  END DO

  ! edges parallel to axis 2
  DO jj = 1, obj%tPoints(2) - 1
    DO ii = 1, obj%tPoints(1)

      p(1) = obj%GetNodeNumber(ii, jj)
      p(2) = obj%GetNodeNumber(ii, jj + 1)

      iedge = iedge + 1
      obj%edges(1:2, iedge) = p
      obj%edge_tfp(iedge) = IntVectorGet( &
                            obj%transfinitePoints(2), jj, math%one_i)

      obj%edge_coeff(iedge) = RealVectorGet( &
                              obj%transfiniteCoeff(2), jj, math%one)

      obj%edge_meshType(iedge) = obj%GetMeshTypeName( &
                                 IntVectorGet(obj%transfiniteMeshType(2), &
                                              jj, math%one_i))
    END DO
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SetEdgesOnAxis

!----------------------------------------------------------------------------
!                                                              SetCurveLoops
!----------------------------------------------------------------------------

SUBROUTINE SetCurveLoops(obj)
  TYPE(GmshStructuredMesh2_), INTENT(INOUT) :: obj

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetCurveLoops()"
#endif
  INTEGER(I4B) :: isurface, ii, jj, lineLoop(4)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  obj%tSurfaces = (obj%tPoints(1) - 1) * (obj%tPoints(2) - 1)
  CALL Reallocate(obj%curveLoops, 4, obj%tSurfaces)

  isurface = 0

  DO jj = 1, obj%tPoints(2) - 1
    DO ii = 1, obj%tPoints(1) - 1
      isurface = isurface + 1

      lineLoop(1) = obj%GetEdgeNumberOnAxis1(ii, jj)
      lineLoop(3) = obj%GetEdgeNumberOnAxis1(ii, jj + 1)

      lineLoop(2) = obj%GetEdgeNumberOnAxis2(ii + 1, jj)
      lineLoop(4) = obj%GetEdgeNumberOnAxis2(ii, jj)

      obj%curveLoops(1:4, isurface) = [lineLoop(1), lineLoop(2), &
                                       -lineLoop(3), -lineLoop(4)]

    END DO
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SetCurveLoops

!----------------------------------------------------------------------------
!                                                           Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
