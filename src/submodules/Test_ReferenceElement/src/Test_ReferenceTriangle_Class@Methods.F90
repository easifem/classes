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

SUBMODULE(Test_ReferenceTriangle_Class) Methods
USE BaseMethod
USE Test_ReferenceLine_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Initiate
  REAL( DFP ) :: xij( 3 , 3 )
  INTEGER( I4B ) :: entityCounts( 4 ), xidimension, name, ii
  TYPE(String) :: nameStr
  TYPE( Test_Topology_ ) :: topology(7)
  !!
  xij = TriangleLagrangeEquidistance( order=1_I4B )
  !!
  entityCounts = [3, 3, 1, 0]
  xidimension = 2
  name=Triangle3
  nameStr = "Triangle3"
  !!
  topology = obj%GetTopology()
  !!
  CALL obj%SetParam( &
    & xij=xij, &
    & entityCounts=entityCounts, &
    & nsd=nsd, &
    & xidimension=xidimension, &
    & name=name, &
    & nameStr=nameStr%chars(), &
    & topology=topology)
  !!
END PROCEDURE refelem_Initiate

!----------------------------------------------------------------------------
!                                                           GetFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetElements
  INTEGER( I4B ), PARAMETER :: n = 3_I4B
  INTEGER( I4B ) :: ii
  !!
  ALLOCATE( ans( n ) )
  !!
  DO ii = 1, n
    ALLOCATE( Test_ReferenceLine_ :: ans(ii)%ptr )
    CALL ans(ii)%ptr%Initiate( nsd=obj%getNSD() )
  END DO
  !!
END PROCEDURE refelem_GetFacetElements

!----------------------------------------------------------------------------
!                                                           GetFacetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetTopology
  INTEGER( I4B ), PARAMETER :: n=3_I4B
  INTEGER( I4B ), ALLOCATABLE :: nptrs( : )
  !!
  ALLOCATE (ans(n))
  !!
  nptrs = [1,2,3]
  !!
  CALL ans(1)%Initiate( nptrs=nptrs(2:3), name=Line2, &
    & xidimension=1_I4B)
  CALL ans(2)%Initiate( nptrs=nptrs(3:1), name=Line2, &
    & xidimension=1_I4B)
  CALL ans(3)%Initiate( nptrs=nptrs(1:2), name=Line2, &
    & xidimension=1_I4B)
  !!
END PROCEDURE refelem_GetFacetTopology

!----------------------------------------------------------------------------
!                                                                GetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetTopology
  INTEGER( I4B ), PARAMETER :: tFacet=7_I4B
  INTEGER( I4B ) :: ii
  !!
  ALLOCATE (ans(tFacet))
  !!
  !! point
  !!
  DO ii = 1, 3
    CALL ans(ii)%Initiate( nptrs=[ii], name=Point, &
      & xidimension=0_I4B)
  END DO
  !!
  !! Lines
  !!
  CALL ans( 4 )%Initiate( nptrs=[2_I4B, 3_I4B], name=Line2, &
    & xidimension=1_I4B )
  CALL ans( 5 )%Initiate( nptrs=[3_I4B, 1_I4B], name=Line2, &
    & xidimension=1_I4B )
  CALL ans( 6 )%Initiate( nptrs=[1_I4B, 2_I4B], name=Line2, &
    & xidimension=1_I4B )
  !!
  !! Triangle
  !!
  CALL ans( 7 )%Initiate( nptrs=[1_I4B, 2_I4B, 3_I4B], &
    & name=Triangle3, xidimension=2_I4B)
  !!
END PROCEDURE refelem_GetTopology

!----------------------------------------------------------------------------
!                                                                 GetMeasure
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetMeasure
  INTEGER( I4B ) :: nsd
  !!
  nsd = obj%getNSD()
  !!
  IF( nsd .EQ. 2_I4B ) THEN
    CALL TRIANGLEAREA2D( xij( 1:2, 1:3 ), ans )
  ELSE
    CALL TRIANGLEAREA3D( xij( 1:3, 1:3), ans )
  END IF
  !!
END PROCEDURE refelem_GetMeasure

!----------------------------------------------------------------------------
!                                                         GetElementQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetElementQuality
  !!
  REAL( DFP ) :: rvar(3)
  !!
  SELECT CASE(measure)
    !!
  CASE( QualityMeasure%area )
    ! Ans=Area(refelem=refelem, xij=xij)
    !!
  CASE( QualityMeasure%maxangle )
    ! Ans=MAXVAL(Angles(refelem=refelem, xij=xij))
    !!
  CASE( QualityMeasure%minangle )
    ! Ans=MINVAL(Angles(refelem=refelem, xij=xij))
    !!
  CASE( QualityMeasure%angleratio )
    ! Ans=3.0_DFP * MINVAL(Angles(refelem=refelem, xij=xij)) / Pi
    !!
  CASE( QualityMeasure%radiusRatio )
    ! Ans=2.0_DFP * InRadius(refelem=refelem, xij=xij) &
    !   & / CircumRadius(refelem=refelem, xij=xij)
    !!
  CASE( QualityMeasure%edgeRatio )
    ! rvar = EdgeLength(refelem=refelem, xij=xij)
    ! Ans = MINVAL(rvar)/MAXVAL(rvar)
    !!
  CASE( QualityMeasure%aspectRatio )
    ! rvar = EdgeLength(refelem=refelem, xij=xij)
    ! Ans = MAXVAL(rvar) * SUM(rvar) &
    !   & / (4.0_DFP*SQRT(3.0_DFP)*Area(refelem=refelem, xij=xij))
    !!
  END SELECT
    !!
!
! TODO #122 Implement GetElementQualityMethod in [[ReferenceTriangle_]]
!
END PROCEDURE refelem_GetElementQuality

!----------------------------------------------------------------------------
!                                                             isPointInside
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_isPointInside
  CALL triangle_contains_point_2d_1(xij(1:2,1:3), x(1:2), ans)
END PROCEDURE refelem_isPointInside

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods