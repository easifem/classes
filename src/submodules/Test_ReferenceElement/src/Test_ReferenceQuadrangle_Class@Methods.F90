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

SUBMODULE(Test_ReferenceQuadrangle_Class) Methods
USE BaseMethod
USE Test_ReferenceLine_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Initiate
  REAL( DFP ) :: xij( 3 , 4 )
  INTEGER( I4B ) :: entityCounts( 4 ), xidimension, name, ii
  TYPE(String) :: nameStr
  TYPE( Test_Topology_ ) :: topology(9)
  !!
  xij = 0.0_DFP
  xij = QuadrangleLagrangeEquidistance( order=1_I4B )
  !!
  entityCounts = [4, 4, 1, 0]
  xidimension = 2
  name = Quadrangle4
  nameStr = "Quadrangle4"
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
  INTEGER( I4B ), PARAMETER :: n = 4_I4B
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
  INTEGER( I4B ), PARAMETER :: n=4_I4B
  INTEGER( I4B ), ALLOCATABLE :: nptrs( : )
  !!
  ALLOCATE (ans(n))
  !!
  nptrs = [1,2,3,4]
  !!
  CALL ans(1)%Initiate( nptrs=nptrs(1:2), name=Line2, &
    & xidimension=1_I4B)
  CALL ans(2)%Initiate( nptrs=nptrs(2:3), name=Line2, &
    & xidimension=1_I4B)
  CALL ans(3)%Initiate( nptrs=nptrs(3:4), name=Line2, &
    & xidimension=1_I4B)
  CALL ans(4)%Initiate( nptrs=nptrs(4:1), name=Line2, &
    & xidimension=1_I4B)
  !!
END PROCEDURE refelem_GetFacetTopology

!----------------------------------------------------------------------------
!                                                                GetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetTopology
  INTEGER( I4B ), PARAMETER :: n=9_I4B
  INTEGER( I4B ) :: ii
  !!
  ALLOCATE (ans(n))
  !!
  !! point
  !!
  DO ii = 1, 4
    CALL ans(ii)%Initiate( nptrs=[ii], name=Point, &
      & xidimension=0_I4B)
  END DO
  !!
  !! Lines
  !!
  CALL ans( 5 )%Initiate( nptrs=[1_I4B, 2_I4B], name=Line2, &
    & xidimension=1_I4B )
  CALL ans( 6 )%Initiate( nptrs=[2_I4B, 3_I4B], name=Line2, &
    & xidimension=1_I4B )
  CALL ans( 7 )%Initiate( nptrs=[3_I4B, 4_I4B], name=Line2, &
    & xidimension=1_I4B )
  CALL ans( 8 )%Initiate( nptrs=[4_I4B, 1_I4B], name=Line2, &
    & xidimension=1_I4B )
  !!
  !! Quadrangle
  !!
  CALL ans( 9 )%Initiate( nptrs=[1_I4B, 2_I4B, 3_I4B, 4_I4B], &
    & name=Quadrangle4, xidimension=2_I4B)
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
  IF( nsd .EQ. 2 ) THEN
    CALL QUADAREA2D( xij( 1:2, 1:4 ), ans )
  ELSE
    CALL QUADAREA3D( xij( 1:3, 1:4 ), ans )
  END IF
  !!
END PROCEDURE refelem_GetMeasure

!----------------------------------------------------------------------------
!                                                         GetElementQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetElementQuality
  CHARACTER( LEN = * ), PARAMETER :: myName="refelem_GetElementQuality"
  CALL e%raiseError(modName //'::'//myName// ' - '// &
    & '[NOT IMPLEMENTED!], This routine is under development')
!
! TODO #123 Implement GetElementQualityMethod in [[ReferenceQuadrangle_]]
!
END PROCEDURE refelem_GetElementQuality

!----------------------------------------------------------------------------
!                                                             isPointInside
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_isPointInside
  CHARACTER( LEN = * ), PARAMETER :: myName=" refelem_isPointInside"
  CALL e%raiseError(modName //'::'//myName// ' - '// &
    & '[NOT IMPLEMENTED!], This routine is under development')
!
! TODO #124 Implement isPointInside in [[ReferenceQuadrangle_]]
!
END PROCEDURE refelem_isPointInside

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods