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

SUBMODULE(Test_ReferencePyramid_Class) Methods
USE BaseMethod
USE Test_ReferenceLine_Class
USE Test_ReferenceTriangle_Class
USE Test_ReferenceQuadrangle_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Initiate
  REAL( DFP ) :: xij( 3 , 5 )
  INTEGER( I4B ) :: entityCounts( 4 ), xidimension, name
  TYPE(String) :: nameStr
  TYPE( Test_Topology_ ) :: topology(19)
  !!
  CALL Display( "debug-1" )
  xij = 0.0_DFP
  xij = PyramidLagrangeEquidistance( order=1_I4B )
  CALL Display( "debug-2" )
  !!
  xidimension = 3
  name = Pyramid5
  nameStr = "Pyramid5"
  entityCounts = TotalEntities( name )
  CALL Display( "debug-3" )
  !!
  topology = obj%GetTopology()
  CALL Display( "debug-4" )
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
  CALL Display( "debug-5" )
  !!
END PROCEDURE refelem_Initiate

!----------------------------------------------------------------------------
!                                                           GetFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetElements
  INTEGER( I4B ), PARAMETER :: n = 5_I4B
  INTEGER( I4B ) :: ii
  !!
  ALLOCATE( ans( n ) )
  !!
  ALLOCATE( Test_ReferenceQuadrangle_ :: ans(1)%ptr )
  CALL ans(1)%ptr%Initiate( nsd=obj%getNSD() )
  !!
  DO ii = 2, 5
    ALLOCATE( Test_ReferenceTriangle_ :: ans(ii)%ptr )
    CALL ans(ii)%ptr%Initiate( nsd=obj%getNSD() )
  END DO
  !!
END PROCEDURE refelem_GetFacetElements

!----------------------------------------------------------------------------
!                                                           GetFacetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetTopology
  INTEGER( I4B ), PARAMETER :: n=5_I4B
  INTEGER( I4B ) :: ii, n4(4, 5)
  !!
  ALLOCATE (ans(n))
  !!
  n4(:,1) = [1,4,3,2]
  n4(:,2) = [2,3,5,0]
  n4(:,3) = [3,4,5,0]
  n4(:,4) = [1,5,4,0]
  n4(:,5) = [1,2,5,0]
  !!
  CALL ans(1)%Initiate( nptrs=n4(1:4,1), name=Quadrangle4, &
    & xidimension=2_I4B )
  !!
  DO ii = 2, 5
    CALL ans(ii)%Initiate( nptrs=n4(1:3,ii), name=Triangle3, &
      & xidimension=2_I4B )
  END DO
  !!
END PROCEDURE refelem_GetFacetTopology

!----------------------------------------------------------------------------
!                                                                GetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetTopology
  INTEGER( I4B ), PARAMETER :: n=19_I4B
  INTEGER( I4B ) :: ii
  INTEGER( I4B ) :: n2(2, 8), n4(4, 5), n5(5)
  !!
  ALLOCATE (ans(n))
  !!
  !! point = 5
  !!
  DO ii = 1, 5
    CALL ans(ii)%Initiate( nptrs=[ii], name=Point, &
      & xidimension=0_I4B)
  END DO
  !!
  !! Lines = 8
  !!
  n2(:,1) = [1,2]
  n2(:,2) = [1,4]
  n2(:,3) = [1,5]
  n2(:,4) = [2,3]
  n2(:,5) = [2,5]
  n2(:,6) = [3,4]
  n2(:,7) = [3,5]
  n2(:,8) = [4,5]
  !!
  DO ii = 1, 8
    CALL ans( 5+ii )%Initiate( nptrs=n2(:,ii), name=Line2, &
      & xidimension=1_I4B )
  END DO
  !!
  n4(:,1) = [1,4,3,2]
  n4(:,2) = [2,3,5,0]
  n4(:,3) = [3,4,5,0]
  n4(:,4) = [1,5,4,0]
  n4(:,5) = [1,2,5,0]
  !!
  CALL ans(13+1)%Initiate( nptrs=n4(1:4,1), name=Quadrangle4, &
    & xidimension=2_I4B )
  !!
  DO ii = 2, 5
    CALL ans(13+ii)%Initiate( nptrs=n4(1:3,ii), name=Triangle3, &
      & xidimension=2_I4B )
  END DO
  !!
  !! Pyramid
  !!
  n5=[1,2,3,4,5]
  !!
  CALL ans(19)%Initiate( nptrs=n5, name=Pyramid5, &
    & xidimension=3_I4B )
  !!
END PROCEDURE refelem_GetTopology

!----------------------------------------------------------------------------
!                                                                 GetMeasure
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetMeasure
  CHARACTER( LEN = * ), PARAMETER :: myName="refelem_GetMeasure"
  CALL e%raiseError(modName //'::'//myName// ' - '// &
    & '[NOT IMPLEMENTED!] This routine is under developement')
! TODO #134 Implement GetMeasure for [[ReferencePyramid_]].
END PROCEDURE refelem_GetMeasure

!----------------------------------------------------------------------------
!                                                         GetElementQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetElementQuality
  CHARACTER( LEN = * ), PARAMETER :: myName="refelem_GetElementQuality"
  CALL e%raiseError(modName //'::'//myName// ' - '// &
    & '[NOT IMPLEMENTED!], This routine is under development')
!
! TODO #135 Implement GetElementQualityMethod in [[ReferencePyramid_]]
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
! TODO #136 Implement isPointInside in [[ReferencePyramid_]]
!
END PROCEDURE refelem_isPointInside

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods