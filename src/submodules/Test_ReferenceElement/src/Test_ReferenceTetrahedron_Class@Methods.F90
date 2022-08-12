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

SUBMODULE(Test_ReferenceTetrahedron_Class) Methods
USE BaseMethod
USE Test_ReferenceLine_Class
USE Test_ReferenceTriangle_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Initiate
  REAL( DFP ) :: xij( 3 , 4 )
  INTEGER( I4B ) :: entityCounts( 4 ), xidimension, name, ii
  TYPE(String) :: nameStr
  TYPE( Test_Topology_ ) :: topology(15)
  !!
  xij = 0.0_DFP
  xij = TetrahedronLagrangeEquidistance( order=1_I4B )
  !!
  xidimension = 3
  name = Tetrahedron4
  nameStr = "Tetrahedron4"
  entityCounts = TotalEntities( name )
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
    ALLOCATE( Test_ReferenceTriangle_ :: ans(ii)%ptr )
    CALL ans(ii)%ptr%Initiate( nsd=obj%getNSD() )
  END DO
  !!
END PROCEDURE refelem_GetFacetElements

!----------------------------------------------------------------------------
!                                                           GetFacetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetTopology
  INTEGER( I4B ), PARAMETER :: n=4_I4B
  INTEGER( I4B ) :: ii, n3( 3, n )
  !!
  ALLOCATE (ans(n))
  !!
  n3(:,1) = [2,3,4]
  n3(:,2) = [1,4,3]
  n3(:,3) = [1,2,4]
  n3(:,4) = [1,3,2]
  !!
  DO ii = 1, 4
    CALL ans(ii)%Initiate( nptrs=n3(:,ii), name=Triangle3, &
      & xidimension=2_I4B )
  END DO
  !!
END PROCEDURE refelem_GetFacetTopology

!----------------------------------------------------------------------------
!                                                                GetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetTopology
  INTEGER( I4B ), PARAMETER :: n=15_I4B
  INTEGER( I4B ) :: ii
  INTEGER( I4B ) :: n2(2, 6), n3(3,4), n4(4)
  !!
  ALLOCATE (ans(n))
  !!
  !! point = 4
  !!
  DO ii = 1, 4
    CALL ans(ii)%Initiate( nptrs=[ii], name=Point, &
      & xidimension=0_I4B)
  END DO
  !!
  !! Lines = 6
  !!
  n2(:, 1) = [3,4]
  n2(:, 2) = [2,4]
  n2(:, 3) = [2,3]
  n2(:, 4) = [1,4]
  n2(:, 5) = [1,3]
  n2(:, 6) = [1,2]
  !!
  DO ii = 1, 6
    CALL ans( 4+ii )%Initiate( nptrs=n2(:,ii), name=Line2, &
      & xidimension=1_I4B )
  END DO
  !!
  !! Triangle
  !!
  n3(:,1) = [2,3,4]
  n3(:,2) = [1,4,3]
  n3(:,3) = [1,2,4]
  n3(:,4) = [1,3,2]
  !!
  DO ii = 1, 4
    CALL ans(10+ii)%Initiate( nptrs=n3(:,ii), name=Triangle3, &
      & xidimension=2_I4B )
  END DO
  !!
  !! Tetrahedron
  !!
  n4=[1,2,3,4]
  CALL ans(15)%Initiate( nptrs=n4, name=Tetrahedron4, &
    & xidimension=3_I4B )
END PROCEDURE refelem_GetTopology

!----------------------------------------------------------------------------
!                                                                 GetMeasure
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetMeasure
  CHARACTER( LEN = * ), PARAMETER :: myName="refelem_GetMeasure"
  CALL e%raiseError(modName //'::'//myName// ' - '// &
    & '[NOT IMPLEMENTED!] This routine is under developement')
! TODO #125 Implement GetMeasure for [[ReferenceTetrahedron_]].
END PROCEDURE refelem_GetMeasure

!----------------------------------------------------------------------------
!                                                         GetElementQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetElementQuality
  CHARACTER( LEN = * ), PARAMETER :: myName="refelem_GetElementQuality"
  CALL e%raiseError(modName //'::'//myName// ' - '// &
    & '[NOT IMPLEMENTED!], This routine is under development')
!
! TODO #126 Implement GetElementQualityMethod in [[ReferenceTetrahedron_]]
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
! TODO #127 Implement isPointInside in [[ReferenceTetrahedron_]]
!
END PROCEDURE refelem_isPointInside

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods