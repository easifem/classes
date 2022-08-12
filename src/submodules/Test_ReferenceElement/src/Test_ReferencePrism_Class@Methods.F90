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

SUBMODULE(Test_ReferencePrism_Class) Methods
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
  REAL( DFP ) :: xij( 3 , 6 )
  INTEGER( I4B ) :: entityCounts( 4 ), xidimension, name, ii
  TYPE(String) :: nameStr
  TYPE( Test_Topology_ ) :: topology(21)
  !!
  xij = 0.0_DFP
  xij = PrismLagrangeEquidistance( order=1_I4B )
  !!
  xidimension = 3
  name = Prism6
  nameStr = "Prism6"
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
  INTEGER( I4B ), PARAMETER :: n = 5_I4B
  INTEGER( I4B ) :: ii
  !!
  ALLOCATE( ans( n ) )
  !!
  ALLOCATE( Test_ReferenceTriangle_ :: ans(1)%ptr )
  CALL ans(1)%ptr%Initiate( nsd=obj%getNSD() )
  ALLOCATE( Test_ReferenceTriangle_ :: ans(5)%ptr )
  CALL ans(5)%ptr%Initiate( nsd=obj%getNSD() )
  !!
  DO ii = 2, 4
    ALLOCATE( Test_ReferenceQuadrangle_ :: ans(ii)%ptr )
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
  n4(:,1) = [1,3,2,0]
  n4(:,2) = [2,3,6,5]
  n4(:,3) = [1,2,5,4]
  n4(:,4) = [1,4,6,3]
  n4(:,5) = [4,5,6,0]
  !!
  CALL ans(1)%Initiate( nptrs=n4(1:3,1), name=Triangle3, &
    & xidimension=2_I4B )
  CALL ans(5)%Initiate( nptrs=n4(1:3,5), name=Triangle3, &
    & xidimension=2_I4B )
  !!
  DO ii = 2, 4
    CALL ans(ii)%Initiate( nptrs=n4(:,ii), name=Quadrangle4, &
      & xidimension=2_I4B )
  END DO
  !!
END PROCEDURE refelem_GetFacetTopology

!----------------------------------------------------------------------------
!                                                                GetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetTopology
  INTEGER( I4B ), PARAMETER :: n=21_I4B
  INTEGER( I4B ) :: ii
  INTEGER( I4B ) :: n2(2, 9), n4(4, 5), n6(6)
  !!
  ALLOCATE (ans(n))
  !!
  !! point = 6
  !!
  DO ii = 1, 6
    CALL ans(ii)%Initiate( nptrs=[ii], name=Point, &
      & xidimension=0_I4B)
  END DO
  !!
  !! Lines = 9
  !!
  n2(:,1) = [1,2]
  n2(:,2) = [1,3]
  n2(:,3) = [1,4]
  n2(:,4) = [2,3]
  n2(:,5) = [2,5]
  n2(:,6) = [3,6]
  n2(:,7) = [4,5]
  n2(:,8) = [4,6]
  n2(:,9) = [5,6]
  !!
  DO ii = 1, 9
    CALL ans( 6+ii )%Initiate( nptrs=n2(:,ii), name=Line2, &
      & xidimension=1_I4B )
  END DO
  !!
  n4(:,1) = [1,3,2,0]
  n4(:,2) = [2,3,6,5]
  n4(:,3) = [1,2,5,4]
  n4(:,4) = [1,4,6,3]
  n4(:,5) = [4,5,6,0]
  !!
  CALL ans(15+1)%Initiate( nptrs=n4(1:3,1), name=Triangle3, &
    & xidimension=2_I4B )
  CALL ans(15+5)%Initiate( nptrs=n4(1:3,5), name=Triangle3, &
    & xidimension=2_I4B )
  !!
  DO ii = 2, 4
    CALL ans(15+ii)%Initiate( nptrs=n4(:,ii), name=Quadrangle4, &
      & xidimension=2_I4B )
  END DO
  !!
  !! Prism
  !!
  n6=[1,2,3,4,5,6]
  !!
  CALL ans(21)%Initiate( nptrs=n6, name=Prism6, &
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
! TODO #131 Implement GetMeasure for [[ReferencePrism_]].
END PROCEDURE refelem_GetMeasure

!----------------------------------------------------------------------------
!                                                         GetElementQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetElementQuality
  CHARACTER( LEN = * ), PARAMETER :: myName="refelem_GetElementQuality"
  CALL e%raiseError(modName //'::'//myName// ' - '// &
    & '[NOT IMPLEMENTED!], This routine is under development')
!
! TODO #132 Implement GetElementQualityMethod in [[ReferencePrism_]]
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
! TODO #133 Implement isPointInside in [[ReferencePrism_]]
!
END PROCEDURE refelem_isPointInside

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods