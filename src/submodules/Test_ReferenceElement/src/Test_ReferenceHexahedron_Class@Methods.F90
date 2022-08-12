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

SUBMODULE(Test_ReferenceHexahedron_Class) Methods
USE BaseMethod
USE Test_ReferenceLine_Class
USE Test_ReferenceQuadrangle_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Initiate
  REAL( DFP ) :: xij( 3 , 8 )
  INTEGER( I4B ) :: entityCounts( 4 ), xidimension, name, ii
  TYPE(String) :: nameStr
  TYPE( Test_Topology_ ) :: topology(27)
  !!
  xij = 0.0_DFP
  xij = HexahedronLagrangeEquidistance( order=1_I4B )
  !!
  xidimension = 3
  name = Hexahedron8
  nameStr = "Hexahedron8"
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
  INTEGER( I4B ), PARAMETER :: n = 6_I4B
  INTEGER( I4B ) :: ii
  !!
  ALLOCATE( ans( n ) )
  !!
  DO ii = 1, n
    ALLOCATE( Test_ReferenceQuadrangle_ :: ans(ii)%ptr )
    CALL ans(ii)%ptr%Initiate( nsd=obj%getNSD() )
  END DO
  !!
END PROCEDURE refelem_GetFacetElements

!----------------------------------------------------------------------------
!                                                           GetFacetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetTopology
  INTEGER( I4B ), PARAMETER :: n=6_I4B
  INTEGER( I4B ) :: ii, n4(4, 6)
  !!
  ALLOCATE (ans(n))
  !!
  n4(:,1) = [1,2,6,5]
  n4(:,2) = [5,6,7,8]
  n4(:,3) = [1,5,8,4]
  n4(:,4) = [2,3,7,6]
  n4(:,5) = [1,4,3,2]
  n4(:,6) = [3,4,8,7]
  !!
  DO ii = 1, 6
    CALL ans(ii)%Initiate( nptrs=n4(:,ii), name=Quadrangle4, &
      & xidimension=2_I4B )
  END DO
  !!
END PROCEDURE refelem_GetFacetTopology

!----------------------------------------------------------------------------
!                                                                GetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetTopology
  INTEGER( I4B ), PARAMETER :: n=27_I4B
  INTEGER( I4B ) :: ii
  INTEGER( I4B ) :: n2(2, 12), n4(4, 6), n8(8)
  !!
  ALLOCATE (ans(n))
  !!
  !! point = 8
  !!
  DO ii = 1, 8
    CALL ans(ii)%Initiate( nptrs=[ii], name=Point, &
      & xidimension=0_I4B)
  END DO
  !!
  !! Lines = 12
  !!
  n2(:,1) = [1,2]
  n2(:,2) = [1,4]
  n2(:,3) = [1,5]
  n2(:,4) = [2,3]
  n2(:,5) = [2,6]
  n2(:,6) = [3,4]
  n2(:,7) = [3,7]
  n2(:,8) = [4,8]
  n2(:,9) = [5,6]
  n2(:,10) = [5,8]
  n2(:,11) = [6,7]
  n2(:,12) = [8,7]
  !!
  DO ii = 1, 12
    CALL ans( 8+ii )%Initiate( nptrs=n2(:,ii), name=Line2, &
      & xidimension=1_I4B )
  END DO
  !!
  !! Quadrangle
  !!
  n4(:,1) = [1,2,6,5]
  n4(:,2) = [5,6,7,8]
  n4(:,3) = [1,5,8,4]
  n4(:,4) = [2,3,7,6]
  n4(:,5) = [1,4,3,2]
  n4(:,6) = [3,4,8,7]
  !!
  DO ii = 1, 6
    CALL ans(20+ii)%Initiate( nptrs=n4(:,ii), name=Quadrangle4, &
      & xidimension=2_I4B )
  END DO
  !!
  !! Hexahedron
  !!
  n8=[1,2,3,4,5,6,7,8]
  !!
  CALL ans(27)%Initiate( nptrs=n8, name=Hexahedron8, &
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
! TODO #128 Implement GetMeasure for [[ReferenceHexahedron_]].
END PROCEDURE refelem_GetMeasure

!----------------------------------------------------------------------------
!                                                         GetElementQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetElementQuality
  CHARACTER( LEN = * ), PARAMETER :: myName="refelem_GetElementQuality"
  CALL e%raiseError(modName //'::'//myName// ' - '// &
    & '[NOT IMPLEMENTED!], This routine is under development')
!
! TODO #129 Implement GetElementQualityMethod in [[ReferenceHexahedron_]]
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
! TODO #130 Implement isPointInside in [[ReferenceHexahedron_]]
!
END PROCEDURE refelem_isPointInside

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods