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

SUBMODULE(Test_ReferenceLine_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Initiate
  REAL( DFP ) :: xij0( 3 ,1 )
  INTEGER( I4B ) :: entityCounts( 4 ), xidimension, name
  TYPE(String) :: nameStr
  TYPE( Test_Topology_ ) :: topology(1)
  !!
  xij0 = 0.0_DFP
  !!
  IF( PRESENT( xij ) ) THEN
    xij0(1:SIZE(xij,1),1) = xij(:,1)
  END IF
  !!
  entityCounts = [1, 0, 0, 0]
  xidimension = 0
  name= Point1
  nameStr = "Point1"
  !!
  CALL topology( 1 )%Initiate( nptrs=[1_I4B], name=Point1, &
    & xidimension=xidimension )
  !!
  CALL obj%SetParam( &
    & xij=xij0, &
    & entityCounts=entityCounts, &
    & nsd=nsd, &
    & order=order, &
    & xidimension=xidimension, &
    & name=name, &
    & nameStr=nameStr%chars(), &
    & topology=topology, &
    & interpolationType=Equidistance)
  !!
END PROCEDURE refelem_Initiate

!----------------------------------------------------------------------------
!                                                           GetFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetElements
  ALLOCATE( ans( 0 ) )
END PROCEDURE refelem_GetFacetElements

!----------------------------------------------------------------------------
!                                                          GetFacetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetTopology
  ALLOCATE( ans( 0 ) )
END PROCEDURE refelem_GetFacetTopology

!----------------------------------------------------------------------------
!                                                                 GetMeasure
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetMeasure
  ans = 0.0_DFP
END PROCEDURE refelem_GetMeasure

!----------------------------------------------------------------------------
!                                                         GetElementQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetElementQuality
  ans = 1.0_DFP
END PROCEDURE refelem_GetElementQuality

!----------------------------------------------------------------------------
!                                                             isPointInside
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_isPointInside
  REAL( DFP ) :: err, x0( 3, 1 )
  REAL( DFP ), PARAMETER :: tol=1.0E-10
  x0 = obj%GetNodeCoord()
  err = NORM2(x0(:,1) - x)
  ans = SOFTEQ( err, zero, tol=tol)
END PROCEDURE refelem_isPointInside

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods