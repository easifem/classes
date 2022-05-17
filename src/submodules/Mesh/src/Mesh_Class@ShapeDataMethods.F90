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

SUBMODULE(Mesh_Class) ShapeDataMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            InitiateElemSD
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiateElemSD1
  !!
  !! main
  !!
  obj%quadTypeForSpace=TRIM(quadTypeForSpace)
  obj%continuityTypeForSpace=TRIM(continuityTypeForSpace)
  obj%interpolTypeForSpace=TRIM(interpolTypeForSpace)
  obj%orderSpace=orderSpace
  !!
  CALL Initiate(obj=obj%quadForSpace, &
    & refelem=spaceElem, &
    & order=orderSpace, &
    & QuadratureType=quadTypeForSpace)
  !!
  CALL Initiate(obj=obj%linSpaceElemSD, &
    & quad=obj%quadForSpace, &
    & refelem=linSpaceElem, &
    & ContinuityType=continuityTypeForSpace, &
    & InterpolType=interpolTypeForSpace)
  !!
  CALL Initiate(obj=obj%spaceElemSD, &
    & quad=obj%quadForSpace, &
    & refelem=spaceElem, &
    & ContinuityType=continuityTypeForSpace, &
    & InterpolType=interpolTypeForSpace)
  !!
END PROCEDURE mesh_initiateElemSD1

!----------------------------------------------------------------------------
!                                                             InitiateElemSD
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiateElemSD2
  !!
  CALL obj%InitiateElemSD( &
    & orderSpace=orderSpace, &
    & linSpaceElem=linSpaceElem, &
    & spaceElem=spaceElem, &
    & quadTypeForSpace=quadTypeForSpace, &
    & continuityTypeForSpace=continuityTypeForSpace, &
    & interpolTypeForSpace=interpolTypeForSpace, &
    & orderTime=orderTime, &
    & linTimeElem=linTimeElem, &
    & timeElem=timeElem, &
    & quadTypeForTime=quadTypeForTime, &
    & continuityTypeForTime=continuityTypeForTime, &
    & interpolTypeForTime=interpolTypeForTime )
  !!
  CALL obj%InitiateElemSD( tvec=tvec )
  !!
END PROCEDURE mesh_initiateElemSD2

!----------------------------------------------------------------------------
!                                                             InitiateElemSD
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiateElemSD3
  !!
  CALL obj%InitiateElemSD( &
    & orderSpace=orderSpace, &
    & linSpaceElem=linSpaceElem, &
    & spaceElem=spaceElem, &
    & quadTypeForSpace=quadTypeForSpace, &
    & continuityTypeForSpace=continuityTypeForSpace, &
    & interpolTypeForSpace=interpolTypeForSpace )
  !!
  obj%quadTypeForTime=TRIM(quadTypeForTime)
  obj%continuityTypeForTime=TRIM(continuityTypeForTime)
  obj%interpolTypeForTime=TRIM(interpolTypeForTime)
  obj%orderTime=orderTime
  !!
  CALL Initiate(obj=obj%quadForTime, &
    & refelem=timeElem, &
    & order=orderTime, &
    & QuadratureType=quadTypeForTime)
  !!
  CALL Initiate(obj=obj%linTimeElemSD, &
    & quad=obj%quadForTime, &
    & refelem=linTimeElem, &
    & ContinuityType=continuityTypeForTime, &
    & InterpolType=interpolTypeForTime)
  !!
  CALL Initiate(obj=obj%timeElemSD, &
    & quad=obj%quadForTime, &
    & refelem=timeElem, &
    & ContinuityType=continuityTypeForTime, &
    & InterpolType=interpolTypeForTime)
  !!
END PROCEDURE mesh_initiateElemSD3

!----------------------------------------------------------------------------
!                                                             InitiateElemSD
!----------------------------------------------------------------------------

MODULE PROCEDURE mesh_initiateElemSD4
  !!
  INTEGER( I4B ) :: ii
  !!
  CALL Set(obj=obj%timeElemSD, &
    & val=reshape(tvec, [1, size(tvec)]), &
    & N=obj%linTimeElemSD%N, &
    & dNdXi=obj%linTimeElemSD%dNdXi)
  !!
  CALL Initiate(obj=obj%stelemsd, elemsd=obj%timeElemSD)
  !!
  DO ii = 1, SIZE(obj%stelemsd)
    CALL Initiate(obj=obj%stelemsd(ii), &
      & quad=obj%quadForSpace, &
      & refelem=obj%spaceElemSD%refelem, &
      & ContinuityType=obj%continuityTypeForSpace%chars(), &
      & InterpolType=obj%interpolTypeForSpace%chars() )
  END DO
  !!
END PROCEDURE mesh_initiateElemSD4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ShapeDataMethods
