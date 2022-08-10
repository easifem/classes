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

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: 	Reference element for point is implemented

MODULE Test_ReferenceLine_Class
USE GlobalData
USE Test_Topology_Class
USE Test_ReferenceElement_Class
USE ExceptionHandler_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName="Test_ReferenceLine_Class"
TYPE(ExceptionHandler_) :: e

!----------------------------------------------------------------------------
!                                                   Test_ReferenceLine_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: 	ReferenceLine class is defined
!
!{!pages/ReferenceLine_.md!}

TYPE, EXTENDS( Test_ReferenceElement_ ) :: Test_ReferenceLine_
  CONTAINS
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => &
    & refelem_Initiate
  !! Initiate an instance of ReferenceLine_
  PROCEDURE, PUBLIC, PASS( obj ) :: GetFacetElements => &
    & refelem_GetFacetElements
  !! Returns the facet elements
  PROCEDURE, PUBLIC, PASS( obj ) :: GetFacetTopology => &
    & refelem_GetFacetTopology
  !! returns the facet topology
  PROCEDURE, PUBLIC, PASS( obj ) :: GetElementQuality => &
    & refelem_GetElementQuality
  !! returns element quality
  PROCEDURE, PUBLIC, PASS( obj ) :: isPointInside => &
    & refelem_isPointInside
  !! returns true if the element is inside the point
END TYPE Test_ReferenceLine_

PUBLIC :: Test_ReferenceLine_

!----------------------------------------------------------------------------
!                                            Test_ReferenceLinePointer_
!----------------------------------------------------------------------------

TYPE :: Test_ReferenceLinePointer_
  CLASS(Test_ReferenceLine_), POINTER :: ptr => NULL()
END TYPE Test_ReferenceLinePointer_

PUBLIC :: Test_ReferenceLinePointer_

!----------------------------------------------------------------------------
!                                                         Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Initiate the instance of Reference element
!
!# Introduction
!
! This routine initiates an instance of reference element. This
! routine should be implemented by the child class

INTERFACE
MODULE SUBROUTINE refelem_Initiate( obj, order, nsd, xij )
  CLASS( Test_ReferenceLine_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: order
  !! not required
  INTEGER( I4B ), INTENT( IN ) :: nsd
  !! spatial dimension
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij( :, : )
  !! coordinate, the number of col should be at least 2
END SUBROUTINE refelem_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFacetElements@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: This routine returns the facet elements
!
!# Introduction
!
! Returns the facet elements. This routine should be implemented by the
! child classes.

INTERFACE
  MODULE SUBROUTINE refelem_GetFacetElements(obj, ans)
    CLASS(Test_ReferenceLine_), INTENT(IN) :: obj
    TYPE(Test_ReferenceElementPointer_), ALLOCATABLE :: ans(:)
  END SUBROUTINE refelem_GetFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFacetTopology@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns the facet topology of reference element
!
!# Introduction
!
! This routine returns the facet topology of [[ReferenceElement_]]
!
! This routine should be implemented by the child classes.

INTERFACE
  MODULE FUNCTION refelem_GetFacetTopology(obj) RESULT(ans)
    CLASS( Test_ReferenceLine_ ), INTENT( IN ) :: obj
    TYPE(Test_Topology_), ALLOCATABLE :: ans(:)
  END FUNCTION refelem_GetFacetTopology
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetMeasureSimplex@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns the  measures for simplex
!
!# Introduction
!
! This routine returns the measure of the reference element.
!
! This routine should be implemented by the child class.

INTERFACE
  MODULE FUNCTION refelem_GetMeasure(obj, xij) RESULT(ans)
    CLASS(Test_ReferenceLine_), INTENT(IN) ::obj
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans
  END FUNCTION refelem_GetMeasure
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetElementQuality@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Measure the quality of the element
!
!# Introduction
!
! This function returns the element quality. This should be
! implemented by the child class.

INTERFACE
  MODULE FUNCTION refelem_GetElementQuality(obj, xij, measure) RESULT(ans)
    CLASS(Test_ReferenceLine_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: ans
  END FUNCTION refelem_GetElementQuality
END INTERFACE

!----------------------------------------------------------------------------
!                                                     isPointInside@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns true if the given point is inside the element
!
!# Introduction
!
! If the given point is inside the referencelement, then
! it returns the true, otherwise it returns false.
!
! This routine should be implemented by the class child.

INTERFACE
  MODULE FUNCTION refelem_isPointInside(obj, xij, x) RESULT(ans)
    CLASS(Test_ReferenceLine_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP), INTENT(IN) :: x(3)
    LOGICAL(LGT) :: ans
  END FUNCTION refelem_isPointInside
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Test_ReferenceLine_Class