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
! summary:         Reference element for point is implemented

MODULE RefPoint_Class
USE GlobalData
USE Topology_Class
USE AbstractRefElement_Class
USE ExceptionHandler_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "RefPoint_Class"
TYPE(ExceptionHandler_) :: e

!----------------------------------------------------------------------------
!                                                   RefPoint_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary:         RefPoint class is defined
!
!{!pages/RefPoint_.md!}

TYPE, EXTENDS(AbstractRefElement_) :: RefPoint_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => &
    & refelem_Initiate
  !! Initiate an instance of RefPoint_
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetElements => &
    & refelem_GetFacetElements
  !! Returns the facet elements
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetTopology => &
    & refelem_GetFacetTopology
  !! returns the facet topology
  PROCEDURE, PUBLIC, PASS(obj) :: GetTopology => &
    & refelem_GetTopology
  !! returns the topology
  PROCEDURE, PUBLIC, PASS(obj) :: GetMeasure => &
    & refelem_GetMeasure
  !! Return the measure
  PROCEDURE, PUBLIC, PASS(obj) :: GetElementQuality => &
    & refelem_GetElementQuality
  !! returns element quality
  PROCEDURE, PUBLIC, PASS(obj) :: isPointInside => &
    & refelem_isPointInside
  !! returns true if the element is inside the point
END TYPE RefPoint_

PUBLIC :: RefPoint_

!----------------------------------------------------------------------------
!                                            RefPointPointer_
!----------------------------------------------------------------------------

TYPE :: RefPointPointer_
  CLASS(RefPoint_), POINTER :: ptr => NULL()
END TYPE RefPointPointer_

PUBLIC :: RefPointPointer_

!----------------------------------------------------------------------------
!                                                         Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Initiate the instance of Reference element
!
!# Introduction
!
! This routine initiates an instance of reference element.

INTERFACE
  MODULE SUBROUTINE refelem_Initiate(obj, nsd)
    CLASS(RefPoint_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nsd
  !! spatial dimension
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
! Returns the facet elements.

INTERFACE
  MODULE SUBROUTINE refelem_GetFacetElements(obj, ans)
    CLASS(RefPoint_), INTENT(IN) :: obj
    TYPE(AbstractRefElementPointer_), ALLOCATABLE :: ans(:)
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
! This routine returns the facet topology of [[AbstractRefElement_]]
!

INTERFACE
  MODULE FUNCTION refelem_GetFacetTopology(obj) RESULT(ans)
    CLASS(RefPoint_), INTENT(IN) :: obj
    TYPE(Topology_), ALLOCATABLE :: ans(:)
  END FUNCTION refelem_GetFacetTopology
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetTopology@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns the facet topology of reference element
!
!# Introduction
!
!- This routine returns the facet topology of [[AbstractRefElement_]]

INTERFACE
  MODULE FUNCTION refelem_GetTopology(obj) RESULT(ans)
    CLASS(RefPoint_), INTENT(IN) :: obj
    TYPE(Topology_), ALLOCATABLE :: ans(:)
  END FUNCTION refelem_GetTopology
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

INTERFACE
  MODULE FUNCTION refelem_GetMeasure(obj, xij) RESULT(ans)
    CLASS(RefPoint_), INTENT(IN) :: obj
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
! This function returns the element quality.

INTERFACE
  MODULE FUNCTION refelem_GetElementQuality(obj, xij, measure) RESULT(ans)
    CLASS(RefPoint_), INTENT(IN) :: obj
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

INTERFACE
  MODULE FUNCTION refelem_isPointInside(obj, xij, x) RESULT(ans)
    CLASS(RefPoint_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP), INTENT(IN) :: x(3)
    LOGICAL(LGT) :: ans
  END FUNCTION refelem_isPointInside
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE RefPoint_Class
