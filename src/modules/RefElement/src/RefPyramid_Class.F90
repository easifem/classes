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

MODULE RefPyramid_Class
USE GlobalData
USE Topology_Class
USE AbstractRefElement_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "RefPyramid_Class"

!----------------------------------------------------------------------------
!                                                               RefPyramid_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary:         RefPyramid class is defined
!
!{!pages/RefPyramid_.md!}

TYPE, EXTENDS(AbstractRefElement_) :: RefPyramid_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: GetName => refelem_GetName
  !! Return the name of the element
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetElements => &
    & refelem_GetFacetElements
  !! Returns the facet elements
  PROCEDURE, PUBLIC, PASS(obj) :: GenerateTopology => &
    & refelem_GenerateTopology
  !! returns the facet topology
END TYPE RefPyramid_

PUBLIC :: RefPyramid_

!----------------------------------------------------------------------------
!                                                         RefPyramidPointer_
!----------------------------------------------------------------------------

TYPE :: RefPyramidPointer_
  CLASS(RefPyramid_), POINTER :: ptr => NULL()
END TYPE RefPyramidPointer_

PUBLIC :: RefPyramidPointer_

!----------------------------------------------------------------------------
!                                                           GetName@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 July 2022
! summary: Returns the name of the element

INTERFACE
  MODULE PURE FUNCTION refelem_GetName(obj) RESULT(ans)
    CLASS(RefPyramid_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION refelem_GetName
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
    CLASS(RefPyramid_), INTENT(IN) :: obj
    TYPE(AbstractRefElementPointer_), ALLOCATABLE :: ans(:)
  END SUBROUTINE refelem_GetFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GenerateTopology@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2022
! summary: Generate topology of reference element
!

INTERFACE
  MODULE SUBROUTINE refelem_GenerateTopology(obj)
    CLASS(RefPyramid_), INTENT(INOUT) :: obj
  END SUBROUTINE refelem_GenerateTopology
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE RefPyramid_Class
