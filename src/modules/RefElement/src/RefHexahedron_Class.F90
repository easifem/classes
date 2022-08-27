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

MODULE RefHexahedron_Class
USE GlobalData
USE Topology_Class
USE AbstractRefElement_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "RefHexahedron_Class"

!----------------------------------------------------------------------------
!                                                   RefHexahedron_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary:         RefHexahedron class is defined
!
!{!pages/RefHexahedron_.md!}

TYPE, EXTENDS(AbstractRefElement_) :: RefHexahedron_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: GetName => refelem_GetName
  !! Return the name of the element
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetElements => &
    & refelem_GetFacetElements
  !! Returns the facet elements
  PROCEDURE, PUBLIC, PASS(obj) :: GenerateTopology => &
    & refelem_GenerateTopology
  !! returns the facet topology
END TYPE RefHexahedron_

PUBLIC :: RefHexahedron_

!----------------------------------------------------------------------------
!                                            RefHexahedronPointer_
!----------------------------------------------------------------------------

TYPE :: RefHexahedronPointer_
  CLASS(RefHexahedron_), POINTER :: ptr => NULL()
END TYPE RefHexahedronPointer_

PUBLIC :: RefHexahedronPointer_

!----------------------------------------------------------------------------
!                                                           GetName@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 July 2022
! summary: Returns the name of the element

INTERFACE
  MODULE PURE FUNCTION refelem_GetName(obj) RESULT(ans)
    CLASS(RefHexahedron_), INTENT(IN) :: obj
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
    CLASS(RefHexahedron_), INTENT(IN) :: obj
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
    CLASS(RefHexahedron_), INTENT(INOUT) :: obj
  END SUBROUTINE refelem_GenerateTopology
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE RefHexahedron_Class
