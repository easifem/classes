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

MODULE RefLine_Class
USE GlobalData
USE Topology_Class
USE AbstractRefElement_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "RefLine_Class"

!----------------------------------------------------------------------------
!                                                   RefLine_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary:         RefLine class is defined
!
!{!pages/RefLine_.md!}

TYPE, EXTENDS(AbstractRefElement_) :: RefLine_
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => &
    & refelem_Initiate
  !! Initiate an instance of RefLine_
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetElements => &
    & refelem_GetFacetElements
  !! Returns the facet elements
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetTopology => &
    & refelem_GetFacetTopology
  !! returns the facet topology
  PROCEDURE, PUBLIC, PASS(obj) :: GetTopology => &
    & refelem_GetTopology
  !! returns the facet topology
END TYPE RefLine_

PUBLIC :: RefLine_

!----------------------------------------------------------------------------
!                                            RefLinePointer_
!----------------------------------------------------------------------------

TYPE :: RefLinePointer_
  CLASS(RefLine_), POINTER :: ptr => NULL()
END TYPE RefLinePointer_

PUBLIC :: RefLinePointer_

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
  MODULE SUBROUTINE refelem_Initiate(obj, nsd)
    CLASS(RefLine_), INTENT(INOUT) :: obj
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
    CLASS(RefLine_), INTENT(IN) :: obj
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
!- This routine returns the facet topology of [[AbstractRefElement_]]

INTERFACE
  MODULE FUNCTION refelem_GetFacetTopology(obj) RESULT(ans)
    CLASS(RefLine_), INTENT(IN) :: obj
    TYPE(Topology_), ALLOCATABLE :: ans(:)
  END FUNCTION refelem_GetFacetTopology
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetTopology@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns the topology of reference element
!
!# Introduction
!
!- This routine returns the topology of [[AbstractRefElement_]]

INTERFACE
  MODULE FUNCTION refelem_GetTopology(obj) RESULT(ans)
    CLASS(RefLine_), INTENT(IN) :: obj
    TYPE(Topology_), ALLOCATABLE :: ans(:)
  END FUNCTION refelem_GetTopology
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE RefLine_Class
