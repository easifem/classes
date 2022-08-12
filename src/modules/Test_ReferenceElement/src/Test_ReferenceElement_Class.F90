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
! summary: 	ReferenceElement Class is implemented

MODULE Test_ReferenceElement_Class
USE GlobalData
USE String_Class, ONLY: String
USE Test_Topology_Class
USE ExceptionHandler_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName="Test_ReferenceElement_Class"
TYPE(ExceptionHandler_) :: e

!----------------------------------------------------------------------------
!                                                   Test_ReferenceElement_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: 	ReferenceElement class is defined
!
!{!pages/ReferenceElement_.md!}

TYPE, ABSTRACT :: Test_ReferenceElement_
  PRIVATE
  REAL(DFP), ALLOCATABLE :: xij(:, :)
    !! Nodal coordinates
  INTEGER(I4B) :: entityCounts(4) = 0_I4B
    !! Number of 0D, 1D, 2D, 3D subentities in the reference element
  INTEGER(I4B) :: xiDimension = -1_I4B
    !! Xidimension  elemType
    !! 0 is for point
    !! 1 is for line
    !! 2 is for surface
    !! 3 is for volume
  INTEGER(I4B) :: name = -1_I4B
    !! name of the element
  TYPE( String ) :: nameStr
    !! name of the element
  INTEGER(I4B) :: nsd = -1_I4B
    !! Number of spatial dimensions
  TYPE(Test_Topology_), ALLOCATABLE :: topology(:)
    !! Topology information of 0D, 1, 2, 3D entities
  !!
  CONTAINS
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => refelem_Initiate
  !! Initiate an instance
  PROCEDURE, PUBLIC, PASS( obj ) :: Copy => refelem_Copy
  !! Initiate an instance by copy
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
  PROCEDURE, PUBLIC, PASS( obj ) :: Deallocate => refelem_Deallocate
  !! Deallocate the data
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => refelem_Display
  !! Display the contents
  PROCEDURE, PUBLIC, PASS( obj ) :: GetNNE => refelem_GetNNE
  !! Returns the number of nodes in the element
  PROCEDURE, PUBLIC, PASS( obj ) :: GetNSD => refelem_GetNSD
  !! Returns the xidimension
  PROCEDURE, PUBLIC, PASS( obj ) :: GetXidimension => refelem_GetXidimension
  !! Returns the xidimension
  PROCEDURE, PUBLIC, PASS( obj ) :: GetElementTopology => &
    & refelem_GetElementTopology
  !! Returns the element topology
  PROCEDURE, PUBLIC, PASS( obj ) :: GetNptrs => refelem_GetNptrs
  !! Returns the connectivity
  GENERIC, PUBLIC :: GetConnectivity => GetNptrs
  !! Returns the connectivity
  PROCEDURE, PUBLIC, PASS( obj ) :: GetFacetMatrix => &
    & refelem_GetFacetMatrix
  !! Returns the facet matrix
  PROCEDURE, PUBLIC, PASS( obj ) :: GetNodeCoord => &
    & refelem_GetNodeCoord
  !! Returns the node coord
  PROCEDURE, PUBLIC, PASS( obj ) :: GetFacetElements => &
    & refelem_GetFacetElements
  PROCEDURE, PUBLIC, PASS( obj ) :: GetFacetTopology => &
    & refelem_GetFacetTopology
    !! Get the vector of topology of facet elements
  PROCEDURE, PUBLIC, PASS( obj ) :: GetTopology => &
    & refelem_GetTopology
    !! Get the vector of topology of facet elements
  PROCEDURE, PUBLIC, PASS( obj ) :: GetElementQuality => &
    & refelem_GetElementQuality
    !! Get the element quality
  PROCEDURE, PUBLIC, PASS( obj ) :: isPointInside => &
    & refelem_isPointInside
    !! Check if a point is inside the reference element
  PROCEDURE, PUBLIC, PASS( obj ) :: SetParam => refelem_SetParam
    !! Set the parameter at once
END TYPE Test_ReferenceElement_

PUBLIC :: Test_ReferenceElement_

!----------------------------------------------------------------------------
!                                            Test_ReferenceElementPointer_
!----------------------------------------------------------------------------

TYPE :: Test_ReferenceElementPointer_
  CLASS(Test_ReferenceElement_), POINTER :: ptr => NULL()
END TYPE Test_ReferenceElementPointer_

PUBLIC :: Test_ReferenceElementPointer_

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
MODULE SUBROUTINE refelem_Initiate( obj, nsd )
  CLASS( Test_ReferenceElement_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nsd
END SUBROUTINE refelem_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Copy@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 March 2021
! summary: This subroutine copies one reference element into other
!
!# Introduction
!
! This subroutine copies one reference element into other
! This subroutine also defines an assignment operator for `obj1=obj2`
! type opertions

INTERFACE
  MODULE PURE SUBROUTINE refelem_Copy(obj, obj2)
    CLASS(Test_ReferenceElement_), INTENT(INOUT) :: obj
    CLASS(Test_ReferenceElement_), INTENT(IN) :: obj2
  END SUBROUTINE refelem_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Deallocates the data stored inside the [[ReferenceElement_]]

INTERFACE
  MODULE PURE SUBROUTINE refelem_Deallocate(obj)
    CLASS(Test_ReferenceElement_), INTENT(INOUT) :: obj
  END SUBROUTINE refelem_Deallocate
END INTERFACE

INTERFACE Deallocate
  MODULE PROCEDURE refelem_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the ReferenceElement

INTERFACE
  MODULE SUBROUTINE refelem_Display(obj, msg, unitno)
    CLASS(Test_ReferenceElement_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
  END SUBROUTINE refelem_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE refelem_Display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                                NNE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns the total number of nodes in the reference element

INTERFACE
  MODULE ELEMENTAL FUNCTION refelem_GetNNE(obj) RESULT(ans)
    CLASS(Test_ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION refelem_GetNNE
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetNSD@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Returns NSD of the reference element

INTERFACE
  MODULE ELEMENTAL FUNCTION refelem_GetNSD(obj) RESULT(ans)
    CLASS(Test_ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION refelem_GetNSD
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetXidimension@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Returns xidimension of the reference element

INTERFACE
  MODULE ELEMENTAL FUNCTION refelem_GetXidimension(obj) RESULT(ans)
    CLASS(Test_ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION refelem_GetXidimension
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetElementTopology@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Get the element topology

INTERFACE
  MODULE ELEMENTAL FUNCTION refelem_GetElementTopology(obj) RESULT(ans)
    CLASS(Test_ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION refelem_GetElementTopology
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetNptrs@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns the connectivity of reference element

INTERFACE
  MODULE PURE FUNCTION refelem_GetNptrs(obj) RESULT(ans)
    CLASS(Test_ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION refelem_GetNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetFacetMatrix@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns the facet matrix
!
!# Introduction
!
! Returns the facet matrix of a reference element.
!
! - Number of rows are equal to the number of facet in an element
! - Number of columns = MAX( NNS )
! - First column => ElementTopology
! - Second Column => XiDimension
! - Third column => NNS
! - 4 to NNS + 3 => Local Nptrs

INTERFACE
  MODULE PURE FUNCTION refelem_GetFacetMatrix(obj) RESULT(ans)
    CLASS(Test_ReferenceElement_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION  refelem_GetFacetMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                     LocalNodeCoord@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Returns the node coordinate of a reference element

INTERFACE
  MODULE PURE FUNCTION refelem_GetNodeCoord(obj) RESULT(ans)
    CLASS(Test_ReferenceElement_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION refelem_GetNodeCoord
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
    CLASS(Test_ReferenceElement_), INTENT(IN) :: obj
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
    CLASS( Test_ReferenceElement_ ), INTENT( IN ) :: obj
    TYPE( Test_Topology_ ), ALLOCATABLE :: ans(:)
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
!- This routine returns the facet topology of [[ReferenceElement_]]
!- This routine should be implemented by the child classes.

INTERFACE
  MODULE FUNCTION refelem_GetTopology(obj) RESULT(ans)
    CLASS( Test_ReferenceElement_ ), INTENT( IN ) :: obj
    TYPE( Test_Topology_ ), ALLOCATABLE :: ans(:)
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
! This routine should be implemented by the child class.

INTERFACE
  MODULE FUNCTION refelem_GetMeasure(obj, xij) RESULT(ans)
    CLASS(Test_ReferenceElement_), INTENT(IN) ::obj
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
    CLASS(Test_ReferenceElement_), INTENT(IN) :: obj
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
    CLASS(Test_ReferenceElement_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP), INTENT(IN) :: x(3)
    LOGICAL(LGT) :: ans
  END FUNCTION refelem_isPointInside
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SetParam@Methods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE refelem_SetParam( obj, xij, entityCounts, &
  & xidimension, name, nameStr, nsd, &
  & topology )
  CLASS( Test_ReferenceElement_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: xij(:,:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: entityCounts(4)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: xidimension
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: name
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: nameStr
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: nsd
  TYPE(Test_Topology_), OPTIONAL, INTENT( IN ) :: topology(:)
END SUBROUTINE refelem_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Test_ReferenceElement_Class