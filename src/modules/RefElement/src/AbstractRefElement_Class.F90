! This program is a part of EASIFEM library
! Copyright (C) 2020-2022  Vikas Sharma, Ph.D
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
! summary: AbstractRefElement Class is implemented

MODULE AbstractRefElement_Class
USE GlobalData
USE BaseType, ONLY: BaseInterpolation_, BaseContinuity_, &
& ReferenceElement_, ReferenceTopology_
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "AbstractRefElement_Class"

PUBLIC :: AbstractRefElement_
PUBLIC :: AbstractRefElementPointer_
PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                       AbstractRefElement_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! update: 18 Aug 2022
! summary: AbstractRefElement class is defined
!
!{!pages/docs-api/AbstractRefElement/AbstractRefElement_.md!}

TYPE, ABSTRACT :: AbstractRefElement_
  PRIVATE
  TYPE(ReferenceElement_) :: refelem
  TYPE(String) :: nameStr
  !! name of the element
  CLASS(BaseContinuity_), ALLOCATABLE :: baseContinuity
  !! continuity or conformity of basis defined on reference
  !! element, following values are allowed
  !! H1, HCurl, HDiv, DG
  CLASS(BaseInterpolation_), ALLOCATABLE :: baseInterpolation
  !! Type of basis functions used for interpolation on reference
  !! element, Following values are allowed
  !! LagrangeInterpolation
  !! HermitInterpolation
  !! SerendipityInterpolation
  !! HierarchyInterpolation
  !! OrthogonalInterpolation
CONTAINS
  
  ! @DeferredMethods
  PROCEDURE(refelem_RefCoord), DEFERRED, PUBLIC, PASS(obj) :: &
    & RefCoord
  !! Reference coordiante of elements
  !! It depends upon the basis type and basis continuity
  PROCEDURE(refelem_GetName), DEFERRED, PUBLIC, PASS(obj) :: &
    & GetName
  !! returns the name
  PROCEDURE(refelem_GetFacetElements), DEFERRED, PUBLIC, PASS(obj) :: &
    & GetFacetElements
  !! returns the facet elements
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => refelem_Initiate
  !! Initiate an instance
  PROCEDURE, PUBLIC, PASS(obj) :: GetTopology => refelem_GetTopology
  !! Get the vector of topology of reference element
  PROCEDURE, PUBLIC, PASS(obj) :: Copy => refelem_Copy
  !! Initiate an instance by copying a reference element
  GENERIC, PUBLIC :: ASSIGNMENT(=) => Copy
  !! Assignment operator
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => refelem_Deallocate
  !! Deallocate the data
  PROCEDURE, PUBLIC, PASS(obj) :: Display => refelem_Display
  !! Display the contents
  PROCEDURE, PUBLIC, PASS(obj) :: MdEncode => refelem_MdEncode
  !! Display the contents
  PROCEDURE, PUBLIC, PASS(obj) :: ReactEncode => refelem_ReactEncode
  !! Display the contents in react components
  PROCEDURE, PUBLIC, PASS(obj) :: GetNNE => refelem_GetNNE
  !! Returns the number of nodes in the element
  PROCEDURE, PUBLIC, PASS(obj) :: GetNSD => refelem_GetNSD
  !! Returns the spatial dimension of reference element
  PROCEDURE, PUBLIC, PASS(obj) :: GetXidimension => refelem_GetXidimension
  !! Returns the xidimension of reference element
  PROCEDURE, PUBLIC, PASS(obj) :: GetElementTopology => &
    & refelem_GetElementTopology
  !! Returns the element topology
  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrs => refelem_GetNptrs
  !! Returns the connectivity
  GENERIC, PUBLIC :: GetConnectivity => GetNptrs
  !! Returns the connectivity
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetMatrix => &
    & refelem_GetFacetMatrix
  !! Returns the facet matrix
  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeCoord => &
    & refelem_GetNodeCoord
  !! Returns the node coord
  PROCEDURE, PUBLIC, PASS(obj) :: GetInterpolationPoint => &
    & refelem_GetInterpolationPoint
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => refelem_SetParam
    !! Set the parameter at once
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => refelem_GetParam
    !! Set the parameter at once
END TYPE AbstractRefElement_

!----------------------------------------------------------------------------
!                                                AbstractRefElementPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractRefElementPointer_
  CLASS(AbstractRefElement_), POINTER :: ptr => NULL()
END TYPE AbstractRefElementPointer_

!----------------------------------------------------------------------------
!                                                         RefCoord@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-09
! summary: Return the reference coordiante of linear element

ABSTRACT INTERFACE
 FUNCTION refelem_RefCoord(obj, baseInterpolation, baseContinuity) RESULT(ans)
    IMPORT AbstractRefElement_, I4B, DFP
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    CHARACTER(*), INTENT(IN) :: baseContinuity
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION refelem_RefCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetName@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 July 2022
! summary: Return the name of the element
!
!# Introduction
!
! This routine returns the name of the element. This should be implemented
! by the child class.

ABSTRACT INTERFACE
  PURE FUNCTION refelem_GetName(obj) RESULT(ans)
    IMPORT AbstractRefElement_, I4B
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION refelem_GetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFacetElements@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2022
! summary: This routine returns the facet elements
!
!# Introduction
!
! Returns the facet elements. This routine should be implemented by the
! child classes.

ABSTRACT INTERFACE
  SUBROUTINE refelem_GetFacetElements(obj, ans)
    IMPORT AbstractRefElement_, AbstractRefElementPointer_
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
    TYPE(AbstractRefElementPointer_), ALLOCATABLE :: ans(:)
  END SUBROUTINE refelem_GetFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Initiate the instance of Reference element

INTERFACE
  MODULE SUBROUTINE refelem_Initiate( &
    & obj,  &
    & nsd, &
    & baseContinuity,  &
    & baseInterpolation,  &
    & xij)
    CLASS(AbstractRefElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nsd
      !! Spatial dimension of element
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuity
      !! Continuity or Conformity of basis function.
      !! This parameter is used to determine the nodal coordinates of
      !! reference element, when xij is not present.
      !! If xij is present then this parameter is ignored
      !! H1 * Default
      !! HDiv
      !! HCurl
      !! DG
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpolation
      !! Basis function family used for interpolation.
      !! This parameter is used to determine the nodal coordinates of
      !! reference element, when xij is not present.
      !! If xij is present then this parameter is ignored
      !! LagrangeInterpolation, LagrangePolynomial
      !! SerendipityInterpolation, SerendipityPolynomial
      !! HierarchyInterpolation, HierarchyPolynomial
      !! OrthogonalInterpolation, OrthogonalPolynomial
      !! HermitInterpolation, HermitPolynomial
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
      !! Nodal coordiantes of reference element
      !! The number of rows in xij are nsd.
      !! The rows  cooresponds to the x , y, and z components
      !! The columns denotes the nodal point
      !! If xij is present then baseContinuity and
      !! baseInterpolation are ignored.
  END SUBROUTINE refelem_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetTopology@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2022
! summary: Returns the topology of reference element
!
!# Introduction
!
!- This routine returns the topology of [[AbstractRefElement_]]

INTERFACE
  MODULE PURE FUNCTION refelem_GetTopology(obj, xidim) RESULT(ans)
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: xidim
    TYPE(ReferenceTopology_), ALLOCATABLE :: ans(:)
  END FUNCTION refelem_GetTopology
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Copy@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 March 2022
! summary: This subroutine copies one reference element into other
!
!# Introduction
!
! This subroutine copies one reference element into other
! This subroutine also defines an assignment operator for `obj1=obj2`
! type opertions

INTERFACE
  MODULE SUBROUTINE refelem_Copy(obj, obj2)
    CLASS(AbstractRefElement_), INTENT(INOUT) :: obj
    CLASS(AbstractRefElement_), INTENT(IN) :: obj2
  END SUBROUTINE refelem_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2022
! summary: Deallocates the data stored inside the [[AbstractRefElement_]]

INTERFACE
  MODULE PURE SUBROUTINE refelem_Deallocate(obj)
    CLASS(AbstractRefElement_), INTENT(INOUT) :: obj
  END SUBROUTINE refelem_Deallocate
END INTERFACE

INTERFACE DEALLOCATE
  MODULE PROCEDURE refelem_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the AbstractRefElement

INTERFACE
  MODULE SUBROUTINE refelem_Display(obj, msg, unitno, notFull)
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: notFull
    !! if present and true then only a summary is printed
  END SUBROUTINE refelem_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE refelem_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                          MdEncode@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the contents in mardown format

INTERFACE
  MODULE FUNCTION refelem_MdEncode(obj) RESULT(ans)
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION refelem_MdEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                        ReactEncode@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the reference elements in react components

INTERFACE
  MODULE FUNCTION refelem_ReactEncode(obj) RESULT(ans)
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION refelem_ReactEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                                NNE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2022
! summary: Returns the total number of nodes in the reference element

INTERFACE
  MODULE ELEMENTAL FUNCTION refelem_GetNNE(obj) RESULT(ans)
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION refelem_GetNNE
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetNSD@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Returns NSD of the reference element

INTERFACE
  MODULE ELEMENTAL FUNCTION refelem_GetNSD(obj) RESULT(ans)
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
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
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION refelem_GetXidimension
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetElementTopology@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Get the element topology

INTERFACE
  MODULE ELEMENTAL FUNCTION refelem_GetElementTopology(obj) RESULT(ans)
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION refelem_GetElementTopology
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetNptrs@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2022
! summary: Returns the connectivity of reference element

INTERFACE
  MODULE PURE FUNCTION refelem_GetNptrs(obj) RESULT(ans)
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
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
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION refelem_GetFacetMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                    LocalNodeCoord@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Returns the node coordinate of a reference element

INTERFACE
  MODULE PURE FUNCTION refelem_GetNodeCoord(obj) RESULT(ans)
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION refelem_GetNodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetInterpolationPoint@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: Get the lattice points on reference element

INTERFACE
  MODULE FUNCTION refelem_GetInterpolationPoint(obj, order, ipType, layout) &
     & RESULT(ans)
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ipType
    CHARACTER(*), INTENT(IN) :: layout
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION refelem_GetInterpolationPoint
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-08
! summary: Set parameters of reference element

INTERFACE
  MODULE SUBROUTINE refelem_SetParam(&
    & obj, &
    & xij, &
    & entityCounts, &
    & xidimension, &
    & name, &
    & nameStr, &
    & nsd, &
    & topology, &
    & baseContinuity,  &
    & baseInterpolation, &
    & refelem)
    CLASS(AbstractRefElement_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityCounts(4)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: xidimension
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: name
    CHARACTER(*), OPTIONAL, INTENT(IN) :: nameStr
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nsd
    TYPE(ReferenceTopology_), OPTIONAL, INTENT(IN) :: topology(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuity
      !! Continuity or Conformity of basis function.
      !! H1 * Default
      !! HDiv
      !! HCurl
      !! DG
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpolation
      !! Basis function family used for interpolation
      !! LagrangeInterpolation, LagrangePolynomial
      !! SerendipityInterpolation, SerendipityPolynomial
      !! HierarchyInterpolation, HierarchyPolynomial
      !! OrthogonalInterpolation, OrthogonalPolynomial
      !! HermitInterpolation, HermitPolynomial
    TYPE(ReferenceElement_), OPTIONAL, INTENT(IN) :: refelem
  END SUBROUTINE refelem_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-08-08
! summary: Get parameters of reference element

INTERFACE
  MODULE SUBROUTINE refelem_GetParam(&
    & obj, &
    & xij, &
    & entityCounts, &
    & xidimension, &
    & name, &
    & nameStr, &
    & nsd, &
    & topology, &
    & baseContinuity,  &
    & baseInterpolation, &
    & refelem)
    CLASS(AbstractRefElement_), INTENT(IN) :: obj
    REAL(DFP), OPTIONAL, ALLOCATABLE, INTENT(OUT) :: xij(:, :)
      !! Nodal coordiantes of reference element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: entityCounts(4)
      !! Entity counts 0D to 3D
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: xidimension
      !! xi dimension of element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: name
      !! name of element
    TYPE(String), OPTIONAL, INTENT(OUT) :: nameStr
      !! string name of element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nsd
      !! spatial dimension of element
   TYPE(ReferenceTopology_), OPTIONAL, ALLOCATABLE, INTENT(OUT) :: topology(:)
      !! vector of point topology
    TYPE(String), OPTIONAL, INTENT(OUT) :: baseContinuity
      !! Continuity or Conformity of basis function.
      !! H1 * Default
      !! HDiv
      !! HCurl
      !! DG
    TYPE(String), OPTIONAL, INTENT(OUT) :: baseInterpolation
      !! Basis function family used for interpolation
      !! LagrangeInterpolation, LagrangePolynomial
      !! SerendipityInterpolation, SerendipityPolynomial
      !! HierarchyInterpolation, HierarchyPolynomial
      !! OrthogonalInterpolation, OrthogonalPolynomial
      !! HermitInterpolation, HermitPolynomial
    TYPE(ReferenceElement_), OPTIONAL, INTENT(OUT) :: refelem
  END SUBROUTINE refelem_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractRefElement_Class
