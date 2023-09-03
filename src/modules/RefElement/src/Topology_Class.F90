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
! summary: Topology class is defined

MODULE Topology_Class
USE GlobalData
USE String_Class, ONLY: String
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                            Topology_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Topology class is defined
!
!{!pages/docs-api/Topology/Topology_.md!}

TYPE :: Topology_
  PRIVATE
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  !! node numbers
  INTEGER(I4B) :: name = 0
  !! name of topology
  INTEGER(I4B) :: xiDimension = 0
  !! xidimension
  !!
CONTAINS
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => topo_Initiate
  !! Initiate the topology object
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the topology object
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content
  PROCEDURE, PUBLIC, PASS(obj) :: MdEncode => obj_MdEncode
  !! Display the content
  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrs => obj_GetNptrs
  !! Get the nptrs
  PROCEDURE, PUBLIC, PASS(obj) :: GetName => obj_GetName
  !! Get the name
  PROCEDURE, PUBLIC, PASS(obj) :: GetXiDimension => obj_GetXiDimension
  !! Get the xidimension
  PROCEDURE, PUBLIC, PASS(obj) :: GetNNE => obj_GetNNE
  !! Return the size of nptrs
END TYPE Topology_

PUBLIC :: Topology_

!----------------------------------------------------------------------------
!                                                     TopologyPointer_
!----------------------------------------------------------------------------

TYPE :: TopologyPointer_
  CLASS(Topology_), POINTER :: ptr => NULL()
END TYPE TopologyPointer_

PUBLIC :: TopologyPointer_

!----------------------------------------------------------------------------
!                                                         Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary:         Initiate the object

INTERFACE
  MODULE PURE SUBROUTINE topo_Initiate(obj, nptrs, name, xiDimension)
    CLASS(Topology_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    INTEGER(I4B), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: xiDimension
  END SUBROUTINE topo_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary:         Deallocate the contents

INTERFACE
  MODULE PURE SUBROUTINE obj_Deallocate(obj)
    CLASS(Topology_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Display the contents

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(Topology_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary: Display the contents

INTERFACE
  MODULE FUNCTION obj_MdEncode(obj) RESULT(ans)
    CLASS(Topology_), INTENT(IN) :: obj
    TYPE(String) :: ans
  END FUNCTION obj_MdEncode
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetNptrs@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary:         Get the nptrs
INTERFACE
  MODULE PURE FUNCTION obj_GetNptrs(obj) RESULT(ans)
    CLASS(Topology_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetName@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary:         Get the name

INTERFACE
  MODULE ELEMENTAL FUNCTION obj_GetName(obj) RESULT(ans)
    CLASS(Topology_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetXidimension@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary:         Get the xidimension

INTERFACE
  MODULE ELEMENTAL FUNCTION obj_GetXiDimension(obj) RESULT(ans)
    CLASS(Topology_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetXiDimension
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetNNE@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 Aug 2022
! summary:         Get the size of nptrs

INTERFACE
  MODULE ELEMENTAL FUNCTION obj_GetNNE(obj) RESULT(ans)
    CLASS(Topology_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNNE
END INTERFACE

!----------------------------------------------------------------------------
!                                                      FacetTopology@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns the facet topology of the given element type

INTERFACE
  MODULE PURE FUNCTION obj_GetFacetTopology(elemType, nptrs) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    TYPE(Topology_), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetFacetTopology
END INTERFACE

INTERFACE GetFacetTopology
  MODULE PROCEDURE obj_GetFacetTopology
END INTERFACE GetFacetTopology

PUBLIC :: GetFacetTopology

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Topology_Class
