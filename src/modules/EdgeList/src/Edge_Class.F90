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

MODULE Edge_Class
USE GlobalData, ONLY: I4B, LGT, DFP
IMPLICIT NONE
PRIVATE

PUBLIC :: Edge_
PUBLIC :: Edge
PUBLIC :: TypeEdge

INTEGER(I4B), PARAMETER :: MAX_INT_STR_LEN = 64
!! maximum length of an integer string

!----------------------------------------------------------------------------
!                                                                      Edge_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Edge

TYPE :: Edge_
  INTEGER(I4B) :: ivertex = 0
  !! vertex number (the index in the [[dag]] `vertices` array)
  !! the "to" vertex that defines an edge. This is part of
  !! the array of vertices contained without the "from" [[vertex]] type.
  !! an edge can also have optional attrubutes for graphviz.
END TYPE Edge_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-23
! summary:  Edge constructor

INTERFACE Edge
  MODULE PURE ELEMENTAL FUNCTION edge_constructor(ivertex) RESULT(ans)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: ivertex
    !! vertex number defining the destination of this edge
    TYPE(Edge_) :: ans
  END FUNCTION edge_constructor
END INTERFACE Edge

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE(Edge_), PARAMETER :: TypeEdge = Edge_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-23
! summary:  a vertex (or node) of a directed acyclic graph (DAG)

TYPE :: Vertex_
  PRIVATE
  TYPE(Edge_), ALLOCATABLE :: edges(:)
  !! these are the vertices that this vertex
  !! depends on. (edges of the graph).
  INTEGER(I4B) :: ivertex = 0
  !! vertex number (the index in the [[dag]] `vertices` array)
  LOGICAL(LGT) :: checking = .FALSE.
  !! used for toposort
  LOGICAL(LGT) :: marked = .FALSE.
  !! used for toposort
CONTAINS
  PRIVATE
  PROCEDURE, PASS(obj) :: vertex_SetEdges1
  PROCEDURE, PASS(obj) :: vertex_SetEdges2
  GENERIC :: SetEdges => vertex_SetEdges1, vertex_SetEdges2
  PROCEDURE :: removeEdge
END TYPE Vertex_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE vertex_SetEdges1(obj, ivertex)
    CLASS(Vertex_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivertex
  END SUBROUTINE vertex_SetEdges1
END INTERFACE

END MODULE Edge_Class
