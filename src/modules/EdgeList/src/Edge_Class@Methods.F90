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

SUBMODULE(Edge_Class) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Edge
!----------------------------------------------------------------------------

MODULE PROCEDURE edge_constructor
IF (PRESENT(ivertex)) ans%ivertex = ivertex
END PROCEDURE edge_constructor

!----------------------------------------------------------------------------
!                                                                 Set_Edges
!----------------------------------------------------------------------------

MODULE PROCEDURE vertex_SetEdges1
TYPE(edge_) :: edge0

edge_ = edge(ivertex=ivertex)

IF (ALLOCATED(obj%edges)) THEN
  IF (.NOT. ANY(ivertex == obj%edges%ivertex)) THEN ! don't add if already there
    me%edges = [obj%edges, edge_]
    CALL sort_ascending(obj%edges)
  END IF
ELSE
  me%edges = [edge_]
END IF

END SUBROUTINE add_edge
END SUBMODULE Methods
