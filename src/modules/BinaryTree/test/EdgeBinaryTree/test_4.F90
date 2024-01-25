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

! In this example, I will show how to make edge_connectivity of mesh

PROGRAM main
USE easifemBase
USE easifemClasses
USE EdgeTreeData_Class
USE EdgeBinaryTree_Class
USE EdgeBinaryTreeUtility

IMPLICIT NONE

TYPE(EdgeTreeData_) :: VALUE
TYPE(EdgeTreeData_), POINTER :: value_ptr
TYPE(EdgeBinaryTree_) :: obj, anode
INTEGER(I4B), ALLOCATABLE :: node_connectivity(:, :), edge_connectivity(:, :)
INTEGER(I4B) :: localEdges(2, 4), nptrs(4), edge(2), sorted_edge(2)
INTEGER(I4B) :: iel, iedge

localEdges(:, 1) = [1, 2]
localEdges(:, 2) = [4, 3]
localEdges(:, 3) = [1, 4]
localEdges(:, 4) = [2, 3]

CALL Reallocate(node_connectivity, 4, 3)
node_connectivity(:, 1) = [1, 2, 3, 4]
node_connectivity(:, 2) = [2, 5, 6, 3]
node_connectivity(:, 3) = [2, 7, 8, 5]

CALL obj%Initiate()

DO iel = 1, SIZE(node_connectivity, 2)
  nptrs = node_connectivity(:, iel)
  DO iedge = 1, SIZE(localEdges, 2)
    edge = nptrs(localEdges(:, iedge))
    sorted_edge = SORT(edge)
    value_ptr => EdgeTreeData_Pointer(sorted_edge)
    CALL obj%Insert(value_ptr)
  END DO
END DO

CALL obj%SetID()

CALL Reallocate(edge_connectivity, 4, SIZE(node_connectivity, 2))

DO iel = 1, SIZE(node_connectivity, 2)
  nptrs = node_connectivity(:, iel)

  DO iedge = 1, SIZE(localEdges, 2)

    edge = nptrs(localEdges(:, iedge))
    VALUE = SORT(edge)
    value_ptr => obj%GetValuePointer(VALUE)
    IF (edge(1) .GT. edge(2)) THEN
      edge_connectivity(iedge, iel) = -value_ptr%id
    ELSE
      edge_connectivity(iedge, iel) = value_ptr%id
    END IF

  END DO

END DO

CALL Display(edge_connectivity, "edge_connectivity:")

END PROGRAM main
