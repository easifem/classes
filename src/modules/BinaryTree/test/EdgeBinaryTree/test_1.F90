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

PROGRAM main
USE easifemBase
USE easifemClasses
USE EdgeBinaryTree_Class
USE EdgeTreeData_Class
IMPLICIT NONE

TYPE(EdgeTreeData_) :: VALUE
TYPE(EdgeTreeData_), POINTER :: value_ptr
TYPE(EdgeBinaryTree_) :: obj, anode
INTEGER(I4B), ALLOCATABLE :: node_connectivity(:, :), edge_connectivity(:, :)
INTEGER(I4B) :: localEdges(2, 4), nptrs(4), edge(2), sorted_edge(2)
INTEGER(I4B) :: iel, iedge, tsize1, tsize2

localEdges(:, 1) = [1, 2]
localEdges(:, 2) = [4, 3]
localEdges(:, 3) = [1, 4]
localEdges(:, 4) = [2, 3]

CALL Reallocate(node_connectivity, 4, 6)
node_connectivity(:, 1) = [1, 2, 5, 4]
node_connectivity(:, 2) = [2, 3, 6, 5]
node_connectivity(:, 3) = [4, 5, 8, 7]
node_connectivity(:, 4) = [5, 6, 9, 8]
node_connectivity(:, 5) = [7, 8, 11, 10]
node_connectivity(:, 6) = [8, 9, 12, 11]

CALL Reallocate(edge_connectivity, 4, 6)

CALL obj%Initiate()

DO iel = 1, SIZE(node_connectivity, 2)

  nptrs = node_connectivity(:, iel)

  DO iedge = 1, SIZE(localEdges, 2)

    edge = nptrs(localEdges(:, iedge))
    sorted_edge = SORT(edge)
    value_ptr => EdgeTreeData_Pointer(sorted_edge)

    tsize1 = obj%SIZE()
    CALL obj%Insert(value_ptr)
    tsize2 = obj%SIZE()

    IF (tsize1 .NE. tsize2) THEN
      edge_connectivity(iedge, iel) = tsize2
      value_ptr%id = tsize2
    ELSE
      CALL Initiate(VALUE, sorted_edge)
      value_ptr => obj%GetValuePointer(VALUE)
      edge_connectivity(iedge, iel) = value_ptr%id
    END IF

  END DO

END DO

CALL obj%Display("edges:")
CALL Display(edge_connectivity, "edge_connectivity")

CALL Initiate(VALUE, [7, 8])
anode = obj%GetNode(VALUE)
CALL obj%Remove(anode)
! CALL obj%Remove(value)

CALL Initiate(VALUE, [8, 9])
anode = obj%GetNode(VALUE)
CALL obj%Remove(anode)

CALL Initiate(VALUE, [8, 11])
anode = obj%GetNode(VALUE)
CALL obj%Remove(anode)

CALL Initiate(VALUE, [5, 8])
anode = obj%GetNode(VALUE)
CALL obj%Remove(anode)

CALL EqualLine()

CALL EdgeBinaryTree_RenumberEdges(obj)

CALL obj%Display("after removing some edges:")

END PROGRAM main
