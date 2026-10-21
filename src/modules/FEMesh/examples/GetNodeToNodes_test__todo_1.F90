!> author: Vikas Sharma, Ph. D.
! summary: This example shows how to use get node to node data for edge-based
!          stabilized FEM.

PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(Mesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B), ALLOCATABLE :: n2n(:), e2e(:, :), n2e(:), nptrs(:), &
    & extranptrs(:), indx(:)
  INTEGER(I4B) :: iel, ii, iel2
  LOGICAL(LGT), ALLOCATABLE :: mask_elem(:), mask_nptrs(:)
  CHARACTER(LEN=*), PARAMETER :: filename="./mesh.h5"

  CALL meshfile%Initiate(FileName=filename, MODE="READ")
  CALL meshfile%Open()
  CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")

  ! Get node to node data for node number 10
  n2n = obj%getNodeToNodes(globalNode=10, IncludeSelf=.FALSE.)
  CALL Display(n2n, "node-to-nodes[10] = ")

  ! Get node to node data for node number 11
  n2n = obj%getNodeToNodes(globalNode=11, IncludeSelf=.FALSE.)
  CALL Display(n2n, "node-to-nodes[11] = ")

  ! Get node to node data for node number 9
  n2n = obj%getNodeToNodes(globalNode=9, IncludeSelf=.FALSE.)
  CALL Display(n2n, "node-to-nodes[9] = ")

  ! Get node to node data for node number 4
  n2n = obj%getNodeToNodes(globalNode=4, IncludeSelf=.FALSE.)
  CALL Display(n2n, "node-to-nodes[4] = ")

  CALL obj%Deallocate()
  CALL meshfile%Deallocate()
END PROGRAM main
