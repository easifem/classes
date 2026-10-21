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
  n2e = obj%getNodeToElements(globalNode=10)
  CALL Display(n2e, "node-to-elements[10] = ")
  !!
  DO iel = 1, SIZE(n2e)
    e2e = obj%getElementToElements(globalElement=n2e(iel), &
      & onlyElements=.TRUE.)
    CALL Display(e2e, "element-to-elements( " // tostring(n2e(iel)) // " ) = ")
    !!
    mask_elem = .NOT. (e2e(:, 1) .ISIN. n2e)
    !!
    DO iel2 = 1, SIZE(mask_elem)
      IF(mask_elem(iel2)) THEN
        indx = obj%getConnectivity(globalElement=e2e(iel2, 1))
        CALL Display(indx, "nptrs ( " // tostring(e2e(iel2, 1)) // " ) = ")
        mask_nptrs = .NOT. (indx .ISIN. n2n)
        CALL APPEND(extranptrs, indx, mask_nptrs)
        CALL Display(extranptrs, " extranptrs = ")
      END IF
    END DO
  END DO

  CALL obj%Deallocate()
  CALL meshfile%Deallocate()
END PROGRAM main
