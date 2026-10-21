!> author: Vikas Sharma, Ph. D.
! summary: GetElementToElements_test_3

PROGRAM main
USE easifemBase
USE easifemClasses
IMPLICIT NONE
TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
  CHARACTER(*), PARAMETER :: filename="../../Mesh/examples/meshdata/small_mesh.h5"

CALL meshfile%Initiate(filename, "READ")
CALL meshfile%OPEN()
CALL obj%Initiate(hdf5=meshfile, group="/curveEntities_1")

! Now we get the full information about the element
! connected to the given global element numbers.

! CALL obj%DisplayMeshInfo("mesh info")

CALL obj%InitiateElementToElements()

BLOCK
  INTEGER(I4B), ALLOCATABLE :: e2e(:, :), nptrs2(:), nptrs1(:)
  INTEGER(I4B), PARAMETER :: globalElement = 5
  INTEGER(I4B) :: ii

  e2e = obj%GetElementToElements(globalElement=globalElement, &
    & onlyElements=.FALSE.)

  CALL OK(ALL(SHAPE(e2e) .EQ. [1, 3]), "GetElementToElements: ")

  DO ii = 1, SIZE(e2e, 1)
    nptrs1 = obj%GetFacetConnectivity(globalElement, e2e(ii, 2))
    nptrs2 = obj%GetFacetConnectivity(e2e(ii, 1), e2e(ii, 3))
    ! CALL Display(nptrs1, "nptrs1: ")
    ! CALL Display(nptrs2, "nptrs2: ")
    CALL OK(ALL(nptrs1 .EQ. nptrs2), "GetElementToElements: ")
  END DO
END BLOCK

CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
