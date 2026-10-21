!> author: Vikas Sharma, Ph. D.
! summary: IsNodePresent_test_1

PROGRAM main
USE easifemBase
USE easifemClasses
IMPLICIT NONE
TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
INTEGER(I4B) :: iel, ii
CHARACTER(*), PARAMETER :: filename="../../Mesh/examples/meshdata/small_mesh.h5"
CALL meshfile%Initiate(FileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL obj%Initiate(hdf5=meshfile, dim=2)
CALL OK(obj%isNodePresent(10), "isNodePresent: ")
CALL OK(obj%isNodePresent(5), "isNodePresent: ")
CALL OK(obj%isNodePresent(5, islocal=.TRUE.), "isNodePresent: ")
CALL OK(.NOT. obj%isNodePresent(50), "isNodePresent: ")
CALL OK(obj%isAnyNodePresent([10, 50]), "isAnyNodePresent: ")
CALL OK(obj%isAnyNodePresent([10, 5]), "isAnyNodePresent: ")
CALL OK(.NOT. obj%isAnyNodePresent([-1, 50]), "isAnyNodePresent: ")
CALL OK(obj%isAnyNodePresent([10, 50], islocal=.true.), "isAnyNodePresent: ")
CALL OK(obj%isAnyNodePresent([10, 5], islocal=.true.), "isAnyNodePresent: ")
CALL OK(.NOT. obj%isAnyNodePresent([54, 50], islocal=.true.), "isAnyNodePresent: ")
CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
