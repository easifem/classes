!> author: Vikas Sharma, Ph. D.
! summary: IsElementPresent_test_1

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
CALL OK(obj%isElementPresent(15), "isElementPresent: ")
CALL OK(obj%isElementPresent(24), "isElementPresent: ")
CALL OK(obj%isElementPresent(5, islocal=.TRUE.), "isElementPresent: ")
CALL OK(obj%isElementPresent(6, islocal=.TRUE.), "isElementPresent: ")
CALL OK(.NOT. obj%isElementPresent(50), "isElementPresent: ")
CALL OK(.NOT. obj%isElementPresent(5), "isElementPresent: ")
CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
