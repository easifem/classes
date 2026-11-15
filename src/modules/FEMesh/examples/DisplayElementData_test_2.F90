!> author: Vikas Sharma, Ph. D.
! date: 2025-06-02
! summary:  Testing DisplayElementData method of FEMesh class
!
! This example shows how to use `DisplayElementData` method to display the
! content of element data inside the mesh.

PROGRAM main
USE FEMesh_Class
USE HDF5File_Class
USE GlobalData

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
INTEGER(I4B) :: iel, ii
CHARACTER(LEN=*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"

CALL meshfile%Initiate(FileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL obj%Initiate(hdf5=meshfile, dim=2)
CALL obj%DisplayElementData("element data:", globalElement=1, islocal=.TRUE.)

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
