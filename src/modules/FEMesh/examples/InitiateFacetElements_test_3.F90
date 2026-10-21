!> author: Vikas Sharma, Ph. D.
! date:  2025-06-03
! summary:  Test for InitiateFacetElements method of FEMesh class
!

PROGRAM main
USE FEMesh_Class
USE HDF5File_Class
USE GlobalData
USE Display_Method
use Test_Method

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(LEN=*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
CHARACTER(*), PARAMETER :: testname="InitiateFacetElements_test_3"
INTEGER(I4B) :: found, want

CALL meshfile%Initiate(FileName=filename, MODE="READ")

CALL meshfile%OPEN()

CALL obj%Initiate(hdf5=meshfile, dim=2)

CALL obj%InitiateFacetElements()

found = obj%GetTotalFacetElements()
want = 25
CALL OK(found .EQ. want, "GetTotalFacetElements: ")

found = obj%GetTotalBoundaryFacetElements()
want = 8
CALL OK(found .EQ. want, "GetTotalBoundaryFacetElements: ")

found = obj%GetTotalInternalFacetElements()
want = 17
CALL OK(found .EQ. want, "GetTotalInternalFacetElements: ")

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()

CALL Display("Finish test: " // testname)
END PROGRAM main
