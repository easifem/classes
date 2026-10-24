!> author: Shion Shimizu
! date: 2026-10-24
! summary: Write and read a rank-1 integer attribute

PROGRAM main
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(HDF5File_) :: h5
INTEGER(I4B), ALLOCATABLE :: version(:)
CALL h5%Initiate(filename="test_attr.h5", mode="NEW")
CALL h5%OPEN()
CALL h5%mkdir("/VTKHDF")
CALL h5%WriteAttribute("/VTKHDF", "Version", [2_I4B, 2_I4B])
CALL h5%ReadAttribute("/VTKHDF", "Version", version)
CALL OK(ALL(version .EQ. [2, 2]), "WriteAttribute rank-1")
CALL h5%DEALLOCATE()
END PROGRAM main
