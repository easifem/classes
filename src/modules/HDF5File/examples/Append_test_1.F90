!> author: Shion Shimizu
! date: 2026-10-24
! summary: Append to extendable datasets and read slabs

PROGRAM main
USE GlobalData, ONLY: I4B, DFP
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(HDF5File_) :: h5
REAL(DFP), ALLOCATABLE :: vals(:), xij(:, :)
INTEGER(I4B) :: offset
CALL h5%Initiate(filename="test_append.h5", mode="NEW")
CALL h5%OPEN()
CALL h5%Append("/steps/values", [0.0_DFP], offset)
CALL OK(offset .EQ. 0, "Append: offset of first call")
CALL h5%Append("/steps/values", [0.1_DFP, 0.2_DFP], offset)
CALL OK(offset .EQ. 1, "Append: offset of second call")
CALL OK(h5%GetExtent("/steps/values") .EQ. 3, "Append: extent")
CALL h5%ReadSlab("/steps/values", 1, 2, vals)
CALL OK(ALL(ABS(vals - [0.1_DFP, 0.2_DFP]) .LT. 1.0E-12_DFP), &
        "ReadSlab rank-1")
! rank-2: points (3, nPoints), appended along nPoints
CALL h5%Append("/points", RESHAPE([0.0_DFP, 0.0_DFP, 0.0_DFP, &
               1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 2]))
CALL h5%Append("/points", RESHAPE([0.0_DFP, 1.0_DFP, 0.0_DFP], &
               [3, 1]), offset)
CALL OK(offset .EQ. 2, "Append rank-2: offset")
CALL h5%ReadSlab("/points", 2, 1, xij)
CALL OK(ABS(xij(2, 1) - 1.0_DFP) .LT. 1.0E-12_DFP, "ReadSlab rank-2")
CALL h5%DEALLOCATE()
END PROGRAM main
