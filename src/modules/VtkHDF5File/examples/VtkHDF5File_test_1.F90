!> author: Shion Shimizu
! date: 2026-10-24
! summary: Initiate a VTKHDF file for writing and reading

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE Test_Method, ONLY: OK
  USE VtkHDF5File_Class, ONLY: VtkHDF5File_
  IMPLICIT NONE
  CHARACTER(*), PARAMETER :: filename = "VtkHDF5File_test_1.vtkhdf"
  TYPE(VtkHDF5File_) :: obj
  LOGICAL :: isok
  CALL obj%InitiateVTKHDF(filename=filename, mode="NEW", &
                          isTransient=math%no)
  CALL obj%DEALLOCATE()
  INQUIRE(FILE=filename, EXIST=isok)
  CALL OK(isok, "InitiateVTKHDF NEW")
  CALL obj%InitiateVTKHDF(filename=filename, mode="READ")
  CALL OK(obj%GetTotalSteps() .EQ. 0, "InitiateVTKHDF READ")
  CALL obj%DEALLOCATE()
END PROGRAM main
