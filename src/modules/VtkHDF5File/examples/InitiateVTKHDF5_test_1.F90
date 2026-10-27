!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-26: InitiateVTKHDF5 with vtkType, more checks
!   - 2026-10-26: IsOutputStep in modes NEW and READ
! summary: Initiate a VTKHDF file for writing and reading

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE Test_Method, ONLY: OK
  USE VtkHDF5File_Class, ONLY: VtkHDF5File_
  IMPLICIT NONE
  CHARACTER(*), PARAMETER :: filename = "InitiateVTKHDF5_test_1.vtkhdf"
  TYPE(VtkHDF5File_) :: obj
  LOGICAL :: isok
  CALL obj%InitiateVTKHDF5(filename=filename, mode="NEW", &
                           vtkType="UnstructuredGrid")
  CALL OK(obj%IsInitiated(), "InitiateVTKHDF5 NEW: initiated")
  CALL OK(obj%PathExists("/VTKHDF"), "InitiateVTKHDF5 NEW: /VTKHDF")
  CALL OK(obj%IsOutputStep(), "IsOutputStep: NEW, not transient")
  CALL obj%DEALLOCATE()
  INQUIRE(FILE=filename, EXIST=isok)
  CALL OK(isok, "InitiateVTKHDF5 NEW: file exists")
  CALL obj%InitiateVTKHDF5(filename=filename, mode="READ", &
                           vtkType="UnstructuredGrid")
  CALL OK(obj%IsInitiated(), "InitiateVTKHDF5 READ: initiated")
  CALL OK(.NOT. obj%IsOutputStep(), "IsOutputStep: READ")
  CALL OK(obj%GetTotalSteps() .EQ. 0, "InitiateVTKHDF5 READ: no steps")
  CALL obj%DEALLOCATE()
  CALL OK(.NOT. obj%IsInitiated(), "Deallocate")
END PROGRAM main
