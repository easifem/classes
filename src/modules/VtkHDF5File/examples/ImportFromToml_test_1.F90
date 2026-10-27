!> author: Shion Shimizu
! date: 2026-10-24
! summary: Import VTKHDF options from toml

PROGRAM main
  USE Test_Method, ONLY: OK
  USE tomlf, ONLY: toml_table
  USE tomlf, ONLY: toml_load
  USE VtkHDF5File_Class, ONLY: VtkHDF5File_
  IMPLICIT NONE
  TYPE(VtkHDF5File_) :: obj
  TYPE(toml_table), ALLOCATABLE :: table
  CALL toml_load(table, "VtkHDF5File_test.toml")
  CALL obj%ImportFromToml(table)
  CALL OK(obj%IsActive(), "ImportFromToml")
  CALL obj%DEALLOCATE()
END PROGRAM main
