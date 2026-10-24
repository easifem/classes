!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: InitiateFacetElements does nothing for a curve mesh
!
! Point and curve meshes (xidim <= 1) have no facet elements, so
! InitiateFacetElements returns without making facet data. The curve
! mesh is the boundary of the unit square (dim=1).

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEMesh_Class, ONLY: FEMesh_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=1)

  CALL obj%InitiateFacetElements()
  CALL OK(.NOT. obj%IsFacetData(), "IsFacetData")
  CALL OK(obj%GetTotalFacetElements() .EQ. 0_I4B, &
    "GetTotalFacetElements")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
