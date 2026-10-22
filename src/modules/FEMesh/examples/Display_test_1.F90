!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Display a mesh
!
! Display prints the content of the mesh. The other Display methods
! print the mesh statistics, node data, element data, and facet
! data. They do not change the mesh.
!
! The mesh very_small_quad4_mesh.h5 is the unit square with four
! Quadrangle4 elements (global numbers 13 to 16) and nine nodes:
!
!   3 ---- 6 ---- 4
!   |  14  |  16  |
!   7 ---- 9 ---- 8
!   |  13  |  15  |
!   1 ---- 5 ---- 2

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
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL obj%Display("mesh:")
  CALL obj%DisplayMeshInfo("mesh info:")
  CALL obj%DisplayNodeData("node data:")
  CALL obj%DisplayElementData("element 13:", globalElement=13_I4B)
  CALL obj%InitiateFacetElements()
  CALL obj%DisplayFacetData("facet data:")
  CALL obj%DisplayInternalFacetData("internal facet data:")
  CALL obj%DisplayBoundaryFacetData("boundary facet data:")
  CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "mesh is not changed")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
