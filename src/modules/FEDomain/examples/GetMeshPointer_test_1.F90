!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the mesh of a dimension
!
! GetMeshPointer returns the mesh of dimension dim, or the mesh of
! dimension nsd if dim is absent. The mesh of a dimension contains
! the elements of all entities of that dimension. There is no
! volume mesh in a 2D domain.
!
! The mesh very_small_quad4_mesh.h5 is the unit square: four
! Quadrangle4 elements (13 to 16), eight Line2 elements (5 to 12) on
! the boundary curves, four Point elements (1 to 4), and nine nodes:
!
!   3 ---- 6 ---- 4
!   |  14  |  16  |
!   7 ---- 9 ---- 8
!   |  13  |  15  |
!   1 ---- 5 ---- 2

PROGRAM main
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj
  CLASS(AbstractMesh_), POINTER :: mesh

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  mesh => obj%GetMeshPointer()
  CALL OK(mesh%GetXidimension() .EQ. 2_I4B, "default is nsd")
  CALL OK(mesh%GetTotalElements() .EQ. 4_I4B, "surface mesh")
  mesh => obj%GetMeshPointer(DIM=1_I4B)
  CALL OK(mesh%GetTotalElements() .EQ. 8_I4B, "curve mesh")
  mesh => obj%GetMeshPointer(DIM=0_I4B)
  CALL OK(mesh%GetTotalElements() .EQ. 4_I4B, "point mesh")
  mesh => obj%GetMeshPointer(DIM=3_I4B)
  CALL OK(.NOT. ASSOCIATED(mesh), "no volume mesh")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
