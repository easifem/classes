!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Set the materials of the elements
!
! SetTotalMedium sets the number of media in the elements of the mesh
! of dimension dim to n. SetMaterial sets the material id of a medium in
! the elements of an entity. GetTotalMaterial returns the number of
! media of an element.
!
! The mesh very_small_quad4_mesh_two_region.h5 is [0, 2] x [0, 1] with
! two surface entities: entity 1 has elements 18 and 19, entity 2 has
! elements 20 and 21. Line element 16 [2, 5] is the interface.
!
!   4 ---- 9 ---- 5 ---- 10 ---- 6
!   |  18  |  19  |  20  |  21   |
!   1 ---- 7 ---- 2 ---- 8 ----- 3

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
    "../../FEMesh/examples/meshdata/very_small_quad4_mesh_two_region.h5"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEDomain_) :: obj
  CLASS(AbstractMesh_), POINTER :: mesh

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, group="")

  CALL obj%SetTotalMedium(DIM=2_I4B, n=1_I4B)
  CALL OK(obj%GetTotalMaterial(DIM=2_I4B, globalElement=18_I4B) &
          .EQ. 1_I4B, "GetTotalMaterial")
  CALL obj%SetMaterial(DIM=2_I4B, entityNum=1_I4B, medium=1_I4B, &
                       material=8_I4B)
  CALL obj%SetMaterial(DIM=2_I4B, entityNum=2_I4B, medium=1_I4B, &
                       material=9_I4B)
  mesh => obj%GetMeshPointer()
  CALL OK(mesh%GetMaterial(medium=1_I4B, globalElement=18_I4B) &
          .EQ. 8_I4B, "entity 1")
  CALL OK(mesh%GetMaterial(medium=1_I4B, globalElement=21_I4B) &
          .EQ. 9_I4B, "entity 2")
  mesh => NULL()

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
