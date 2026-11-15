!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Testing SetMaterialToMesh of MeshSelection_
!
! The surface mesh has entity 1 (elements 18, 19) and entity 2 (elements
! 20, 21). The curve mesh has entity 1 with elements 7, 8.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE MeshSelection_Class, ONLY: MeshSelection_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = &
                           "../../FEMesh/examples/meshdata/very_small_quad4_mesh_two_region.h5"
TYPE(MeshSelection_) :: obj
TYPE(FEDomain_) :: dom
TYPE(HDF5File_) :: meshfile
CLASS(AbstractMesh_), POINTER :: mesh

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')
CALL dom%SetTotalMedium(dim=2_I4B, n=2_I4B)
CALL dom%SetTotalMedium(dim=1_I4B, n=1_I4B)

! Test 1: selection by mesh id and by element number, explicit dim
BLOCK
  mesh => dom%GetMeshPointer(dim=2)

  CALL obj%Initiate()
  CALL obj%Add(dim=2, meshID=[2])
  CALL obj%Set()
  CALL obj%SetMaterialToMesh(dom=dom, dim=2_I4B, medium=1_I4B, &
                             material=5_I4B)
  CALL OK(mesh%GetMaterial(medium=1, globalElement=20) .EQ. 5, &
          "meshID: first element of entity")
  CALL OK(mesh%GetMaterial(medium=1, globalElement=21) .EQ. 5, &
          "meshID: second element of entity")

  CALL obj%Initiate()
  CALL obj%Add(dim=2, elemNum=[18])
  CALL obj%Set()
  CALL obj%SetMaterialToMesh(dom=dom, dim=2_I4B, medium=1_I4B, &
                             material=7_I4B)
  CALL OK(mesh%GetMaterial(medium=1, globalElement=18) .EQ. 7, &
          "elemNum: selected element")
  CALL OK(mesh%GetMaterial(medium=1, globalElement=19) .NE. 7, &
          "elemNum: other element is unchanged")
  CALL OK(mesh%GetMaterial(medium=1, globalElement=20) .EQ. 5, &
          "elemNum: other entity is unchanged")
END BLOCK

! Test 2: dim defaults to nsd, each medium has its own material
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=2, meshID=[1])
  CALL obj%Set()
  CALL obj%SetMaterialToMesh(dom=dom, medium=2_I4B, material=3_I4B)
  CALL OK(mesh%GetMaterial(medium=2, globalElement=19) .EQ. 3, &
          "nsd: medium=2 is set")
  CALL OK(mesh%GetMaterial(medium=1, globalElement=18) .EQ. 7, &
          "nsd: medium=1 is unchanged")
END BLOCK

! Test 3: lower dimensional mesh
BLOCK
  CLASS(AbstractMesh_), POINTER :: curve

  curve => dom%GetMeshPointer(dim=1)
  CALL obj%Initiate()
  CALL obj%Add(dim=1, meshID=[1])
  CALL obj%Set()
  CALL obj%SetMaterialToMesh(dom=dom, dim=1_I4B, medium=1_I4B, &
                             material=9_I4B)
  CALL OK(curve%GetMaterial(medium=1, globalElement=7) .EQ. 9, &
          "curve mesh: selected element")
  curve => NULL()
END BLOCK

mesh => NULL()
CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
