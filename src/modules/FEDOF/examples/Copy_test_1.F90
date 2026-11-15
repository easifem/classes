!> author: Vikas Sharma, Ph. D.
! date: 2025-06-06
! summary: This method tests the COPY method for FEDOF class.

PROGRAM main
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE HDF5File_Class, ONLY: HDF5File_

  USE GlobalData, ONLY: I4B
  USE Test_Method, ONLY: ok
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  IMPLICIT NONE

  TYPE(FEDOF_) :: obj, obj2
  TYPE(FEDomain_) :: dom
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  CHARACTER(*), PARAMETER :: &
    filename = "./meshdata/small_tri3_mesh.h5", &
    baseInterpolation = "Hierarchical", &
    baseContinuity = "H1"

  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: found, want

  CALL e%setQuietMode(EXCEPTION_INFORMATION, .TRUE.)
  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')

  meshptr => dom%GetMeshPointer()

  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, order=1, dom=dom)
!CALL fedof%Display("FEDOF:")
  CALL obj2%copy(obj)

  found = obj2%GetTotalDOF()
  want = meshptr%GetTotalNodes()
  CALL OK(found == want, "Total DOF (order=1): ")

  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, order=2, dom=dom)
  obj2 = obj
  found = obj2%GetTotalDOF()
  want = meshptr%GetTotalNodes() + meshptr%GetTotalFaces()
  CALL OK(found == want, "Total DOF (order=2): ")

  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, order=3, dom=dom)
  CALL obj2%Copy(obj)

  found = obj2%GetTotalDOF()
  want = meshptr%GetTotalNodes() + 2 * meshptr%GetTotalFaces() + &
    meshptr%GetTotalCells()
  CALL OK(found == want, "Total DOF (order=3): ")

  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, order=4, dom=dom)
  obj2 = obj

  found = obj2%GetTotalDOF()
  want = meshptr%GetTotalNodes() + 3 * meshptr%GetTotalFaces() + &
    3 * meshptr%GetTotalCells()
  CALL OK(found == want, "Total DOF (order=4): ")

!CALL dom%Display("domain:")
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()

END PROGRAM main
