!> author: Vikas Sharma, Ph. D.
! date: 2025-06-07
! summary:  Get case name

PROGRAM main
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE HDF5File_Class, ONLY: HDF5File_

  USE Test_Method, ONLY: ok
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE BaseType, ONLY: TypeQuadratureOpt

  IMPLICIT NONE

  TYPE(FEDOF_) :: obj
  TYPE(FEDomain_) :: dom
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/small_tri3_mesh.h5"
  TYPE(HDF5File_) :: meshfile
  CHARACTER(6) :: found, want

  CALL e%setQuietMode(EXCEPTION_INFORMATION, .TRUE.)
  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')

  meshptr => dom%GetMeshPointer()

  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                    order=1, dom=dom)
  found = obj%GetCaseName()
  want = "H1HIER"
  CALL OK(found == want, "GetCaseName H1HIER: ")

  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                    order=1, dom=dom)
  found = obj%GetCaseName()
  want = "H1HIER"
  CALL OK(found == want, "GetCaseName H1HIER: ")

  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Lagrange", &
                    order=1, dom=dom, ipType=TypeQuadratureOpt%equidistance)
  found = obj%GetCaseName()
  want = "H1LAGR"
  CALL OK(found == want, "GetCaseName H1LAGR: ")

!CALL dom%Display("domain:")
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()

END PROGRAM main
