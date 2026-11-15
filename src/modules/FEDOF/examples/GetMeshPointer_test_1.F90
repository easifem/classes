!> author: Vikas Sharma, Ph. D.
! date: 2026-10-24
! summary: Get the mesh, the domain and the basic data of the FEDOF
!
! IsInitiated returns true after Initiate and false after Deallocate.
! GetMeshPointer returns the mesh of dimension nsd of the domain, and
! GetDomainPointer returns the domain. GetBaseInterpolation returns the
! first four letters of baseInterpolation in upper case ("HIER" for
! "Hierarchical"), and GetTotalVertexDOF returns the number of vertex
! nodes (one degree of freedom per vertex).
!
! The mesh very_small_quad4_mesh.h5 is the unit square with four
! Quadrangle4 elements (13 to 16) and nine nodes:
!
!   3 ---- 6 ---- 4
!   |  14  |  16  |
!   7 ---- 9 ---- 8
!   |  13  |  15  |
!   1 ---- 5 ---- 2

PROGRAM main
  USE AbstractDomain_Class, ONLY: AbstractDomain_
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEDOF_) :: obj
  TYPE(FEDomain_) :: dom
  TYPE(HDF5File_) :: meshfile
  CLASS(AbstractMesh_), POINTER :: meshptr
  CLASS(AbstractDomain_), POINTER :: domptr

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, "")

  CALL OK(.NOT. obj%IsInitiated(), "not initiated before Initiate")

  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                    order=1, dom=dom)
  CALL OK(obj%IsInitiated(), "IsInitiated")

  meshptr => obj%GetMeshPointer()
  CALL OK(ASSOCIATED(meshptr, dom%GetMeshPointer()), "GetMeshPointer")
  domptr => obj%GetDomainPointer()
  CALL OK(ASSOCIATED(domptr), "GetDomainPointer")
  CALL OK(domptr%GetTotalNodes() .EQ. 9_I4B, "domain of the FEDOF")

  CALL OK(obj%GetBaseInterpolation() .EQ. "HIER", "GetBaseInterpolation")
  CALL OK(obj%GetTotalVertexDOF() .EQ. 9_I4B, "GetTotalVertexDOF")

  meshptr => NULL()
  domptr => NULL()
  CALL obj%DEALLOCATE()
  CALL OK(.NOT. obj%IsInitiated(), "not initiated after Deallocate")

  CALL obj%DEALLOCATE()
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
