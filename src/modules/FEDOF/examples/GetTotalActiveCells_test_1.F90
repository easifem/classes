!> author: Vikas Sharma, Ph. D.
! date: 2025-06-06
! summary:  Test get total active cells.

PROGRAM main
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE BaseType, ONLY: math => TypeMathOpt
  USE BaseType, ONLY: quadopt => TypeQuadratureOpt
  USE Display_Method, ONLY: Display
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK
  USE Test_Method, ONLY: IS
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
  TYPE(FEDOF_) :: obj
  TYPE(FEDomain_) :: dom
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: found, want
  LOGICAL(LGT) :: isok

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')
  meshptr => dom%GetMeshPointer()

  CALL obj%Initiate( &
                     order=1, &
                     dom=dom, &
                     baseContinuity="H1", &
                     baseInterpolation="Hierarchical", &
                     quadratureIsHomogeneous=math%yes, &
                     quadratureType=[quadopt%GaussLegendre], &
                     quadratureOrder=[2], &
                     quadratureIsOrder=math%yes)

  found = obj%GetTotalActiveCells()
  want = meshptr%GetTotalElements()
  isok = found .EQ. want
  CALL OK(isok, "GetTotalActiveCells order = 1: ")

END PROGRAM main
! ok 1 - GetTotalCells order = 1:
