!> author: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Lagrange polynomial is tested in this example

PROGRAM main
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE HDF5File_Class, ONLY: HDF5File_
  USE Display_Method, ONLY: Display
  USE Display_Method, ONLY: ToString
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE GlobalData, ONLY: DFP
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE BaseType, ONLY: poly => TypePolynomialOpt
  USE BaseType, ONLY: math => TypeMathOpt
  USE Test_Method, ONLY: OK
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri6_mesh.h5"
  INTEGER(I4B), PARAMETER :: order = 2, ipType = poly%monomial
  CHARACTER(*), PARAMETER :: baseContinuity = "H1"
  CHARACTER(*), PARAMETER :: baseInterpolation = "Lagrange"

  TYPE(FEDOF_) :: fedof
  TYPE(FEDomain_) :: dom
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: found, want
  LOGICAL(LGT) :: isok

  CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')
  meshptr => dom%GetMeshPointer()

  CALL fedof%Initiate( &
                       baseContinuity=baseContinuity, ipType=ipType, &
                       baseInterpolation=baseInterpolation, &
                       order=order, dom=dom)

  found = fedof%GetTotalDOF()
  want = meshptr%GetTotalNodes()
  isok = found .EQ. want

  CALL OK(isok, "TotalDOF mathched for order=2: ")

  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
