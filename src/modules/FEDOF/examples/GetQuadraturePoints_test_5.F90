!> author: Vikas Sharma, Ph. D.
! date: 2025-06-01
! summary:  Initiate FEDOF object by import from toml

PROGRAM main
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE HDF5File_Class, ONLY: HDF5File_
  USE Display_Method, ONLY: Display
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE GlobalData, ONLY: DFP
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE BaseType, ONLY: QuadraturePoint_
  USE BaseType, ONLY: math => TypeMathOpt
  USE AbstractFE_Class, ONLY: AbstractFE_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
    toml_filename = "./toml/ImportFromToml_test_3.toml"
  TYPE(FEDOF_) :: obj
  TYPE(FEDomain_) :: dom
  TYPE(HDF5File_) :: meshfile
  TYPE(QuadraturePoint_) :: quad
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  CLASS(AbstractFE_), POINTER :: feptr => NULL()

  CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')

  meshptr => dom%GetMeshPointer()

  CALL obj%ImportFromToml(tomlName="test1", filename=toml_filename, &
                          dom=dom)

  CALL obj%SetFE(globalElement=1, islocal=math%yes)
  feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
  CALL feptr%GetQuadraturePoints(quad=quad)
  CALL OK(SIZE(quad%points, 2) .EQ. 3_I4B, "number of points")
  CALL OK(ABS(SUM(quad%points(SIZE(quad%points, 1), :)) - 0.5_DFP) &
          .LT. 1.0E-8_DFP, "sum of weights")

  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
