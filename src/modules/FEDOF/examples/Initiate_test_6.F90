!> author: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Initiate method for Hierarchical bases with different orders

PROGRAM main
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE HDF5File_Class, ONLY: HDF5File_
  USE Display_Method, ONLY: ToString
  USE Display_Method, ONLY: Display
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE ReallocateUtility, ONLY: Reallocate
  USE BaseType, ONLY: math => TypeMathOpt
  USE Test_Method, ONLY: OK
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
  CHARACTER(*), PARAMETER :: baseContinuity = "H1", &
    baseInterpolation = "Hierarchical"
  TYPE(FEDOF_) :: fedof
  TYPE(FEDomain_) :: dom
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  TYPE(HDF5File_) :: meshfile
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: found, want, order
  INTEGER(I4B), ALLOCATABLE :: cellOrder(:, :), aintvec(:)

  CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')
  meshptr => dom%GetMeshPointer()
  CALL Reallocate(cellOrder, 2, meshptr%GetTotalCells())

  aintvec = [14, 15, 17, 24, 18]

  order = 1
  cellOrder(1, 1:5) = aintvec
  cellOrder(2, 1:5) = order

  aintvec = [22, 23, 25, 21]
  order = 2
  cellOrder(1, 6:9) = aintvec
  cellOrder(2, 6:9) = order

  aintvec = [19, 20, 26, 16, 13]
  order = 3
  cellOrder(1, 10:14) = aintvec
  cellOrder(2, 10:14) = order

  CALL fedof%Initiate(baseContinuity=baseContinuity, &
                      baseInterpolation=baseInterpolation, &
                      order=cellOrder, dom=dom)

  found = fedof%GetTotalDOF()
  want = 39
  isok = found == want
  CALL OK(isok, "TotalDOF matched: ")
  IF (.NOT. isok) CALL Display([found, want], "found, want: ")

  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
