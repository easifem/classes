!> author: Vikas Sharma, Ph. D.
! date: 2025-06-06
! summary: Initiate fedof with H1 and Hierarchical bases, order is a
!   vector.

PROGRAM main
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE HDF5File_Class, ONLY: HDF5File_
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

  TYPE(FEDOF_) :: fedof
  TYPE(FEDomain_) :: dom
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
    baseInterpolation = "Hierarchical", &
    baseContinuity = "H1"
  TYPE(HDF5File_) :: meshfile
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: found, want, order, ii, iel
  INTEGER(I4B), ALLOCATABLE :: cellOrder(:), aintvec(:)

  CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')

  meshptr => dom%GetMeshPointer()
  CALL Reallocate(cellOrder, meshptr%GetTotalCells())

  aintvec = [14, 15, 17, 24, 18]
  order = 1
  DO iel = 1, SIZE(aintvec)
    isok = meshptr%IsElementPresent(aintvec(iel))
    IF (.NOT. isok) CYCLE

    ii = meshptr%GetLocalElemNumber(aintvec(iel))
    cellOrder(ii) = order
  END DO

  aintvec = [22, 23, 25, 21]
  order = 2
  DO iel = 1, SIZE(aintvec)
    isok = meshptr%IsElementPresent(aintvec(iel))
    IF (.NOT. isok) CYCLE

    ii = meshptr%GetLocalElemNumber(aintvec(iel))
    cellOrder(ii) = order
  END DO

  aintvec = [19, 20, 26, 16, 13]
  order = 3
  DO iel = 1, SIZE(aintvec)
    isok = meshptr%IsElementPresent(aintvec(iel))
    IF (.NOT. isok) CYCLE

    ii = meshptr%GetLocalElemNumber(aintvec(iel))
    cellOrder(ii) = order
  END DO

  CALL fedof%Initiate(baseContinuity=baseContinuity, &
                      baseInterpolation=baseInterpolation, &
                      order=cellOrder, dom=dom, islocal=math%yes)
  found = fedof%GetTotalDOF()
  want = 39
  isok = found == want
  CALL OK(isok, "Total DOF: ")
  IF (.NOT. isok) CALL Display([found, want], "found, want: ")

  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
