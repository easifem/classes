!> author: Vikas Sharma, Ph. D.
! date: 2025-06-07
! summary:  Testing GetTotalCellDOF method of FEDOF class
! H1 Hierarchical Second Order Triangular Mesh

PROGRAM main
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE HDF5File_Class, ONLY: HDF5File_
  USE Display_Method, ONLY: ToString
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE Test_Method, ONLY: is
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
    baseContinuity = "H1", baseInterpolation = "Hierarchical", &
    testname = baseContinuity // " " // baseInterpolation // &
    " GetTotalCellDOF test"

  TYPE(FEDOF_) :: fedof
  TYPE(FEDomain_) :: dom
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: order, totalVertexNodes, totalFaces
  LOGICAL(LGT) :: isok

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, .TRUE.)

  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')
  meshptr => dom%GetMeshPointer()
  totalVertexNodes = meshptr%GetTotalVertexNodes()
  totalFaces = meshptr%GetTotalFaces()

  CALL test1
  CALL test2
  CALL test3
  CALL test4

  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()

CONTAINS

!---------------------------------------------------------------------------
!                                                                      test1
!---------------------------------------------------------------------------

  SUBROUTINE test1

    INTEGER(I4B) :: tsize, want, found

    order = 1
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        order=order, dom=dom)

    found = fedof%GetTotalCellDOF(globalCell=1, islocal=.TRUE.)
    want = 0

    CALL IS(found, want, &
            testname // " (order= " // ToString(order) // "): ")

  END SUBROUTINE test1

!---------------------------------------------------------------------------
!                                                                      test2
!---------------------------------------------------------------------------

  SUBROUTINE test2

    INTEGER(I4B) :: tsize, want, found

    order = 2
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        order=order, dom=dom)

    found = fedof%GetTotalCellDOF(globalCell=1, islocal=.TRUE.)
    want = 0

    CALL IS(found, want, &
            testname // " (order= " // ToString(order) // "): ")

  END SUBROUTINE test2

!---------------------------------------------------------------------------
!                                                                     test3
!---------------------------------------------------------------------------

  SUBROUTINE test3

    INTEGER(I4B) :: tsize, want, found

    order = 3
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        order=order, dom=dom)

    found = fedof%GetTotalCellDOF(globalCell=1, islocal=.TRUE.)
    want = 1

    CALL IS(found, want, &
            testname // " (order= " // ToString(order) // "): ")

  END SUBROUTINE test3

!---------------------------------------------------------------------------
!                                                                     test3
!---------------------------------------------------------------------------

  SUBROUTINE test4

    INTEGER(I4B) :: tsize, want, found

    order = 4
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        order=order, dom=dom)

    found = fedof%GetTotalCellDOF(globalCell=1, islocal=.TRUE.)
    want = 3

    CALL IS(found, want, &
            testname // " (order= " // ToString(order) // "): ")

  END SUBROUTINE test4

!---------------------------------------------------------------------------
!
!---------------------------------------------------------------------------

END PROGRAM main
