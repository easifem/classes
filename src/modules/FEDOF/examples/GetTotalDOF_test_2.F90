!> author: Vikas Sharma, Ph. D.
! date: 2025-06-08
! summary:  Testing GetTotalDOF method of FEDOF class

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

  CHARACTER(*), PARAMETER :: &
    filename = "./meshdata/small_tri6_mesh.h5", &
    baseContinuity = "H1", &
    baseInterpolation = "Hierarchical", &
    testname = baseContinuity // " " // baseInterpolation // " GetTotalDOF"

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

  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()

CONTAINS

!---------------------------------------------------------------------------
!                                                                      test1
!---------------------------------------------------------------------------

  SUBROUTINE test1
    INTEGER(I4B) :: tsize, found, want

    order = 1
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        order=order, dom=dom)

    found = fedof%GetTotalDOF()
    want = meshptr%GetTotalVertexNodes()
    CALL IS(found, want, testname // "  interface 1 (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE.)
    want = meshptr%GetTotalVertexNodes(globalElement=1, islocal=.TRUE.)
    CALL IS(found, want, testname // " interface 2 (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE., opt="A")
    want = meshptr%GetTotalVertexNodes(globalElement=1, islocal=.TRUE.)
    CALL IS(found, want, testname // "  interface 3 a (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE., opt="E")
    want = 0
    CALL IS(found, want, testname // "  interface 3 e (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE., opt="F")
    want = 0
    CALL IS(found, want, testname // "  interface 3 f (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE., opt="c")
    want = 0
    CALL IS(found, want, testname // "  interface 3 c (order= " // &
            ToString(order) // "): ")

  END SUBROUTINE test1

!---------------------------------------------------------------------------
!                                                                      test2
!---------------------------------------------------------------------------

  SUBROUTINE test2
    INTEGER(I4B) :: tsize, found, want

    order = 2
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        order=order, dom=dom)

    found = fedof%GetTotalDOF()
    want = &
      meshptr%GetTotalVertexNodes() + meshptr%GetTotalFaces() * (order - 1)
    CALL IS(found, want, testname // "  interface 1 (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE.)
    want = 6
    CALL IS(found, want, testname // " interface 2 (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE., opt="A")
    want = 6
    CALL IS(found, want, testname // "  interface 3 a (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE., opt="E")
    want = 0
    CALL IS(found, want, testname // "  interface 3 e (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE., opt="F")
    want = 3
    CALL IS(found, want, testname // "  interface 3 f (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE., opt="c")
    want = 0
    CALL IS(found, want, testname // "  interface 3 c (order= " // &
            ToString(order) // "): ")

  END SUBROUTINE test2

!---------------------------------------------------------------------------
!                                                                     test3
!---------------------------------------------------------------------------

  SUBROUTINE test3
    INTEGER(I4B) :: tsize, found, want

    order = 3
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        order=order, dom=dom)

    found = fedof%GetTotalDOF()
    want = meshptr%GetTotalVertexNodes() + &
      meshptr%GetTotalFaces() * (order - 1) + &
      meshptr%GetTotalCells() * (order - 2) * (order - 1) * 0.5
    CALL IS(found, want, testname // "  interface 1 (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE.)
    want = 10
    CALL IS(found, want, testname // " interface 2 (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE., opt="A")
    want = 10
    CALL IS(found, want, testname // "  interface 3 a (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE., opt="E")
    want = 0
    CALL IS(found, want, testname // "  interface 3 e (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE., opt="F")
    want = 6
    CALL IS(found, want, testname // "  interface 3 f (order= " // &
            ToString(order) // "): ")

    found = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE., opt="c")
    want = 1
    CALL IS(found, want, testname // "  interface 3 c (order= " // &
            ToString(order) // "): ")

  END SUBROUTINE test3

!---------------------------------------------------------------------------
!
!---------------------------------------------------------------------------

END PROGRAM main
