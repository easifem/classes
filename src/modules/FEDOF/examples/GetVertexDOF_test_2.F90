!> author: Vikas Sharma, Ph. D.
! date: 2025-06-07
! summary:  Testing GetVertexDOF method of FEDOF class
! H1 Hierarchical Second Order Triangular Mesh

PROGRAM main
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE HDF5File_Class, ONLY: HDF5File_
  USE Display_Method, ONLY: ToString
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE Test_Method, ONLY: ok
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE ReallocateUtility, ONLY: Reallocate
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: &
    filename = "./meshdata/small_tri6_mesh.h5", &
    baseContinuity = "H1", &
    baseInterpolation = "Hierarchical", &
    testname = baseContinuity // " " // baseInterpolation

  TYPE(FEDOF_) :: fedof
  TYPE(FEDomain_) :: dom
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B), ALLOCATABLE :: found(:), want(:)
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

    INTEGER(I4B) :: tsize

    order = 1
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        order=order, dom=dom)

    CALL Reallocate(found, 2, want, 2)
    CALL fedof%GetVertexDOF(globalNode=1, islocal=.TRUE., &
                            ans=found, tsize=tsize)

    want(1) = 1

    isok = ALL(found == want)
    CALL OK(isok, testname // " GetVertexDOF " // ToString(order) // "): ")

  END SUBROUTINE test1

!---------------------------------------------------------------------------
!                                                                      test2
!---------------------------------------------------------------------------

  SUBROUTINE test2

    INTEGER(I4B) :: tsize

    order = 2
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        order=order, dom=dom)

    CALL Reallocate(found, 2, want, 2)
    CALL fedof%GetVertexDOF(globalNode=1, islocal=.TRUE., &
                            ans=found, tsize=tsize)

    want(1) = 1

    isok = ALL(found == want)
    CALL OK(isok, testname // " GetVertexDOF (order= " // &
            ToString(order) // "): ")

  END SUBROUTINE test2

!---------------------------------------------------------------------------
!                                                                     test3
!---------------------------------------------------------------------------

  SUBROUTINE test3

    INTEGER(I4B) :: tsize

    order = 3
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, &
                        order=order, dom=dom)

    CALL Reallocate(found, 2, want, 2)
    CALL fedof%GetVertexDOF(globalNode=1, islocal=.TRUE., &
                            ans=found, tsize=tsize)

    want(1) = 1

    isok = ALL(found == want)
    CALL OK(isok, testname // " GetVertexDOF (order= " // &
            ToString(order) // "): ")

  END SUBROUTINE test3

!---------------------------------------------------------------------------
!
!---------------------------------------------------------------------------

END PROGRAM main
