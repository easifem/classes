!> author: Vikas Sharma, Ph. D.
! date: 2025-06-07
! summary:  Testing GetConnectivity method of FEDOF class
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
  USE BaseType, ONLY: TypeQuadratureOpt
  USE AppendUtility, ONLY: Append
  USE AppendUtility, ONLY: OPERATOR(.APPEND.)
  USE ArangeUtility, ONLY: Arange
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: &
    filename = "./meshdata/small_tri6_mesh.h5", &
    baseContinuity = "H1", &
    baseInterpolation = "Lagrange", &
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

    INTEGER(I4B), ALLOCATABLE :: globalNode(:)
    INTEGER(I4B) :: ent(4), tVertices, tdof, localElemNum

    order = 1
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, order=order, &
                        dom=dom, ipType=TypeQuadratureOpt%equidistance)

    tVertices = &
      meshptr%GetTotalVertexNodes(globalElement=[1], islocal=.TRUE.)

    ent = meshptr%GetTotalEntities(globalElement=1, islocal=.TRUE.)

    tdof = fedof%GetTotalDOF(globalElement=1, islocal=.TRUE.)
    localElemNum = &
      meshptr%GetLocalElemNumber(globalElement=1, islocal=.TRUE.)

    ! Get all connectivity for local element 1
    found = fedof%GetConnectivity(opt="A", globalElement=1, islocal=.TRUE.)

    globalNode = meshptr%GetConnectivity(globalElement=1, islocal=.TRUE.)

    want = meshptr%GetLocalNodenumber(globalNode=globalNode(1:tVertices), &
                                      islocal=.FALSE.)

    isok = ALL(found == want)
    CALL OK(isok, testname // " GetConnectivity (order= " // &
            ToString(order) // "): ")

  END SUBROUTINE test1

!---------------------------------------------------------------------------
!                                                                      test2
!---------------------------------------------------------------------------

  SUBROUTINE test2
    INTEGER(I4B), ALLOCATABLE :: globalNode(:), temp1(:)
    INTEGER(I4B) :: ent(4), a, b, tVertices

    order = 2
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, order=order, &
                        dom=dom, ipType=TypeQuadratureOpt%equidistance)

    found = fedof%GetConnectivity(opt="A", globalElement=1, islocal=.TRUE.)
    globalNode = meshptr%GetConnectivity(globalElement=1, islocal=.TRUE.)
    tVertices = &
      meshptr%GetTotalVertexNodes(globalElement=[1], islocal=.TRUE.)

    want = meshptr%GetLocalNodenumber(globalNode=globalNode(1:tVertices), &
                                      islocal=.FALSE.)
    a = meshptr%GetTotalVertexNodes() + 1
    ent = meshptr%GetTotalEntities(globalElement=1, islocal=.TRUE.)

    b = meshptr%GetTotalVertexNodes() + ent(3) * (order - 1)
    temp1 = Arange(a, b)

    want = want.APPEND.temp1

    isok = ALL(found == want)

    CALL OK(isok, testname // " GetConnectivity (order= " // &
            ToString(order) // "): ")

  END SUBROUTINE test2

!---------------------------------------------------------------------------
!                                                                     test3
!---------------------------------------------------------------------------

  SUBROUTINE test3
    INTEGER(I4B), ALLOCATABLE :: globalNode(:), temp1(:)
    INTEGER(I4B) :: ent(4), a, b, tVertices

    order = 3
    CALL fedof%Initiate(baseContinuity=baseContinuity, &
                        baseInterpolation=baseInterpolation, order=order, &
                        dom=dom, ipType=TypeQuadratureOpt%equidistance)

    found = fedof%GetConnectivity(opt="A", globalElement=1, islocal=.TRUE.)

    globalNode = meshptr%GetConnectivity(globalElement=1, islocal=.TRUE.)

    tVertices = &
      meshptr%GetTotalVertexNodes(globalElement=[1], islocal=.TRUE.)

    want = meshptr%GetLocalNodenumber(globalNode=globalNode(1:tVertices), &
                                      islocal=.FALSE.)

    ent = meshptr%GetTotalEntities(globalElement=1, islocal=.TRUE.)

    a = totalVertexNodes + 1
    b = a - 1 + ent(3) * (order - 1)

    temp1 = Arange(a, b)

    want = want.APPEND.temp1

    a = totalVertexNodes + totalFaces * (order - 1) + 1
    b = a - 1 + ent(4) * (order - 2) * (order - 1) * 0.5

    temp1 = Arange(a, b)
    want = want.APPEND.temp1

    isok = ALL(found == want)

    CALL OK(isok, testname // " GetConnectivity (order= " // &
            ToString(order) // "): ")
  END SUBROUTINE test3

END PROGRAM main
