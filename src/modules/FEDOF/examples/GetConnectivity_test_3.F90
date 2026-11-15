!> author: Vikas Sharma, Ph. D.
! date: 2025-06-07
! summary:  Testing GetConnectivity method of FEDOF class
! H1 Hierarchical Second Order Triangular Mesh

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AppendUtility, ONLY: Append
USE AppendUtility, ONLY: OPERATOR(.APPEND.)
USE ArangeUtility, ONLY: Arange
USE BaseType, ONLY: TypeQuadratureOpt
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: ToString
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: ok

IMPLICIT NONE

CHARACTER(*), PARAMETER ::  filename = "./meshdata/small_tri3_mesh.h5"
TYPE(FEDOF_) :: fedof
TYPE(FEDomain_) :: dom
CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
TYPE(HDF5File_) :: meshfile
INTEGER(I4B) :: totalVertexNodes, totalFaces

CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')
meshptr => dom%GetMeshPointer()

totalVertexNodes = meshptr%GetTotalVertexNodes()
totalFaces = meshptr%GetTotalFaces()

! CALL test1
BLOCK
  INTEGER(I4B), ALLOCATABLE :: globalNode(:)
  INTEGER(I4B) :: ent(4)
  INTEGER(I4B) :: tVertices, order
  INTEGER(I4B), ALLOCATABLE :: found(:), want(:)
  LOGICAL(LGT) :: isok

  order = 1
  CALL fedof%Initiate(baseContinuity="H1", &
                      baseInterpolation="LAGR", &
                      isBrokenSpace=math%no, &
                      order=order, dom=dom, &
                      ipType=TypeQuadratureOpt%equidistance)

  tVertices = meshptr%GetTotalVertexNodes(globalElement=[1], &
                                          islocal=math%yes)

  ent = meshptr%GetTotalEntities(globalElement=1, islocal=math%yes)

  ! Get all connectivity for local element 1
  found = fedof%GetConnectivity(opt="A", globalElement=1, islocal=math%yes)

  globalNode = meshptr%GetConnectivity(globalElement=1, islocal=math%yes)

  want = meshptr%GetLocalNodenumber(globalNode=globalNode(1:tVertices), &
                                    islocal=math%no)

  isok = ALL(found == want)
  CALL OK(isok, fedof%GetCaseName()//" GetConnectivity (order= "// &
          ToString(order)//"): ")
END BLOCK

! H1, LAGR, No BrokenSpace, order=2
BLOCK
  INTEGER(I4B), ALLOCATABLE :: globalNode(:), temp1(:)
  INTEGER(I4B), ALLOCATABLE :: found(:), want(:)
  INTEGER(I4B) :: ent(4)
  INTEGER(I4B) :: a, b, tVertices, order
  LOGICAL(LGT) :: isok

  order = 2
  CALL fedof%Initiate(baseContinuity="H1", &
                      baseInterpolation="LAGR", &
                      isBrokenSpace=math%no, order=order, &
                      dom=dom, ipType=TypeQuadratureOpt%equidistance)

  found = fedof%GetConnectivity(opt="A", globalElement=1, islocal=math%yes)
  globalNode = meshptr%GetConnectivity(globalElement=1, islocal=math%yes)
  tVertices = meshptr%GetTotalVertexNodes(globalElement=[1], islocal=math%yes)

  want = meshptr%GetLocalNodenumber(globalNode=globalNode(1:tVertices), &
                                    islocal=math%no)
  a = meshptr%GetTotalVertexNodes() + 1
  ent = meshptr%GetTotalEntities(globalElement=1, islocal=math%yes)

  b = meshptr%GetTotalVertexNodes() + ent(3) * (order - 1)
  temp1 = Arange(a, b)

  want = want .APPEND. temp1

  isok = ALL(found == want)

  CALL OK(isok, fedof%GetCaseName()//" GetConnectivity (order= "// &
          ToString(order)//"): ")

END BLOCK

! H1, LAGR, No brokenSpace, order=3
BLOCK
  INTEGER(I4B), ALLOCATABLE :: globalNode(:), temp1(:)
  INTEGER(I4B), ALLOCATABLE :: found(:), want(:)
  INTEGER(I4B) :: ent(4)
  INTEGER(I4B) :: a, b, tVertices, order
  LOGICAL(LGT) :: isok

  order = 3
  CALL fedof%Initiate(baseContinuity="H1", &
                      baseInterpolation="LAGR", &
                      isBrokenSpace=math%no, order=order, &
                      dom=dom, ipType=TypeQuadratureOpt%equidistance)

  found = fedof%GetConnectivity(opt="A", globalElement=1, islocal=math%yes)

  globalNode = meshptr%GetConnectivity(globalElement=1, islocal=math%yes)

  tVertices = meshptr%GetTotalVertexNodes(globalElement=[1], islocal=math%yes)

  want = meshptr%GetLocalNodenumber(globalNode=globalNode(1:tVertices), &
                                    islocal=math%no)

  ent = meshptr%GetTotalEntities(globalElement=1, islocal=math%yes)

  a = totalVertexNodes + 1
  b = a - 1 + ent(3) * (order - 1)

  temp1 = Arange(a, b)

  want = want .APPEND. temp1

  a = totalVertexNodes + totalFaces * (order - 1) + 1
  b = a - 1 + ent(4) * (order - 2) * (order - 1) / 2

  temp1 = Arange(a, b)
  want = want .APPEND. temp1

  isok = ALL(found == want)

  CALL OK(isok, fedof%GetCaseName()//" GetConnectivity (order= "// &
          ToString(order)//"): ")

END BLOCK

CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
CALL fedof%DEALLOCATE()
END PROGRAM main
