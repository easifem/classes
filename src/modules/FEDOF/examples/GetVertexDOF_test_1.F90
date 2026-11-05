!> author: Vikas Sharma, Ph. D.
! date: 2025-06-07
! summary:  Testing GetVertexDOF method of FEDOF class
! H1 Hierarchical Second Order Triangular Mesh

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE Display_Method, ONLY: ToString
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: ok
IMPLICIT NONE

CHARACTER(*), PARAMETER ::  filename = "./meshdata/small_tri3_mesh.h5"
TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom
CLASS(AbstractMesh_), POINTER :: mesh => NULL()
TYPE(HDF5File_) :: meshfile
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()

CALL dom%Initiate(meshfile, '')
mesh => dom%GetMeshPointer()

BLOCK
  INTEGER(I4B) :: tsize
  INTEGER(I4B) :: found(1), want(1)
  INTEGER(I4B) :: order
  CHARACTER(*), PARAMETER :: baseContinuity = "H1", &
                             baseInterpolation = "Hierarchical"

  order = 1
  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, &
                    isBrokenSpace=math%no, &
                    order=order, dom=dom)

  CALL obj%GetVertexDOF(globalNode=1, islocal=math%yes, &
                        ans=found, tsize=tsize)

  want(1) = 1

  isok = ALL(found .EQ. want)
  CALL OK(isok, "GetVertexDOF works for (order=1)")
END BLOCK

BLOCK
  INTEGER(I4B) :: tsize
  INTEGER(I4B) :: found(1), want(1)
  INTEGER(I4B) :: order
  CHARACTER(*), PARAMETER :: baseContinuity = "H1", &
                             baseInterpolation = "Hierarchical"

  order = 3
  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, &
                    isBrokenSpace=math%no, &
                    order=order, dom=dom)

  CALL obj%GetVertexDOF(globalNode=1, islocal=math%yes, &
                        ans=found, tsize=tsize)

  want(1) = 1

  isok = ALL(found .EQ. want)
  CALL OK(isok, "GetVertexDOF works for (order=3) H1 Hierarchical")
END BLOCK

BLOCK
  INTEGER(I4B) :: tsize
  INTEGER(I4B) :: found(1), want(1)
  INTEGER(I4B) :: order
  CHARACTER(*), PARAMETER :: baseContinuity = "H1", &
                             baseInterpolation = "Lagrange"

  order = 3
  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, &
                    ipType=TypeInterpolationOpt%Equidistance, &
                    isBrokenSpace=math%no, order=order, dom=dom)

  CALL obj%GetVertexDOF(globalNode=1, islocal=math%yes, ans=found, &
                        tsize=tsize)

  want(1) = 1

  isok = ALL(found .EQ. want)
  CALL OK(isok, "GetVertexDOF works for (order=3) H1 Lagrange")
END BLOCK

BLOCK
  INTEGER(I4B) :: tsize
  INTEGER(I4B) :: found(1)
  INTEGER(I4B) :: order
  CHARACTER(*), PARAMETER ::   baseContinuity = "H1", &
                             baseInterpolation = "Hierarchical"

  order = 1
  CALL obj%Initiate(baseContinuity=baseContinuity, &
                    baseInterpolation=baseInterpolation, &
                    isBrokenSpace=math%yes, order=order, dom=dom)

  CALL obj%GetVertexDOF(globalNode=1, islocal=math%yes, &
                        ans=found, tsize=tsize)

  CALL OK(tsize .EQ. 0, "GetVertexDOF for broken space returns zero size:")
END BLOCK

CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
CALL obj%DEALLOCATE()
END PROGRAM main
