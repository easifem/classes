!> author: Vikas Sharma, Ph. D.
! date: 2025-06-07
! summary:  Testing GetFaceDOF method of FEDOF class
! H1 Hierarchical Second Order Triangular Mesh

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: ToString
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: is
USE Test_Method, ONLY: ok
IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "./meshdata/very_small_quad4_mesh.h5"
TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom
CLASS(AbstractMesh_), POINTER :: mesh => NULL()
TYPE(HDF5File_) :: meshfile

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')
mesh => dom%GetMeshPointer()

! testing order 1
CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
                  isBrokenSpace=math%no, order=1, dom=dom)

BLOCK
  INTEGER(I4B), ALLOCATABLE :: con(:)

  con = obj%GetConnectivity(globalElement=1, islocal=math%yes, opt="A")

  CALL OK(SIZE(con) .EQ. 4, &
          obj%GetCaseName()//" correct size of connectivity order=1: ")

  CALL OK(ALL(con .EQ. [1, 5, 9, 7]), &
          obj%GetCaseName()//" correct connectivity order=1: ")
END BLOCK

BLOCK
  INTEGER(I4B) :: ans(10), tsize

  CALL obj%GetFaceDOF(globalFace=1, islocal=math%yes, ans=ans, &
                      tsize=tsize)

  CALL OK(tsize .EQ. 0, &
          obj%GetCaseName()//" Size of faceDOF is zero for order=1")

  tsize = obj%GetTotalFaceDOF(globalFace=1, islocal=math%yes)

  CALL OK(tsize .EQ. 0, &
          obj%GetCaseName()//" Size of faceDOF is zero for order=1")
END BLOCK

! testing order 3
CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
                  isBrokenSpace=math%no, order=3, dom=dom)

BLOCK
  INTEGER(I4B) :: ans(10), tsize

  CALL obj%GetFaceDOF(globalElement=13, islocal=math%no, &
                      localFaceNumber=2, ans=ans, tsize=tsize)

  CALL OK(tsize .EQ. 2, &
          obj%GetCaseName()//" Size of faceDOF is 2 for order=2")

  tsize = obj%GetTotalFaceDOF(globalElement=13, islocal=math%no, &
                              localFaceNumber=2)

  CALL OK(tsize .EQ. 2, &
          obj%GetCaseName()//" GetTotalFaceDOF 2 for order=2")

  CALL OK(ALL(ans(1:tsize) .EQ. [12, 13]), &
          obj%GetCaseName()//" faceDOF for order=2")
END BLOCK

CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
CALL obj%DEALLOCATE()
END PROGRAM main
