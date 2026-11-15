!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: ScalarMeshFieldInitiate with a space dependent user function
!
! We create a scalar mesh field which varies in space, and fill it with
! f(x, y) = x + y by using a user function (function pointer).
!
! - Insert(func, globalElement) fills one element at a time.
! - Set(func, globalElement) updates one element.
! - Set(func) updates all the elements.

PROGRAM main
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: InterfaceScalarSubroutine
USE BaseType, ONLY: math => TypeMathOpt
USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEMesh_Class, ONLY: FEMesh_
USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE MeshField_Class, ONLY: MeshField_
USE MeshField_Class, ONLY: ScalarMeshFieldInitiate
USE Test_Method, ONLY: OK
USE UserFunction_Class, ONLY: UserFunction_

IMPLICIT NONE

CHARACTER(*), PARAMETER :: &
  filename = "./meshdata/small_tri3_mesh.h5"
INTEGER(I4B), PARAMETER :: nsd = 2
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

TYPE(HDF5File_) :: meshfile
TYPE(FEMesh_) :: amesh
TYPE(MeshField_) :: obj
TYPE(UserFunction_) :: func1, func2
PROCEDURE(InterfaceScalarSubroutine), POINTER :: fptr => NULL()
INTEGER(I4B) :: iel, telements, nns, nrow, ncol, tsize
REAL(DFP), ALLOCATABLE :: xij(:, :), found(:)
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(fileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, dim=nsd)

nns = amesh%GetMaxNNE()
telements = amesh%GetTotalElements()

! func1 = x + y
CALL func1%Initiate(name="func1", returnType=fevaropt%scalar, &
                    argType=fevaropt%space, numArgs=nsd)
fptr => SumXY
CALL func1%SetScalarFunctionPointer(func=fptr)

! func2 = 2 * (x + y)
CALL func2%Initiate(name="func2", returnType=fevaropt%scalar, &
                    argType=fevaropt%space, numArgs=nsd)
fptr => TwiceSumXY
CALL func2%SetScalarFunctionPointer(func=fptr)

CALL ScalarMeshFieldInitiate( &
  obj=obj, name="scalarMeshField", fieldType=fieldopt%normal, &
  varType=fevaropt%space, engine=engineopt%native_serial, &
  defineOn=fevaropt%nodal, nns=nns, mesh=amesh)

CALL OK(obj%GetRank() .EQ. fevaropt%scalar, "GetRank")
CALL OK(obj%GetVarType() .EQ. fevaropt%space, "GetVarType")
CALL OK(obj%GetMaxNNE() .EQ. nns, "GetMaxNNE")

! Insert element by element
DO iel = 1, telements
  CALL obj%Insert(func=func1, globalElement=iel, islocal=math%yes)
END DO

ALLOCATE (xij(3, nns), found(nns))
xij = 0.0_DFP

CALL OK(Check(1.0_DFP), "Insert(func, globalElement)")

! Update element 1 by func2
CALL obj%Set(func=func2, globalElement=1, islocal=math%yes)
CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol, &
                        globalElement=1, islocal=math%yes)
CALL obj%GetSpaceScalarField_(globalElement=1, ans=found, tsize=tsize, &
                              islocal=math%yes)
isok = ALL(ABS(found(1:tsize) - &
               2.0_DFP * (xij(1, 1:ncol) + xij(2, 1:ncol))) .LT. tol)
CALL OK(isok, "Set(func, globalElement)")

! Update all the elements by func2
CALL obj%Set(func=func2)
CALL OK(Check(2.0_DFP), "Set(func)")

CALL obj%DEALLOCATE()
CALL func1%DEALLOCATE()
CALL func2%DEALLOCATE()
CALL amesh%DEALLOCATE()
CALL meshfile%CLOSE()
CALL meshfile%DEALLOCATE()

CONTAINS

SUBROUTINE SumXY(args, nargs, ans)
  REAL(DFP), INTENT(IN) :: args(:)
  INTEGER(I4B), INTENT(IN) :: nargs
  REAL(DFP), INTENT(INOUT) :: ans
  ans = args(1) + args(2)
END SUBROUTINE SumXY

SUBROUTINE TwiceSumXY(args, nargs, ans)
  REAL(DFP), INTENT(IN) :: args(:)
  INTEGER(I4B), INTENT(IN) :: nargs
  REAL(DFP), INTENT(INOUT) :: ans
  ans = 2.0_DFP * (args(1) + args(2))
END SUBROUTINE TwiceSumXY

! Check that every element contains scale * (x + y) at its nodes
FUNCTION Check(scale) RESULT(ans)
  REAL(DFP), INTENT(IN) :: scale
  LOGICAL(LGT) :: ans
  INTEGER(I4B) :: jj

  ans = .TRUE.
  DO jj = 1, telements
    CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol, &
                            globalElement=jj, islocal=math%yes)
    CALL obj%GetSpaceScalarField_(globalElement=jj, ans=found, &
                                  tsize=tsize, islocal=math%yes)
    ans = ans .AND. (tsize .EQ. ncol)
    ans = ans .AND. ALL(ABS(found(1:tsize) - scale * &
                            (xij(1, 1:ncol) + xij(2, 1:ncol))) .LT. tol)
  END DO
END FUNCTION Check

END PROGRAM main
