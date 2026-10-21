!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate a mesh field at quadrature points from a user function
!
! First we create quadField, a vector mesh field which contains the
! quadrature points (see InitiateQuadraturePoints). Then
! Initiate(quadField, func, ...) creates a mesh field with
! defineOn = quadrature. Insert(quadField, func, globalElement) evaluates
! func at the quadrature points of one element. Here
! f(x, y) = x + y.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: InterfaceScalarSubroutine
USE BaseType, ONLY: math => TypeMathOpt
USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE MeshField_Class, ONLY: InitiateQuadraturePoints
USE MeshField_Class, ONLY: MeshField_
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
USE UserFunction_Class, ONLY: UserFunction_

IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "MeshField_test.toml"
INTEGER(I4B), PARAMETER :: nsd = 2
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

TYPE(FEDomain_) :: dom
TYPE(FEDOF_) :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(MeshField_) :: quadField, obj
TYPE(UserFunction_) :: func
PROCEDURE(InterfaceScalarSubroutine), POINTER :: fptr => NULL()
CLASS(AbstractMesh_), POINTER :: mesh
INTEGER(I4B) :: iel, telements, nrow, ncol, tsize
REAL(DFP), ALLOCATABLE :: xij(:, :), found(:)
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)

CALL dom%ImportFromToml(fileName=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      fileName=tomlFileName, tomlName="u")
mesh => dom%GetMeshPointer()
telements = mesh%GetTotalElements()

CALL InitiateQuadraturePoints(obj=quadField, fedof=fedof, mesh=mesh, &
                              geofedof=geofedof, &
                              engine=engineopt%native_serial)

CALL func%Initiate(name="func", returnType=fevaropt%scalar, &
                   argType=fevaropt%space, numArgs=nsd)
fptr => SumXY
CALL func%SetScalarFunctionPointer(func=fptr)

CALL obj%Initiate(quadField=quadField, func=func, name="func", &
                  engine=engineopt%native_serial)

CALL OK(obj%GetDefineOn() .EQ. fevaropt%quadrature, "GetDefineOn")
CALL OK(obj%GetVarType() .EQ. fevaropt%space, "GetVarType")
CALL OK(obj%GetMaxNNE() .EQ. quadField%GetMaxNNE(), "GetMaxNNE")

DO iel = 1, telements
  CALL obj%Insert(quadField=quadField, func=func, globalElement=iel, &
                  islocal=math%yes)
END DO

ALLOCATE (xij(3, quadField%GetMaxNNE()), found(quadField%GetMaxNNE()))
isok = .TRUE.
DO iel = 1, telements
  CALL quadField%GetSpaceVectorField_(globalElement=iel, ans=xij, &
                                      nrow=nrow, ncol=ncol, &
                                      islocal=math%yes)
  CALL obj%GetSpaceScalarField_(globalElement=iel, ans=found, &
                                tsize=tsize, islocal=math%yes)
  isok = isok .AND. (tsize .EQ. ncol)
  isok = isok .AND. ALL(ABS(found(1:tsize) - &
                            (xij(1, 1:ncol) + xij(2, 1:ncol))) .LT. tol)
END DO
CALL OK(isok, "Insert(quadField, func, globalElement)")

NULLIFY (mesh)
CALL obj%DEALLOCATE()
CALL quadField%DEALLOCATE()
CALL func%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

SUBROUTINE SumXY(args, nargs, ans)
  REAL(DFP), INTENT(IN) :: args(:)
  INTEGER(I4B), INTENT(IN) :: nargs
  REAL(DFP), INTENT(INOUT) :: ans
  ans = args(1) + args(2)
END SUBROUTINE SumXY

END PROGRAM main
