!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Testing ScalarField GetVtkUnstructuredGridValue

PROGRAM main
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE ApproxUtility, ONLY: SoftEq
  USE BaseType, ONLY: math => TypeMathOpt
  USE BaseType, ONLY: TypeFileOpt
  USE BaseType, ONLY: TypeFEVariableOpt
  USE BaseType, ONLY: InterfaceScalarSubroutine
  USE Display_Method, ONLY: Display
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: INT8
  USE ScalarField_Class, ONLY: ScalarField_
  USE Test_Method, ONLY: OK
  USE UserFunction_Class, ONLY: UserFunction_
  IMPLICIT NONE
  CHARACTER(*), PARAMETER :: tomlFileName = &
    "GetVtkUnstructuredGridValue_test_1.toml"
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_) :: fedof, geofedof
  TYPE(ScalarField_) :: u
  CLASS(AbstractMesh_), POINTER :: mesh
  INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
  INTEGER(INT8), ALLOCATABLE :: types(:)
  REAL(DFP), ALLOCATABLE :: points(:, :)
  TYPE(UserFunction_) :: func
  PROCEDURE(InterfaceScalarSubroutine), POINTER :: procptr

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

! make user function
  CALL func%Initiate(NAME="func1", returnType=TypeFEVariableOpt%scalar, &
                     argType=TypeFEVariableOpt%space, numArgs=4, &
                     numReturns=1)

  procptr => scalarFunc2
  CALL func%SetScalarFunctionPointer(func=procptr)

! import domain
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")

! import u
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

  CALL u%Set(math%zero)
  CALL u%SetByFunction(func=func)

! Get grid connectivity
  CALL u%GetVtkUnstructuredGridData(points=points, &
                                    connectivity=connectivity, &
                                    offsets=offsets, types=types)

! clean stuff
  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()

CONTAINS
  SUBROUTINE scalarFunc2(args, nargs, ans)
    REAL(DFP), INTENT(IN) :: args(:)
    INTEGER(I4B), INTENT(IN) :: nargs
    REAL(DFP), INTENT(INOUT) :: ans
    ans = args(1) * (args(1) - 10.0_DFP) * args(2) * (args(2) - 10.0_DFP)
  END SUBROUTINE scalarFunc2
END PROGRAM main
