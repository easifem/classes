!> author: Shion Shimizu
! date: 2026-10-24
! summary: Write a Lagrange grid and a field

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
  USE VtkHDF5File_Class, ONLY: VtkHDF5File_
  IMPLICIT NONE
  CHARACTER(*), PARAMETER :: tomlFileName = "VtkHDF5File_test.toml", &
    filename = "WriteUnstructuredGrid_test_2.vtkhdf"
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_) :: fedof, geofedof
  TYPE(ScalarField_) :: u
  TYPE(VtkHDF5File_) :: obj
  CLASS(AbstractMesh_), POINTER :: mesh
  REAL(DFP), ALLOCATABLE :: VALUE(:)
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

  CALL u%Set(0.0_DFP)
  CALL u%SetByFunction(func=func)

! Get grid connectivity
  CALL u%GetVtkUnstructuredGridData(points=points, &
                                    connectivity=connectivity, &
                                    offsets=offsets, types=types)

  CALL OK(ALLOCATED(points), "points allocated:")
  CALL OK(ALL(SHAPE(points) .EQ. [3, 32]), "shape of points:")
  CALL OK(ALLOCATED(connectivity), "connectivity allocated:")
  CALL OK(SIZE(connectivity) .EQ. 32, "shape of connectivity:")
  CALL OK(ALLOCATED(offsets), "offsets allocated:")
  CALL OK(SIZE(offsets) .EQ. 3, "size of offsets:")
  CALL OK(ALLOCATED(types), "types allocated:")
  CALL OK(SIZE(types) .EQ. 2, "size of types:")

  BLOCK
    INTEGER(I4B) :: found(16), want(16)

    IF (ALLOCATED(connectivity)) THEN
      found = connectivity(1:16)
      want = [1, 2, 3, 4, 5, 6, 7, 8, 10, 9, 12, 11, 13, 14, 16, 15] &
        - math%one_i
      CALL OK(ALL(found .EQ. want), "elem=1, connectivity: ")

      found = connectivity(17:32)
      want = 16 + [1, 2, 3, 4, 5, 6, 7, 8, 10, 9, 12, 11, 13, 14, 16, 15] &
        - math%one_i
      CALL OK(ALL(found .EQ. want), "elem=2, connectivity: ")
    END IF
  END BLOCK

  BLOCK
    REAL(DFP) :: found(3, 16), want(3, 16)
    REAL(DFP), PARAMETER :: tol = 1.0E-5_DFP

    IF (ALLOCATED(points)) THEN
      found = points(1:3, 1:16)
      want(1, :) = [0.0_DFP, 5.0_DFP, 5.0_DFP, 0.0_DFP, &
                    5.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
                    5.0_DFP, 5.0_DFP, &
                    10.0_DFP / 3.0_DFP, 5.0_DFP / 3.0_DFP, &
                    0.0_DFP, 0.0_DFP, &
                    5.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
                    10.0_DFP / 3.0_DFP, 5.0_DFP / 3.0_DFP]

      want(2, :) = [0.0_DFP, 0.0_DFP, 10.0_DFP, 10.0_DFP, &
                    0.0_DFP, 0.0_DFP, &
                    10.0_DFP / 3.0_DFP, 20.0_DFP / 3.0_DFP, &
                    10.0_DFP, 10.0_DFP, &
                    20.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
                    10.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
                    20.0_DFP / 3.0_DFP, 20.0_DFP / 3.0_DFP]
      want(3, :) = 0.0_DFP
      CALL OK(ALL(SoftEq(found, want, tol)), "elem=1, points: ")

      found = points(1:3, 17:32)
      want(1, :) = 5.0_DFP + [0.0_DFP, 5.0_DFP, 5.0_DFP, 0.0_DFP, &
                              5.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
                              5.0_DFP, 5.0_DFP, &
                              10.0_DFP / 3.0_DFP, 5.0_DFP / 3.0_DFP, &
                              0.0_DFP, 0.0_DFP, &
                              5.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
                              10.0_DFP / 3.0_DFP, 5.0_DFP / 3.0_DFP]

      want(2, :) = [0.0_DFP, 0.0_DFP, 10.0_DFP, 10.0_DFP, &
                    0.0_DFP, 0.0_DFP, &
                    10.0_DFP / 3.0_DFP, 20.0_DFP / 3.0_DFP, &
                    10.0_DFP, 10.0_DFP, &
                    20.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
                    10.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
                    20.0_DFP / 3.0_DFP, 20.0_DFP / 3.0_DFP]
      want(3, :) = 0.0_DFP
      CALL OK(ALL(SoftEq(found, want, tol)), "elem=2, points: ")
    END IF

  END BLOCK

  CALL u%GetVtkUnstructuredValue(VALUE=VALUE)

  CALL OK(ALLOCATED(VALUE), "value allocated: ")
  IF (ALLOCATED(VALUE)) THEN
    CALL ok(SIZE(VALUE) .EQ. 32, "size of value: ")
  END IF

  CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%new, &
                           vtkType="UnstructuredGrid")

  CALL obj%WriteUnstructuredGrid(points, connectivity, offsets, types)
  CALL obj%WritePointData(NAME="u", VALUE=VALUE)

  BLOCK
    REAL(DFP) :: found(16)

    found = VALUE(1:16)
    CALL Display(found, "value for iel=1", full=math%yes)

  END BLOCK

! clean stuff
  CALL obj%DEALLOCATE()
  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()

CONTAINS
  SUBROUTINE scalarFunc2(args, nargs, ans)
    REAL(DFP), INTENT(IN) :: args(:)
    INTEGER(I4B), INTENT(IN) :: nargs
    REAL(DFP), INTENT(INOUT) :: ans
    ans = args(1)
  END SUBROUTINE scalarFunc2
END PROGRAM main
