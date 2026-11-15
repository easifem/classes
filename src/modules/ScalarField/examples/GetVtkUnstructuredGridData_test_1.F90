!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: testing scalarField GetVtkUnstructuredGridData

PROGRAM main
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE ApproxUtility, ONLY: SoftEq
  USE BaseType, ONLY: math => TypeMathOpt
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
  IMPLICIT NONE
  CHARACTER(*), PARAMETER :: tomlFileName = &
    "GetVtkUnstructuredGridData_test_1.toml"
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_) :: fedof, geofedof
  TYPE(ScalarField_) :: u
  CLASS(AbstractMesh_), POINTER :: mesh
  INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
  INTEGER(INT8), ALLOCATABLE :: types(:)
  REAL(DFP), ALLOCATABLE :: points(:, :)

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

! import domain
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")

! import u
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")
  CALL u%Set(2.0_DFP)

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

! clean stuff
  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
! ok 1 - points allocated:
! ok 2 - shape of points:
! ok 3 - connectivity allocated:
! ok 4 - shape of connectivity:
! ok 5 - offsets allocated:
! ok 6 - size of offsets:
! ok 7 - types allocated:
! ok 8 - size of types:
! ok 9 - elem=1, connectivity:
! ok 10 - elem=2, connectivity:
! ok 11 - elem=1, points:
! ok 12 - elem=2, points:
