!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set and get space-time values in rank 3 arrays and at
!   selected nodes
!
!- Set(VALUE(:, :, :)) sets all values; VALUE(s, t, i) is space
!  component s, time component t of node i,
!- Get(VALUE(:, :, :), storageFMT, dim1, dim2, dim3) returns all values,
!  NODES_FMT: (spaceCompo, timeCompo, tNodes), DOF_FMT: (tNodes,
!  spaceCompo, timeCompo),
!- Set(VALUE(:, :), globalNode(:), islocal) sets the same space-time
!  matrix at the selected nodes,
!- Set(globalNode(:), islocal, VALUE(:, :, :)) sets the selected nodes
!  from a rank 3 array,
!- Set(VALUE(:), globalNode(:), islocal, spaceCompo, timeCompo) sets
!  one component of the selected nodes,
!- Get(VALUE(:, :, :), storageFMT, globalNode(:), islocal, dim1, dim2,
!  dim3) and Get(VALUE(:), tsize, globalNode(:), islocal, spaceCompo,
!  timeCompo) read the selected nodes.
!
! The test values are f(s, t, i) = 100 s + 10 t + i.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DOF_FMT
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: NODES_FMT
USE STVectorField_Class, ONLY: STVectorField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STVectorField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorField_) :: u
REAL(DFP) :: v3(2, 2, 6), g3(2, 2, 6), found3(6, 2, 2), w3(2, 2, 3), &
             mat(2, 2), found(6)
INTEGER(I4B) :: ii, jj, kk, dim1, dim2, dim3, tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

DO kk = 1, 6
  DO jj = 1, 2
    DO ii = 1, 2
      v3(ii, jj, kk) = f(ii, jj, kk)
    END DO
  END DO
END DO

! Test 1: all values from a rank 3 array, NODES_FMT
BLOCK
  CALL u%Set(VALUE=v3)
  g3 = 0.0_DFP
  CALL u%Get(VALUE=g3, storageFMT=NODES_FMT, dim1=dim1, dim2=dim2, &
             dim3=dim3)
  CALL OK(dim1 .EQ. 2 .AND. dim2 .EQ. 2 .AND. dim3 .EQ. 6, &
          "Get all, NODES: dims")
  CALL OK(ALL(ABS(g3 - v3) .LT. tol), &
          "Get all, NODES: values")
END BLOCK

! Test 2: all values, DOF_FMT
BLOCK
  found3 = 0.0_DFP
  CALL u%Get(VALUE=found3, storageFMT=DOF_FMT, dim1=dim1, dim2=dim2, &
             dim3=dim3)
  CALL OK(dim1 .EQ. 6 .AND. dim2 .EQ. 2 .AND. dim3 .EQ. 2, &
          "Get all, DOF: dims")
  CALL OK(ALL(ABS(RESHAPE(found3, [6, 2, 2]) - &
                  RESHAPE(v3, [6, 2, 2], order=[2, 3, 1])) .LT. tol), &
          "Get all, DOF: values")
END BLOCK

! Test 3: same matrix at selected nodes
BLOCK
  mat = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2])
  CALL u%Set(VALUE=mat, globalNode=[2, 5], islocal=math%yes)
  CALL u%Get(VALUE=w3, storageFMT=NODES_FMT, globalNode=[2, 5, 3], &
             islocal=math%yes, dim1=dim1, dim2=dim2, dim3=dim3)
  CALL OK(dim3 .EQ. 3, "Get nodes: dim3")
  CALL OK(ALL(ABS(w3(:, :, 1) - mat) .LT. tol) .AND. &
          ALL(ABS(w3(:, :, 2) - mat) .LT. tol), "Set matrix at nodes")
  CALL OK(ALL(ABS(w3(:, :, 3) - v3(:, :, 3)) .LT. tol), &
          "Set matrix at nodes: node 3 unchanged")
END BLOCK

! Test 4: selected nodes from a rank 3 array
BLOCK
  w3 = 2.0_DFP * v3(:, :, 1:3)
  CALL u%Set(globalNode=[6, 4, 1], islocal=math%yes, VALUE=w3)
  w3 = 0.0_DFP
  CALL u%Get(VALUE=w3, storageFMT=NODES_FMT, globalNode=[6, 4, 1], &
             islocal=math%yes, dim1=dim1, dim2=dim2, dim3=dim3)
  CALL OK(ALL(ABS(w3 - 2.0_DFP * v3(:, :, 1:3)) .LT. tol), &
          "Set nodes rank 3")
END BLOCK

! Test 5: one component of selected nodes
BLOCK
  CALL u%Set(VALUE=[7.0_DFP, 8.0_DFP], globalNode=[3, 5], &
             islocal=math%yes, spaceCompo=2, timeCompo=2)
  CALL u%Get(VALUE=found, tsize=tsize, globalNode=[3, 5], &
             islocal=math%yes, spaceCompo=2, timeCompo=2)
  CALL OK(tsize .EQ. 2, "Get component of nodes: tsize")
  CALL OK(ALL(ABS(found(1:2) - [7.0_DFP, 8.0_DFP]) .LT. tol), &
          "Set component of nodes")
  CALL u%Get(VALUE=found, tsize=tsize, globalNode=[3, 5], &
             islocal=math%yes, spaceCompo=1, timeCompo=2)
  CALL OK(ALL(ABS(found(1:2) - [f(1, 2, 3), mat(1, 2)]) .LT. tol), &
          "Set component of nodes: other component unchanged")
END BLOCK

CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

PURE FUNCTION f(sc, tc, nd) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: sc, tc, nd
  REAL(DFP) :: ans
  ans = 100.0_DFP * sc + 10.0_DFP * tc + nd
END FUNCTION f

END PROGRAM main
