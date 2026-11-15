!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set and get space-time values with FEVariable, and copy
!
!- Get(VALUE=fevar, globalNode(:), islocal) and GetFEVariable return
!  the values of the selected nodes in a nodal, vector, space-time
!  FEVariable,
!- Set(VALUE=fevar, globalNode(:), islocal) sets the selected nodes from
!  such a FEVariable,
!- Set(VALUE=v) copies another space-time vector field.
!
! The values of nodes 1, 5, 2 are read in a FEVariable and set at
! nodes 3, 6, 4. The test values are f(s, t, i) = 100 s + 10 t + i.

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
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
TYPE(STVectorField_) :: u, v
TYPE(FEVariable_) :: fevar, fevar2
REAL(DFP) :: v3(2, 2, 6), a3(2, 2, 3), b3(2, 2, 3), found(24), &
             found2(24)
INTEGER(I4B) :: ii, jj, kk, dim1, dim2, dim3, tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

DO kk = 1, 6
  DO jj = 1, 2
    DO ii = 1, 2
      v3(ii, jj, kk) = 100.0_DFP * ii + 10.0_DFP * jj + kk
    END DO
  END DO
END DO
CALL u%Set(VALUE=v3)

! Test 1: FEVariable round trip
BLOCK
  CALL u%Get(VALUE=fevar, globalNode=[1, 5, 2], islocal=math%yes)
  CALL u%GetFEVariable(VALUE=fevar2, globalNode=[1, 5, 2], &
                       islocal=math%yes)
  CALL u%Set(VALUE=fevar, globalNode=[3, 6, 4], islocal=math%yes)
  CALL u%Get(VALUE=a3, storageFMT=NODES_FMT, globalNode=[1, 5, 2], &
             islocal=math%yes, dim1=dim1, dim2=dim2, dim3=dim3)
  CALL u%Get(VALUE=b3, storageFMT=NODES_FMT, globalNode=[3, 6, 4], &
             islocal=math%yes, dim1=dim1, dim2=dim2, dim3=dim3)
  CALL OK(ALL(ABS(a3 - b3) .LT. tol), &
          "Set(FEVariable) from Get(FEVariable)")
  CALL OK(ALL(ABS(a3(:, :, 2) - v3(:, :, 5)) .LT. tol), &
          "Set(FEVariable): source nodes unchanged")
END BLOCK

! Test 2: GetFEVariable gives the same FEVariable as Get
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%Set(VALUE=fevar2, globalNode=[3, 6, 4], islocal=math%yes)
  CALL u%Get(VALUE=b3, storageFMT=NODES_FMT, globalNode=[3, 6, 4], &
             islocal=math%yes, dim1=dim1, dim2=dim2, dim3=dim3)
  CALL OK(ALL(ABS(a3 - b3) .LT. tol), "GetFEVariable")
END BLOCK

! Test 3: copy another field
BLOCK
  CALL u%Set(VALUE=v3)
  CALL v%Initiate(obj2=u)
  CALL v%Set(VALUE=0.0_DFP)
  CALL v%Set(VALUE=u)
  CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL v%GetMultiple(istart=1, iend=24, stride=1, VALUE=found2, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - found2) .LT. tol), "Set(STVectorField_)")
END BLOCK

CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
