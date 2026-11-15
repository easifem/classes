!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Set and Get of a LIS space-time vector field
!
! u (STVectorFieldLis_test.toml, engine LIS_OMP, spaceCompo = 2,
! timeCompo = 2, six nodes) is set by the Set methods of
! STVectorField_, which call SetSingle, SetAll and SetMultiple of
! STVectorFieldLis_. The values must be in the LIS vector:
!
!- Get returns them,
!- Display, which gathers the LIS vector into realVec, keeps them,
!- a native field set from u (Set14 → Copy) gets them.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE STVectorFieldLis_Class, ONLY: STVectorFieldLis_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STVectorFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorFieldLis_) :: u
REAL(DFP) :: found(24), want(24), areal
INTEGER(I4B) :: ii, s, t, tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

! want: value of node i, space s, time t is 100 s + 10 t + i
DO t = 1, 2
  DO s = 1, 2
    DO ii = 1, 6
      want((t - 1) * 12 + (s - 1) * 6 + ii) = &
        100.0_DFP * s + 10.0_DFP * t + ii
    END DO
  END DO
END DO

! Test 1: Set a value at all DOFs (Set11 → SetAll)
BLOCK
  CALL u%Set(VALUE=3.0_DFP)
  CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(tsize .EQ. 24 .AND. ALL(ABS(found - 3.0_DFP) .LT. tol), &
          "Set11: all values")
END BLOCK

! Test 2: Set one space-time component (Set8 → SetMultiple)
BLOCK
  DO t = 1, 2
    DO s = 1, 2
      CALL u%Set(VALUE=[(100.0_DFP * s + 10.0_DFP * t + ii, ii=1, 6)], &
                 spaceCompo=s, timeCompo=t)
    END DO
  END DO
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=1)
  CALL OK(tsize .EQ. 6 .AND. &
          ALL(ABS(found(1:6) - [(210.0_DFP + ii, ii=1, 6)]) .LT. tol), &
          "Set8 and Get: one component")
END BLOCK

! Test 3: one value (Set9 → SetSingle) and GetSingle
BLOCK
  CALL u%Set(globalNode=3, islocal=math%yes, VALUE=-1.0_DFP, &
             spaceCompo=1, timeCompo=2)
  CALL u%GetSingle(indx=12 + 3, VALUE=areal)
  CALL OK(ABS(areal + 1.0_DFP) .LT. tol, "Set9 and GetSingle")
  CALL u%Set(globalNode=3, islocal=math%yes, VALUE=want(15), &
             spaceCompo=1, timeCompo=2)
END BLOCK

! Test 4: Display gathers the LIS vector into realVec; values are kept
BLOCK
  CALL u%Display("u")
  CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "Display: values kept")
END BLOCK

CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
