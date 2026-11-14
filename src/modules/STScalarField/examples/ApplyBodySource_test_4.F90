!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Body source given by a space-time field with another fedof
!
! Same problem as ApplyBodySource_test_3, but the body source f has its
! own fedof, geofedof and timefedof (separate objects, same mesh and
! same order), so ApplyBodySource interpolates f at the quadrature
! points of u with the shape functions of f.
!
! The nodes are 1 (0,0), 2 (10,0), 3 (10,10), 4 (0,10), 5 (5,0),
! 6 (5,10). In space, integral(N_I) = 12.5 for nodes 1 to 4 and 25
! for nodes 5, 6, and integral(N_I x) = 125/6 for nodes 1, 4, 625/6
! for nodes 2, 3 and 125 for nodes 5, 6. In time, integral(T_a) = 1/2,
! integral(T_1 t) = 1/6 and integral(T_2 t) = 1/3.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE STScalarField_Class, ONLY: STScalarField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: times(2) = [0.0_DFP, 1.0_DFP]
REAL(DFP), PARAMETER :: space(6) = [12.5_DFP, 12.5_DFP, 12.5_DFP, &
                                    12.5_DFP, 25.0_DFP, 25.0_DFP]
REAL(DFP), PARAMETER :: xnode(6) = [0.0_DFP, 10.0_DFP, 10.0_DFP, &
                                    0.0_DFP, 5.0_DFP, 5.0_DFP]
REAL(DFP), PARAMETER :: spaceX(6) = [125.0_DFP / 6.0_DFP, &
                                     625.0_DFP / 6.0_DFP, &
                                     625.0_DFP / 6.0_DFP, &
                                     125.0_DFP / 6.0_DFP, &
                                     125.0_DFP, 125.0_DFP]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof, fedof2, geofedof2
TYPE(TimeFEDOF_), TARGET :: timefedof, timefedof2
TYPE(STScalarField_) :: u, f
REAL(DFP) :: found(12)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)
CALL f%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof2, geofedof=geofedof2, &
                      timefedof=timefedof2, dom=dom)

! Test 1: f = 1
BLOCK
  CALL f%Set(VALUE=1.0_DFP)
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyBodySource(bodySource=f, scale=1.0_DFP, times=times)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - 0.5_DFP * space) .LT. tol), &
          "f = 1: time 1")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - 0.5_DFP * space) .LT. tol), &
          "f = 1: time 2")
END BLOCK

! Test 2: f = t, added with scale 6
BLOCK
  CALL f%Set(VALUE=[0.0_DFP, 1.0_DFP])
  CALL u%Set(VALUE=1.0_DFP)
  CALL u%ApplyBodySource(bodySource=f, scale=6.0_DFP, times=times)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - (1.0_DFP + space)) .LT. tol), &
          "f = t: time 1")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - (1.0_DFP + 2.0_DFP * space)) .LT. tol), &
          "f = t: time 2")
END BLOCK

! Test 3: f = x
BLOCK
  CALL f%SetMultiple(istart=1, iend=12, stride=1, VALUE=[xnode, xnode])
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyBodySource(bodySource=f, scale=1.0_DFP, times=times)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - 0.5_DFP * spaceX) .LT. tol), &
          "f = x: time 1")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - 0.5_DFP * spaceX) .LT. tol), &
          "f = x: time 2")
END BLOCK

! Test 4: f = x t (0 at time node 1, x at time node 2)
BLOCK
  CALL f%SetMultiple(istart=1, iend=12, stride=1, &
                     VALUE=[0.0_DFP * xnode, xnode])
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyBodySource(bodySource=f, scale=1.0_DFP, times=times)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - spaceX / 6.0_DFP) .LT. tol), &
          "f = x t: time 1")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - spaceX / 3.0_DFP) .LT. tol), &
          "f = x t: time 2")
END BLOCK

CALL f%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof2%DEALLOCATE()
CALL fedof2%DEALLOCATE()
CALL geofedof2%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
