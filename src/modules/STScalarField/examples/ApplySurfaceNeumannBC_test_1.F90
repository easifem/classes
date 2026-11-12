!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Apply surface Neumann BCs of a space-time scalar field
!
! The field u (order 1 in space and in time on [0, 1], timeCompo = 2)
! has two Neumann BCs (see ApplySurfaceNeumannBC_test_1.toml): q = 2 on
! the right edge (nodes 2, 3, length 10) and q = 5 on the bottom edge
! (nodes 1, 5, 2, two segments of length 5). Node 2 is shared.
!
! For a constant q on a segment of length L each end node gets
! q * L / 2 in space, and each time node gets 1/2 of it in time:
!   node 1: 6.25, node 5: 12.5, node 2: 5 + 6.25 = 11.25, node 3: 5,
!   nodes 4, 6: 0, for both time components.
! The total of each time component is (2 * 10 + 5 * 10) / 2 = 35.

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

CHARACTER(*), PARAMETER :: tomlFileName = &
                           "ApplySurfaceNeumannBC_test_1.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: times(2) = [0.0_DFP, 1.0_DFP]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STScalarField_) :: u, nbcField
REAL(DFP) :: found(12), want(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)
CALL nbcField%Initiate(obj2=u, copyFull=math%no, &
                       copyStructure=math%yes, usePointer=math%yes)

CALL OK(u%GetTotalNBC() .EQ. 2, "GetTotalNBC")

want = [6.25_DFP, 11.25_DFP, 5.0_DFP, 0.0_DFP, 12.5_DFP, 0.0_DFP]

! Test 1: values of both time components
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplySurfaceNeumannBC(nbcField=nbcField, scale=1.0_DFP, &
                               times=times)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "time 1")
  CALL OK(ABS(SUM(found(1:6)) - 35.0_DFP) .LT. tol, "time 1: total")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - want) .LT. tol), "time 2")
END BLOCK

CALL nbcField%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
