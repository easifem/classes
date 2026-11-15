!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set all entries, one node and one entry of a space-time
!   vector field
!
!- Set(VALUE) sets all entries to one value,
!- Set(VALUE(:, :)) sets every node to the same space-time matrix,
!  VALUE(spaceCompo, timeCompo),
!- Set(globalNode, islocal, VALUE(:, :)) sets the space-time matrix of
!  one node,
!- Set(VALUE, globalNode, islocal, spaceCompo, timeCompo) sets one
!  entry.
!
! The values are stored in DOF format: entry
! ((t - 1) * spaceCompo + s - 1) * tNodes + i is space component s and
! time component t of node i (tNodes = 6, spaceCompo = 2).
!
! Without addContribution the entries become scale * VALUE; with
! addContribution = .TRUE. scale * VALUE is added.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
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
REAL(DFP) :: found(24), mat(2, 2), val
INTEGER(I4B) :: tsize, nrow, ncol

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

! Test 1: all entries to one value
BLOCK
  CALL u%Set(VALUE=1.0_DFP)
  CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found - 1.0_DFP) .LT. tol), "Set all")
END BLOCK

! Test 2: the same space-time matrix at every node
BLOCK
  mat = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2])
  CALL u%Set(VALUE=mat)
  CALL u%GetMultiple(istart=1, iend=24, stride=1, VALUE=found, &
                     tsize=tsize)
  CALL OK(ALL(ABS(found(1:6) - 1.0_DFP) .LT. tol) .AND. &
          ALL(ABS(found(7:12) - 2.0_DFP) .LT. tol) .AND. &
          ALL(ABS(found(13:18) - 3.0_DFP) .LT. tol) .AND. &
          ALL(ABS(found(19:24) - 4.0_DFP) .LT. tol), &
          "Set matrix at all nodes: DOF format")
END BLOCK

! Test 3: space-time matrix of one node
BLOCK
  mat = RESHAPE([5.0_DFP, 6.0_DFP, 7.0_DFP, 8.0_DFP], [2, 2])
  CALL u%Set(globalNode=2, islocal=math%yes, VALUE=mat)
  CALL u%Get(VALUE=mat, nrow=nrow, ncol=ncol, globalNode=2, &
             islocal=math%yes)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "Get node: nrow, ncol")
  CALL OK(ALL(ABS(mat - RESHAPE([5.0_DFP, 6.0_DFP, 7.0_DFP, &
                                 8.0_DFP], [2, 2])) .LT. tol), &
          "Set node")
  ! node 2 is entry 2, 8, 14, 20
  CALL u%GetMultiple(indx=[2, 8, 14, 20], VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found(1:4) - [5.0_DFP, 6.0_DFP, 7.0_DFP, &
                                8.0_DFP]) .LT. tol), &
          "Set node: DOF format")

  mat = 1.0_DFP
  CALL u%Set(globalNode=2, islocal=math%yes, VALUE=mat, &
             scale=2.0_DFP, addContribution=math%yes)
  CALL u%Get(VALUE=mat, nrow=nrow, ncol=ncol, globalNode=2, &
             islocal=math%yes)
  CALL OK(ALL(ABS(mat - RESHAPE([7.0_DFP, 8.0_DFP, 9.0_DFP, &
                                 10.0_DFP], [2, 2])) .LT. tol), &
          "Set node: add 2 * value")
END BLOCK

! Test 4: one entry
BLOCK
  CALL u%Set(VALUE=9.0_DFP, globalNode=3, islocal=math%yes, &
             spaceCompo=2, timeCompo=1)
  CALL u%Get(VALUE=val, globalNode=3, islocal=math%yes, spaceCompo=2, &
             timeCompo=1)
  CALL OK(ABS(val - 9.0_DFP) .LT. tol, "Set single")
  CALL u%Get(VALUE=val, globalNode=3, islocal=math%yes, spaceCompo=1, &
             timeCompo=1)
  CALL OK(ABS(val - 1.0_DFP) .LT. tol, "Set single: other component")
  CALL u%Set(VALUE=1.0_DFP, globalNode=3, islocal=math%yes, &
             spaceCompo=2, timeCompo=1, addContribution=math%yes)
  CALL u%Get(VALUE=val, globalNode=3, islocal=math%yes, spaceCompo=2, &
             timeCompo=1)
  CALL OK(ABS(val - 10.0_DFP) .LT. tol, "Set single: add")
END BLOCK

CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
