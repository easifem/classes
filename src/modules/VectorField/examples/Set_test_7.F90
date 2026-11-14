!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Set one space component from a scalar field (Set6)
!
! Set(VALUE=field, spaceCompo) sets component spaceCompo of all nodes
! from the values of a node field with one value for each node (for
! example a ScalarField_); with addContribution, scale * VALUE is
! added. u is a VectorField_ with 6 nodes and spaceCompo = 2, s is a
! ScalarField_ on the same FEDOF.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorField_) :: u
TYPE(ScalarField_) :: s
REAL(DFP) :: vec(6), src(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL s%Initiate(NAME="s", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof)

src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
CALL s%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)

! Test 1: component 2 is set from s, component 1 is not changed
BLOCK
  CALL u%Set(VALUE=-1.0_DFP)
  CALL u%Set(VALUE=s, spaceCompo=2)
  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(vec - src) .LT. tol), "Set6: component from field")
  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(vec + 1.0_DFP) .LT. tol), "Set6: other component")
END BLOCK

! Test 2: set with scale
BLOCK
  CALL u%Set(VALUE=s, spaceCompo=1, scale=2.0_DFP)
  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(vec - 2.0_DFP * src) .LT. tol), "Set6: scale")
END BLOCK

! Test 3: add scale * s
BLOCK
  CALL u%Set(VALUE=s, spaceCompo=1, scale=-1.0_DFP, &
             addContribution=math%yes)
  CALL u%Get(VALUE=vec, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(vec - src) .LT. tol), "Set6: add")
END BLOCK

CALL s%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
