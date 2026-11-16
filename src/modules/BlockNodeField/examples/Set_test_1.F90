!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set and get all values of a block node field
!
! The block field u has two physical variables on six nodes: V (two
! space components) and P (one component), so it has 18 values.
! Set(VALUE) and u = VALUE set all values to a scalar,
! Set(VALUE(:)) sets all values from a vector, and Get(VALUE(:), tsize)
! returns all values. With scale and addContribution the values are
! added: u = u + scale * VALUE.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BlockNodeField_Class, ONLY: BlockNodeField_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DOF_FMT
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "BlockNodeField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(FEDOFPointer_) :: fedofs(1), geofedofs(1)
TYPE(BlockNodeField_) :: u
REAL(DFP) :: src(18), found(18)
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL fedof%ImportFromToml(tomlName="space", filename=tomlFileName, &
                          dom=dom)
CALL geofedof%ImportFromToml(tomlName="geofedof", &
                             filename=tomlFileName, dom=dom)
fedofs(1)%ptr => fedof
geofedofs(1)%ptr => geofedof

CALL u%Initiate( &
  name="u", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
  fedof=fedofs, geofedof=geofedofs)

CALL OK(u%IsInitiated(), "Initiate: IsInitiated")
CALL OK(u%SIZE() .EQ. 18, "Initiate: Size")

! Set all values to a scalar (obj_Set1), Get all values (obj_Get2)
CALL u%Set(VALUE=2.0_DFP)
CALL u%Get(VALUE=found, tsize=tsize)
CALL OK(tsize .EQ. 18, "Get all: tsize")
CALL OK(ALL(ABS(found - 2.0_DFP) .LT. tol), "Set all to a scalar")

! Assignment (obj_Assign)
u = 3.0_DFP
CALL u%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 3.0_DFP) .LT. tol), "Assignment u = 3")

! Add a scalar: u = u + 2 * 1
CALL u%Set(VALUE=1.0_DFP, scale=2.0_DFP, addContribution=math%yes)
CALL u%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 5.0_DFP) .LT. tol), "Add a scalar")

! Set all values from a vector (obj_Set2)
src = [(REAL(ii, DFP), ii=1, 18)]
CALL u%Set(VALUE=src)
CALL u%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - src) .LT. tol), "Set all from a vector")

! Add a vector: u = u + 2 * src
CALL u%Set(VALUE=src, scale=2.0_DFP, addContribution=math%yes)
CALL u%Get(VALUE=found, tsize=tsize)
CALL OK(ALL(ABS(found - 3.0_DFP * src) .LT. tol), "Add a vector")

CALL u%DEALLOCATE()
CALL OK(.NOT. u%IsInitiated(), "DEALLOCATE: not initiated")
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
