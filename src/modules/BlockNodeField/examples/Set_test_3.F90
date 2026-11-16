!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set all degrees of freedom of a physical variable at nodes
!
! The block field u has two physical variables on six nodes: V (two
! space components, entries 1-12) and P (one component, entries 13-18),
! stored in DOF format.
!
! Set(globalNode(:), islocal, VALUE, ivar) sets all DOFs of ivar at the
! nodes to one value. Set(globalNode(:), VALUE(:), islocal, ivar) sets
! them from a vector in DOF order: all nodes of DOF 1 of ivar, then all
! nodes of DOF 2, and so on; SIZE(VALUE) = SIZE(globalNode) * tdof(ivar).

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
REAL(DFP) :: raw(18), want(18)
INTEGER(I4B) :: tsize

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

! all DOFs of V at nodes 2, 3 to one value (obj_Set4)
u = 0.0_DFP
want = 0.0_DFP
CALL u%Set(globalNode=[2, 3], islocal=math%yes, VALUE=5.0_DFP, ivar=1)
want([2, 3, 8, 9]) = 5.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set4: ivar = 1")

! all DOFs of P at nodes 1, 6 to one value (obj_Set4)
CALL u%Set(globalNode=[1, 6], islocal=math%yes, VALUE=8.0_DFP, ivar=2)
want([13, 18]) = 8.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set4: ivar = 2")

! all DOFs of V at nodes 4, 5 from a vector (obj_Set5)
u = 0.0_DFP
want = 0.0_DFP
CALL u%Set(globalNode=[4, 5], islocal=math%yes, ivar=1, &
           VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP])
want([4, 5, 10, 11]) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP]
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set5: ivar = 1")

! all DOFs of P at nodes 2, 3 from a vector (obj_Set5)
CALL u%Set(globalNode=[2, 3], islocal=math%yes, ivar=2, &
           VALUE=[9.0_DFP, 10.0_DFP])
want([14, 15]) = [9.0_DFP, 10.0_DFP]
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set5: ivar = 2")

! add: u = u + 2 * value
CALL u%Set(globalNode=[2, 3], islocal=math%yes, ivar=2, &
           VALUE=[1.0_DFP, 1.0_DFP], scale=2.0_DFP, &
           addContribution=math%yes)
want([14, 15]) = [11.0_DFP, 12.0_DFP]
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "Set5: add")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
