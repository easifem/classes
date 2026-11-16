!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set and get values of a LIS block node field by node
!
! The LIS library is started by LisOmpEngine_%Initiate before any LIS
! field is made, and stopped by LisOmpEngine_%DEALLOCATE at the end.
! The fields have V (two space components) and P (one component) on
! six nodes, 18 values in DOF format: V_x of nodes 1-6 (entries 1-6),
! V_y (7-12), then P (13-18).
!
! The Set and Get methods of BlockNodeField_ (all values, by ivar and
! idof, by ivar, by ivar, spaceCompo and timeCompo, FEVariable) work on
! the LIS vector, through SetSingle, SetAll, SetMultiple, GetSingle and
! GetMultiple of BlockNodeFieldLis_.

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: math => TypeMathOpt
USE BlockNodeFieldLis_Class, ONLY: BlockNodeFieldLis_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DOF_FMT
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "BlockNodeFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(FEDOFPointer_) :: fedofs(1), geofedofs(1)
TYPE(BlockNodeFieldLis_) :: u
TYPE(FEVariable_) :: fevar
REAL(DFP) :: raw(18), want(18), src(18), vec(3), val
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL fedof%ImportFromToml(tomlName="space", filename=tomlFileName, &
                          dom=dom)
CALL geofedof%ImportFromToml(tomlName="geofedof", &
                             filename=tomlFileName, dom=dom)
fedofs(1)%ptr => fedof
geofedofs(1)%ptr => geofedof

CALL u%Initiate( &
  name="u", engine="LIS_OMP", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
  fedof=fedofs, geofedof=geofedofs)

! all values: assignment, Set(VALUE(:)) and Get(VALUE(:), tsize)
u = 0.0_DFP
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(tsize .EQ. 18, "Get all: tsize")
CALL OK(ALL(ABS(raw) .LT. tol), "assignment u = 0")
src = [(REAL(ii, DFP), ii=1, 18)]
CALL u%Set(VALUE=src)
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - src) .LT. tol), "Set all from a vector")

u = 0.0_DFP
want = 0.0_DFP

! one entry by ivar and idof: P of node 5
CALL u%Set(globalNode=5, islocal=math%yes, VALUE=6.0_DFP, ivar=2, &
           idof=1)
want(17) = 6.0_DFP
CALL u%Get(VALUE=val, globalNode=5, islocal=math%yes, ivar=2, idof=1)
CALL OK(ABS(val - 6.0_DFP) .LT. tol, "Set/Get one entry")

! all DOFs of V at nodes 2, 3 to one value
CALL u%Set(globalNode=[2, 3], islocal=math%yes, VALUE=5.0_DFP, ivar=1)
want([2, 3, 8, 9]) = 5.0_DFP

! all DOFs of P at nodes 2, 3 from a vector
CALL u%Set(globalNode=[2, 3], islocal=math%yes, ivar=2, &
           VALUE=[9.0_DFP, 10.0_DFP])
want([14, 15]) = [9.0_DFP, 10.0_DFP]

! V_x of nodes 4, 5, 6 by ivar and idof
CALL u%Set(globalNode=[4, 5, 6], islocal=math%yes, &
           VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP], ivar=1, idof=1)
want(4:6) = [1.0_DFP, 2.0_DFP, 3.0_DFP]
CALL u%Get(VALUE=vec, tsize=tsize, globalNode=[4, 5, 6], &
           islocal=math%yes, ivar=1, idof=1)
CALL OK(ALL(ABS(vec - [1.0_DFP, 2.0_DFP, 3.0_DFP]) .LT. tol), &
        "Set/Get by ivar, idof")

! V_y of nodes 1, 2, 3 by spaceCompo and timeCompo
CALL u%Set(globalNode=[1, 2, 3], islocal=math%yes, &
           VALUE=[11.0_DFP, 12.0_DFP, 13.0_DFP], ivar=1, spaceCompo=2, &
           timeCompo=1)
want(7:9) = [11.0_DFP, 12.0_DFP, 13.0_DFP]
CALL u%Get(VALUE=vec, tsize=tsize, globalNode=[1, 2, 3], &
           islocal=math%yes, ivar=1, spaceCompo=2, timeCompo=1)
CALL OK(ALL(ABS(vec - [11.0_DFP, 12.0_DFP, 13.0_DFP]) .LT. tol), &
        "Set/Get by spaceCompo, timeCompo")

CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - want) .LT. tol), "all entries")

! V at nodes 2, 4 as a vector, space FEVariable, val(spaceCompo, node)
CALL u%Get(VALUE=fevar, globalNode=[2, 4], islocal=math%yes, ivar=1)
CALL OK(ALL(ABS(fevar%val(1:4) - [5.0_DFP, 12.0_DFP, 1.0_DFP, &
                                  0.0_DFP]) .LT. tol), &
        "Get FEVariable")

! add to all values: u = u + 2 * 1
CALL u%Set(VALUE=1.0_DFP, scale=2.0_DFP, addContribution=math%yes)
CALL u%Get(VALUE=raw, tsize=tsize)
CALL OK(ALL(ABS(raw - (want + 2.0_DFP)) .LT. tol), "add to all values")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
