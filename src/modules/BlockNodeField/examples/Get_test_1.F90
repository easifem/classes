!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Get values of a block node field as a nodal FEVariable
!
! u has V (two space components) and P (one component) on six nodes;
! w has U (two space, two time components) and T (one space, three
! time components). Entry k of each field has value k.
!
!- Get(fevar, globalNode(:), islocal, ivar, idof) and
!  Get(fevar, globalNode(:), islocal, ivar, spaceCompo, timeCompo)
!  return one DOF as a scalar, space FEVariable.
!- Get(fevar, globalNode(:), islocal, ivar) and GetFEVariable return all
!  DOFs of ivar: scalar space (val(node)), vector space
!  (val(spaceCompo, node)), scalar space-time (val(node, timeCompo)) or
!  vector space-time (val(spaceCompo, node, timeCompo)).

PROGRAM main
USE BaseType, ONLY: FEVariable_
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
TYPE(BlockNodeField_) :: u, w
TYPE(FEVariable_) :: fevar
REAL(DFP) :: src1(18), src2(42)
INTEGER(I4B) :: ii

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

CALL w%Initiate( &
  name="w", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[2, 3], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=2, physicalVarNames=["U", "T"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=42, &
  fedof=fedofs, geofedof=geofedofs)

src1 = [(REAL(ii, DFP), ii=1, 18)]
CALL u%Set(VALUE=src1)
src2 = [(REAL(ii, DFP), ii=1, 42)]
CALL w%Set(VALUE=src2)

! one DOF by idof (obj_Get4): V_y at nodes 2, 4
CALL u%Get(VALUE=fevar, globalNode=[2, 4], islocal=math%yes, ivar=1, &
           idof=2)
CALL OK(fevar%len .EQ. 2, "Get4: len")
CALL OK(ALL(ABS(fevar%val(1:2) - [8.0_DFP, 10.0_DFP]) .LT. tol), &
        "Get4: V_y at nodes 2, 4")

! one DOF by components (obj_Get7): P at nodes 2, 4
CALL u%Get(VALUE=fevar, globalNode=[2, 4], islocal=math%yes, ivar=2, &
           spaceCompo=1, timeCompo=1)
CALL OK(ALL(ABS(fevar%val(1:2) - [14.0_DFP, 16.0_DFP]) .LT. tol), &
        "Get7: P at nodes 2, 4")

! vector space (obj_Get5): V at nodes 2, 4, val(spaceCompo, node)
CALL u%Get(VALUE=fevar, globalNode=[2, 4], islocal=math%yes, ivar=1)
CALL OK(ALL(fevar%s(1:2) .EQ. [2, 2]), "Get5 vector space: shape")
CALL OK(ALL(ABS(fevar%val(1:4) - [2.0_DFP, 8.0_DFP, 4.0_DFP, &
                                  10.0_DFP]) .LT. tol), &
        "Get5 vector space: values")

! scalar space (obj_Get5): P at nodes 2, 4
CALL u%Get(VALUE=fevar, globalNode=[2, 4], islocal=math%yes, ivar=2)
CALL OK(fevar%len .EQ. 2, "Get5 scalar space: len")
CALL OK(ALL(ABS(fevar%val(1:2) - [14.0_DFP, 16.0_DFP]) .LT. tol), &
        "Get5 scalar space: values")

! GetFEVariable: same as Get5, ivar = 1 by default
CALL u%GetFEVariable(globalNode=[2, 4], islocal=math%yes, VALUE=fevar, &
                     ivar=2)
CALL OK(ALL(ABS(fevar%val(1:2) - [14.0_DFP, 16.0_DFP]) .LT. tol), &
        "GetFEVariable: ivar = 2")
CALL u%GetFEVariable(globalNode=[2, 4], islocal=math%yes, VALUE=fevar)
CALL OK(ALL(ABS(fevar%val(1:4) - [2.0_DFP, 8.0_DFP, 4.0_DFP, &
                                  10.0_DFP]) .LT. tol), &
        "GetFEVariable: default ivar")

! vector space-time (obj_Get5): U at nodes 2, 4,
! val(spaceCompo, node, timeCompo)
CALL w%Get(VALUE=fevar, globalNode=[2, 4], islocal=math%yes, ivar=1)
CALL OK(ALL(fevar%s(1:3) .EQ. [2, 2, 2]), &
        "Get5 vector space-time: shape")
CALL OK(ALL(ABS(fevar%val(1:8) - [2.0_DFP, 8.0_DFP, 4.0_DFP, 10.0_DFP, &
                                  14.0_DFP, 20.0_DFP, 16.0_DFP, &
                                  22.0_DFP]) .LT. tol), &
        "Get5 vector space-time: values")

! scalar space-time (obj_Get5): T at nodes 2, 4, val(node, timeCompo)
CALL w%Get(VALUE=fevar, globalNode=[2, 4], islocal=math%yes, ivar=2)
CALL OK(ALL(fevar%s(1:2) .EQ. [2, 3]), "Get5 scalar space-time: shape")
CALL OK(ALL(ABS(fevar%val(1:6) - [26.0_DFP, 28.0_DFP, 32.0_DFP, &
                                  34.0_DFP, 38.0_DFP, 40.0_DFP]) &
            .LT. tol), "Get5 scalar space-time: values")

CALL u%DEALLOCATE()
CALL w%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
