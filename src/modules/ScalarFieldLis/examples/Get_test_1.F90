!> author: Vikas Sharma, Ph. D.
! date: 2026-11-18
! summary: Get selected entries of a LIS scalar field in an FEVariable
!
! Get(VALUE=fevar, globalNode, islocal) (Get4) and GetFEVariable return
! the values of the selected nodes as a scalar, nodal, space
! FEVariable. fevar must be initiated with enough capacity first. The
! field is u = [10, 20, 30, 40, 50, 60]. The native version is
! src/modules/ScalarField/examples/Set_test_3.F90.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableSpace
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ScalarFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarFieldLis_) :: u
TYPE(FEVariable_) :: fevar
REAL(DFP) :: src(6)
INTEGER(I4B) :: ii

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")

src = [(10.0_DFP * REAL(ii, DFP), ii=1, 6)]
CALL u%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)

! Test 1: Get(VALUE=fevar, globalNode, islocal)
BLOCK
  fevar = NodalVariable(6, TypeFEVariableScalar, TypeFEVariableSpace)
  CALL u%Get(VALUE=fevar, globalNode=[6, 1, 4], islocal=math%yes)
  CALL OK(fevar%len .EQ. 3, "Get FEVariable: len")
  CALL OK(ALL(ABS(fevar%val(1:3) - [60.0_DFP, 10.0_DFP, 40.0_DFP]) &
              .LT. tol), "Get FEVariable: values")
END BLOCK

! Test 2: GetFEVariable
BLOCK
  fevar = NodalVariable(6, TypeFEVariableScalar, TypeFEVariableSpace)
  CALL u%GetFEVariable(globalNode=[5, 2], islocal=math%yes, VALUE=fevar)
  CALL OK(fevar%len .EQ. 2, "GetFEVariable: len")
  CALL OK(ALL(ABS(fevar%val(1:2) - [50.0_DFP, 20.0_DFP]) .LT. tol), &
          "GetFEVariable: values")
END BLOCK

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
