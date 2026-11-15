!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Initiate a LIS scalar field
!
! The LIS library is started by LisOmpEngine_%Initiate before any LIS
! field is made, and stopped by LisOmpEngine_%DEALLOCATE at the end.
!
! The field u (order 1 Lagrange on square_3x2.h5, six DOFs) is made by
! ImportFromToml with engine = "LIS_OMP" (ScalarFieldLis_test.toml).
! A second field v is initiated with Initiate by passing the name, the
! engine and the same FEDOFs. Each field has its own LIS vector.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
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
TYPE(ScalarFieldLis_) :: u, v
REAL(DFP) :: found(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL v%Initiate(NAME="v", engine="LIS_OMP", fedof=fedof, &
                geofedof=geofedof)

! Test 1: both fields are initiated, with one value for each DOF
CALL OK(u%IsInitiated(), "u: IsInitiated")
CALL OK(v%IsInitiated(), "v: IsInitiated")
CALL OK(u%SIZE() .EQ. 6, "u: Size")
CALL OK(v%SIZE() .EQ. fedof%GetTotalDOF(), "v: Size = total DOF")
CALL OK(v%GetTotalPhysicalVars() .EQ. 1, "v: one physical variable")

! Test 2: u and v have their own LIS vectors
BLOCK
  CALL u%Set(VALUE=1.0_DFP)
  CALL v%Set(VALUE=2.0_DFP)
  CALL u%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - 1.0_DFP) .LT. tol), "u: values")
  CALL v%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - 2.0_DFP) .LT. tol), "v: values")
END BLOCK

CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
