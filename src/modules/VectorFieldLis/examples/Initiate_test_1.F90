!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Initiate a LIS vector field
!
! The LIS library is started by LisOmpEngine_%Initiate before any LIS
! field is made, and stopped by LisOmpEngine_%DEALLOCATE at the end.
!
! The field u (order 1 Lagrange on square_3x2.h5, two space components,
! 6 x 2 = 12 values) is made by ImportFromToml with engine = "LIS_OMP"
! (VectorFieldLis_test.toml). The field v is initiated with Initiate by
! passing the name, the engine, spaceCompo and the same FEDOFs, and w is
! initiated as a copy of v with Initiate(obj2). Each field must have its
! own LIS vector (lis_ptr is not zero) of local size Size().

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT64
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE Test_Method, ONLY: OK
USE VectorFieldLis_Class, ONLY: VectorFieldLis_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "VectorFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorFieldLis_) :: u, v, w
REAL(DFP) :: raw(12)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL v%Initiate(NAME="v", engine="LIS_OMP", spaceCompo=[2], &
                fedof=fedof, geofedof=geofedof)

! Test 1: the fields are initiated, with spaceCompo values per node
CALL OK(u%IsInitiated(), "u: IsInitiated")
CALL OK(v%IsInitiated(), "v: IsInitiated")
CALL OK(u%spaceCompo .EQ. 2, "u: spaceCompo")
CALL OK(u%SIZE() .EQ. 12, "u: Size = 12")
CALL OK(v%SIZE() .EQ. 2 * fedof%GetTotalDOF(), "v: Size = 2 * total DOF")
CALL OK(v%GetTotalPhysicalVars() .EQ. 1, "v: one physical variable")

! Test 2: each field has its own LIS vector
CALL OK(u%lis_ptr .NE. 0_INT64, "u: LIS vector is created")
CALL OK(v%lis_ptr .NE. 0_INT64, "v: LIS vector is created")
CALL OK(u%lis_ptr .NE. v%lis_ptr, "u and v: different LIS vectors")

! Test 3: Initiate(obj2) makes a new LIS vector with the values of v
BLOCK
  CALL v%Set(VALUE=3.0_DFP)
  CALL w%Initiate(obj2=v)
  CALL OK(w%spaceCompo .EQ. 2, "Initiate(obj2): spaceCompo")
  CALL OK(w%SIZE() .EQ. 12, "Initiate(obj2): Size")
  CALL OK(w%lis_ptr .NE. 0_INT64 .AND. w%lis_ptr .NE. v%lis_ptr, &
          "Initiate(obj2): own LIS vector")
  CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, &
                     tsize=tsize)
  CALL OK(ALL(ABS(raw - 3.0_DFP) .LT. tol), "Initiate(obj2): values")
END BLOCK

CALL w%DEALLOCATE()
CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
