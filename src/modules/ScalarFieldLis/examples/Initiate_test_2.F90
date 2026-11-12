!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Initiate a LIS scalar field by copying another one
!
! Initiate(obj2, copyFull) copies the data of u (name, sizes, DOF data,
! FEDOF pointers and values) into w and creates a new LIS vector for
! w. w must hold the values of u, and changing w must not change u.
! The values are copied from the LIS vector of u; for a native
! ScalarField_ n they are copied from its Fortran vector.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE ScalarField_Class, ONLY: ScalarField_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ScalarFieldLis_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarFieldLis_) :: u, w, w2
TYPE(ScalarField_) :: n
REAL(DFP) :: found(6), want(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")

want = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
CALL u%Set(VALUE=want)
CALL w%Initiate(obj2=u, copyFull=math%yes)

! Test 1: structure is copied
CALL OK(w%IsInitiated(), "w: IsInitiated")
CALL OK(w%SIZE() .EQ. u%SIZE(), "w: Size = u%Size")

! Test 2: values are copied
BLOCK
  CALL w%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "w: values of u")
END BLOCK

! Test 3: w has its own LIS vector
BLOCK
  CALL w%Set(VALUE=0.0_DFP)
  CALL u%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "u unchanged")
END BLOCK

! Test 4: copy of a native scalar field
BLOCK
  CALL n%Initiate(NAME="n", engine="NATIVE_SERIAL", fedof=fedof, &
                  geofedof=geofedof)
  CALL n%Set(VALUE=-want)
  CALL w2%Initiate(obj2=n, copyFull=math%yes)
  CALL w2%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found + want) .LT. tol), "w2: values of n")
END BLOCK

CALL w2%DEALLOCATE()
CALL n%DEALLOCATE()
CALL w%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
