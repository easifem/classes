!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Display a LIS vector field
!
! Display gathers the values of the LIS vector into the Fortran vector
! and prints the data of the node field. If the LIS vector is not
! created, it prints only msg and "obj%lis_ptr not available.". The
! output is written to a file and read back to check which case ran.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LisOmpEngine_Class, ONLY: LisOmpEngine_
USE Test_Method, ONLY: OK
USE VectorFieldLis_Class, ONLY: VectorFieldLis_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "VectorFieldLis_test.toml"
CHARACTER(*), PARAMETER :: outFileName = "Display_test_1.out"
TYPE(LisOmpEngine_) :: lis
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorFieldLis_) :: u
INTEGER(I4B) :: unitno, ios, nlines
CHARACTER(256) :: line, iomsg
LOGICAL :: isMissing

CALL e%SetQuietMode(math%yes)
CALL lis%Initiate()
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL u%Set(VALUE=1.0_DFP)

OPEN (NEWUNIT=unitno, FILE=outFileName, STATUS="REPLACE", &
      ACTION="READWRITE")
CALL u%Display("u: ", unitno=unitno)
REWIND (unitno)

nlines = 0
isMissing = .FALSE.
readLines: DO
  READ (unitno, "(A)", IOSTAT=ios, IOMSG=iomsg) line
  IF (ios .NE. 0) EXIT readLines
  nlines = nlines + 1
  IF (INDEX(line, "lis_ptr not available") .GT. 0) isMissing = .TRUE.
END DO readLines
CLOSE (unitno, STATUS="DELETE")

! Test 1: the LIS vector is found and the field data is printed
CALL OK(.NOT. isMissing, "Display: LIS vector is available")
CALL OK(nlines .GT. 2, "Display: field data is printed")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL lis%DEALLOCATE()
END PROGRAM main
