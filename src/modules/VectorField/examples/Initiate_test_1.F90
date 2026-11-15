!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Initiate a vector field
!
! The FEDOFs (order 1 Lagrange on square_3x2.h5, six DOFs) are made by
! importing the field u from VectorField_test.toml. The field v is
! initiated with Initiate by passing the name, the engine, the number
! of space components and the same FEDOFs. A vector field has one
! physical variable with spaceCompo components, so v has 6 x 2 values.
! The field w is then initiated as a copy of v with Initiate(obj2).

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE FieldOpt_Class, ONLY: TypeFieldOpt
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE Test_Method, ONLY: OK
  USE VectorField_Class, ONLY: VectorField_
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(VectorField_) :: u, v, w
  CHARACTER(1) :: names(1)
  REAL(DFP) :: raw(12)
  INTEGER(I4B) :: tsize

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")
  CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", spaceCompo=[2], &
                  fedof=fedof, geofedof=geofedof)

  CALL OK(v%IsInitiated(), "IsInitiated")
  CALL OK(v%spaceCompo .EQ. 2, "spaceCompo")
  CALL OK(v%Size() .EQ. 2 * fedof%GetTotalDOF(), "Size = 2 * total DOF")
  CALL OK(v%Size() .EQ. 12, "Size = 12")
  CALL OK(v%GetTotalPhysicalVars() .EQ. 1, "GetTotalPhysicalVars")
  CALL v%GetPhysicalNames(names)
  CALL OK(names(1) .EQ. "v", "GetPhysicalNames")
  CALL OK(ALL(v%GetSpaceCompo(1) .EQ. [2]), "GetSpaceCompo")
  CALL OK(ALL(v%GetTimeCompo(1) .EQ. [1]), "GetTimeCompo")
  CALL OK(v%GetStorageFMT() .EQ. TypeFieldOpt%storageFormatDOF, &
          "GetStorageFMT")

  ! copy
  CALL v%Set(VALUE=3.0_DFP)
  CALL w%Initiate(obj2=v)
  CALL OK(w%spaceCompo .EQ. 2, "Initiate(obj2): spaceCompo")
  CALL OK(w%Size() .EQ. 12, "Initiate(obj2): Size")
  CALL w%GetMultiple(istart=1, iend=12, stride=1, VALUE=raw, &
                     tsize=tsize)
  CALL OK(ALL(ABS(raw - 3.0_DFP) .LT. tol), "Initiate(obj2): values")

  CALL w%DEALLOCATE()
  CALL OK(.NOT. w%IsInitiated(), "DEALLOCATE: not initiated")
  CALL OK(w%spaceCompo .EQ. 0, "DEALLOCATE: spaceCompo = 0")

  CALL v%DEALLOCATE()
  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
