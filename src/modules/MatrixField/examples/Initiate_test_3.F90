!> author: Vikas Sharma, Ph. D.
! date: 2026-11-09
! summary: Initiate a matrix field from arguments
!
! In this test we initiate MatrixField_ from arguments
!
! (   obj, name, engine, fieldType, storageFMT, comm, local_n, global_n, &
!     spaceCompo, isSpaceCompo, isSpaceCompoScalar, timeCompo, isTimeCompo, &
!     isTimeCompoScalar, tPhysicalVarNames, physicalVarNames, &
!     isPhysicalVarNames, isPhysicalVarNamesScalar, tNodes, isTNodes, &
!     isTNodesScalar, tSize, fedof, geofedof, timefedof)
!
PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE EngineOpt_Class, ONLY: TypeEngineOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE MatrixField_Class, ONLY: MatrixField_
USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "MatrixField_test.toml"
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(MatrixField_) :: obj

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")

! (   obj, name, engine, fieldType, storageFMT, comm, local_n, global_n, &
!     spaceCompo, isSpaceCompo, isSpaceCompoScalar, timeCompo, isTimeCompo, &
!     isTimeCompoScalar, tPhysicalVarNames, physicalVarNames, &
!     isPhysicalVarNames, isPhysicalVarNamesScalar, tNodes, isTNodes, &
!     isTNodesScalar, tSize, fedof, geofedof, timefedof)
!
! It is necessary to pass local_n, otherwise size will not be set.
! But size can be determined from the fedof and space and time compo
CALL obj%Initiate(name="mat", engine=TypeEngineOpt%native_serial, &
                  fieldType=TypeFieldOpt%normal, fedof=fedof, &
                  geofedof=geofedof, spaceCompo=[1], &
                  isSpaceCompo=math%yes, &
                  isSpaceCompoScalar=math%yes, &
                  tNodes=[fedof%GetTotalDOF()])
CALL obj%Display("After calling Initiate")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
