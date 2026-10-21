PROGRAM main
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: TypeQuadratureOpt
USE BaseType, ONLY: math => TypeMathOpt
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE FEDomain_Class, ONLY: FEDomain_
USE STVectorField_Class, ONLY: STVectorField_
USE FEDOF_Class, ONLY: FEDOF_
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK, IS
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE TimeOpt_Class, ONLY: TimeOpt_
USE TimeOpt_Class, ONLY: TypeTimeOpt

IMPLICIT NONE

! Parameters are defined here
CHARACTER(*), PARAMETER :: engine = "NATIVE_SERIAL"
CHARACTER(*), PARAMETER :: &
  meshfilename = "../../FEMesh/examples/meshdata/small_tri3_mesh.h5"
CHARACTER(*), PARAMETER :: tomlfilename = "./toml/STVectorField.toml"
INTEGER(I4B), PARAMETER :: nsd = 2
INTEGER(I4B), PARAMETER :: order = 1

! Variables are defined here
TYPE(FEDomain_) :: dom
TYPE(FEDOF_) :: fedof, geofedof
TYPE(STVectorField_) :: obj
TYPE(TimeFEDOF_) :: timefedof
TYPE(HDF5File_) :: meshfile
TYPE(TimeOpt_) :: timeopt

CALL e%setQuietMode(EXCEPTION_INFORMATION, math%yes)

! Create FEDomain object
CALL meshfile%Initiate(filename=meshfilename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(hdf5=meshfile, group="")

! Create TimeOpt object
CALL timeopt%ImportFromToml(tomlName="timeOpt", &
                            filename=tomlfilename)

! Create STVectorField
! fedof, timefedof, and geofedof will be created
! while reading the STVectorField
CALL obj%ImportFromToml(tomlName="field1", &
                        filename=tomlfilename, &
                        fedof=fedof, &
                        geofedof=geofedof, &
                        timefedof=timefedof, &
                        dom=dom)

CALL obj%Display("VectorField obj:")
CALL timefedof%Display("TimeFEDOF obj:")

CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL meshfile%DEALLOCATE()

END PROGRAM main
