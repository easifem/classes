!> author: Vikas Sharma, Ph. D.
! date:
! summary:  Initiate fedof with H1 and Heirarchical bases, order is a vector.

PROGRAM main
USE FEDOF_Class
USE FEDomain_Class
USE AbstractMesh_Class
USE HDF5File_Class
USE Display_Method
USE GlobalData
USE Test_Method
USE ExceptionHandler_Class, ONLY: e, EXCEPTION_INFORMATION
USE ReallocateUtility

IMPLICIT NONE

TYPE(FEDOF_) :: fedof
TYPE(FEDomain_) :: dom
CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
CHARACTER(*), PARAMETER :: filename = &
                           "../../FEMesh/examples/meshdata/small_tri3_mesh.h5"
TYPE(HDF5File_) :: meshfile
LOGICAL(LGT) :: isok
INTEGER(I4B) :: found, want, order, ii, iel

CALL e%setQuietMode(EXCEPTION_INFORMATION, .TRUE.)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')

meshptr => dom%GetMeshPointer()

CALL fedof%ImportFromToml(tomlName="test1", &
                  filename="./toml/_ImportFromToml_test_2.toml", mesh=meshptr)

! CALL fedof%DisplayCellOrder("Debug Cell order", full=.TRUE.)

found = fedof%GetTotalDOF()
want = 39
isok = found == want
CALL IS(found, want, "Total DOF ")

! CALL Display(fedof%GetConnectivity(globalElement=13, islocal=.FALSE., opt="A"), &
!              "connectivity of global element 13", full=.TRUE.)

CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()

END PROGRAM main
