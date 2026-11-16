!> author: Vikas Sharma, Ph. D.
! date: 2026-11-18
! summary: Write vector fields in a VTK file
!
! NodeFieldsWriteData(obj, vtk) writes the mesh of the fields and the
! values of all fields as point data in an open VTKFile_. The values are
! read at the local node numbers of the mesh, in the order of the
! points. vtk is not closed on return.
!
! u has the value [10 i + 1, 10 i + 2] at node i, and w = 2 * u. The file
! is written in ASCII format, and the values of the data arrays u_* and
! w_* are read back from it.

PROGRAM main
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldPointer_
USE AbstractNodeField_Class, ONLY: NodeFieldsWriteData
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
USE VectorField_Class, ONLY: VectorField_
USE VTKFile_Class, ONLY: VTKFile_
USE VTKFile_Class, ONLY: VTK_ASCII
USE VTKFile_Class, ONLY: VTK_UNSTRUCTUREDGRID
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
CHARACTER(*), PARAMETER :: vtkFileName = "WriteData_test_1.vtu"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorField_), TARGET :: u, w
TYPE(AbstractNodeFieldPointer_) :: fields(2)
TYPE(VTKFile_) :: vtk
REAL(DFP) :: want(12), found(12)
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL w%Initiate(NAME="w", engine="NATIVE_SERIAL", spaceCompo=2, &
                fedof=fedof, geofedof=geofedof)

! want(2 i - 1 : 2 i) is the value at node i, as written in the file
DO ii = 1, 6
  want(2 * ii - 1:2 * ii) = [10.0_DFP * ii + 1.0_DFP, &
                             10.0_DFP * ii + 2.0_DFP]
  CALL u%Set(globalNode=ii, islocal=math%yes, &
             VALUE=want(2 * ii - 1:2 * ii))
END DO
CALL w%Set(VALUE=0.0_DFP)
CALL w%AXPY(x=u, scale=2.0_DFP)

fields(1)%ptr => u
fields(2)%ptr => w

! Test 1: write the fields
BLOCK
  CALL vtk%InitiateVTKFile(filename=vtkFileName, mode="NEW", &
                           dataFormat=VTK_ASCII, &
                           dataStructureType=VTK_UNSTRUCTUREDGRID)
  CALL NodeFieldsWriteData(obj=fields, vtk=vtk)
  CALL vtk%CLOSE()
  CALL vtk%DEALLOCATE()
  INQUIRE (FILE=vtkFileName, EXIST=isok)
  CALL OK(isok, "WriteData: file written")
END BLOCK

! Test 2: the values in the file
BLOCK
  CALL ReadDataArray(vtkFileName, 'Name="u_', found, isok)
  CALL OK(isok, "WriteData: data array of u found")
  CALL OK(ALL(ABS(found - want) .LT. tol), "WriteData: values of u")

  CALL ReadDataArray(vtkFileName, 'Name="w_', found, isok)
  CALL OK(isok, "WriteData: data array of w found")
  CALL OK(ALL(ABS(found - 2.0_DFP * want) .LT. tol), &
          "WriteData: values of w")
END BLOCK

CALL w%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

! Read the values of the first DataArray whose tag contains key
SUBROUTINE ReadDataArray(filename, key, val, isfound)
  CHARACTER(*), INTENT(IN) :: filename, key
  REAL(DFP), INTENT(OUT) :: val(:)
  LOGICAL(LGT), INTENT(OUT) :: isfound

  CHARACTER(1024) :: line
  CHARACTER(256) :: msg
  CHARACTER(:), ALLOCATABLE :: buf
  INTEGER(I4B) :: unitno, ios, i1, i2
  LOGICAL(LGT) :: isin

  isfound = .FALSE.
  isin = .FALSE.
  val = 0.0_DFP
  buf = ""

  OPEN (NEWUNIT=unitno, FILE=filename, STATUS="OLD", ACTION="READ")
  lines: DO
    READ (unitno, "(A)", IOSTAT=ios, IOMSG=msg) line
    IF (ios .NE. 0) EXIT lines

    IF (.NOT. isin) THEN
      i1 = INDEX(line, key)
      IF (i1 .EQ. 0) CYCLE lines
      isin = .TRUE.
      i2 = INDEX(line(i1:), ">")
      line = line(i1 + i2:)
    END IF

    i2 = INDEX(line, "</DataArray>")
    IF (i2 .GT. 0) THEN
      buf = buf//" "//line(1:i2 - 1)
      isfound = .TRUE.
      EXIT lines
    END IF
    buf = buf//" "//TRIM(line)
  END DO lines
  CLOSE (unitno)

  IF (isfound) READ (buf, *, IOSTAT=ios, IOMSG=msg) val
  IF (isfound) isfound = ios .EQ. 0
END SUBROUTINE ReadDataArray

END PROGRAM main
