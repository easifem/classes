!> author: Vikas Sharma, Ph. D.
! date: 2026-11-18
! summary: Write a scalar field in a VTK file
!
! obj%WriteData_vtk(vtk) writes the mesh of the field and the vertex
! values of the field as point data in an initiated VTKFile_, and closes
! the file. The name of the data array is <fieldname>_<dofname>.
!
! p1 and p2 have the same physical variable name (p), so the field name
! is needed to tell their arrays apart. p1 has the value 10 i + 1 at
! node i, and p2 = 2 * p1. Each field is written in its own file in
! ASCII format, and the values of the data arrays p1_p and p2_p are read
! back from the files.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
USE VTKFile_Class, ONLY: VTKFile_
USE VTKFile_Class, ONLY: VTK_ASCII
USE VTKFile_Class, ONLY: VTK_UNSTRUCTUREDGRID
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
CHARACTER(*), PARAMETER :: vtkFileName1 = "WriteData_test_1_p1.vtu"
CHARACTER(*), PARAMETER :: vtkFileName2 = "WriteData_test_1_p2.vtu"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(ScalarField_) :: p1, p2
TYPE(VTKFile_) :: vtk
REAL(DFP) :: want(6), found(6)
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL p1%Initiate(NAME="p1", engine="NATIVE_SERIAL", fedof=fedof, &
                 geofedof=geofedof)
CALL p2%Initiate(NAME="p2", engine="NATIVE_SERIAL", fedof=fedof, &
                 geofedof=geofedof)

! want(i) is the value at node i, as written in the file
DO ii = 1, 6
  want(ii) = 10.0_DFP * ii + 1.0_DFP
  CALL p1%Set(globalNode=ii, islocal=math%yes, VALUE=want(ii))
  CALL p2%Set(globalNode=ii, islocal=math%yes, VALUE=2.0_DFP * want(ii))
END DO

! Test 1: write the fields, one file for each field
BLOCK
  CALL vtk%InitiateVTKFile(filename=vtkFileName1, mode="NEW", &
                           dataFormat=VTK_ASCII, &
                           dataStructureType=VTK_UNSTRUCTUREDGRID)
  CALL p1%WriteData_vtk(vtk=vtk)
  CALL vtk%DEALLOCATE()
  INQUIRE (FILE=vtkFileName1, EXIST=isok)
  CALL OK(isok, "WriteData: file of p1 written")

  CALL vtk%InitiateVTKFile(filename=vtkFileName2, mode="NEW", &
                           dataFormat=VTK_ASCII, &
                           dataStructureType=VTK_UNSTRUCTUREDGRID)
  CALL p2%WriteData_vtk(vtk=vtk)
  CALL vtk%DEALLOCATE()
  INQUIRE (FILE=vtkFileName2, EXIST=isok)
  CALL OK(isok, "WriteData: file of p2 written")
END BLOCK

! Test 2: the array names and the values in the files
BLOCK
  CALL ReadDataArray(vtkFileName1, 'Name="p1_p"', found, isok)
  CALL OK(isok, "WriteData: data array p1_p found")
  CALL OK(ALL(ABS(found - want) .LT. tol), "WriteData: values of p1")

  CALL ReadDataArray(vtkFileName2, 'Name="p2_p"', found, isok)
  CALL OK(isok, "WriteData: data array p2_p found")
  CALL OK(ALL(ABS(found - 2.0_DFP * want) .LT. tol), &
          "WriteData: values of p2")
END BLOCK

CALL p2%DEALLOCATE()
CALL p1%DEALLOCATE()
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
