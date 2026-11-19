!> author: Vikas Sharma, Ph. D.
! date: 2026-11-19
! summary: Write a scalar field in a VTK file, step by step
!
! This example writes the mesh and the nodal values of a scalar field
! in a VTK file. It uses only VTKFile_, the mesh and Get of the field.
! It does not call WriteData_vtk or ExportToVTK of the field.
!
! The steps are:
!
! 1. Open the VTK file with InitiateVTKFile.
! 2. Write the mesh with ExportToVTK of the mesh. Keep the Piece tag
!    open (closeTag = .FALSE.).
! 3. Open the point data section.
! 4. Get the values at the local nodes 1, ..., tNodes. This is the order
!    of the points in the file. Write them as one data array.
! 5. Close the point data section, the Piece tag and the file.
!
! The field u has the value 10 i + 1 at node i. The data array is named
! <fieldname>_<physicalname>, here u_u. The file is written in ASCII
! format, and the values are read back from it.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE ScalarField_Class, ONLY: ScalarField_
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
USE VTKFile_Class, ONLY: VTKFile_
USE VTKFile_Class, ONLY: VTK_ASCII
USE VTKFile_Class, ONLY: VTK_UNSTRUCTUREDGRID
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
CHARACTER(*), PARAMETER :: vtkFileName = "ExportToVTK_test_1.vtu"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(VTKFile_) :: vtk
CLASS(AbstractMesh_), POINTER :: meshptr
CHARACTER(1) :: dofnames(1)
CHARACTER(:), ALLOCATABLE :: arrayName
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
REAL(DFP), ALLOCATABLE :: nodeVal(:), want(:), found(:)
INTEGER(I4B) :: ii, tnodes, tsize
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")

meshptr => fedof%GetMeshPointer()
tnodes = meshptr%GetTotalNodes()
nptrs = [(ii, ii=1, tnodes)]

! want(i) is the value at node i
ALLOCATE (want(tnodes), nodeVal(tnodes), found(tnodes))
DO ii = 1, tnodes
  want(ii) = 10.0_DFP * ii + 1.0_DFP
  CALL u%Set(globalNode=ii, islocal=math%yes, VALUE=want(ii))
END DO

CALL u%GetPhysicalNames(dofnames)
arrayName = u%GetName()//"_"//dofnames(1)

! Test 1: write the mesh and the values
BLOCK
  ! step 1: open the file
  CALL vtk%InitiateVTKFile(filename=vtkFileName, mode="NEW", &
                           dataFormat=VTK_ASCII, &
                           dataStructureType=VTK_UNSTRUCTUREDGRID)

  ! step 2: points and cells of the mesh, the Piece tag stays open
  CALL meshptr%ExportToVTK(vtk=vtk, opentag=math%yes, content=math%yes, &
                           closetag=math%no)

  ! step 3: open the point data section
  CALL vtk%WriteDataArray(location=String("node"), action=String("open"))

  ! step 4: values at the nodes, one component
  CALL u%Get(VALUE=nodeVal, globalNode=nptrs, islocal=math%yes, &
             tsize=tsize)
  CALL vtk%WriteDataArray(name=String(arrayName), x=nodeVal(1:tsize), &
                          numberOfComponents=1)

  ! step 5: close the point data, the Piece tag and the file
  CALL vtk%WriteDataArray(location=String("node"), action=String("close"))
  CALL vtk%WritePiece()
  CALL vtk%CLOSE()
  CALL vtk%DEALLOCATE()

  INQUIRE (FILE=vtkFileName, EXIST=isok)
  CALL OK(isok, "ExportToVTK: file written")
END BLOCK

! Test 2: the values in the file
BLOCK
  CALL ReadDataArray(vtkFileName, 'Name="'//arrayName//'"', found, isok)
  CALL OK(isok, "ExportToVTK: data array "//arrayName//" found")
  CALL OK(ALL(ABS(found - want) .LT. tol), "ExportToVTK: values of u")
END BLOCK

NULLIFY (meshptr)
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
