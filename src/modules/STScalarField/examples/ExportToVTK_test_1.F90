!> author: Vikas Sharma, Ph. D.
! date: 2026-11-19
! summary: Write a space-time scalar field in a VTK file, step by step
!
! This example writes the mesh and the nodal values of a space-time
! scalar field in a VTK file. It uses only VTKFile_, the mesh and Get of
! the field. STScalarField_ has no VTK method of its own.
!
! The steps are:
!
! 1. Open the VTK file with InitiateVTKFile.
! 2. Write the mesh with ExportToVTK of the mesh. Keep the Piece tag
!    open (closeTag = .FALSE.).
! 3. Open the point data section.
! 4. For each time component a, get the values at the local nodes
!    1, ..., tNodes. This is the order of the points in the file. Write
!    them as one data array <fieldname>_<physicalname>_t<a>.
! 5. Close the point data section, the Piece tag and the file.
!
! The field u has two time components. Its value at node i and time
! component a is 10 i + a. The data arrays are u_u_t1 and u_u_t2. The
! file is written in ASCII format, and the values are read back from it.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: ToString
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE String_Class, ONLY: String
USE STScalarField_Class, ONLY: STScalarField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE VTKFile_Class, ONLY: VTKFile_
USE VTKFile_Class, ONLY: VTK_ASCII
USE VTKFile_Class, ONLY: VTK_UNSTRUCTUREDGRID
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
CHARACTER(*), PARAMETER :: vtkFileName = "ExportToVTK_test_1.vtu"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STScalarField_) :: u
TYPE(VTKFile_) :: vtk
CLASS(AbstractMesh_), POINTER :: meshptr
CHARACTER(1) :: dofnames(1)
CHARACTER(:), ALLOCATABLE :: baseName
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
REAL(DFP), ALLOCATABLE :: nodeVal(:), want(:, :), found(:)
INTEGER(I4B) :: ii, ia, tnodes, tsize, timeCompo(1)
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

meshptr => fedof%GetMeshPointer()
tnodes = meshptr%GetTotalNodes()
nptrs = [(ii, ii=1, tnodes)]
timeCompo = u%GetTimeCompo(1)

! want(i, a) is the value at node i and time component a
ALLOCATE (want(tnodes, timeCompo(1)), nodeVal(tnodes), found(tnodes))
DO ii = 1, tnodes
  DO ia = 1, timeCompo(1)
    want(ii, ia) = 10.0_DFP * ii + ia
  END DO
  CALL u%Set(globalNode=ii, islocal=math%yes, VALUE=want(ii, :))
END DO

CALL u%GetPhysicalNames(dofnames)
baseName = u%GetName()//"_"//dofnames(1)//"_t"

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

  ! step 4: one data array for each time component
  DO ia = 1, timeCompo(1)
    CALL u%Get(VALUE=nodeVal, globalNode=nptrs, tsize=tsize, &
               islocal=math%yes, timeCompo=ia)
    CALL vtk%WriteDataArray(name=String(baseName//ToString(ia)), &
                            x=nodeVal(1:tsize), numberOfComponents=1)
  END DO

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
  DO ia = 1, timeCompo(1)
    CALL ReadDataArray(vtkFileName, &
                       'Name="'//baseName//ToString(ia)//'"', found, isok)
    CALL OK(isok, "ExportToVTK: data array "//baseName//ToString(ia)// &
            " found")
    CALL OK(ALL(ABS(found - want(:, ia)) .LT. tol), &
            "ExportToVTK: values of u at time component "//ToString(ia))
  END DO
END BLOCK

NULLIFY (meshptr)
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
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
