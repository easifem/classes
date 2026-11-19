!> author: Vikas Sharma, Ph. D.
! date: 2026-11-19
! summary: Write a block node field in a VTK file, step by step
!
! This example writes the mesh and the nodal values of a block node
! field in a VTK file. It uses only VTKFile_, the mesh and Get of the
! field. BlockNodeField_ has no VTK method of its own.
!
! The steps are:
!
! 1. Open the VTK file with InitiateVTKFile.
! 2. Write the mesh with ExportToVTK of the mesh. Keep the Piece tag
!    open (closeTag = .FALSE.).
! 3. Open the point data section.
! 4. For each physical variable and each time component a, get each
!    space component at the local nodes 1, ..., tNodes. This is the
!    order of the points in the file. Write them as one data array:
!    - one space component: a scalar array (one component),
!    - more space components: a vector array (x, y, z). A missing
!      component (z for a 2D variable) is written as zero.
!    The array is named <fieldname>_<physicalname>. A variable with more
!    than one time component gets one array for each time component,
!    named <fieldname>_<physicalname>_t<a>.
! 5. Close the point data section, the Piece tag and the file.
!
! u has V (two space components, one time component) and T (one space
! component, two time components). The value of variable ivar at node i,
! space component c and time component a is
! 1000 ivar + 100 a + 10 i + c. The data arrays are u_V, u_T_t1 and
! u_T_t2. The file is written in ASCII format, and the values are read
! back from it.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE BlockNodeField_Class, ONLY: BlockNodeField_
USE Display_Method, ONLY: ToString
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DOF_FMT
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
USE VTKFile_Class, ONLY: VTKFile_
USE VTKFile_Class, ONLY: VTK_ASCII
USE VTKFile_Class, ONLY: VTK_UNSTRUCTUREDGRID
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "BlockNodeField_test.toml"
CHARACTER(*), PARAMETER :: vtkFileName = "ExportToVTK_test_1.vtu"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
INTEGER(I4B), PARAMETER :: tvar = 2
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(FEDOFPointer_) :: fedofs(tvar), geofedofs(tvar)
TYPE(BlockNodeField_) :: u
TYPE(VTKFile_) :: vtk
CLASS(AbstractMesh_), POINTER :: meshptr
CHARACTER(1) :: dofnames(tvar)
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
REAL(DFP), ALLOCATABLE :: xyz(:, :), want(:, :), found(:)
INTEGER(I4B) :: ii, ic, ia, ivar, tnodes, tsize, nc
INTEGER(I4B) :: spaceCompo(tvar), timeCompo(tvar)
LOGICAL(LGT) :: isok

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL fedof%ImportFromToml(tomlName="space", filename=tomlFileName, &
                          dom=dom)
CALL geofedof%ImportFromToml(tomlName="geofedof", &
                             filename=tomlFileName, dom=dom)
fedofs(1)%ptr => fedof
fedofs(2)%ptr => fedof
geofedofs(1)%ptr => geofedof
geofedofs(2)%ptr => geofedof

CALL u%Initiate( &
  name="u", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
  spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
  timeCompo=[1, 2], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
  tPhysicalVarNames=tvar, physicalVarNames=["V", "T"], &
  isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
  tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=24, &
  fedof=fedofs, geofedof=geofedofs)

meshptr => fedof%GetMeshPointer()
tnodes = meshptr%GetTotalNodes()
nptrs = [(ii, ii=1, tnodes)]
spaceCompo = u%GetSpaceCompo(tvar)
timeCompo = u%GetTimeCompo(tvar)
CALL u%GetPhysicalNames(dofnames)

DO ivar = 1, tvar
  DO ia = 1, timeCompo(ivar)
    DO ic = 1, spaceCompo(ivar)
      CALL u%Set(globalNode=nptrs, islocal=math%yes, &
                 VALUE=Want0(ivar, ia, ic), ivar=ivar, &
                 spaceCompo=ic, timeCompo=ia)
    END DO
  END DO
END DO

ALLOCATE (xyz(tnodes, 3), want(3, tnodes), found(3 * tnodes))

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

  ! step 4: one data array for each physical variable and time component
  DO ivar = 1, tvar
    DO ia = 1, timeCompo(ivar)
      xyz = 0.0_DFP
      DO ic = 1, spaceCompo(ivar)
        CALL u%Get(VALUE=xyz(:, ic), tsize=tsize, globalNode=nptrs, &
                   islocal=math%yes, ivar=ivar, spaceCompo=ic, &
                   timeCompo=ia)
      END DO

      IF (spaceCompo(ivar) .EQ. 1) THEN
        CALL vtk%WriteDataArray(name=String(ArrayName(ivar, ia)), &
                                x=xyz(:, 1), numberOfComponents=1)
      ELSE
        CALL vtk%WriteDataArray(name=String(ArrayName(ivar, ia)), &
                                x=xyz(:, 1), y=xyz(:, 2), z=xyz(:, 3))
      END IF
    END DO
  END DO

  ! step 5: close the point data, the Piece tag and the file
  CALL vtk%WriteDataArray(location=String("node"), action=String("close"))
  CALL vtk%WritePiece()
  CALL vtk%CLOSE()
  CALL vtk%DEALLOCATE()

  INQUIRE (FILE=vtkFileName, EXIST=isok)
  CALL OK(isok, "ExportToVTK: file written")
END BLOCK

! Test 2: the values in the file; a vector array has x y z for each
! point
BLOCK
  DO ivar = 1, tvar
    DO ia = 1, timeCompo(ivar)
      nc = 3
      IF (spaceCompo(ivar) .EQ. 1) nc = 1
      want = 0.0_DFP
      DO ic = 1, spaceCompo(ivar)
        want(ic, :) = Want0(ivar, ia, ic)
      END DO

      CALL ReadDataArray(vtkFileName, &
                         'Name="'//ArrayName(ivar, ia)//'"', &
                         found(1:nc * tnodes), isok)
      CALL OK(isok, "ExportToVTK: data array "//ArrayName(ivar, ia)// &
              " found")
      CALL OK(ALL(ABS(found(1:nc * tnodes) - &
                      RESHAPE(want(1:nc, :), [nc * tnodes])) .LT. tol), &
              "ExportToVTK: values of "//ArrayName(ivar, ia))
    END DO
  END DO
END BLOCK

NULLIFY (meshptr)
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

! Values of variable jvar, time component ja and space component jc at
! the nodes: 1000 jvar + 100 ja + 10 i + jc
FUNCTION Want0(jvar, ja, jc) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: jvar, ja, jc
  REAL(DFP) :: ans(tnodes)

  INTEGER(I4B) :: jj

  ans = [(1000.0_DFP * jvar + 100.0_DFP * ja + 10.0_DFP * jj + jc, &
          jj=1, tnodes)]
END FUNCTION Want0

! Name of the data array of variable jvar and time component ja
FUNCTION ArrayName(jvar, ja) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: jvar, ja
  CHARACTER(:), ALLOCATABLE :: ans

  ans = u%GetName()//"_"//dofnames(jvar)
  IF (timeCompo(jvar) .GT. 1) ans = ans//"_t"//ToString(ja)
END FUNCTION ArrayName

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
