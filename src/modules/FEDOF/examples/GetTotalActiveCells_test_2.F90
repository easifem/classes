!> author: Vikas Sharma, Ph. D.
! date: 2026-11-07
! summary: Testing FEDOF numbering on a mesh with an inactive cell
!
! Local cell 1 of the mesh is deactivated before the FEDOF is initiated.
! An inactive cell has an empty cell block. In a broken space the DOFs are
! the blocks of the active cells. In H1 all mesh vertices keep their DOFs,
! and the entity blocks have no holes: every non-vertex DOF belongs to an
! active cell.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE ElemData_Class, ONLY: Elemdata_
USE ElemData_Class, ONLY: Elemdata_Set
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom
TYPE(HDF5File_) :: meshfile
CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
TYPE(Elemdata_), POINTER :: elemptr => NULL()
INTEGER(I4B) :: tcells, tvertices

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')

meshptr => dom%GetMeshPointer()
tcells = meshptr%GetTotalElements()
tvertices = meshptr%GetTotalVertexNodes()

elemptr => meshptr%GetElemDataPointer(globalElement=1, islocal=math%yes)
CALL Elemdata_Set(obj=elemptr, isActive=math%no)
elemptr => NULL()

! Test 1: broken space
BLOCK
  INTEGER(I4B), PARAMETER :: order = 2, nloc = 6
  INTEGER(I4B) :: con(20), tsize

  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
                    isBrokenSpace=math%yes, order=order, dom=dom)

  CALL OK(obj%GetTotalActiveCells() .EQ. tcells - 1, &
          "broken: GetTotalActiveCells")
  CALL OK(obj%GetTotalCells() .EQ. tcells, "broken: GetTotalCells")
  CALL OK(obj%GetTotalDOF() .EQ. (tcells - 1) * nloc, &
          "broken: GetTotalDOF counts active cells")
  CALL OK(obj%GetSumElemTotalDOF() .EQ. (tcells - 1) * nloc, &
          "broken: GetSumElemTotalDOF counts active cells")
  CALL OK(obj%GetMaxTotalConnectivity() .EQ. nloc, &
          "broken: GetMaxTotalConnectivity")
  CALL OK(obj%GetTotalCellDOF(globalCell=1, islocal=math%yes) .EQ. 0, &
          "broken: inactive cell block is empty")

  CALL obj%GetConnectivity_(ans=con, tsize=tsize, opt="All", &
                            globalElement=2, islocal=math%yes)
  CALL OK(tsize .EQ. nloc, "broken: size of the next block")
  IF (tsize .EQ. nloc) CALL OK(con(1) .EQ. 1, &
                               "broken: next block starts at one")
END BLOCK

! Test 2: H1
BLOCK
  INTEGER(I4B), PARAMETER :: order = 3, nloc = 10
  INTEGER(I4B) :: con(20), tsize, iel, tdof
  INTEGER(I4B), ALLOCATABLE :: counts(:)
  LOGICAL(LGT) :: isok

  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
                    order=order, dom=dom)

  tdof = obj%GetTotalDOF()
  CALL OK(obj%GetTotalActiveCells() .EQ. tcells - 1, &
          "H1: GetTotalActiveCells")
  CALL OK(obj%GetTotalVertexDOF() .EQ. tvertices, &
          "H1: all vertices keep their DOFs")
  CALL OK(obj%GetMaxTotalConnectivity() .EQ. nloc, &
          "H1: GetMaxTotalConnectivity")
  CALL OK(obj%GetTotalCellDOF(globalCell=1, islocal=math%yes) .EQ. 0, &
          "H1: inactive cell has no interior DOF")
  CALL OK(obj%GetTotalCellDOF(globalCell=2, islocal=math%yes) .EQ. 1, &
          "H1: active cell has one interior DOF")

  ALLOCATE (counts(tdof))
  counts = 0
  isok = math%yes
  DO iel = 2, tcells
    CALL obj%GetConnectivity_(ans=con, tsize=tsize, opt="All", &
                              globalElement=iel, islocal=math%yes)
    isok = isok .AND. (tsize .EQ. nloc) .AND. ALL(con(1:tsize) .LE. tdof)
    IF (isok) counts(con(1:tsize)) = counts(con(1:tsize)) + 1
  END DO
  CALL OK(isok, "H1: active cells have the complete basis")
  CALL OK(ALL(counts(tvertices + 1:tdof) .GE. 1), &
          "H1: no holes in the entity blocks")
  DEALLOCATE (counts)
END BLOCK

meshptr => NULL()
CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
