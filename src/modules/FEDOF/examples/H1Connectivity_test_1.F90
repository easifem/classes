!> author: Vikas Sharma, Ph. D.
! date: 2026-10-30
! summary: Check H1 connectivity options and shared DOFs
!
! Uses two H1 quadrangles with sparse node and element identifiers.
! The adjacent cells share their complete conforming trace.

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: TypeInterpolationOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE MSHFile_Class, ONLY: MSHFile_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FEDOF_) :: obj
TYPE(FEDomain_), TARGET :: dom
TYPE(HDF5File_) :: meshfile
TYPE(MSHFile_) :: SOURCE
CLASS(AbstractMesh_), POINTER :: mesh => NULL()
CHARACTER(12), PARAMETER :: families(2) = &
                            [CHARACTER(12) :: "Lagrange", "Hierarchical"]
INTEGER(I4B), PARAMETER :: elements(2) = [101_I4B, 205_I4B]
INTEGER(I4B) :: order, family, iel
INTEGER :: unitno
CHARACTER(6), PARAMETER :: options(5) = &
                           [CHARACTER(6) :: "Vertex", "Edge", "Face", &
                           "Cell", "All"]
INTEGER(I4B) :: shortCon(64), longCon(64), nshort, nlong, ii, jj, SHARED
INTEGER(I4B), ALLOCATABLE :: first(:), second(:), allocatedCon(:)

CALL e%SetQuietMode(EXCEPTION_INFORMATION, .TRUE.)
CALL SOURCE%Initiate(filename="./fixtures/h1_two_quads.msh.txt", &
                     STATUS="OLD", ACTION="READ")
CALL SOURCE%OPEN()
CALL SOURCE%READ()
CALL meshfile%Initiate(filename="H1Connectivity_test_1.h5", mode="NEW")
CALL meshfile%SetOverwriteStat(.TRUE.)
CALL meshfile%OPEN()
CALL SOURCE%Export(hdf5=meshfile, group="")
CALL SOURCE%DEALLOCATE()
CALL dom%Initiate(meshfile, "")
mesh => dom%GetMeshPointer()
CALL OK(mesh%GetTotalElements() .EQ. 2_I4B, "Fixture has two cells")
CALL OK(mesh%GetTotalVertexNodes() .EQ. 6_I4B, "Fixture has six vertices")
CALL OK(mesh%GetMaxNodeNumber() .GT. 6_I4B, "Mesh vertex IDs have gaps")

DO family = 1, SIZE(families)
  DO order = 1, 3
    CALL obj%Initiate(order=order, dom=dom, baseContinuity="H1", &
                      baseInterpolation=TRIM(families(family)), &
                      ipType=TypeInterpolationOpt%Equidistance)
    CALL OK(obj%GetTotalDOF() .EQ. &
            6_I4B + 7_I4B * (order - 1_I4B) + &
            2_I4B * (order - 1_I4B)**2, "Uniform H1 global DOF count")
    DO iel = 1, SIZE(elements)
      DO ii = 1, SIZE(options)
        CALL obj%GetConnectivity_(ans=shortCon, tsize=nshort, &
                                  opt=options(ii) (1:1), &
                                  globalElement=elements(iel))
        CALL obj%GetConnectivity_(ans=longCon, tsize=nlong, &
                                  opt=TRIM(options(ii)), &
                                  globalElement=elements(iel))
        CALL OK(nlong .EQ. nshort, "Long and short option sizes agree")
        IF (nlong .EQ. nshort) CALL OK( &
          ALL(longCon(1:nlong) .EQ. shortCon(1:nshort)), &
          "Long and short option contents agree")
        allocatedCon = obj%GetConnectivity(opt=TRIM(options(ii)), &
                                           globalElement=elements(iel))
        CALL OK(SIZE(allocatedCon) .EQ. nshort, "Allocated output size")
        IF (SIZE(allocatedCon) .EQ. nshort) CALL OK( &
          ALL(allocatedCon .EQ. shortCon(1:nshort)), "Allocated output IDs")
      END DO
      CALL OK(obj%GetTotalDOF(globalElement=elements(iel)) .EQ. &
              (order + 1_I4B)**2, "Uniform quadrangle local basis size")
    END DO
    first = obj%GetConnectivity(opt="All", globalElement=elements(1))
    second = obj%GetConnectivity(opt="All", globalElement=elements(2))
    SHARED = 0
    DO jj = 1, SIZE(first)
      IF (ANY(second .EQ. first(jj))) SHARED = SHARED + 1
    END DO
    CALL OK(SHARED .EQ. order + 1_I4B, "Shared H1 trace DOFs counted once")
  END DO
END DO

CALL obj%DEALLOCATE()
mesh => NULL()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
OPEN (NEWUNIT=unitno, FILE="H1Connectivity_test_1.h5", &
      STATUS="old", ACTION="read")
CLOSE (unitno, STATUS="delete")
END PROGRAM main
