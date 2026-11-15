!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Testing GetElemNum and GetTotalElemNum of MeshSelection_
!
! The mesh has two surface entities. Entity 1 has elements 18, 19 and
! entity 2 has elements 20, 21. Curve entity 1 has elements 7, 8, and
! curve entity 5 has element 15.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE MeshSelection_Class, ONLY: MeshSelection_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = &
                           "../../FEMesh/examples/meshdata/very_small_quad4_mesh_two_region.h5"
TYPE(MeshSelection_) :: obj
TYPE(FEDomain_) :: dom
TYPE(HDF5File_) :: meshfile
INTEGER(I4B) :: ans(32), tsize

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')

! Test 1: without dom only the element lists are used
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=2, meshID=[1])
  CALL OK(obj%GetTotalElemNum(dim=2) .EQ. 0, "no elemNum: total")
  CALL obj%GetElemNum(dim=2, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 0, "no elemNum: GetElemNum")
  CALL OK(obj%GetTotalElemNum() .EQ. 0, "no elemNum: total of all dim")
END BLOCK

! Test 2: selection by element number
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=2, elemNum=[20, 18])
  CALL obj%Add(dim=1, elemNum=[15])
  CALL obj%Set()

  CALL OK(obj%GetTotalElemNum(dim=2) .EQ. 2, "elemNum: total dim=2")
  CALL OK(obj%GetTotalElemNum(dim=3) .EQ. 0, "elemNum: total dim=3")
  CALL OK(obj%GetTotalElemNum() .EQ. 3, "elemNum: total of all dim")

  CALL obj%GetElemNum(dim=2, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 2, "elemNum: size dim=2")
  CALL OK(ALL(ans(1:tsize) .EQ. [18, 20]), "elemNum: values dim=2")

  CALL obj%GetElemNum(ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 3, "elemNum: size of all dim")
  CALL OK(ALL(ans(1:tsize) .EQ. [15, 18, 20]), "elemNum: all dim")
END BLOCK

! Test 3: selection by mesh id needs dom
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=2, meshID=[2])
  CALL obj%Add(dim=1, meshID=[1, 5])
  CALL obj%Set()

  CALL OK(obj%GetTotalElemNum(dim=2, dom=dom) .EQ. 2, "meshID: total dim=2")
  CALL obj%GetElemNum(dim=2, dom=dom, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 2, "meshID: size dim=2")
  CALL OK(ALL(ans(1:tsize) .EQ. [20, 21]), "meshID: values dim=2")

  CALL OK(obj%GetTotalElemNum(dim=1, dom=dom) .EQ. 3, "meshID: total dim=1")
  CALL obj%GetElemNum(dim=1, dom=dom, ans=ans, tsize=tsize)
  CALL OK(ALL(ans(1:tsize) .EQ. [7, 8, 15]), "meshID: values dim=1")

  CALL OK(obj%GetTotalElemNum(dom=dom) .EQ. 5, "meshID: total of all dim")
  CALL obj%GetElemNum(dom=dom, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 5, "meshID: size of all dim")
  CALL OK(ALL(ans(1:tsize) .EQ. [7, 8, 15, 20, 21]), "meshID: all dim")
END BLOCK

! Test 4: both selections are joined, element lists come first
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=2, elemNum=[18])
  CALL obj%Add(dim=2, meshID=[2])
  CALL obj%Set()

  CALL OK(obj%GetTotalElemNum(dim=2, dom=dom) .EQ. 3, "both: total")
  CALL obj%GetElemNum(dim=2, dom=dom, ans=ans, tsize=tsize)
  CALL OK(ALL(ans(1:tsize) .EQ. [18, 20, 21]), "both: values")
END BLOCK

! Test 5: an element in both selections is counted twice
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=2, elemNum=[20])
  CALL obj%Add(dim=2, meshID=[2])
  CALL obj%Set()

  CALL OK(obj%GetTotalElemNum(dim=2, dom=dom) .EQ. 3, "overlap: total")
  CALL obj%GetElemNum(dim=2, dom=dom, ans=ans, tsize=tsize)
  CALL OK(ALL(ans(1:tsize) .EQ. [20, 20, 21]), "overlap: values")
END BLOCK

! Test 6: a list of a dimension above nsd is skipped
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=3, meshID=[1])
  CALL obj%Add(dim=2, meshID=[2])
  CALL obj%Set()

  CALL OK(obj%GetTotalElemNum(dom=dom) .EQ. 2, "dim above nsd: total")
  CALL obj%GetElemNum(dom=dom, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 2, "dim above nsd: size")
  CALL OK(ALL(ans(1:tsize) .EQ. [20, 21]), "dim above nsd: values")
END BLOCK

CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
