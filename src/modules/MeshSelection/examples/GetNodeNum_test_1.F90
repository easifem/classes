!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Testing GetNodeNum and GetTotalNodeNum of MeshSelection_
!
! Node numbers come from the two region quad4 mesh of FEMesh examples.
! Surface entity 2 has nodes 2, 3, 5, 6, 8, 10 and element 18 has nodes
! 1, 4, 7, 9. Curve entity 1 has nodes 1, 2, 7 and curve entity 5 has
! nodes 1, 4. Point entity 3 has node 3.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE MeshSelection_Class, ONLY: MeshSelection_
USE SortUtility, ONLY: Sort
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

! Test 1: no node list
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=2, meshID=[1])
  CALL obj%Set()
  CALL OK(obj%GetTotalNodeNum() .EQ. 0, "no nodeNum: total")
  CALL obj%GetNodeNum(ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 0, "no nodeNum: GetNodeNum")
END BLOCK

! Test 2: selection by node number
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(nodeNum=[10, 9])
  CALL obj%Set()
  CALL OK(obj%GetTotalNodeNum() .EQ. 2, "nodeNum: total")
  CALL obj%GetNodeNum(ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 2, "nodeNum: size")
  CALL OK(ALL(ans(1:tsize) .EQ. [9, 10]), "nodeNum: values")
END BLOCK

! Test 3: nodes of a mesh entity
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=2, meshID=[2])
  CALL obj%Set()
  CALL OK(obj%GetTotalNodeNum(dim=2, dom=dom) .EQ. 6, "meshID: total")
  CALL obj%GetNodeNum(dim=2, dom=dom, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 6, "meshID: size")
  CALL OK(ALL(Sort(ans(1:tsize)) .EQ. [2, 3, 5, 6, 8, 10]), &
          "meshID: values")
END BLOCK

! Test 4: a node shared by two entities is counted for each entity
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=1, meshID=[1, 5])
  CALL obj%Set()
  CALL OK(obj%GetTotalNodeNum(dim=1, dom=dom) .EQ. 5, "shared node: total")
  CALL obj%GetNodeNum(dim=1, dom=dom, ans=ans, tsize=tsize)
  CALL OK(ALL(Sort(ans(1:tsize)) .EQ. [1, 1, 2, 4, 7]), &
          "shared node: values")
END BLOCK

! Test 5: nodes of selected elements
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=2, elemNum=[18])
  CALL obj%Set()
  CALL OK(obj%GetTotalNodeNum(dim=2, dom=dom) .EQ. 4, "elemNum: total")
  CALL obj%GetNodeNum(dim=2, dom=dom, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 4, "elemNum: size")
  CALL OK(ALL(Sort(ans(1:tsize)) .EQ. [1, 4, 7, 9]), "elemNum: values")
END BLOCK

! Test 5a: a node shared by selected elements is counted once
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=2, elemNum=[18, 19])
  CALL obj%Set()
  CALL OK(obj%GetTotalNodeNum(dim=2, dom=dom) .EQ. 6, "elements: total")
  CALL obj%GetNodeNum(dim=2, dom=dom, ans=ans, tsize=tsize)
  CALL OK(ALL(Sort(ans(1:tsize)) .EQ. [1, 2, 4, 5, 7, 9]), &
          "elements: values")
END BLOCK

! Test 6: node list, then mesh entities, onlydim skips the node list
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(nodeNum=[10, 9])
  CALL obj%Add(dim=2, meshID=[2])
  CALL obj%Set()
  CALL OK(obj%GetTotalNodeNum(dim=2, dom=dom) .EQ. 8, "dim: with nodeNum")
  CALL OK(obj%GetTotalNodeNum(dim=2, dom=dom, onlydim=math%yes) .EQ. 6, &
          "onlydim: skips nodeNum")

  CALL obj%GetNodeNum(dim=2, dom=dom, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 8, "dim: size with nodeNum")
  CALL OK(ALL(ans(1:2) .EQ. [9, 10]), "dim: nodeNum comes first")
END BLOCK

! Test 6a: onlydim=.FALSE. keeps the node list, same as onlydim absent
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(nodeNum=[10, 9])
  CALL obj%Add(dim=2, meshID=[2])
  CALL obj%Set()
  CALL OK(obj%GetTotalNodeNum(dim=2, dom=dom, onlydim=math%no) .EQ. 8, &
          "onlydim false: total")

  CALL obj%GetNodeNum(dim=2, dom=dom, ans=ans, tsize=tsize, &
                      onlydim=math%no)
  CALL OK(tsize .EQ. 8, "onlydim false: size")
  CALL OK(ALL(Sort(ans(1:tsize)) .EQ. [2, 3, 5, 6, 8, 9, 10, 10]), &
          "onlydim false: values")
END BLOCK

! Test 7: all dimensions, the node list is used once
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(nodeNum=[10, 9])
  CALL obj%Add(dim=0, meshID=[3])
  CALL obj%Add(dim=2, meshID=[2])
  CALL obj%Set()
  CALL OK(obj%GetTotalNodeNum(dom=dom) .EQ. 9, "all dim: total")
  CALL obj%GetNodeNum(dom=dom, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 9, "all dim: size")
  CALL OK(ALL(Sort(ans(1:tsize)) .EQ. &
              [2, 3, 3, 5, 6, 8, 9, 10, 10]), "all dim: values")
END BLOCK

! Test 8: a list of a dimension above nsd is skipped
BLOCK
  CALL obj%Initiate()
  CALL obj%Add(dim=3, meshID=[1])
  CALL obj%Add(dim=2, meshID=[2])
  CALL obj%Set()
  CALL OK(obj%GetTotalNodeNum(dom=dom) .EQ. 6, "dim above nsd: total")
END BLOCK

CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
