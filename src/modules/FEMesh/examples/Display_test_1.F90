!> author: Vikas Sharma, Ph. D.
! date: 2025-06-01
! summary:  Display method is tested  in this example.
!
! Display method of FEMesh.
!
! The result of this example is given below.
!
! Results:
!
!```txt
! Mesh object initiated: TRUE
! showTime: FALSE
! readFromFile: TRUE
! isNodeToElementsInitiated: FALSE
! isNodeToNodesInitiated: FALSE
! isExtraNodeToNodesInitiated: FALSE
! isElementToElementsInitiated: TRUE
! isEdgeConnectivityInitiated: FALSE
! isFaceConnectivityInitiated: TRUE
! isBoundaryDataInitiated: FALSE
! isFacetDataInitiated: FALSE
! uid: 1
! Total elements (topology wise)
! ==============================
! point: 0
! line: 0
! triangle: 14
! quadrangle: 0
! tetrahedron: 0
! hexahedron: 0
! prism: 0
! pyramid: 0
!
! Total topologies: 1
! Topologies(1): Triangle3
! maxNNE: 6
! nsd: 2
! xidim: 2
! maxNptrs: 37
! minNptrs: 1
! maxElemNum: 26
! minElemNum: 13
! tNodes: 37
! tEdges: 0
! tFaces: 25
! tElements: 14
! minX: 0.00000
! maxX: 2.00000
! minY: 0.00000
! maxY: 2.00000
! minZ: 0.00000
! maxZ: 0.00000
! X: 0.00000
! Y: 0.00000
! Z: 0.00000
! boundingEntity ALLOCATED: TRUE
! boundingEntity:
! ----------------
!        1
!        2
!        3
!        4
! local_elemNumber ALLOCATED: TRUE
! local_nptrs ALLOCATED: TRUE
! quality ALLOCATED: FALSE
! facetElementType ALLOCATED: FALSE
! nodeData ALLOCATED: TRUE
! nodeData Size: 37
! elementData ALLOCATED: TRUE
! elementData Size: 14
! facetData ALLOCATED: FALSE
! kdtree Associated: FALSE
! kdresult Allocated: FALSE
!```

PROGRAM main
USE FEMesh_Class, only: FEMesh_
USE HDF5File_Class, only: HDF5File_
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(LEN=*), PARAMETER :: filename = "./meshdata/small_tri6_mesh.h5"

INTEGER(I4B), PARAMETER :: nsd = 2

CALL meshfile%Initiate(FileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL obj%Initiate(hdf5=meshfile, dim=nsd)
CALL obj%Display("FEMesh")

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
