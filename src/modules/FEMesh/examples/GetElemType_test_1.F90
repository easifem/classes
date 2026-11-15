!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the element type and topology
!
! GetElemType returns the element name (Quadrangle4), and
! GetElemTopology returns the topology (Quadrangle). Without an
! element, GetElemTopology returns the topologies in the mesh.
! GetElemTopologyIndx returns the index of the topology in the
! order Point, Line, Triangle, Quadrangle, Tetrahedron, Hexahedron,
! Prism, Pyramid.
!
! The mesh very_small_quad4_mesh.h5 is the unit square with four
! Quadrangle4 elements (global numbers 13 to 16) and nine nodes:
!
!   3 ---- 6 ---- 4
!   |  14  |  16  |
!   7 ---- 9 ---- 8
!   |  13  |  15  |
!   1 ---- 5 ---- 2

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE BaseType, ONLY: TypeElemNameOpt
  USE BaseType, ONLY: TypeQuadrangleNameOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEMesh_Class, ONLY: FEMesh_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: topo(8)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=2)

  CALL OK(obj%GetElemType(13_I4B) .EQ. &
    TypeQuadrangleNameOpt%quadrangle4, "GetElemType")
  CALL OK(obj%GetElemTopology(13_I4B) .EQ. TypeElemNameOpt%quadrangle, &
    "GetElemTopology")
  CALL OK(obj%GetElemTopologyIndx(13_I4B) .EQ. 4_I4B, &
    "GetElemTopologyIndx")
  CALL OK(obj%GetTotalTopology() .EQ. 1_I4B, "GetTotalTopology")
  topo = obj%GetElemTopology()
  CALL OK(topo(1) .EQ. TypeElemNameOpt%quadrangle, "topologies")
  topo = obj%GetTotalElementsTopologyWise()
  CALL OK(ALL(topo .EQ. [0, 0, 0, 4, 0, 0, 0, 0]), &
    "GetTotalElementsTopologyWise")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
