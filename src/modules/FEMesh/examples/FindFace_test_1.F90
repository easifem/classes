PROGRAM main
USE FEMesh_Class, ONLY: FEMesh_
USE HDF5File_Class, ONLY: HDF5File_
USE GlobalData, ONLY: I4B, LGT, Quadrangle
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: Display
USE Test_Method

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER :: filename = "../../Mesh/examples/meshdata/small_quad4_mesh.h5"
INTEGER(I4B), PARAMETER :: nsd = 2
CHARACTER(*), PARAMETER :: testname = "FindFace"

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile

! Initiate and open the mesh file which is in `HDF5File_` format.
CALL meshfile%Initiate(FileName=filename, MODE="READ")
! Open the mesh file
CALL meshfile%OPEN()
! Initiate an instance of `Mesh_`
CALL obj%Initiate(hdf5=meshfile, dim=nsd)
CALL meshfile%DEALLOCATE()

BLOCK
  INTEGER(I4B) :: localelement, localfacenumber
  INTEGER(I4B), ALLOCATABLE :: cellcon(:), facecon(:)
  LOGICAL(LGT) :: isface, onlyboundaryelement, isok

  localelement = 1
  cellcon = obj%GetConnectivity(globalElement=localelement, islocal=.TRUE.)
  ! CALL display(cellcon, 'cellcon=')
  ! 1, 5, 17, 11

  onlyboundaryelement = obj%IsBoundaryElement(globalElement=localelement, &
                                              islocal=.TRUE.)

  facecon = [5, 1]
  CALL obj%FindFace(globalElement=localelement, islocal=.TRUE., &
            facecon=facecon, isface=isface, localfacenumber=localfacenumber, &
                    onlyboundaryelement=onlyboundaryelement)
  isok = isface
  CALL ok(isok, testname)
  isok = localfacenumber == 1
  CALL ok(isok, testname)

  facecon = [11, 17]
  CALL obj%FindFace(globalElement=localelement, islocal=.TRUE., &
            facecon=facecon, isface=isface, localfacenumber=localfacenumber, &
                    onlyboundaryelement=onlyboundaryelement)
  isok = isface
  CALL ok(isok, testname)
  isok = localfacenumber == 3
  CALL ok(isok, testname)

END BLOCK

CALL obj%DEALLOCATE()
END PROGRAM main

! total nodes = 25
! total elements = 16
! total faces = 40
! total edges = 0
