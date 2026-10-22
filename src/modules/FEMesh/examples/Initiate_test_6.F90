!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Initiate a volume mesh (dim=3)
!
! Initiate reads the Hexahedron8 elements of the volume entity.
!
! The mesh small_hexa8_mesh.h5 is the unit cube with two Hexahedron8
! elements: 35 = [1, 9, 10, 3, 5, 11, 12, 7] and
! 36 = [9, 2, 4, 10, 11, 6, 8, 12]. They share the face
! [9, 10, 12, 11] at x = 0.5.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE BaseType, ONLY: TypeHexahedronNameOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEMesh_Class, ONLY: FEMesh_
  USE GlobalData, ONLY: I4B
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/small_hexa8_mesh.h5"
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=filename, mode="READ")
  CALL meshfile%OPEN()
  CALL obj%Initiate(hdf5=meshfile, DIM=3)

  CALL OK(obj%GetNSD() .EQ. 3_I4B, "GetNSD")
  CALL OK(obj%GetXidimension() .EQ. 3_I4B, "GetXidimension")
  CALL OK(obj%GetTotalElements() .EQ. 2_I4B, "GetTotalElements")
  CALL OK(obj%GetTotalNodes() .EQ. 12_I4B, "GetTotalNodes")
  CALL OK(obj%GetElemType(35_I4B) .EQ. &
    TypeHexahedronNameOpt%hexahedron8, "GetElemType")

  CALL obj%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
