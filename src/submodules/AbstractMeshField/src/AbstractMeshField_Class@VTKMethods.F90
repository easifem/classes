! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>

SUBMODULE(AbstractMeshField_Class) VTKMethods
USE VTKFile_Class, ONLY: VTKFile_, VTK_BINARY_APPENDED, VTK_POLYDATA
USE BaseType, ONLY: TypeFEVariableOpt
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: Display, ToString

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteData_vtk
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_WriteData_vtk()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%rank)
CASE (TypeFEVariableOpt%scalar)
  CALL WriteScalarData(obj=obj, nodeCoordField=nodeCoordField, &
                       filename=filename)
CASE (TypeFEVariableOpt%vector)
  CALL WriteVectorData(obj=obj, nodeCoordField=nodeCoordField, &
                       filename=filename)
CASE (TypeFEVariableOpt%matrix)
  CALL WriteMatrixData(obj=obj, nodeCoordField=nodeCoordField, &
                       filename=filename)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_WriteData_vtk

!----------------------------------------------------------------------------
!                                                   CheckErrorNodeCoordField
!----------------------------------------------------------------------------

SUBROUTINE CheckErrorNodeCoordField(nodeCoordField)
  CLASS(AbstractMeshField_), INTENT(INOUT) :: nodeCoordField

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CheckErrorNodeCoordField()"
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, jj, iel, ss(TypeFEVariableOpt%maxRank)
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = nodeCoordField%rank .EQ. TypeFEVariableOpt%Vector
  CALL AssertError1(isok, myName, &
                    "nodeCoordField must be a vector mesh field.")
#endif

#ifdef DEBUG_VER
  isok = nodeCoordField%totalShape .EQ. 2
  CALL AssertError1(isok, myName, &
                    "nodeCoordField%totalShape should be 2.")
#endif

#ifdef DEBUG_VER
  DO iel = 1, nodeCoordField%tsize
    ii = nodeCoordField%indxShape(iel)
    jj = nodeCoordField%indxShape(iel + 1) - 1
    isok = (jj - ii + 1) .EQ. 2
    CALL AssertError1(isok, myName, &
           "In nodeCoordField, for iel = "//ToString(iel)//" indxShape error")

    ss(1:nodeCoordField%totalShape) = nodeCoordField%ss(ii:jj)
    isok = ss(1) .EQ. 3
    CALL AssertError1(isok, myName, &
                      "The number of components in nodeCoordField must be 3.")
  END DO
#endif

#ifdef DEBUG_VER
  isok = nodeCoordField%varType .EQ. TypeFEVariableOpt%Space
  CALL AssertError1(isok, myName, &
                    "nodeCoordField must be a spatial field.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE CheckErrorNodeCoordField

!----------------------------------------------------------------------------
!                                                            WriteScalarData
!----------------------------------------------------------------------------

SUBROUTINE CheckErrorWriteScalarData(obj, nodeCoordField)
  CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
  CLASS(AbstractMeshField_), INTENT(INOUT) :: nodeCoordField

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CheckErrorWriteScalarData()"
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, jj, iel, ss(TypeFEVariableOpt%maxRank)
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL CheckErrorNodeCoordField(nodeCoordField=nodeCoordField)

#ifdef DEBUG_VER
  DO iel = 1, nodeCoordField%tsize
    ii = nodeCoordField%indxShape(iel)
    jj = nodeCoordField%indxShape(iel + 1) - 1
    ss(1:nodeCoordField%totalShape) = nodeCoordField%ss(ii:jj)

    ii = obj%indxVal(iel)
    jj = obj%indxVal(iel + 1) - 1
    isok = (jj - ii + 1) .EQ. ss(2)

    CALL AssertError1(isok, myName, &
             "In nodeCoordField, for iel = "//ToString(iel)//" indxVal error")
  END DO
#endif

#ifdef DEBUG_VER
  isok = nodeCoordField%tsize .EQ. obj%tsize
  CALL AssertError1(isok, myName, &
                    "tsize of obj and nodeCoordField must be same.")
#endif

#ifdef DEBUG_VER
  isok = obj%rank .EQ. TypeFEVariableOpt%Scalar
  CALL AssertError1(isok, myName, &
                    "obj must be a scalar mesh field.")
#endif

#ifdef DEBUG_VER
  isok = obj%totalShape .EQ. 1_I4B
  CALL AssertError1(isok, myName, &
                    "obj%totalShape should be 1.")
#endif

#ifdef DEBUG_VER
  isok = obj%varType .EQ. TypeFEVariableOpt%Space
  CALL AssertError1(isok, myName, &
                    "obj must be a spatial mesh field.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE CheckErrorWriteScalarData

!----------------------------------------------------------------------------
!                                                   CheckErrorWriteVectorData
!----------------------------------------------------------------------------

SUBROUTINE CheckErrorWriteVectorData(obj, nodeCoordField)
  CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
  CLASS(AbstractMeshField_), INTENT(INOUT) :: nodeCoordField

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CheckErrorWriteVectorData()"
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: ii, jj, iel, ss(TypeFEVariableOpt%maxRank), &
                  tpoints_obj, tpoints_xij
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL CheckErrorNodeCoordField(nodeCoordField=nodeCoordField)

#ifdef DEBUG_VER
  isok = nodeCoordField%tsize .EQ. obj%tsize
  CALL AssertError1(isok, myName, &
                    "tsize of obj and nodeCoordField must be same.")
#endif

#ifdef DEBUG_VER
  isok = obj%rank .EQ. TypeFEVariableOpt%Vector
  CALL AssertError1(isok, myName, &
                    "obj must be a Vector MeshField.")
#endif

#ifdef DEBUG_VER
  isok = obj%totalShape .EQ. 2_I4B
  CALL AssertError1(isok, myName, &
                    "obj%totalShape should be 2.")
#endif

#ifdef DEBUG_VER
  isok = obj%varType .EQ. TypeFEVariableOpt%Space
  CALL AssertError1(isok, myName, &
                    "obj must be a spatial mesh field.")
#endif

#ifdef DEBUG_VER
  DO iel = 1, obj%tsize
    ii = obj%indxShape(iel)
    jj = obj%indxShape(iel + 1) - 1
    isok = (jj - ii + 1) .EQ. 2
    CALL AssertError1(isok, myName, &
                    "In obj for iel = "//ToString(iel)//" indxShape error(1)")

    ss(1:nodeCoordField%totalShape) = obj%ss(ii:jj)
    tpoints_obj = ss(2)

    ii = nodeCoordField%indxShape(iel)
    jj = nodeCoordField%indxShape(iel + 1) - 1
    ss(1:nodeCoordField%totalShape) = nodeCoordField%ss(ii:jj)
    tpoints_xij = nodeCoordField%ss(ii + 1)
    isok = tpoints_obj .EQ. tpoints_xij
    CALL AssertError1(isok, myName, &
                    "In obj for iel = "//ToString(iel)//" indxShape error(2)")

    ii = obj%indxVal(iel)
    jj = obj%indxVal(iel + 1) - 1
    isok = (jj - ii + 1) .EQ. (ss(1) * ss(2))
    CALL AssertError1(isok, myName, &
                      "In obj for iel = "//ToString(iel)//" indxVal error(2)")
  END DO
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE CheckErrorWriteVectorData

!----------------------------------------------------------------------------
!                                                             WriteScalarData
!----------------------------------------------------------------------------

SUBROUTINE WriteScalarData(obj, nodeCoordField, filename)
  CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
  CLASS(AbstractMeshField_), INTENT(INOUT) :: nodeCoordField
  CHARACTER(*), INTENT(IN) :: filename

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "WriteScalarData()"
#endif

  TYPE(VTKFile_) :: vtk
  INTEGER(I4B) :: telements, iel, tpoints, ss(TypeFEVariableOpt%maxRank), &
                  ii, jj
  TYPE(String) :: node_str, open_str, close_str

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ! Check error
  CALL CheckErrorWriteScalarData(obj=obj, nodeCoordField=nodeCoordField)

  node_str = String("node")
  open_str = String("open")
  close_str = String("close")

  telements = obj%tsize

  CALL vtk%InitiateVTKFile( &
    filename=filename, mode="NEW", DataFormat=VTK_BINARY_APPENDED, &
    DataStructureType=VTK_POLYDATA)

  DO iel = 1, telements
    ii = nodeCoordField%indxShape(iel)
    jj = nodeCoordField%indxShape(iel + 1) - 1
    ss(1:nodeCoordField%totalShape) = nodeCoordField%ss(ii:jj)

    ii = nodeCoordField%indxVal(iel)
    jj = nodeCoordField%indxVal(iel + 1) - 1
    tpoints = ss(2)

    CALL vtk%WritePiece( &
      nPoints=tpoints, nVerts=0_I4B, nLines=0_I4B, nStrips=0_I4B, &
      nPolys=0_I4B)

    CALL vtk%WritePoints( &
      x=nodeCoordField%val(ii:jj:ss(1)), &
      y=nodeCoordField%val(ii + 1:jj:ss(1)), &
      z=nodeCoordField%val(ii + 2:jj:ss(1)))

    CALL vtk%WriteDataArray(location=node_str, action=open_str)

    ii = obj%indxVal(iel)
    jj = obj%indxVal(iel + 1) - 1

    CALL vtk%WriteDataArray(name=obj%name, x=obj%val(ii:jj))

    CALL vtk%WriteDataArray(location=node_str, action=close_str)
    CALL vtk%WritePiece()
  END DO

  CALL vtk%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE WriteScalarData

!----------------------------------------------------------------------------
!                                                             WriteVectorData
!----------------------------------------------------------------------------

SUBROUTINE WriteVectorData(obj, nodeCoordField, filename)
  CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
  CLASS(AbstractMeshField_), INTENT(INOUT) :: nodeCoordField
  CHARACTER(*), INTENT(IN) :: filename

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "WriteVectorData()"
#endif

  TYPE(VTKFile_) :: vtk
  INTEGER(I4B) :: telements, iel, tpoints, ss(TypeFEVariableOpt%maxRank), &
                  ii, jj
  TYPE(String) :: node_str, open_str, close_str

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL CheckErrorWriteVectorData(obj=obj, nodeCoordField=nodeCoordField)

  node_str = String("node")
  open_str = String("open")
  close_str = String("close")

  CALL vtk%InitiateVTKFile( &
    filename=filename, mode="NEW", DataFormat=VTK_BINARY_APPENDED, &
    DataStructureType=VTK_POLYDATA)

  telements = obj%mesh%GetTotalCells()

  DO iel = 1, telements
    ii = nodeCoordField%indxShape(iel)
    jj = nodeCoordField%indxShape(iel + 1) - 1
    ss(1:nodeCoordField%totalShape) = nodeCoordField%ss(ii:jj)
    tpoints = ss(2)

    CALL vtk%WritePiece( &
      nPoints=tpoints, nVerts=0_I4B, nLines=0_I4B, nStrips=0_I4B, &
      nPolys=0_I4B)

    ii = nodeCoordField%indxVal(iel)
    jj = nodeCoordField%indxVal(iel + 1) - 1

    CALL vtk%WritePoints( &
      x=nodeCoordField%val(ii:jj:3), y=nodeCoordField%val(ii + 1:jj:3), &
      z=nodeCoordField%val(ii + 2:jj:3))

    CALL vtk%WriteDataArray(location=node_str, action=open_str)

    ii = obj%indxShape(iel)
    jj = obj%indxShape(iel + 1) - 1
    ss(1:obj%totalShape) = obj%ss(ii:jj)
    tpoints = ss(2)

    ii = nodeCoordField%indxVal(iel)
    jj = nodeCoordField%indxVal(iel + 1) - 1

    CALL vtk%WriteDataArray( &
      name=obj%name, x=obj%val(ii:jj), numberOfComponents=ss(1))

    CALL vtk%WriteDataArray(location=node_str, action=close_str)

    CALL vtk%WritePiece()
  END DO

  CALL vtk%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE WriteVectorData

!----------------------------------------------------------------------------
!                                                             WriteMatrixData
!----------------------------------------------------------------------------

SUBROUTINE WriteMatrixData(obj, nodeCoordField, filename)
  CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
  CLASS(AbstractMeshField_), INTENT(INOUT) :: nodeCoordField
  CHARACTER(*), INTENT(IN) :: filename

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "WriteMatrixData()"
#endif

  TYPE(VTKFile_) :: vtk
  INTEGER(I4B) :: telements, iel, tpoints, ss(TypeFEVariableOpt%maxRank), &
                  ii, jj
  TYPE(String) :: node_str, open_str, close_str

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

! CALL obj%mesh%ExportToVTK(vtk=vtk, filename=filename)

  node_str = String("node")
  open_str = String("open")
  close_str = String("close")

  CALL vtk%InitiateVTKFile( &
    filename=filename, mode="NEW", DataFormat=VTK_BINARY_APPENDED, &
    DataStructureType=VTK_POLYDATA)

  telements = obj%mesh%GetTotalCells()

  DO iel = 1, telements

    ii = obj%indxShape(iel)
    jj = obj%indxShape(iel + 1) - 1

    ss(1:obj%totalShape) = obj%ss(ii:jj)

    ii = obj%indxVal(iel)
    jj = obj%indxVal(iel + 1) - 1
    tpoints = obj%indxVal(iel + 1) - obj%indxVal(iel)
    tpoints = tpoints / 3

    CALL vtk%WritePiece( &
      nPoints=tpoints, nVerts=0_I4B, nLines=0_I4B, nStrips=0_I4B, &
      nPolys=0_I4B)

    CALL vtk%WritePoints(x=obj%val(ii:jj:3), y=obj%val(ii + 1:jj:3), &
                         z=obj%val(ii + 2:jj:3))
    CALL vtk%WriteDataArray(location=node_str, action=open_str)
    CALL vtk%WriteDataArray( &
      name=obj%name, x=obj%val(ii:jj:3), y=obj%val(ii + 1:jj:3), &
      z=obj%val(ii + 2:jj:3))
    CALL vtk%WriteDataArray(location=node_str, action=close_str)

    CALL vtk%WritePiece()

  END DO

  CALL vtk%DEALLOCATE()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE WriteMatrixData

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE VTKMethods
