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
!

MODULE GmshEarthenDamMesh2_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: math => TypeMathOpt
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE BaseType, ONLY: RealMatrix_
USE BaseType, ONLY: IntVector_
USE BaseType, ONLY: RealVector_
USE Tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE Gmsh_Class, ONLY: Gmsh_
USE GmshPoint_Class, ONLY: GmshPointPointer_
USE GmshLine_Class, ONLY: GmshLinePointer_
USE GmshPlaneSurface_Class, ONLY: GmshPlaneSurfacePointer_
USE GmshPhysicalGroup_Class, ONLY: GmshPhysicalGroupPointer_
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshEarthenDamMesh2_
PUBLIC :: GmshEarthenDamMesh2Pointer_
PUBLIC :: TypeGmshEarthenDamMesh2Opt

!----------------------------------------------------------------------------
!                                                    GmshEarthenDamMesh2Opt_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-22
! summary: options for GmshEarthenDamMesh2

TYPE :: GmshEarthenDamMesh2Opt_
END TYPE GmshEarthenDamMesh2Opt_

!----------------------------------------------------------------------------
!                                                 TypeGmshEarthenDamMesh2Opt
!----------------------------------------------------------------------------

TYPE(GmshEarthenDamMesh2Opt_), PARAMETER :: TypeGmshEarthenDamMesh2Opt = &
                                            GmshEarthenDamMesh2Opt_()

!----------------------------------------------------------------------------
!                                                      GmshEarthenDamMesh2_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-03
! summary:  The data type to create structured mesh by using Gmsh

TYPE :: GmshEarthenDamMesh2_
  PRIVATE
  LOGICAL(LGT) :: recombineAll = math%no
  !! All surfaces will be recombine into quad or hexahedron
  TYPE(String) :: filename
  !! Name of the mesh file to be generated
  TYPE(GmshPointPointer_), ALLOCATABLE :: points(:)
  !! points of dams
  TYPE(GmshLinePointer_), ALLOCATABLE :: lines(:)
  !! lines
  TYPE(GmshPlaneSurfacePointer_), ALLOCATABLE :: surfaces(:)
  !! plane surfaces
  TYPE(GmshPhysicalGroupPointer_), ALLOCATABLE :: physicalGroups(:)
  !! physical groups

CONTAINS
  PRIVATE

  !@Methods
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Generate => obj_Generate
  !! Generate the model and the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display

  !@TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import obj from toml table
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import obj from toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
  !! Generic method for importing obj from toml
END TYPE GmshEarthenDamMesh2_

!----------------------------------------------------------------------------
!                                                 GmshEarthenDamMesh2Pointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-26
! summary: Pointer to GmshEarthenDamMesh2_
!
!# GmshEarthenDamMesh2Pointer_
!
! Pointer to GmshEarthenDamMesh2.
!
TYPE :: GmshEarthenDamMesh2Pointer_
  CLASS(GmshEarthenDamMesh2_), POINTER :: ptr => NULL()
END TYPE GmshEarthenDamMesh2Pointer_

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Deallocate data

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(GmshEarthenDamMesh2_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ii
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Generate@GenerateMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-03
! summary: Generate mesh

INTERFACE
  MODULE SUBROUTINE obj_Generate(obj, gmsh)
    CLASS(GmshEarthenDamMesh2_), INTENT(inout) :: obj
    CLASS(Gmsh_), INTENT(INOUT) :: gmsh
  END SUBROUTINE obj_Generate
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Display the content

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(GmshEarthenDamMesh2_), INTENT(inout) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-20
! summary: Import obj from toml table
!
!# ImportFromToml
!
! Import object from toml table.
!

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(GmshEarthenDamMesh2_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary: Import data From toml file
!
!# ImportFromToml
!
! Initiate obj from toml file.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, tomlName, afile, filename, printToml)
    CLASS(GmshEarthenDamMesh2_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    !! name of the key
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    !! txt file where toml config is stored
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    !! you can pass the filename, then we will make
    !! the file, open it and read the toml config and
    !! close the file.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    !! if it is true then we will print the toml config
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshEarthenDamMesh2_Class
