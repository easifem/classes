! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

MODULE GmshPoint_Class
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE BaseType, ONLY: math => TypeMathOpt
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshPoint_
PUBLIC :: GmshPointPointer_
PUBLIC :: GmshPointImportFromToml

!----------------------------------------------------------------------------
!                                                                  GmshPoint_
!----------------------------------------------------------------------------

TYPE :: GmshPoint_
  PRIVATE
  REAL(DFP) :: x = math%zero
  REAL(DFP) :: y = math%zero
  REAL(DFP) :: z = math%zero
  REAL(DFP) :: meshSize = math%one
  INTEGER(I4B) :: indx = math%one_i

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate

  PROCEDURE, PUBLIC, PASS(obj) :: SetX => obj_SetX
  PROCEDURE, PUBLIC, PASS(obj) :: SetY => obj_SetY
  PROCEDURE, PUBLIC, PASS(obj) :: SetZ => obj_SetZ
  PROCEDURE, PUBLIC, PASS(obj) :: SetMeshSize => obj_SetMeshSize
  PROCEDURE, PUBLIC, PASS(obj) :: SetIndx => obj_SetIndx

  PROCEDURE, PUBLIC, PASS(obj) :: GetX => obj_GetX
  PROCEDURE, PUBLIC, PASS(obj) :: GetY => obj_GetY
  PROCEDURE, PUBLIC, PASS(obj) :: GetZ => obj_GetZ
  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshSize => obj_GetMeshSize
  PROCEDURE, PUBLIC, PASS(obj) :: GetIndx => obj_GetIndx

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2

END TYPE GmshPoint_

!----------------------------------------------------------------------------
!                                                          GmshPointPointer_
!----------------------------------------------------------------------------

TYPE :: GmshPointPointer_
  CLASS(GmshPoint_), POINTER :: ptr => NULL()
END TYPE GmshPointPointer_

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Initiate gmsh points
!
!# Initiate
!
! Initiate gmsh points.

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, x, y, z, meshSize, indx)
    CLASS(GmshPoint_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x, y, z, meshSize
    INTEGER(I4B), INTENT(IN) :: indx
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                               SetX@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set x
!
!# SetX
!
! Set x in gmsh point.

INTERFACE
  MODULE SUBROUTINE obj_SetX(obj, x)
    CLASS(GmshPoint_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x
  END SUBROUTINE obj_SetX
END INTERFACE

!----------------------------------------------------------------------------
!                                                               SetY@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set y
!
!# SetY
!
! Set y in gmsh point.

INTERFACE
  MODULE SUBROUTINE obj_SetY(obj, y)
    CLASS(GmshPoint_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: y
  END SUBROUTINE obj_SetY
END INTERFACE

!----------------------------------------------------------------------------
!                                                               SetZ@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set z
!
!# SetZ
!
! Set z in gmsh point.

INTERFACE
  MODULE SUBROUTINE obj_SetZ(obj, z)
    CLASS(GmshPoint_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: z
  END SUBROUTINE obj_SetZ
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetMeshSize@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set mesh size
!
!# SetMeshSize
!
! Set mesh size in gmsh point.

INTERFACE
  MODULE SUBROUTINE obj_SetMeshSize(obj, meshSize)
    CLASS(GmshPoint_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: meshSize
  END SUBROUTINE obj_SetMeshSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetIndx@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set indx
!
!# SetIndx
!
! Set indx

INTERFACE
  MODULE SUBROUTINE obj_SetIndx(obj, indx)
    CLASS(GmshPoint_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
  END SUBROUTINE obj_SetIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                               GetX@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get x
!
!# GetX
!
! Get x from Gmsh point.

INTERFACE
  MODULE FUNCTION obj_GetX(obj) RESULT(ans)
    CLASS(GmshPoint_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_GetX
END INTERFACE

!----------------------------------------------------------------------------
!                                                               GetY@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get y
!
!# GetY
!
! Get x from Gmsh point.

INTERFACE
  MODULE FUNCTION obj_GetY(obj) RESULT(ans)
    CLASS(GmshPoint_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_GetY
END INTERFACE

!----------------------------------------------------------------------------
!                                                               GetZ@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get z
!
!# GetZ
!
! Get x from Gmsh point.

INTERFACE
  MODULE FUNCTION obj_GetZ(obj) RESULT(ans)
    CLASS(GmshPoint_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_GetZ
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetMeshSize@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get mesh size
!
!# GetMeshSize
!
! Get meshSize from Gmsh point.

INTERFACE
  MODULE FUNCTION obj_GetMeshSize(obj) RESULT(ans)
    CLASS(GmshPoint_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_GetMeshSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetIndx@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get mesh size
!
!# GetIndx
!
! Get indx from Gmsh point.

INTERFACE
  MODULE FUNCTION obj_GetIndx(obj) RESULT(ans)
    CLASS(GmshPoint_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_GetIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Display gmsh point
!
!# Display
!
! Display gmsh point.

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(GmshPoint_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Display gmsh point
!
!# Display
!
! Import Gmsh point.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(GmshPoint_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Import gmsh points from toml file
!
!# ImportFromToml
!
! Import gmsh points from toml file. After getting the toml table from
! the provided file, this method calls ImportFromToml1.
!
!## Examples
!
!```fortran
!{{% fortran-code file="examples/ImportFromToml_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(GmshPoint_), INTENT(INOUT) :: obj
    !! Gmsh point
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
!                                                    ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary: Initiate a vector of GmshPoint_ from the toml table

INTERFACE GmshPointImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml3(obj, table, tomlName)
    TYPE(GmshPointPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
    CHARACTER(*), INTENT(IN) :: tomlName
  END SUBROUTINE obj_ImportFromToml3
END INTERFACE GmshPointImportFromToml

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-25
! summary: Import a vector of GmshPointPointer_ from toml file

INTERFACE GmshPointImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml4(obj, tomlName, afile, filename, &
                                        printToml)
    TYPE(GmshPointPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Gmsh point
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
  END SUBROUTINE obj_ImportFromToml4
END INTERFACE GmshPointImportFromToml

END MODULE GmshPoint_Class
