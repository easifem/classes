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

MODULE GmshPhysicalGroup_Class
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE BaseType, ONLY: math => TypeMathOpt
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE String_Class, ONLY: String
USE Gmsh_Class, ONLY: Gmsh_
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshPhysicalGroup_
PUBLIC :: GmshPhysicalGroupPointer_
PUBLIC :: GmshPhysicalGroupImportFromToml

!----------------------------------------------------------------------------
!                                                         GmshPhysicalGroup_
!----------------------------------------------------------------------------

TYPE :: GmshPhysicalGroup_
  PRIVATE
  INTEGER(I4B) :: dim = math%zero_i
  !! dimension of physical groups
  INTEGER(I4B) :: indx = math%zero_i
  !! physical id of physical group
  TYPE(String) :: name
  !! name of physical groups
  INTEGER(I4B), ALLOCATABLE :: tags(:)
  !! ids of geometric entities

CONTAINS

  ! @Methods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: SetDim => obj_SetDim
  PROCEDURE, PUBLIC, PASS(obj) :: SetIndx => obj_SetIndx
  PROCEDURE, PUBLIC, PASS(obj) :: SetName => obj_SetName
  PROCEDURE, PUBLIC, PASS(obj) :: SetTags => obj_SetTags
  PROCEDURE, PUBLIC, PASS(obj) :: GetDim => obj_GetDim
  PROCEDURE, PUBLIC, PASS(obj) :: GetIndx => obj_GetIndx
  PROCEDURE, PUBLIC, PASS(obj) :: GetName => obj_GetName
  PROCEDURE, PUBLIC, PASS(obj) :: GetTags => obj_GetTags
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: Copy => obj_Copy

  ! @TomlMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2

  !@GmshMethods
  PROCEDURE, PUBLIC, PASS(obj) :: CreateGmshModel => obj_CreateGmshModel
END TYPE GmshPhysicalGroup_

!----------------------------------------------------------------------------
!                                                      GmshPhysicalGroupPointer_
!----------------------------------------------------------------------------

TYPE :: GmshPhysicalGroupPointer_
  CLASS(GmshPhysicalGroup_), POINTER :: ptr => NULL()
END TYPE GmshPhysicalGroupPointer_

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Initiate GmshPhysicalGroup
!
!# Initiate
!
! Initiate GmshPhysicalGroup.

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, dim, tags, name, indx)
    CLASS(GmshPhysicalGroup_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tags(:)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: indx
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetDim@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set dim GmshPhysicalGroup
!
!# SetDim
!
! Set dim in GmshPhysicalGroup

INTERFACE
  MODULE SUBROUTINE obj_SetDim(obj, dim)
    CLASS(GmshPhysicalGroup_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
  END SUBROUTINE obj_SetDim
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetIndx@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set indx GmshPhysicalGroup
!
!# SetIndx
!
! Set indx in GmshPhysicalGroup

INTERFACE
  MODULE SUBROUTINE obj_SetIndx(obj, indx)
    CLASS(GmshPhysicalGroup_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
  END SUBROUTINE obj_SetIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetName@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set name GmshPhysicalGroup
!
!# SetName
!
! Set name in GmshPhysicalGroup

INTERFACE
  MODULE SUBROUTINE obj_SetName(obj, name)
    CLASS(GmshPhysicalGroup_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
  END SUBROUTINE obj_SetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SetTags@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Set tags in GmshPhysicalGroup
!
!# SetTags
!
! Set tags in GmshPhysicalGroup.

INTERFACE
  MODULE SUBROUTINE obj_SetTags(obj, tags)
    CLASS(GmshPhysicalGroup_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tags(:)
  END SUBROUTINE obj_SetTags
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetDim@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get dim from GmshPhysicalGroup
!
!# GetDim
!
! Get dim from GmshPhysicalGroup.

INTERFACE
  MODULE FUNCTION obj_GetDim(obj) RESULT(ans)
    CLASS(GmshPhysicalGroup_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetDim
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetIndx@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get indx from GmshPhysicalGroup
!
!# GetIndx
!
! Get indx from GmshPhysicalGroup.

INTERFACE
  MODULE FUNCTION obj_GetIndx(obj) RESULT(ans)
    CLASS(GmshPhysicalGroup_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetName@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get name from GmshPhysicalGroup
!
!# GetName
!
! Get name from GmshPhysicalGroup.

INTERFACE
  MODULE FUNCTION obj_GetName(obj) RESULT(ans)
    CLASS(GmshPhysicalGroup_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetTags@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Get tags from GmshPhysicalGroup
!
!# GetTags
!
! Get tags from GmshPhysicalGroup.

INTERFACE
  MODULE FUNCTION obj_GetTags(obj) RESULT(ans)
    CLASS(GmshPhysicalGroup_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetTags
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Copy@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-25
! summary: Copy GmshPhysicalGroup

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(GmshPhysicalGroup_), INTENT(INOUT) :: obj
    CLASS(GmshPhysicalGroup_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Display@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Display GmshPhysicalGroup
!
!# Display
!
! Display GmshPhysicalGroup.

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(GmshPhysicalGroup_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Import from toml table.
!
!# ImportFromToml
!
! Import GmshPhysicalGroup from toml table.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(GmshPhysicalGroup_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-24
! summary: Import GmshPhysicalGroup from toml file
!
!# ImportFromToml
!
! Import GmshPhysicalGroup from toml file. After getting the toml table from
! the provided file, this method calls ImportFromToml1.
!
!## Examples
!
!```fortran
!{{% fortran-code file="examples/ImportFromToml_test_1.F90" %}}
!```

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, tomlName, afile, filename, printToml)
    CLASS(GmshPhysicalGroup_), INTENT(INOUT) :: obj
    !! GmshPhysicalGroup
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
! summary: Initiate a vector of GmshPhysicalGroup_ from the toml table

INTERFACE GmshPhysicalGroupImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml3(obj, table, tomlName)
    TYPE(GmshPhysicalGroupPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
    CHARACTER(*), INTENT(IN) :: tomlName
  END SUBROUTINE obj_ImportFromToml3
END INTERFACE GmshPhysicalGroupImportFromToml

!----------------------------------------------------------------------------
!                                                 ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-25
! summary: Import a vector of GmshPhysicalGroupPointer_ from toml file

INTERFACE GmshPhysicalGroupImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml4( &
    obj, tomlName, afile, filename, printToml)
    TYPE(GmshPhysicalGroupPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
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
END INTERFACE GmshPhysicalGroupImportFromToml

!----------------------------------------------------------------------------
!                                                 CreateGmshModel@GmshMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-26
! summary: Create gmsh model for GmshPhysicalGroup_
!
!# CreateGmshModel
!
! Create gmsh model for GmshPhysicalGroup_

INTERFACE
  MODULE SUBROUTINE obj_CreateGmshModel(obj, gmsh)
    CLASS(GmshPhysicalGroup_), INTENT(INOUT) :: obj
    TYPE(Gmsh_), INTENT(INOUT) :: gmsh
  END SUBROUTINE obj_CreateGmshModel
END INTERFACE

END MODULE GmshPhysicalGroup_Class
