# AGENTS.md - Development Guide for easifemClasses

This guide provides essential information for AI coding agents and developers working on the EASIFEM (Expandable And Scalable Infrastructure for Finite Element Methods) library.

## Project Overview

- **Language**: Modern Fortran (F2018 standard)
- **Build System**: CMake 3.20+ with Ninja generator
- **License**: GPL-3.0
- **Architecture**: Object-oriented with module/submodule separation
- **Documentation**: FORD (FORtran Documenter)

## Build Commands

### Configure and Build

```bash
# Development build (Debug)
python3 build.py

# Manual CMake configuration
cmake -S ./ -B $HOME/temp/easifem/classes/build \
  -G "Ninja" \
  -D CMAKE_BUILD_TYPE=Debug \
  -D BUILD_SHARED_LIBS:BOOL=ON \
  -D USE_GMSH_SDK:BOOL=ON \
  -D CMAKE_INSTALL_PREFIX:PATH=${EASIFEM_CLASSES}

# Build
cmake --build $HOME/temp/easifem/classes/build
```

### Install

```bash
python3 install.py
# OR
cmake --build $HOME/temp/easifem/classes/build --target install
```

### Documentation Generation

```bash
ford FORDsetup.md
# Output: ./docs/ directory
```

### Linting

```bash
fortitude check src/
# Configuration: fortitude.toml (line-length: 78)
```

## Testing

**Note**: This project currently has no formal test infrastructure. Tests are excluded via `.gitignore`. When adding tests:

- Create a `tests/` directory at project root
- Consider pFUnit or FRUIT frameworks
- Follow the module structure pattern

## Code Style Guidelines

### File Organization

#### Directory Structure

```
src/
├── modules/          # Interface definitions (public API)
│   └── ModuleName/
│       ├── CMakeLists.txt
│       └── src/
│           └── ModuleName_Class.F90
│
└── submodules/       # Implementation (private)
    └── ModuleName/
        ├── CMakeLists.txt
        └── src/
            ├── ModuleName_Class@ConstructorMethods.F90
            ├── ModuleName_Class@GetMethods.F90
            ├── ModuleName_Class@SetMethods.F90
            └── ModuleName_Class@IOMethods.F90
```

#### File Extensions

- **`.F90`** (uppercase): Preprocessed files - use for all new code
- **`.f90`** (lowercase): Non-preprocessed - legacy only

#### Submodule Naming Pattern

`ClassName@MethodCategory.F90`

- Example: `Domain_Class@ConstructorMethods.F90`
- Categories: `ConstructorMethods`, `GetMethods`, `SetMethods`, `IOMethods`

### Module Structure

#### Module Interface (in `src/modules/`)

```fortran
! Standard GPL3 header (lines 1-15)

!> authors: Author Name, Ph. D.
! date: YYYY-MM-DD
! summary: Brief description

MODULE ModuleName_Class
  USE BaseType, ONLY: SpecificTypes_
  USE GlobalData, ONLY: DFP, I4B, LGT
  USE ExceptionHandler_Class, ONLY: e
  
  IMPLICIT NONE
  PRIVATE
  
  PUBLIC :: ModuleName_
  PUBLIC :: ModuleNamePointer_
  
  CHARACTER(*), PARAMETER :: modName = "ModuleName_Class"
  
  TYPE :: ModuleName_
    PRIVATE
    ! Member variables
  CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC, PASS(obj) :: MethodName => obj_MethodName
  END TYPE ModuleName_
  
  TYPE :: ModuleNamePointer_
    CLASS(ModuleName_), POINTER :: ptr => NULL()
  END TYPE ModuleNamePointer_
  
  INTERFACE
    MODULE SUBROUTINE obj_MethodName(obj)
      CLASS(ModuleName_), INTENT(INOUT) :: obj
    END SUBROUTINE obj_MethodName
  END INTERFACE

END MODULE ModuleName_Class
```

#### Submodule Implementation (in `src/submodules/`)

```fortran
! Standard GPL3 header

!> authors: Author Name, Ph. D.
! date: YYYY-MM-DD
! summary: Implementation of method category

SUBMODULE(ModuleName_Class) MethodCategory
  USE RequiredModule_Class, ONLY: RequiredType_
  IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                        MethodName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MethodName
  ! Implementation
END PROCEDURE obj_MethodName

END SUBMODULE MethodCategory
```

### Naming Conventions

#### Types and Classes

- Pattern: `EntityName_` (trailing underscore)
- Examples: `Domain_`, `Mesh_`, `DirichletBC_`
- Pointer types: `EntityNamePointer_`

#### Procedures

- Pattern: `obj_ActionName` or `ModuleName_ActionName`
- Overloads: `obj_ActionName1`, `obj_ActionName2`
- Examples: `obj_Deallocate`, `obj_Display`, `obj_GetNodeToElements1`

#### Variables

- Use camelCase: `meshVolume`, `local_nptrs`, `elemType`
- Common names: `obj` (object instance), `ans` (result)
- Type parameters: `I4B`, `DFP`, `LGT`

#### Constants

- Module name: `CHARACTER(*), PARAMETER :: modName = "ClassName"`

### Import Statements

Always use explicit `ONLY` clauses:

```fortran
USE BaseType, ONLY: CSRSparsity_
USE GlobalData, ONLY: DFP, I4B, LGT
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
```

When we import two or more items from a module, then for each item imported from the module we should write it in its own line.
For example, If are importing two items foo and bar from XXX module then instead of using

```fortran
USE XXX, ONLY: foo, bar
```

We should write it in two lines as shown below:

```fortran
USE XXX, ONLY: foo
USE XXX, ONLY: bar
```

### Type Declarations

Use explicit kind parameters from `GlobalData`:

```fortran
INTEGER(I4B) :: counter
INTEGER(I4B), ALLOCATABLE :: indices(:)
REAL(DFP) :: value
REAL(DFP), POINTER :: data(:, :)
LOGICAL(LGT) :: isValid
```

### Access Control

Default private, explicit public exports:

```fortran
IMPLICIT NONE
PRIVATE

PUBLIC :: TypeName_
PUBLIC :: PublicSubroutineName

TYPE :: TypeName_
  PRIVATE
  ! members
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: PublicMethod => obj_PublicMethod
END TYPE TypeName_
```

### Documentation

#### File Headers

```fortran
! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
! [GPL3 license text - lines 1-15]

!> authors: Your Name, Ph. D.
! date: YYYY-MM-DD
! update:
!   - YYYY-MM-DD: Description of update
! summary: Module description
```

#### FORD Documentation

- Use `!>` for primary documentation
- Use `!!` for inline variable documentation
- Reference external docs: `!{!pages/docs-api/Module/topic.md!}`
- Section markers: `! @SectionName`

#### Section Delimiters

```fortran
!----------------------------------------------------------------------------
!                                                    SubroutineName
!----------------------------------------------------------------------------
```

### Error Handling

Use the exception handler:

```fortran
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (error_condition) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[ERROR] :: Error description')
END IF
```

### Preprocessor Usage

#### Debug Guards

```fortran
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SubroutineName"
CALL e%RaiseInformation(modName//'::'//myName//' - [START]')
#endif
```

#### External Libraries

```fortran
#include "lisf.h"  ! For LIS solver integration
```

## Compiler Flags

### GNU Fortran (gfortran)

- Standard: `-std=f2018`
- Form: `-ffree-form -ffree-line-length-none`
- Strict: `-fimplicit-none`
- Release: `-O3`
- Debug: `-fbounds-check -g -fbacktrace -Wall -Wextra -ftest-coverage`

## Dependencies

- **easifemBase**: Required base library
- **HDF5**: File I/O
- **GMSH SDK**: Mesh generation (optional: `USE_GMSH_SDK`)
- **LIS**: Linear solver library (optional: `USE_LIS`)

## Common Patterns

### Finalizer Pattern

```fortran
PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
FINAL :: obj_Final

MODULE PROCEDURE obj_Final
  CALL obj%DEALLOCATE()
END PROCEDURE obj_Final
```

### Generic Interface with Multiple Implementations

```fortran
INTERFACE GetValue
  MODULE PROCEDURE obj_GetValue1
  MODULE PROCEDURE obj_GetValue2
END INTERFACE GetValue
```

## Best Practices

1. **Always separate interface from implementation** using module/submodule pattern
2. **Keep line length ≤ 78 characters** (fortitude.toml standard)
3. **Use explicit ONLY clauses** in all USE statements
4. **Document all public interfaces** with FORD-compatible comments
5. **Add GPL3 header** to all new files
6. **Use preprocessor guards** for debug code
7. **Follow camelCase** for variables, `EntityName_` for types
8. **Default to PRIVATE** access, explicit PUBLIC exports
9. **Use kind parameters** (`I4B`, `DFP`, `LGT`) from `GlobalData`
10. **Group related methods** in submodules by category (Constructor, Get, Set, IO)

## Quick Reference

- Module interface: `src/modules/ClassName/src/ClassName_Class.F90`
- Submodule impl: `src/submodules/ClassName/src/ClassName_Class@Category.F90`
- Build dir: `$HOME/temp/easifem/classes/build` (default)
- Docs: `./docs/` (generated by FORD)
- Standard types: `I4B` (int32), `DFP` (float64), `LGT` (logical)
