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

### Examples and tests

- Example/test programs are standalone programs named
  `<Name>_test_<N>.F90` in `src/modules/<Module>/examples/`.
- Do not add the GPL header to test programs. A test file starts
  directly with its `!> author:` docblock (or with `PROGRAM main`).
- These programs are the source for the `Examples` section of the
  docblocks (see "FORD Docblock Style").
- Do not compile the library or run examples without explicit
  permission from the user.

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

!> author: Your Name, Ph. D.
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

#### FORD Docblock Style

Rules for the docblock above each `INTERFACE` (and above each `TYPE`):

1. Use `!>` only on the *first line* of a docblock. All the following
   lines of the same block use a plain `!`.
2. The first line is always `!> author: Name, Ph. D.` (singular
   `author`, not `authors`). Do not add title lines such as
   `!# Initiate`, `!# Introduction`, `!> Subroutine: name`; the name is
   already given by the declaration that follows the block.
3. Mandatory fields: `date:`, an optional `update:` list
   (`!   - YYYY-MM-DD: description`), and `summary:`. A summary that
   needs more than one line continues on an indented `!` line.
4. After the fields, use the following sections in this order (omit a
   section only if it does not apply):
   - Description: one or more paragraphs explaining what the routine
     does, its algorithm steps, and the rules it follows.
   - `Arguments:` short list `name : TYPE - meaning`; give array shapes
     with `SIZE(...)` where relevant.
   - `Returns:` only for functions.
   - `Formula:` only when there is real mathematics (data layout,
     index rules, etc.); use LaTeX display math `$$ ... $$`.
   - `Examples` (see below).
   - `See also:` links to related procedures with `[[ProcName]]`.
5. Keep the `!!` inline comments on every dummy argument. They are the
   source of truth for argument meaning; fill in the missing ones.
6. For type-bound procedures, the full docblock goes on the
   `INTERFACE` of the specific procedure (`obj_XXX`). Inside
   `TYPE ... CONTAINS`, each binding keeps only a one-line `!!`
   comment. The `TYPE` itself gets a docblock describing the data
   layout and the meaning of its components.
7. Keep docblocks factual. Do not copy-paste descriptions from another
   routine without adapting them (argument names, called methods,
   section banners must match the routine being documented).
8. Wrap every docblock line to 78 characters or less. Use only simple
   LaTeX that the documentation renderer supports.

#### Examples Section in Docblocks

- Every public routine (or generic interface) has one `Examples`
  section, written as `! Examples` on its own line. Do not name it
  `Example`, and do not add a separate short `Example:` snippet.
- The example is a complete, runnable program: `PROGRAM main` ...
  `END PROGRAM main`, with all `USE ..., ONLY:` lines (one item per
  line), `IMPLICIT NONE`, declarations, the calls, and `CALL OK(...)`
  checks from `Test_Method` so the result can be verified.
- Fence the program with `` !```fortran `` and `` !``` `` on their own
  comment lines. Every line in the block starts with `! ` (plain `!`
  and one space); never `!>`.
- The program is normally the same as the matching test in
  `src/modules/<Module>/examples/`, without the `!> author:` docblock
  of the test file. Helper procedures go after `CONTAINS` inside the
  program.
- For generic interfaces (`Initiate`, `Set`, `Insert`, ...) write one
  example per distinct use case, not one per specific procedure. Thin
  wrapper procedures refer to the main one via `See also:`.
- Use only the current API. Old examples in the website docs that use
  removed APIs (for example `ParameterList_`-based initiation) must be
  updated before being used.

Example docblock:

```fortran
!> author: Vikas Sharma, Ph. D.
! date: 2026-02-27
! summary: Get the rank of mesh field
!
! Returns the rank (scalar, vector, or matrix) of the data stored in
! obj.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!
! Returns:
!   ans : INTEGER(I4B) - rank of the field
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: I4B
! USE MeshField_Class, ONLY: MeshField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(MeshField_) :: obj
! ! ... initiate obj ...
! CALL OK(obj%GetRank() .EQ. 0_I4B, "GetRank")
! END PROGRAM main
!```
!
! See also: [[obj_GetVarType]]

INTERFACE
  MODULE FUNCTION obj_GetRank(obj) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! mesh field
    INTEGER(I4B) :: ans
    !! rank of the field
  END FUNCTION obj_GetRank
END INTERFACE
```

(The example above is abbreviated; real docblocks must contain the
complete program.)

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
4. **Document all public interfaces** with FORD docblocks (see "FORD Docblock Style"), including a runnable `Examples` program
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
