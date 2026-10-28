# AGENTS.md - Development Guide for easifemClasses

This guide provides essential information for AI coding agents and developers working on the EASIFEM (Expandable And Scalable Infrastructure for Finite Element Methods) library.

## Project Overview

- **Language**: Modern Fortran (F2018 standard)
- **Build System**: CMake 3.20+ with Ninja generator
- **License**: GPL-3.0
- **Architecture**: Object-oriented with module/submodule separation
- **Documentation**: FORD (FORtran Documenter)

## Build Commands

Use the `easifem` command line tool to build, install, and clean the
library. Building and installing need explicit permission from the
user. Cleaning needs explicit permission every time.

### Build

```bash
easifem dev classes
```

### Install

```bash
easifem install classes --no-download
```

### Clean

Do not clean the library without explicit permission from the user.

```bash
easifem clean classes
```

### Run an example

Run a test program from its examples folder (only with permission,
see "Examples and tests"):

```bash
easifem run Initiate_test_1.F90
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

```text
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

#### Keeping Docs in Sync with Code

Whenever a method is changed, update its documentation in the same
change. A change is not complete while its docs describe the old
behaviour. This applies to any change of the interface (arguments
added, removed or renamed, intent, optional, defaults) and of the
behaviour (algorithm, modes, checks, errors, return values).

Update all of the following that are affected:

- the docblock of the `INTERFACE` (description, `Arguments:`,
  `Returns:`, `Formula:`, `See also:`), and add an `update:` entry
  (`!   - YYYY-MM-DD: description`);
- the `!!` inline comments of the dummy arguments;
- the one-line `!!` comment of the binding in `TYPE ... CONTAINS`;
- the `TYPE` docblock and the `!!` comments of its components, when
  defaults or the meaning of components change;
- the `Examples` program in the docblock and the matching test in
  `src/modules/<Module>/examples/` (keep both the same);
- the examples and docs of other methods that call the changed method
  or refer to it (for example a renamed binding, or a new required
  argument), including their tests;
- section banners (`!---` blocks) when a procedure is renamed.

After the change, search the module, submodules and examples for the
old name or the removed argument (`grep`) to make sure nothing still
refers to it. If some of the affected docs or tests are left for
later, say so explicitly in the report to the user.

Do not list the documentation changes in the report to the user;
updating the docs is expected with every change. Report only the
changes of code and behaviour, the checks that were run, and the open
questions (including docs or tests that were not updated).

#### Examples Section in Docblocks

- Every public routine (or generic interface) has one `Examples`
  section, written as `! Examples` on its own line. Do not name it
  `Example`, and do not add a separate short `Example:` snippet.
- The example is a complete, runnable program: `PROGRAM main` ...
  `END PROGRAM main`, with all `USE ..., ONLY:` lines (one item per
  line), `IMPLICIT NONE`, declarations, the calls, and `CALL OK(...)`
  checks from `Test_Method` so the result can be verified.
- Fence the program with `` !```fortran `` and `` !``` `` on their own
  comment lines. Every line in the block starts with `!` (plain `!`
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

Error checks are done in DEBUG mode only. The release build does not
check or report errors and has no fallbacks for wrong input (for
example, an output array may be left uninitialised). Users run their
program in DEBUG mode first, and use the release build only when it
runs without errors. Therefore:

- put every error check inside `#ifdef DEBUG_VER ... #endif`;
- declare `modName`, `myName`, and the variables used only by the
  checks (e.g. `isok`) inside `#ifdef DEBUG_VER` as well;
- do not add release-mode handling of errors, and do not report its
  absence as a problem in reviews.

Use the exception handler, or `AssertError1/2/3` from
`src/submodules/include/errors.F90` (included at the end of the
submodule):

```fortran
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
IF (error_condition) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[ERROR] :: Error description')
END IF
#endif

#ifdef DEBUG_VER
CALL AssertError1(.NOT. error_condition, myName, 'Error description')
#endif
```

### Preprocessor Usage

#### Debug Guards

Every procedure (module procedures and internal helper routines) raises
a `[START]` message at its beginning and an `[END]` message at its end,
both in DEBUG mode only. If a procedure does not have them yet, add them
when you edit it. If the procedure returns early (`RETURN`), raise the
`[END]` message before the `RETURN` as well.

```fortran
MODULE PROCEDURE obj_MethodName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_MethodName()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (nothing_to_do) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

! ... body ...

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_MethodName
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
   - **Update the docs whenever a method changes** (see "Keeping Docs in Sync with Code")
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
