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

MODULE GmshOnelabInterface
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_DOUBLE
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_PTR
IMPLICIT NONE
PRIVATE

PUBLIC :: gmshOnelabSet
PUBLIC :: gmshOnelabGet
PUBLIC :: gmshOnelabGetNames
PUBLIC :: gmshOnelabGetNumber
PUBLIC :: gmshOnelabSetString
PUBLIC :: gmshOnelabGetString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Set one or more parameters in the ONELAB database
!
!# GmshOnelabSet
!
! Set one or more parameters in the ONELAB database, encoded in `format'.
!
!### CInterface
!
!```c
! GMSH_API void gmshOnelabSet(const char *data,
! const char *format,
! int *ierr);
!```

INTERFACE
  SUBROUTINE gmshOnelabSet(DATA, FORMAT, ierr) &
    BIND(C, name="gmshOnelabSet")
    IMPORT
    TYPE(C_PTR), VALUE, INTENT(IN) :: DATA
    TYPE(C_PTR), VALUE, INTENT(IN) :: FORMAT
    INTEGER(C_INT), INTENT(IN) :: ierr
  END SUBROUTINE gmshOnelabSet
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Get all the parameters from the ONELAB database
!
!# GmshOnelabGet
!
! Get all the parameters (or a single one if `name` is specified) from the
! ONELAB database, encoded in `format'.
!
!
!### CInterface
!
!```c
! GMSH_API void gmshOnelabGet(char **data,
!                             const char *name,
!                             const char *format,
!                             int *ierr);
!```

INTERFACE
  SUBROUTINE gmshOnelabGet(DATA, name, FORMAT, ierr) &
    BIND(C, name="gmshOnelabGet")
    IMPORT
    TYPE(C_PTR), INTENT(IN) :: DATA
    TYPE(C_PTR), VALUE, INTENT(IN) :: name
    TYPE(C_PTR), VALUE, INTENT(IN) :: FORMAT
    INTEGER(C_INT), INTENT(IN) :: ierr
  END SUBROUTINE gmshOnelabGet
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Get the names of the parameters in the ONELAB database
!
!# GmshOnelabGetNames
!
!  Get the names of the parameters in the ONELAB database matching the
!  `search` regular expression. If `search` is empty, return all the names.
!
!### CInterface
!
!```c
! GMSH_API void gmshOnelabGetNames(char ***names, size_t *names_n,
!                                  const char *search,
!                                  int *ierr);
!```

INTERFACE
  SUBROUTINE gmshOnelabGetNames(names, names_n, search, ierr) &
    BIND(C, name="gmshOnelabGetNames")
    IMPORT
    TYPE(C_PTR), INTENT(IN) :: names
    INTEGER(C_SIZE_T), INTENT(out) :: names_n
    TYPE(C_PTR), VALUE, INTENT(IN) :: search
    INTEGER(C_INT), INTENT(IN) :: ierr
  END SUBROUTINE gmshOnelabGetNames
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:
!
!# GmshOnelabGetNumber
!
! Get the value of the number parameter `name` from the ONELAB database.
!  Return an empty vector if the parameter does not exist.
!
!
!### CInterface
!
!```c
! GMSH_API void gmshOnelabGetNumber(const char *name,
! double **value, size_t *value_n, int *ierr);
!```

INTERFACE
  SUBROUTINE gmshOnelabGetNumber(name, VALUE, value_n, ierr) &
    BIND(C, name="gmshOnelabGetNumber")
    IMPORT
    TYPE(C_PTR), VALUE, INTENT(IN) :: name
    TYPE(C_PTR), INTENT(IN) :: VALUE
    INTEGER(C_SIZE_T), INTENT(out) :: value_n
    INTEGER(C_INT), INTENT(IN) :: ierr
  END SUBROUTINE gmshOnelabGetNumber
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:
!
!# GmshOnelabSetString
!
! Set the value of the string parameter `name` in the ONELAB database. Create
! the parameter if it does not exist; update the value if the parameter
! exists.
!
!### CInterface
!
!```c
! GMSH_API void gmshOnelabSetString(const char *name,
!                                   char **value, size_t value_n,
!                                   int *ierr);
!```

INTERFACE
  SUBROUTINE gmshOnelabSetString(name, VALUE, value_n, ierr) &
    BIND(C, name="gmshOnelabSetString")
    IMPORT
    TYPE(C_PTR), VALUE, INTENT(IN) :: name
    TYPE(C_PTR), INTENT(IN) :: VALUE
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: value_n
    INTEGER(C_INT), INTENT(IN) :: ierr
  END SUBROUTINE gmshOnelabSetString
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Get the value of the string parameter `name`
!
!# GmshOnelabGetString
!
!  Get the value of the string parameter `name` from the ONELAB database.
!  Return an empty vector if the parameter does not exist.
!
!### CInterface
!
!```c
! GMSH_API void gmshOnelabGetString(const char *name,
!                                   char ***value, size_t *value_n,
!                                   int *ierr);
!```

INTERFACE
  SUBROUTINE gmshOnelabGetString(name, VALUE, value_n, ierr) &
    BIND(C, name="gmshOnelabGetString")
    IMPORT
    TYPE(C_PTR), VALUE, INTENT(IN) :: name
    TYPE(C_PTR), INTENT(IN) :: VALUE
    INTEGER(C_SIZE_T), INTENT(out) :: value_n
    INTEGER(C_INT), INTENT(IN) :: ierr
  END SUBROUTINE gmshOnelabGetString
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshOnelabInterface
