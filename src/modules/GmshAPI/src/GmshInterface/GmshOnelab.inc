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

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Set one or more parameters in the ONELAB database, encoded in `format'.
!
!### Introduction
!
! Set one or more parameters in the ONELAB database, encoded in `format'.
!
!
!### CInterface
!
!```c
! GMSH_API void gmshOnelabSet(const char *data,
! const char *format,
! int *ierr);
!```

INTERFACE
SUBROUTINE gmshOnelabSet(data, format, ierr) &
  & BIND(C, name = "gmshOnelabSet")
  IMPORT
  _CPTR_V_IN_ :: data
  _CPTR_V_IN_ :: format
  _I_OUT_ :: ierr
END SUBROUTINE gmshOnelabSet
END INTERFACE

PUBLIC :: gmshOnelabSet

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Get all the parameters from the ONELAB database
!
!### Introduction
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
SUBROUTINE gmshOnelabGet(data, name, format, ierr) &
  & BIND(C, name = "gmshOnelabGet")
  IMPORT
  _CPTR_IN_ :: data
  _CPTR_V_IN_ :: name
  _CPTR_V_IN_ :: format
  _I_OUT_ :: ierr
END SUBROUTINE gmshOnelabGet
END INTERFACE

PUBLIC :: gmshOnelabGet

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Get the names of the parameters in the ONELAB database
!
!### Introduction
!
!  Get the names of the parameters in the ONELAB database matching the
!  `search` regular expression. If `search` is empty, return all the names.
!
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
  & BIND(C, name="gmshOnelabGetNames")
  IMPORT
  _CPTR_IN_ :: names
  _ST_OUT_ :: names_n
  _CPTR_V_IN_ :: search
  _I_OUT_ :: ierr
END SUBROUTINE gmshOnelabGetNames
END INTERFACE

PUBLIC :: gmshOnelabGetNames

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:
!
!### Introduction
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
SUBROUTINE gmshOnelabGetNumber( name, value, value_n, ierr ) &
  & BIND(C, name="gmshOnelabGetNumber")
  IMPORT
  _CPTR_V_IN_ :: name
  _CPTR_IN_ :: value
  _ST_OUT_ :: value_n
  _I_OUT_ :: ierr
END SUBROUTINE gmshOnelabGetNumber
END INTERFACE

PUBLIC :: gmshOnelabGetNumber

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary:
!
!### Introduction
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
SUBROUTINE gmshOnelabSetString( name, value, value_n, ierr )&
  & BIND( C, name="gmshOnelabSetString" )
  IMPORT
  _CPTR_V_IN_ :: name
  _CPTR_IN_ :: value
  _ST_V_IN_ :: value_n
  _I_OUT_ :: ierr
END SUBROUTINE gmshOnelabSetString
END INTERFACE

PUBLIC :: gmshOnelabSetString

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 Sept 2021
! summary: Get the value of the string parameter `name`
!
!### Introduction
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
SUBROUTINE gmshOnelabGetString(name, value, value_n, ierr ) &
  & BIND( C, name="gmshOnelabGetString")
  IMPORT
  _CPTR_V_IN_ :: name
  _CPTR_IN_ :: value
  _ST_OUT_ :: value_n
  _I_OUT_ :: ierr
END SUBROUTINE gmshOnelabGetString
END INTERFACE

PUBLIC :: gmshOnelabGetString