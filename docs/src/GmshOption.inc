
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


! /* Set a numerical option to `value'. `name' is of the form "category.option"
!  * or "category[num].option". Available categories and options are listed in
!  * the Gmsh reference manual. */
!
! GMSH_API void gmshOptionSetNumber(const char *name,
!                                   const double value,
!                                   int *ierr);

INTERFACE
SUBROUTINE gmshOptionSetNumber(name, val, ierr) &
  & BIND( C, NAME="gmshOptionSetNumber" )
  USE ISO_C_BINDING, ONLY : C_PTR, C_DOUBLE, C_INT
  TYPE( C_PTR ), VALUE, INTENT( IN ) :: name
  REAL( C_DOUBLE ), VALUE, INTENT( IN ) :: val
  INTEGER( C_INT ), INTENT(OUT) :: ierr
END SUBROUTINE gmshOptionSetNumber
END INTERFACE

PUBLIC :: gmshOptionSetNumber


! /* Get the `value' of a numerical option. `name' is of the form
!  * "category.option" or "category[num].option". Available categories and
!  * options are listed in the Gmsh reference manual. */
!
! GMSH_API void gmshOptionGetNumber(const char *name,
!                                   double *value,
!                                   int *ierr);

INTERFACE
SUBROUTINE gmshOptionGetNumber(name, val, ierr) &
  & BIND( C, NAME="gmshOptionGetNumber" )
  USE ISO_C_BINDING, ONLY : C_PTR, C_DOUBLE, C_INT
  TYPE( C_PTR ), VALUE, INTENT( IN ) :: name
  REAL( C_DOUBLE ), INTENT( OUT ) :: val
  INTEGER( C_INT ), INTENT(OUT) :: ierr
END SUBROUTINE gmshOptionGetNumber
END INTERFACE

PUBLIC :: gmshOptionGetNumber

! /* Set a string option to `value'. `name' is of the form "category.option" or
!  * "category[num].option". Available categories and options are listed in the
!  * Gmsh reference manual. */
!
! GMSH_API void gmshOptionSetString(const char *name,
!                                   const char *value,
!                                   int *ierr);

INTERFACE
SUBROUTINE gmshOptionSetString(name, val, ierr) &
  & BIND( C, NAME="gmshOptionSetString" )
  USE ISO_C_BINDING, ONLY : C_PTR, C_INT
  TYPE( C_PTR ), VALUE, INTENT( IN ) :: name
  TYPE( C_PTR ), VALUE, INTENT( IN ) :: val
  INTEGER( C_INT ), INTENT(OUT) :: ierr
END SUBROUTINE gmshOptionSetString
END INTERFACE

PUBLIC :: gmshOptionSetString

! /* Get the `value' of a string option. `name' is of the form "category.option"
!  * or "category[num].option". Available categories and options are listed in
!  * the Gmsh reference manual. */
! GMSH_API void gmshOptionGetString(const char *name,
!                                   char **value,
!                                   int *ierr);

INTERFACE
SUBROUTINE gmshOptionGetString(name, val, ierr) &
  & BIND( C, NAME="gmshOptionGetString" )
  USE ISO_C_BINDING, ONLY : C_PTR, C_INT
  TYPE( C_PTR ), VALUE, INTENT( IN ) :: name
  TYPE( C_PTR ), INTENT( IN ) :: val
  INTEGER( C_INT ), INTENT(OUT) :: ierr
END SUBROUTINE gmshOptionGetString
END INTERFACE

PUBLIC :: gmshOptionGetString

! /* Set a color option to the RGBA value (`r', `g', `b', `a'), where where `r',
!  * `g', `b' and `a' should be integers between 0 and 255. `name' is of the
!  * form "category.option" or "category[num].option". Available categories and
!  * options are listed in the Gmsh reference manual, with the "Color." middle
!  * string removed. */
! GMSH_API void gmshOptionSetColor(const char *name,
!                                  const int r,
!                                  const int g,
!                                  const int b,
!                                  const int a,
!                                  int *ierr);

INTERFACE
SUBROUTINE gmshOptionSetColor(name, r, g, b, a, ierr) &
  & BIND( C, NAME="gmshOptionSetColor" )
  USE ISO_C_BINDING, ONLY : C_PTR, C_INT
  TYPE( C_PTR ), VALUE, INTENT( IN ) :: name
  INTEGER( C_INT ), VALUE, INTENT(IN) :: r, g, b, a
  INTEGER( C_INT ), INTENT(OUT) :: ierr
END SUBROUTINE gmshOptionSetColor
END INTERFACE

PUBLIC :: gmshOptionSetColor

! /* Get the `r', `g', `b', `a' value of a color option. `name' is of the form
!  * "category.option" or "category[num].option". Available categories and
!  * options are listed in the Gmsh reference manual, with the "Color." middle
!  * string removed. */
! GMSH_API void gmshOptionGetColor(const char *name,
!                                  int *r,
!                                  int *g,
!                                  int *b,
!                                  int *a,
!                                  int *ierr);

INTERFACE
SUBROUTINE gmshOptionGetColor(name, r, g, b, a, ierr) &
  & BIND( C, NAME="gmshOptionGetColor" )
  USE ISO_C_BINDING, ONLY : C_PTR, C_INT
  TYPE( C_PTR ), VALUE, INTENT( IN ) :: name
  INTEGER( C_INT ), INTENT(OUT) :: r, g, b, a
  INTEGER( C_INT ), INTENT(OUT) :: ierr
END SUBROUTINE gmshOptionGetColor
END INTERFACE

PUBLIC :: gmshOptionGetColor