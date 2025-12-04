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
!

SUBMODULE(ScalarFieldLis_Class) BlasMethods
IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                                     AXPY1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AXPY1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AXPY1()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(x%lis_ptr, ierr)
CALL CHKERR(ierr)
#endif

! alpha, x, y, ierr
CALL lis_vector_axpy(scale, x%lis_ptr, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_AXPY1

!----------------------------------------------------------------------------
!                                                                   AXPY2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AXPY2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AXPY2()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
CALL lis_vector_is_null(x1%lis_ptr, ierr)
CALL CHKERR(ierr)
CALL lis_vector_is_null(x2%lis_ptr, ierr)
CALL CHKERR(ierr)
#endif

! alpha, x, y, ierr
CALL lis_vector_axpy(a1, x1%lis_ptr, obj%lis_ptr, ierr)
CALL lis_vector_axpy(a2, x2%lis_ptr, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_AXPY2

!----------------------------------------------------------------------------
!                                                                   AXPY3
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AXPY3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AXPY3()"
#endif

INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
CALL lis_vector_is_null(x1%lis_ptr, ierr)
CALL CHKERR(ierr)
CALL lis_vector_is_null(x2%lis_ptr, ierr)
CALL CHKERR(ierr)
CALL lis_vector_is_null(x3%lis_ptr, ierr)
CALL CHKERR(ierr)
#endif

! alpha, x, y, ierr
CALL lis_vector_axpy(a1, x1%lis_ptr, obj%lis_ptr, ierr)
CALL lis_vector_axpy(a2, x2%lis_ptr, obj%lis_ptr, ierr)
CALL lis_vector_axpy(a3, x3%lis_ptr, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_AXPY3

!----------------------------------------------------------------------------
!                                                                     SCAL
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SCAL
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SCAL()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
#endif

! alpha, x, ierr
CALL lis_vector_scale(scale, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_SCAL

!----------------------------------------------------------------------------
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_COPY
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_COPY()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)

CALL lis_vector_is_null(obj2%lis_ptr, ierr)
CALL CHKERR(ierr)
#endif

CALL lis_vector_copy(obj2%lis_ptr, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_COPY

!----------------------------------------------------------------------------
!                                                                    NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Norm2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Norm2()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
#endif

ans = 0.0_DFP
CALL lis_vector_nrm2(obj%lis_ptr, ans, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Norm2

!----------------------------------------------------------------------------
!                                                                    NORM1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Norm1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Norm1()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
#endif

ans = 0.0
CALL lis_vector_nrm1(obj%lis_ptr, ans, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Norm1

!----------------------------------------------------------------------------
!                                                                    NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Normi
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Normi()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
#endif

ans = 0.0_DFP
CALL lis_vector_nrmi(obj%lis_ptr, ans, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Normi

!----------------------------------------------------------------------------
!                                                               DOT_PRODUCT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DOT_PRODUCT
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_DOT_PRODUCT()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)

CALL lis_vector_is_null(obj2%lis_ptr, ierr)
CALL CHKERR(ierr)
#endif

ans = 0.0_DFP
CALL lis_vector_dot(obj%lis_ptr, obj2%lis_ptr, ans, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_DOT_PRODUCT

!----------------------------------------------------------------------------
!                                                               DOT_PRODUCT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PMUL
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_PMUL()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
CALL lis_vector_is_null(obj1%lis_ptr, ierr)
CALL CHKERR(ierr)
CALL lis_vector_is_null(obj2%lis_ptr, ierr)
CALL CHKERR(ierr)
#endif

CALL lis_vector_pmul(obj1%lis_ptr, obj2%lis_ptr, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_PMUL

!----------------------------------------------------------------------------
!                                                                 Reciprocal
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Reciprocal
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Reciprocal()"
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
#endif

CALL lis_vector_reciprocal(obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Reciprocal

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE BlasMethods
