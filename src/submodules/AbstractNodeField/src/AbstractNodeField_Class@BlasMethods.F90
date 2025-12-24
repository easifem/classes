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

SUBMODULE(AbstractNodeField_Class) BlasMethods
USE RealVector_Method, ONLY: Axpy
USE RealVector_Method, ONLY: Copy
USE RealVector_Method, ONLY: Norm1
USE RealVector_Method, ONLY: Norm2
USE RealVector_Method, ONLY: Normi
USE RealVector_Method, ONLY: Dot_Product
USE RealVector_Method, ONLY: PMUL
USE RealVector_Method, ONLY: Reciprocal
USE RealVector_Method, ONLY: SCAL

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                      AXPY1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AXPY1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AXPY1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL AXPY(X=x%realvec, Y=obj%realvec, A=scale)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_AXPY1

!----------------------------------------------------------------------------
!                                                                      AXPY2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AXPY2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AXPY2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL AXPY(X=x1%realvec, Y=obj%realvec, A=a1)
CALL AXPY(X=x2%realvec, Y=obj%realvec, A=a2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_AXPY2

!----------------------------------------------------------------------------
!                                                                      AXPY3
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AXPY3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AXPY3()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL AXPY(x=x1%realvec, y=obj%realvec, a=a1)
CALL AXPY(x=x2%realvec, y=obj%realvec, a=a2)
CALL AXPY(x=x3%realvec, y=obj%realvec, a=a3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_AXPY3

!----------------------------------------------------------------------------
!                                                                       SCAL
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SCAL
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SCAL()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL SCAL(x=obj%realvec, a=scale)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_SCAL

!----------------------------------------------------------------------------
!                                                                       COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_COPY
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_COPY()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL COPY(y=obj%realvec, x=obj2%realvec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_COPY

!----------------------------------------------------------------------------
!                                                                      NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Norm2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Norm2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0.0_DFP
ans = NORM2(obj=obj%realvec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Norm2

!----------------------------------------------------------------------------
!                                                                       NORM1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Norm1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Norm1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0.0
ans = NORM1(obj=obj%realvec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Norm1

!----------------------------------------------------------------------------
!                                                                      NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Normi
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Normi()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0.0_DFP
ans = NORMi(obj=obj%realvec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Normi

!----------------------------------------------------------------------------
!                                                                DOT_PRODUCT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DOT_PRODUCT
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_DOT_PRODUCT()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0.0_DFP
ans = DOT_PRODUCT(obj1=obj%realvec, obj2=obj2%realvec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_DOT_PRODUCT

!----------------------------------------------------------------------------
!                                                                       PMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PMUL
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_PMUL()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL PMUL(obj=obj%realvec, obj1=obj1%realvec, obj2=obj2%realvec)

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Reciprocal(obj1=obj%realvec, obj2=obj%realvec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Reciprocal

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE BlasMethods
