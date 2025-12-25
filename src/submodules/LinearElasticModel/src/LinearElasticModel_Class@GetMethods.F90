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

SUBMODULE(LinearElasticModel_Class) GetMethods
USE Display_Method, ONLY: ToString
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            GetElasticParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElasticParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElasticParam()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%GetParam( &
  nu=poissonRatio, G=shearModulus, youngsModulus=youngsModulus, &
  lambda=lambda, C=C, invC=invC, stiffnessPower=stiffnessPower)

isok = PRESENT(nrowC)
IF (isok) nrowC = obj%nc

isok = PRESENT(ncolC)
IF (isok) ncolC = obj%nc

isok = PRESENT(nrowInvC)
IF (isok) nrowInvC = obj%nc

isok = PRESENT(ncolInvC)
IF (isok) ncolInvC = obj%nc

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElasticParam

!----------------------------------------------------------------------------
!                                                               GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(elasticityType)
IF (isok) elasticityType = obj%elasticityType

isok = PRESENT(nu)
IF (isok) nu = obj%nu

isok = PRESENT(G)
IF (isok) G = obj%G

isok = PRESENT(youngsModulus)
IF (isok) youngsModulus = obj%E

isok = PRESENT(lambda)
IF (isok) lambda = obj%lambda

isok = PRESENT(C)
IF (isok) C(1:obj%nc, 1:obj%nc) = obj%C(1:obj%nc, 1:obj%nc)

isok = PRESENT(invC)
IF (isok) invC(1:obj%nc, 1:obj%nc) = obj%invC(1:obj%nc, 1:obj%nc)

isok = PRESENT(stiffnessPower)
IF (isok) stiffnessPower = obj%stiffnessPower

isok = PRESENT(nc)
IF (isok) nc = obj%nc

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                                       GetC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetC()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%nc
ncol = obj%nc
C(1:nrow, 1:nrow) = obj%C(1:nrow, 1:ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetC

!----------------------------------------------------------------------------
!                                                                    GetinvC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetinvC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetinvC()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%nc
ncol = obj%nc
invC(1:nrow, 1:ncol) = obj%invC(1:nrow, 1:ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetinvC

!----------------------------------------------------------------------------
!                                                         GetElasticityType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElasticityType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElasticityType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%elasticityType

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElasticityType

!----------------------------------------------------------------------------
!                                                               GetDataSize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDataSize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDataSize()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0
SELECT CASE (obj%elasticityType)
CASE (TypeElasticityOpt%isotropic)
  ans = 2
CASE (TypeElasticityOpt%anisotropic)
  ans = 21
CASE (TypeElasticityOpt%transIsotropic)
  ans = 5
CASE (TypeElasticityOpt%orthotropic)
  ans = 9

#ifdef DEBUG_VER
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
          'No case found for elasticityType = '//ToString(obj%elasticityType))
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDataSize

!----------------------------------------------------------------------------
!                                                                    GetData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%elasticityType)
CASE (TypeElasticityOpt%isotropic)
  CALL LinearElasticModelGetData_Iso(obj, DATA)
CASE (TypeElasticityOpt%anisotropic)
  CALL LinearElasticModelGetData_Aniso(obj, DATA)
CASE (TypeElasticityOpt%transIsotropic)
  CALL LinearElasticModelGetData_Trans(obj, DATA)
CASE (TypeElasticityOpt%orthotropic)
  CALL LinearElasticModelGetData_Ortho(obj, DATA)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
          'No case found for elasticityType = '//ToString(obj%elasticityType))
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetData

!----------------------------------------------------------------------------
!                                             LinearElasticModelGetData_Iso
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-25
! summary:  Get the  data from the model for Isotropic elasticity
!
!# Introduction
!  This routine returns the data for Isotropic linear elasticity
! Data(1) contains the lambda
! Data(2) contains the G

SUBROUTINE LinearElasticModelGetData_Iso(obj, DATA, tsize)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: DATA(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "LinearElasticModelGetData_Iso()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  DATA(1) = obj%lambda
  DATA(2) = obj%G
  tsize = 2

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE LinearElasticModelGetData_Iso

!----------------------------------------------------------------------------
!                                            LinearElasticModelGetData_Aniso
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the  data from the model for anisotropic elasticity
SUBROUTINE LinearElasticModelGetData_Aniso(obj, DATA, tsize)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: DATA(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "LinearElasticModelGetData_Aniso()"
#endif

  INTEGER(I4B) :: ii, jj, kk

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  tsize = 0
  DO jj = 1, 6
    DO ii = jj, 6
      tsize = tsize + 1
      DATA(tsize) = obj%C(ii, jj)
    END DO
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE LinearElasticModelGetData_Aniso

!----------------------------------------------------------------------------
!                                           LinearElasticModelGetData_Ortho
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the  data from the model for Orthotropic elasticity

SUBROUTINE LinearElasticModelGetData_Ortho(obj, DATA, tsize)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: DATA(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "LinearElasticModelGetData_Ortho()"
#endif

  INTEGER(I4B) :: ii

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  tsize = 9

  DO ii = 1, 6
    DATA(ii) = obj%C(ii, ii)
  END DO
  DATA(7) = obj%C(1, 2)
  DATA(8) = obj%C(2, 3)
  DATA(9) = obj%C(1, 3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE LinearElasticModelGetData_Ortho

!----------------------------------------------------------------------------
!                                           LinearElasticModelGetData_Trans
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-30
! summary:  Get the  data from the model for Trans Isotropic elasticity

SUBROUTINE LinearElasticModelGetData_Trans(obj, DATA, tsize)
  CLASS(LinearElasticModel_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: DATA(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "LinearElasticModelGetData_Trans()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  tsize = 5

  DATA(1) = obj%C(1, 1)
  DATA(2) = obj%C(3, 3)
  DATA(3) = obj%C(5, 5)
  DATA(4) = obj%C(1, 2)
  DATA(5) = obj%C(1, 3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE LinearElasticModelGetData_Trans

END SUBMODULE GetMethods
