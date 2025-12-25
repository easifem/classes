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
! date: 27 Aug 2021
! summary: This submodule contains input-output methods

SUBMODULE(LinearElasticModel_Class) HDFMethods
USE Display_Method, ONLY: Display, ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Import
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Import
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Import()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

! INTEGER(I4B) :: elasticityType
! TYPE(String) :: dsetname, strval
! LOGICAL(LGT) :: isPlaneStrain, isPlaneStress, isIsotropic
! LOGICAL(LGT) :: isok
! REAL(DFP) :: poissonRatio, youngsModulus, shearModulus, lambda, stiffnessPower
! REAL(DFP), ALLOCATABLE :: C(:, :), invC(:, :)
! TYPE(ParameterList_) :: param
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! #ifdef DEBUG_VER
! isok = .NOT. obj%isInitiated()
! CALL AssertError1(isok, myName, &
!                   'The object is already initiated, deallocate first!')
! #endif
!
! #ifdef DEBUG_VER
! isok = hdf5%isOpen()
! CALL AssertError1(isok, myName, &
!                   'HDF5 file is not opened')
! #endif
!
! #ifdef DEBUG_VER
! isok = hdf5%isRead()
! CALL AssertError1(isok, myName, &
!                   'HDF5 file does not have read permission')
! #endif
!
! ! READ name
! dsetname = TRIM(group)//"/name"
!
! #ifdef DEBUG_VER
! isok = hdf5%pathExists(dsetname%chars())
! CALL AssertError1(isok, myName, &
!                   'The dataset name should be present')
! #endif
!
! CALL hdf5%READ(dsetname=dsetname%chars(), vals=obj%name)
!
! ! READ elasticityType
! dsetname = TRIM(group)//"/elasticityType"
!
! #ifdef DEBUG_VER
! isok = hdf5%pathExists(dsetname%chars())
! CALL AssertError1(isok, myName, &
!                   'The dataset elasticityType should be present')
! #endif
! CALL hdf5%READ(dsetname=dsetname%chars(), vals=strval)
! elasticityType = TypeElasticityOpt%ToNumber(strval%chars())
!
! ! READ isPlaneStrain
! isPlaneStrain = .FALSE.
! dsetname = TRIM(group)//"/isPlaneStrain"
! isok = hdf5%pathExists(dsetname%chars())
! IF (isok) CALL hdf5%READ(dsetname=dsetname%chars(), vals=isPlaneStrain)
!
! ! READ isPlaneStress
! isPlaneStress = .FALSE.
! dsetname = TRIM(group)//"/isPlaneStress"
! isok = hdf5%pathExists(dsetname%chars())
! IF (isok) CALL hdf5%READ(dsetname=dsetname%chars(), vals=isPlaneStress)
!
! isIsotropic = elasticityType .EQ. TypeElasticityOpt%isotropic
!
! ! If isotropic then read poissonRatio, youngsModulus, shearModulus, lambda,
! IF (isIsotropic) THEN
!   dsetname = TRIM(group)//"/poissonRatio"
!
! #ifdef DEBUG_VER
!   isok = hdf5%pathExists(dsetname%chars())
!   CALL AssertError1(isok, myName, &
!                     'The dataset poissonRatio should be present')
! #endif
!
!   CALL hdf5%READ(dsetname=dsetname%chars(), vals=poissonRatio)
!
!   dsetname = TRIM(group)//"/youngsModulus"
!
! #ifdef DEBUG_VER
!   isok = hdf5%pathExists(dsetname%chars())
!   CALL AssertError1(isok, myName, &
!                     'The dataset youngsModulus should be present')
! #endif
!   CALL hdf5%READ(dsetname=dsetname%chars(), vals=youngsModulus)
!
!   dsetname = TRIM(group)//"/shearModulus"
! #ifdef DEBUG_VER
!   isok = hdf5%pathExists(dsetname%chars())
!   CALL AssertError1(isok, myName, &
!                     'The dataset shearModulus should be present')
! #endif
!   CALL hdf5%READ(dsetname=dsetname%chars(), vals=shearModulus)
!
!   dsetname = TRIM(group)//"/lambda"
! #ifdef DEBUG_VER
!   isok = hdf5%pathExists(dsetname%chars())
!   CALL AssertError1(isok, myName, &
!                     'The dataset lambda should be present')
! #endif
!   CALL hdf5%READ(dsetname=dsetname%chars(), vals=lambda)
!
! END IF
!
! ! if not isotropic then read C and invC
!
! IF (.NOT. isIsotropic) THEN
!   dsetname = TRIM(group)//"/c"
!
! #ifdef DEBUG_VER
!   isok = hdf5%pathExists(dsetname%chars())
!   CALL AssertError1(isok, myName, &
!                     'The dataset c should be present')
! #endif
!
!   CALL hdf5%READ(dsetname=dsetname%chars(), vals=C)
!
!   dsetname = TRIM(group)//"/invC"
! #ifdef DEBUG_VER
!   isok = hdf5%pathExists(dsetname%chars())
!   CALL AssertError1(isok, myName, &
!                     'The dataset invC should be present')
! #endif
!
!   CALL hdf5%READ(dsetname=dsetname%chars(), vals=invC)
! END IF
!
! stiffnessPower = 0.0_DFP
! dsetname = TRIM(group)//"/stiffnessPower"
! isok = hdf5%pathExists(dsetname%chars())
! IF (isok) CALL hdf5%READ(dsetname=dsetname%chars(), vals=stiffnessPower)
!
! CALL param%initiate()
!
! CALL SetLinearElasticModelParam(param=param, &
!                                 elasticityType=elasticityType, &
!                                 isPlaneStrain=isPlaneStrain, &
!                                 isPlaneStress=isPlaneStress, &
!                                 poissonRatio=poissonRatio, &
!                                 youngsModulus=youngsModulus, &
!                                 shearModulus=shearModulus, &
!                                 lambda=lambda, &
!                                 stiffnessPower=stiffnessPower, &
!                                 C=C, invC=invC)
!
! CALL obj%Initiate(param)
!
! CALL param%DEALLOCATE()
!
! IF (ALLOCATED(C)) DEALLOCATE (C)
! IF (ALLOCATED(invC)) DEALLOCATE (invC)
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[END] ')
! #endif
END PROCEDURE obj_Import

!----------------------------------------------------------------------------
!                                                                    Export
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Export
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Export()"
#endif

TYPE(String) :: dsetname, strval
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%isInitiated()
CALL AssertError1(isok, myName, &
                  'The object is not initiated, initiate first!')
#endif

#ifdef DEBUG_VER
isok = hdf5%isOpen()
CALL AssertError1(isok, myName, &
                  'HDF5 file is not opened')
#endif

#ifdef DEBUG_VER
isok = hdf5%isWrite()
CALL AssertError1(isok, myName, &
                  'HDF5 file does not have write permission')
#endif

! WRITE name
dsetname = TRIM(group)//"/name"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%name)

! WRITE elasticityType
dsetname = TRIM(group)//"/elasticityType"
strval = TypeElasticityOpt%ToString(obj%elasticityType)
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=strval)

! WRITE isPlaneStrain
dsetname = TRIM(group)//"/isPlaneStrain"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isPlaneStrain())

! WRITE isPlaneStress
dsetname = TRIM(group)//"/isPlaneStress"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%isPlaneStress())

! C and invC
isok = obj%elasticityType .EQ. TypeElasticityOpt%isotropic
IF (isok) THEN
  dsetname = TRIM(group)//"/poissonRatio"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%nu)
  dsetname = TRIM(group)//"/youngsModulus"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%E)
  dsetname = TRIM(group)//"/shearModulus"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%G)
  dsetname = TRIM(group)//"/lambda"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%lambda)
  dsetname = TRIM(group)//"/c"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%C)
  dsetname = TRIM(group)//"/invC"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%invC)
ELSE
  dsetname = TRIM(group)//"/c"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%C)
  dsetname = TRIM(group)//"/invC"
  CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%invC)
END IF

dsetname = TRIM(group)//"/stiffnessPower"
CALL hdf5%WRITE(dsetname=dsetname%chars(), vals=obj%stiffnessPower)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Export

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE HDFMethods
