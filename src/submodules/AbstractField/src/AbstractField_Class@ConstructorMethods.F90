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

SUBMODULE(AbstractField_Class) ConstructorMethods
USE InputUtility, ONLY: Input
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "AbstractField_Class@ConstructorMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
isok = obj2%IsInitiated()
CALL AssertError1(isok, myName, &
                  'obj2 is not initiated.')
#endif

#ifdef DEBUG_VER
isok = .NOT. obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'obj is already initiated.')
#endif

obj%isInit = obj2%isInit
obj%fieldType = obj2%fieldType
obj%name = obj2%name
obj%engine = obj2%engine
obj%comm = obj2%comm
obj%myRank = obj2%myRank
obj%numProcs = obj2%numProcs
obj%global_n = obj2%global_n
obj%local_n = obj2%local_n
obj%is = obj2%is
obj%ie = obj2%ie
obj%lis_ptr = obj2%lis_ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

! main

! note: We should not call deallocate in abstract classes.
! This is because, in concrete classes we may set some
! parameters before calling this method.
! All those parameters will be gone if we call deallocate
! here.
! CALL obj%DEALLOCATE()

obj%isInit = math%yes
obj%name = name
obj%engine = engine
obj%fieldType = Input(option=fieldType, default=TypeField%normal)
obj%comm = Input(option=comm, default=math%zero_i)
obj%local_n = Input(option=local_n, default=math%zero_i)
obj%global_n = Input(option=global_n, default=math%zero_i)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = math%no
obj%fieldType = TypeField%normal
obj%name = ""
obj%engine = ""
obj%comm = math%zero_i
obj%myRank = math%zero_i
obj%numProcs = math%one_i
obj%global_n = math%zero_i
obj%local_n = math%zero_i
obj%is = math%zero_i
obj%ie = math%zero_i
obj%lis_ptr = math%zero_i
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                             Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
