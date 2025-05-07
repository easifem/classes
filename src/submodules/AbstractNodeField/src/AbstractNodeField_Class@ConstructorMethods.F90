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

SUBMODULE(AbstractNodeField_Class) ConstructorMethods
USE Display_Method, ONLY: ToString

USE RealVector_Method, ONLY: RealVector_Deallocate => DEALLOCATE, &
                             RealVector_Initiate => Initiate, &
                             RealVector_Size => Size

USE DOF_Method, ONLY: DOF_Deallocate => DEALLOCATE, &
                      DOF_Initiate => Initiate

USE AbstractField_Class, ONLY: AbstractFieldInitiate, &
                               AbstractFieldInitiate2, &
                               AbstractFieldDeallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                               AbstractNodeFieldCheckError
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractNodeFieldCheckError
CHARACTER(*), PARAMETER :: myName = "AbstractNodeFieldCheckError()"
INTEGER(I4B) :: ivar, tvar
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER)
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

problem = obj%dof_tPhysicalVars .EQ. 0_I4B
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
         '[INTERNAL ERROR] :: AbstractNodeField_::obj%dof_tPhysicalVars is 0')
  RETURN
END IF

problem = .NOT. ALLOCATED(obj%dof_spaceCompo)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
             '[INTERNAL ERROR] :: AbstractNodeField_::obj%dof_spaceCompo '// &
                    ' is NOT ALLOCATED')
  RETURN
END IF

tvar = SIZE(obj%dof_spaceCompo)
problem = tvar .NE. obj%dof_tPhysicalVars
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
            '[INTERNAL ERROR] :: size of dof_spaceCompo ('//ToString(tvar)// &
                    ') is not same as dof_tPhysicalVars ('// &
                    ToString(obj%dof_tPhysicalVars)//')')
  RETURN
END IF

problem = .NOT. ALLOCATED(obj%dof_timeCompo)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[INTERNAL ERROR] :: AbstractNodeField_::obj%dof_timeCompo '// &
                    ' is NOT ALLOCATED')
  RETURN
END IF

tvar = SIZE(obj%dof_timeCompo)
problem = tvar .NE. obj%dof_tPhysicalVars
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
             '[INTERNAL ERROR] :: size of dof_timeCompo ('//ToString(tvar)// &
                    ') is not same as dof_tPhysicalVars ('// &
                    ToString(obj%dof_tPhysicalVars)//')')
  RETURN
END IF

IF (.NOT. ALLOCATED(obj%dof_tNodes)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                 '[INTERNAL ERROR] :: AbstractNodeField_::obj%dof_tNodes '// &
                    ' is NOT ALLOCATED')
  RETURN
END IF

tvar = SIZE(obj%dof_tNodes)
problem = tvar .NE. obj%dof_tPhysicalVars
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                '[INTERNAL ERROR] :: size of dof_tNodes ('//ToString(tvar)// &
                    ') is not same as dof_tPhysicalVars ('// &
                    ToString(obj%dof_tPhysicalVars)//')')
  RETURN
END IF

IF (.NOT. ALLOCATED(obj%dof_names_char)) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
             '[InitIATE ERROR] :: AbstractNodeField_::obj%dof_names_char '// &
                    ' is NOT ALLOCATED')
  RETURN
END IF

tvar = SIZE(obj%dof_names_char)
problem = tvar .NE. obj%dof_tPhysicalVars
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
            '[INTERNAL ERROR] :: size of dof_names_char ('//ToString(tvar)// &
                    ') is not same as dof_tPhysicalVars ('// &
                    ToString(obj%dof_tPhysicalVars)//')')
  RETURN
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE AbstractNodeFieldCheckError

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(:), ALLOCATABLE :: prefix
INTEGER(I4B) :: local_n, global_n

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

prefix = obj%GetPrefix()

CALL AbstractFieldInitiate(obj=obj, param=param, fedof=fedof)

CALL AbstractNodeFieldCheckError(obj)

!INFO: So when a child calls this routine then dof_** are already set
! That is also the reason why we are not calling deallocate in the begining

CALL DOF_Initiate(obj=obj%dof, tNodes=obj%dof_tNodes, &
                  names=obj%dof_names_char, spaceCompo=obj%dof_spaceCompo, &
                  timeCompo=obj%dof_timeCompo, storageFMT=obj%dof_storageFMT)

CALL RealVector_Initiate(obj=obj%realVec, dofobj=obj%dof)

obj%tSize = RealVector_SIZE(obj%realVec)

CALL obj%GetParam(local_n=local_n, global_n=global_n)
IF (local_n .EQ. 0) CALL obj%SetParam(local_n=obj%tSize)
IF (global_n .EQ. 0) CALL obj%SetParam(global_n=obj%tSize)

prefix = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
CALL AbstractFieldInitiate2(obj=obj, obj2=obj2, copyFull=copyFull, &
                           copyStructure=copyStructure, usePointer=usePointer)

SELECT TYPE (obj2); CLASS IS (AbstractNodeField_)
  obj%dof_tPhysicalVars = obj2%dof_tPhysicalVars
  obj%dof_storageFMT = obj2%dof_storageFMT

  IF (ALLOCATED(obj2%dof_spaceCompo)) obj%dof_spaceCompo = obj2%dof_spaceCompo
  IF (ALLOCATED(obj2%dof_timeCompo)) obj%dof_timeCompo = obj2%dof_timeCompo
  IF (ALLOCATED(obj2%dof_tNodes)) obj%dof_tNodes = obj2%dof_tNodes
  IF (ALLOCATED(obj2%dof_names_char)) obj%dof_names_char = obj2%dof_names_char
  obj%tSize = obj2%tSize
  obj%realVec = obj2%realVec
  obj%dof = obj2%dof
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                            obj_Initiate3
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
INTEGER(I4B) :: ivar, tvar, local_n, global_n
LOGICAL(LGT) :: problem
CHARACTER(:), ALLOCATABLE :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

prefix = obj%GetPrefix()

CALL AbstractFieldInitiate(obj=obj, param=param, fedof=fedof)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'Calling AbstractNodeFieldCheckError()')
#endif

CALL AbstractNodeFieldCheckError(obj)

CALL DOF_Initiate(obj=obj%dof, tNodes=obj%dof_tNodes, &
                  names=obj%dof_names_char, spaceCompo=obj%dof_spaceCompo, &
                  timeCompo=obj%dof_timeCompo, storageFMT=obj%dof_storageFMT)

CALL RealVector_Initiate(obj=obj%realVec, dofobj=obj%dof)

obj%tSize = RealVector_SIZE(obj%realVec)

CALL obj%GetParam(local_n=local_n, global_n=global_n)

IF (local_n .EQ. 0) CALL obj%SetParam(local_n=obj%tSize)
IF (global_n .EQ. 0) CALL obj%SetParam(global_n=obj%tSize)

prefix = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
CALL AbstractFieldDeallocate(obj)
obj%dof_tPhysicalVars = 0
obj%dof_storageFMT = NODES_FMT
IF (ALLOCATED(obj%dof_spaceCompo)) DEALLOCATE (obj%dof_spaceCompo)
IF (ALLOCATED(obj%dof_timeCompo)) DEALLOCATE (obj%dof_timeCompo)
IF (ALLOCATED(obj%dof_tNodes)) DEALLOCATE (obj%dof_tNodes)
IF (ALLOCATED(obj%dof_names_char)) DEALLOCATE (obj%dof_names_char)
obj%tSize = 0
CALL RealVector_Deallocate(obj%realVec)
CALL DOF_Deallocate(obj%dof)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
