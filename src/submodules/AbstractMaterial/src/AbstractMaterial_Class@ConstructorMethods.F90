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

SUBMODULE(AbstractMaterial_Class) ConstructorMethods
USE BaseMethod
USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & 'This routine does nothing... It should be implemented by child class.')
END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                SetAbstractMaterialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractMaterialParam
CALL Set(obj=param, prefix=prefix, key="name", VALUE=name, dataType="char")
END PROCEDURE SetAbstractMaterialParam

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
CHARACTER(:), ALLOCATABLE :: prefix0

! main
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Initiate()')
#endif

CALL obj%DEALLOCATE()
! check essential param
CALL obj%CheckEssentialParam(param)

IF (PRESENT(prefix)) THEN
  prefix0 = TRIM(prefix)
ELSE
  prefix0 = obj%GetPrefix()
END IF

obj%isInit = .TRUE.

! name
CALL GetValue(obj=param, prefix=prefix0, key="name", VALUE=obj%name)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Initiate()')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%isInit = .FALSE.
obj%name = ""
obj%tProperties = 0
! CALL Deallocate(obj%matProps)
! CALL tbl%Deallocate()
END PROCEDURE obj_Deallocate

END SUBMODULE ConstructorMethods
