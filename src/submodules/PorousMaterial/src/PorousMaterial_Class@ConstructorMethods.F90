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

SUBMODULE(PorousMaterial_Class) ConstructorMethods
USE MaterialFactory
USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     setPorousMaterialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE setPorousMaterialParam
CALL SetAbstractMaterialParam(param=param, &
  & prefix=myprefix, name=name)
CALL Set(obj=param, prefix=myprefix, key="stressStrainModel",  &
  & VALUE=stressStrainModel, dataType="char")
END PROCEDURE setPorousMaterialParam

!----------------------------------------------------------------------------
!                                                        CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE Porous_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "Porous_CheckEssentialParam"
IF (.NOT. param%isPresent(key=myprefix//"/name")) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & myprefix//'/name should be present in param')
END IF
IF (ASSOCIATED(obj%stressStrainModel)) THEN
  CALL obj%stressStrainModel%CheckEssentialParam(param)
END IF
END PROCEDURE Porous_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Porous_Initiate
CHARACTER(*), PARAMETER :: myName = "Porous_Initiate"
TYPE(String) :: prefix0, stressStrainModel
LOGICAL(LGT) :: bool1
! main

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Initiate()')
#endif

IF (PRESENT(prefix)) THEN
  prefix0 = prefix
ELSE
  prefix0 = obj%GetPrefix()
END IF

CALL AbstractMaterialInitiate(obj=obj, param=param, prefix=prefix0%chars())

! stressStrainModel

CALL GetValue(obj=param, prefix=prefix0%chars(), key="stressStrainModel",  &
  & VALUE=stressStrainModel)

IF (ASSOCIATED(obj%stressStrainModel)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "[CONFIG ERROR] :: The "//prefix0//"/stressStrainModel is "//  &
    & "already associated, "//  &
    & CHAR_LF//"nullify it first.")
  RETURN
END IF

bool1 = param%isPresent(key=prefix0//"/stressStrainModel")
IF (bool1) THEN
  obj%stressStrainModel => PoroMechanicsModelFactory( &
    & stressStrainModel%chars())
  CALL obj%stressStrainModel%Initiate(param)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Initiate()')
#endif
END PROCEDURE Porous_Initiate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Porous_Deallocate
CALL AbstractMaterialDeallocate(obj)
IF (ASSOCIATED(obj%stressStrainModel)) THEN
  DEALLOCATE (obj%stressStrainModel)
  NULLIFY (obj%stressStrainModel)
END IF
END PROCEDURE Porous_Deallocate

!----------------------------------------------------------------------------
!                                                                    Final
!----------------------------------------------------------------------------

MODULE PROCEDURE Porous_Final
CALL obj%DEALLOCATE()
END PROCEDURE Porous_Final

END SUBMODULE ConstructorMethods
