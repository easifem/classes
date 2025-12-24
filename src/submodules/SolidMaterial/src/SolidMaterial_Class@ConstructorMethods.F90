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

SUBMODULE(SolidMaterial_Class) ConstructorMethods
USE Display_Method, ONLY: ToString
USE MaterialFactory, ONLY: SolidMechanicsModelFactory
USE AbstractMaterial_Class, ONLY: AbstractMaterialInitiate, &
                                  AbstractMaterialDeallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = .NOT. ASSOCIATED(obj%stressStrainModel)
CALL AssertError1(isok, myName, &
                 "stressStrainModel is already associated, nullify it first.")
#endif

CALL AbstractMaterialInitiate(obj=obj, name=name)

! If strassStrainModel is not provided, then nothing to do here
isok = PRESENT(stressStrainModel)
IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'stressStrainModel not provided, Nothing to do here.')

  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

! This code is called when stressStrainModel is defined
! We are not triming here, it is user's responsibility
! We may use uppercase in the SolidMechanicsModelFactory
obj%stressStrainModel => SolidMechanicsModelFactory(stressStrainModel)

! We are not initiating stressStrainModel.
! After this method call, user should get the pointer of
! stressStrainModel and call Initiate method on it.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractMaterialDeallocate(obj)

isok = ASSOCIATED(obj%stressStrainModel)
IF (isok) THEN
  CALL obj%stressStrainModel%DEALLOCATE()
  obj%stressStrainModel => NULL()
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                    Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Deallocate_Vector()"
#endif
#include "../../include/deallocate_vector.F90"
END PROCEDURE Deallocate_Vector

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Deallocate_Ptr_Vector()"
#endif
#include "../../include/deallocate_vector_ptr.F90"
END PROCEDURE Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                             Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Reallocate_Vector()"
#endif
#include "../../include/reallocate_vector.F90"
END PROCEDURE Reallocate_Vector

!----------------------------------------------------------------------------
!                                                             Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Reallocate_Ptr_Vector()"
#endif
#include "../../include/reallocate_vector_ptr.F90"
END PROCEDURE Reallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
