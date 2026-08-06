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

SUBMODULE(PorousMaterial_Class) Methods
USE Display_Method, ONLY: ToString
USE Display_Method, ONLY: Display
USE MaterialFactory, ONLY: PoroMechanicsModelFactory
USE MaterialFactory, ONLY: PorousMaterialFactory
USE CapillaryFactory, ONLY: CapillaryModelFactory
USE AbstractMaterial_Class, ONLY: AbstractMaterialInitiate
USE AbstractMaterial_Class, ONLY: AbstractMaterialDeallocate
USE AbstractMaterial_Class, ONLY: AbstractMaterialDisplay
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "PorousMaterial_Class@Methods.F90"
#endif

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
                  "stressStrainModel is already associated..")
#endif

#ifdef DEBUG_VER
isok = .NOT. ASSOCIATED(obj%capillaryModel)
CALL AssertError1(isok, myName, &
                  "capillaryModel is already associated..")
#endif

CALL AbstractMaterialInitiate(obj=obj, name=name)

! If strassStrainModel is not provided, then nothing to do here
isok = PRESENT(stressStrainModel)
IF (isok) THEN
  ! This code is called when stressStrainModel is defined
  ! We are not triming here, it is user's responsibility
  ! We may use uppercase in the SolidMechanicsModelFactory
  obj%stressStrainModel => PoroMechanicsModelFactory(stressStrainModel)
  ! We are not initiating stressStrainModel.
  ! After this method call, user should get the pointer of
  ! stressStrainModel and call Initiate method on it.
END IF

! If capillaryModel is not provided, then nothing to do here
isok = PRESENT(capillaryModel)
IF (isok) THEN
  ! This code is called when capillaryModel is defined
  ! We are not triming here, it is user's responsibility
  ! We may use uppercase in the
  obj%capillaryModel => CapillaryModelFactory(capillaryModel)
  ! We are not initiating capillaryModel.
  ! After this method call, user should get the pointer of
  ! capillaryModel and call Initiate method on it.
END IF

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

isok = ASSOCIATED(obj%capillaryModel)
IF (isok) THEN
  CALL obj%capillaryModel%DEALLOCATE()
  obj%capillaryModel => NULL()
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
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL AbstractMaterialDisplay(obj=obj, msg=msg, unitNo=unitNo)

isok = ASSOCIATED(obj%stressStrainModel)
CALL Display(isok, "stressStrainModel ASSOCIATED: ", unitNo=unitNo)
IF (isok) THEN
  CALL obj%stressStrainModel%Display(msg="stressStrainModel:", &
                                     unitNo=unitNo)
END IF

isok = ASSOCIATED(obj%capillaryModel)
CALL Display(isok, "capillaryModel ASSOCIATED: ", unitNo=unitNo)
IF (isok) THEN
  CALL obj%capillaryModel%Display(msg="capillaryModel:", unitNo=unitNo)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                     Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display_Vector()"
#endif
#include "../../include/display_vector.F90"
END PROCEDURE obj_Display_Vector

!----------------------------------------------------------------------------
!                                                                     Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display_Ptr_Vector()"
#endif
#include "../../include/display_vector_ptr.F90"
END PROCEDURE obj_Display_Ptr_Vector

!----------------------------------------------------------------------------
!                                                    obj_AddPorousMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddPorousMaterial
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AddPorousMaterial"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize1
#endif

LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = materialNo .LE. tMaterials
CALL AssertError1(isok, myName, &
     'Given MaterialNo [='//ToString(materialNo)//'] is greater than &
     &total number of PorousMaterials [='//ToString(tMaterials)//']!')
#endif

abool = PRESENT(region) .AND. PRESENT(PorousMaterialToMesh)

#ifdef DEBUG_VER
isok = math%yes
tsize1 = 0
IF (abool) THEN
  tsize1 = SIZE(PorousMaterialToMesh)
  isok = materialNo .LE. tsize1
END IF
CALL AssertError1(isok, myName, &
                  'Given MaterialNo [='//TOSTRING(materialNo)// &
                  '] is greater than the size of PorousMaterialToMesh [='// &
                  ToString(tsize1)//']!')
#endif

IF (abool) PorousMaterialToMesh(materialNo) = region

abool = PRESENT(materialName)

#ifdef DEBUG_VER
isok = math%yes
tsize1 = 0
IF (abool) THEN
  tsize1 = SIZE(obj)
  isok = materialNo .LE. tsize1
END IF
CALL AssertError1(isok, myName, &
                  'Given MaterialNo [='//ToString(materialNo)// &
                  '] is greater than the size of PorousMaterial[='// &
                  ToString(tsize1)//']!')

isok = .NOT. ASSOCIATED(obj(materialNo)%ptr)
CALL AssertError1(isok, myName, &
                  'PorousMaterial('//ToString(materialNo)// &
                  ')%ptr is already associated.')
#endif

IF (abool) THEN
  obj(materialNo)%ptr => PorousMaterialFactory(TRIM(materialName))
  !! Info: Porous material factory is defined in MaterialFactory.
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AddPorousMaterial

!----------------------------------------------------------------------------
!                                                   GetPorousMaterialPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPorousMaterialPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPorousMaterialPointer()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(obj)

#ifdef DEBUG_VER
isok = materialNo .LE. tsize
CALL AssertError1(isok, myName, &
                  'materialNo = '//Tostring(materialNo)// &
                  ' is greater than total materials = '//Tostring(tsize))
#endif

ans => NULL()
ans => obj(materialNo)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPorousMaterialPointer

!----------------------------------------------------------------------------
!                                                GetStressStrainModelPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetStressStrainModelPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetStressStrainModelPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => obj%stressStrainModel

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetStressStrainModelPointer

!----------------------------------------------------------------------------
!                                                   GetCapillaryModelPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCapillaryModelPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCapillaryModelPointer()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans => obj%capillaryModel

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetCapillaryModelPointer

!----------------------------------------------------------------------------
!                                                 IsCapillaryModelAssociated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsCapillaryModelAssociated
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsCapillaryModelAssociated()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = ASSOCIATED(obj%capillaryModel)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_IsCapillaryModelAssociated

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
