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

SUBMODULE(BasisOpt_Class) SetMethods
USE LagrangePolynomialUtility, ONLY: LagrangeDOF
USE HierarchicalPolynomialUtility, ONLY: HierarchicalDOF
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: ToString, Display

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetTotalDOF()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%baseInterpolation)
CASE ("LAGR")
  IF (obj%isIsotropicOrder) THEN
    obj%tdof = LagrangeDOF(order=obj%order, elemType=obj%topoName)
  END IF

  IF (obj%isAnisotropicOrder) THEN
    obj%tdof = LagrangeDOF(p=obj%anisoOrder(1), q=obj%anisoOrder(2), &
                           r=obj%anisoOrder(3), elemType=obj%topoName)
  END IF

CASE ("HIER", "HEIR")

  obj%tdof = HierarchicalDOF(elemType=obj%topoName, cellOrder=obj%cellOrder, &
                             faceOrder=obj%faceOrder, edgeOrder=obj%edgeOrder)

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetTotalDOF

!----------------------------------------------------------------------------
!                                                                    SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
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
END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                    SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%baseInterpolation)
CASE ("LAGR")
  CALL obj%SetLagrangeOrder(order=order, anisoOrder=anisoOrder, &
                            errCheck=errCheck)

CASE ("HIER", "HEIR")
  CALL obj%SetHierarchicalOrder(cellOrder=cellOrder, faceOrder=faceOrder, &
          edgeOrder=edgeOrder, cellOrient=cellOrient, faceOrient=faceOrient, &
                      edgeOrient=edgeOrient, errCheck=errCheck, tcell=tcell, &
                                tface=tface, tedge=tedge)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for baseInterpolation is not defined.')
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetOrder

!----------------------------------------------------------------------------
!                                                       ResetIsotropicOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ResetIsotropicOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ResetIsotropicOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isIsotropicOrder = TypeBasisOpt%isIsotropicOrder
obj%order = TypeBasisOpt%order

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ResetIsotropicOrder

!----------------------------------------------------------------------------
!                                                       ResetAnisotropicOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ResetAnisotropicOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ResetAnisotropicOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isAnisotropicOrder= TypeBasisOpt%isAnisotropicOrder
obj%anisoOrder= TypeBasisOpt%anisoOrder

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ResetAnisotropicOrder

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
