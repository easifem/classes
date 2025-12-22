! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(OneDimLagrangeFE_Class) Methods
USE BaseType, ONLY: TypeFeVariableOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                      GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemShapeData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%Lagrange_GetLocalElemShapeData(elemsd=elemsd, quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                                    OneDimLagrangeFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_OneDimLagrangeFEPointer1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_OneDimLagrangeFEPointer1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ALLOCATE (ans)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_OneDimLagrangeFEPointer1

!----------------------------------------------------------------------------
!                                                    OneDimLagrangeFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_OneDimLagrangeFEPointer2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_OneDimLagrangeFEPointer2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ALLOCATE (ans)
CALL ans%Initiate( &
  fetype=TypeFeVariableOpt%scalar, baseContinuity=baseContinuity, &
  baseInterpolation="LAGRANGE", ipType=ipType, basisType=basisType, &
  alpha=alpha, beta=beta, lambda=lambda, order=order)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_OneDimLagrangeFEPointer2

!----------------------------------------------------------------------------
!                                                           OneDimLagrangeFE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_OneDimLagrangeFE
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_OneDimLagrangeFE()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL ans%Initiate( &
  fetype=TypeFeVariableOpt%scalar, baseContinuity=baseContinuity, &
  baseInterpolation="LAGRANGE", ipType=ipType, basisType=basisType, &
  alpha=alpha, beta=beta, lambda=lambda, order=order)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_OneDimLagrangeFE

!----------------------------------------------------------------------------
!                                                    FiniteElementDeallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Deallocate_Vector()"
#endif

#include "../../../include/deallocate_vector.F90"

END PROCEDURE Deallocate_Vector

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Deallocate_Ptr_Vector()"
#endif

#include "../../../include/deallocate_vector_ptr.F90"

END PROCEDURE Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                               Include error
!----------------------------------------------------------------------------

#include "../../../include/errors.F90"

END SUBMODULE Methods
