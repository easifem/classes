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

SUBMODULE(AbstractDomain_Class) MeshDataMethods
USE GlobalData, ONLY: stdout
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE Kdtree2_Module, ONLY: Kdtree2_create
USE CPUTime_Class, ONLY: CPUTime_
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
  "AbstractDomain_Class@MeshDataMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                            InitiateKdtree
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateKdtree
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateKdtree()"
  LOGICAL(LGT) :: isok
#endif

  INTEGER(I4B) :: nsd

#ifdef DEBUG_VER
  TYPE(CPUTime_) :: TypeCPUTime
#endif

#ifdef DEBUG_VER
  IF (obj%showTime) CALL TypeCPUTime%SetStartTime()
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL obj%DeallocateKdtree()

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%nodeCoord)
  CALL AssertError1(isok, myName, &
                    "obj%nodeCoord not allocated")
#endif

  nsd = obj%nsd
  obj%kdtree => Kdtree2_Create(input_data=obj%nodeCoord(1:nsd, :), &
                               DIM=nsd, sort=math%no, &
                               rearrange=math%yes)

  ALLOCATE(obj%kdresult(obj%tNodes))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

#ifdef DEBUG_VER
  IF (obj%showTime) THEN
    CALL TypeCPUTime%SetEndTime()
    CALL Display(modName // " : " // myName // &
                 " : time : " // &
                 ToString(TypeCPUTime%GetTime()), unitno=stdout)
  END IF
#endif
END PROCEDURE obj_InitiateKdtree

!----------------------------------------------------------------------------
!                                                     InitiateNodeToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToElements
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateNodeToElements()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "This routine should be implemented by child classes.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_InitiateNodeToElements

!----------------------------------------------------------------------------
!                                                     InitiateNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateNodeToNodes
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateExtraNodeToNodes()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "This routine should be implemented by child classes.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_InitiateNodeToNodes

!----------------------------------------------------------------------------
!                                                  InitiateElementToElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateElementToElements
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateElementToElements()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "This routine should be implemented by child classes.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_InitiateElementToElements

!----------------------------------------------------------------------------
!                                                     InitiateFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetElements
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateFacetElements()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "This routine should be implemented by child classes.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_InitiateFacetElements

!----------------------------------------------------------------------------
!                                                   InitiateExtraNodeToNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateExtraNodeToNodes
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_InitiateExtraNodeToNodes()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "This routine should be implemented by child classes.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_InitiateExtraNodeToNodes

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE MeshDataMethods
