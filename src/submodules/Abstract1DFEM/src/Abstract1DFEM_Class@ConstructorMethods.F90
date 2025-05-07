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

SUBMODULE(Abstract1DFEM_Class) ConstructorMethods

USE GlobalData, ONLY: DOF_FMT

USE CSRMatrix_Method, ONLY: CSRMatrix_Initiate => Initiate, &
                            CSRMatrix_SetSparsity => SetSparsity

USE DOF_Method, ONLY: DOF_Initiate => Initiate, &
                      DOF_SIZE => Size

USE RealVector_Method, ONLY: RealVector_Initiate => Initiate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                              -                     obj_ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
  & 'child classes')
#endif

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                   obj_ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isConnectivity = .FALSE.
obj%baseContinuityForSpace = "H1"
obj%baseInterpolationForSpace = "LAGR"
obj%verbosity = 0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! NOTE: let's make it more generic
! one idea is passing spaceCompo, timeCompo, and storageFMT
! and return nrow, ncol
MODULE PROCEDURE obj_InitiateFields
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateFields()"
#endif

INTEGER(I4B), PARAMETER :: storageFMT = DOF_FMT
CHARACTER(LEN=1), PARAMETER :: names(1) = ["u"]
INTEGER(I4B) :: tnodes(1), spaceCompo(1), timeCompo(1), nrow, ncol, ii, &
                con(256), tcon, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

spaceCompo(1) = 1
timeCompo(1) = 1
tnodes(1) = obj%totalVertexDOFSpace + obj%totalEdgeDOFSpace

CALL DOF_Initiate(obj=obj%dof, tNodes=tnodes, names=names, &
            spacecompo=spacecompo, timecompo=timecompo, storageFMT=storageFMT)

nrow = DOF_SIZE(obj%dof)
ncol = nrow

CALL CSRMatrix_Initiate(obj=obj%tanmat, ncol=ncol, nrow=nrow, &
                        idof=obj%dof, jdof=obj%dof)

DO ii = 1, obj%totalSpaceElements
  CALL obj%GetConnectivity(spaceElemNum=ii, ans=con, tsize=tcon)
  DO jj = 1, tcon
    CALL CSRMatrix_SetSparsity(obj=obj%tanmat, row=con(jj), col=con(1:tcon))
  END DO
END DO

CALL CSRMatrix_SetSparsity(obj=obj%tanmat)

CALL RealVector_Initiate(obj%sol, nrow)
CALL RealVector_Initiate(obj%rhs, nrow)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_InitiateFields

!----------------------------------------------------------------------------
!                                                      InitiateConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateConnectivity()"
#endif

INTEGER(I4B) :: ii, jj, icount, iedgedof, tnodes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%isConnectivity) RETURN

obj%isConnectivity = .TRUE.

tnodes = 0
DO ii = 1, obj%totalSpaceElements
  tnodes = tnodes + obj%totalDOFSpace(ii)
END DO

CALL Reallocate(obj%conIA, obj%totalSpaceElements + 1)
CALL Reallocate(obj%conJA, tnodes)
obj%conIA(1) = 1

icount = 1
iedgedof = obj%totalVertexDOFSpace

DO ii = 1, obj%totalSpaceElements
  obj%conJA(icount) = ii
  icount = icount + 1
  obj%conJA(icount) = ii + 1
  icount = icount + 1
  obj%conIA(ii + 1) = obj%conIA(ii) + obj%totalDOFSpace(ii)

  DO jj = 1, obj%totalDOFSpace(ii) - 2
    iedgedof = iedgedof + 1
    obj%conJA(icount) = iedgedof
    icount = icount + 1
  END DO
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_InitiateConnectivity

!----------------------------------------------------------------------------
!                                                           SetTotalDOFSpace
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
