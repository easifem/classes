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

SUBMODULE(AbstractKernel_Class) AssembleTanmatMethods
! USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            AssembleTanmat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleTanmat
CHARACTER(*), PARAMETER :: myName = "obj_AssembleTanmat"
CALL e%raiseError(modName//'::'//myName//" - "// &
& '[IMPLEMENTATION ERROR] :: the routine should be implemented by subclass')
END PROCEDURE obj_AssembleTanmat

!----------------------------------------------------------------------------
!                                                            AssembleMassMat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleMassMat
CHARACTER(*), PARAMETER :: myName = "obj_AssembleMassMat"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This module has not been implemented yet')
! TODO: Implement obj_AssembleMassMat
END PROCEDURE obj_AssembleMassMat

!----------------------------------------------------------------------------
!                                                       AssembleStiffnessMat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleStiffnessMat
CHARACTER(*), PARAMETER :: myName = "obj_AssembleStiffnessMat"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This module has not been implemented yet')
! TODO: Implement obj_AssembleStiffnessMat
END PROCEDURE obj_AssembleStiffnessMat

!----------------------------------------------------------------------------
!                                                     AssembleDampingMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleDampingMat
CHARACTER(*), PARAMETER :: myName = "obj_AssembleDampingMat"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This module has not been implemented yet')
! TODO: Implement obj_AssembleDampingMat
END PROCEDURE obj_AssembleDampingMat

!----------------------------------------------------------------------------
!                                                     AssembleNitscheMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleNitscheMat
CHARACTER(*), PARAMETER :: myName = "obj_AssembleNitscheMat"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This module has not been implemented yet')
! TODO: Implement obj_AssembleNitscheMat
END PROCEDURE obj_AssembleNitscheMat

END SUBMODULE AssembleTanmatMethods
