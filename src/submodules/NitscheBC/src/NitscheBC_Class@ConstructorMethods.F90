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

SUBMODULE(NitscheBC_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_checkEssentialParam
CALL AbstractBCcheckEssentialParam(&
& obj=obj, &
& param=param, &
& prefix=myprefix)
END PROCEDURE bc_checkEssentialParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE setNitscheBCParam
CALL setAbstractBCParam(&
& param=param, &
& prefix=myprefix, &
& name=name, &
& idof=idof, &
& nodalValueType=nodalValueType, &
& useFunction=input(option=useFunction, default=.FALSE.), &
& isNormal=input(option=isNormal, default=.FALSE.), &
& useExternal=input(option=useExternal, default=.FALSE.), &
& isTangent=input(option=isTangent, default=.FALSE.) &
& )
END PROCEDURE setNitscheBCParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Initiate
CALL AbstractBCInitiate(obj=obj, &
& param=param, &
& prefix=myprefix, &
& boundary=boundary, &
& dom=dom)
END PROCEDURE bc_Initiate

!----------------------------------------------------------------------------
!                                                            Final
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Final
CALL obj%DEALLOCATE()
END PROCEDURE bc_Final

END SUBMODULE ConstructorMethods
