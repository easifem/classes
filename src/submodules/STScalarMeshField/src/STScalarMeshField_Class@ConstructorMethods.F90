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

SUBMODULE(STScalarMeshField_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetSTScalarMeshFieldParam
!!
IF (fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL SetAbstractMeshFieldParam( &
    & param=param, &
    & prefix="STScalarMeshField", &
    & name=name, &
    & fieldType=fieldType, &
    & varType=varType, &
    & engine=engine, &
    & defineOn=defineOn, &
    & rank=Scalar, &
    & s=[1])
ELSE
  CALL SetAbstractMeshFieldParam( &
    & param=param, &
    & prefix="STScalarMeshField", &
    & name=name, &
    & fieldType=fieldType, &
    & varType=varType, &
    & engine=engine, &
    & defineOn=defineOn, &
    & rank=Scalar, &
    & s=[nns, nnt])
END IF
!!
END PROCEDURE SetSTScalarMeshFieldParam

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_CheckEssentialParam
CALL AbstractMeshFieldCheckEssentialParam( &
  & obj=obj, &
  & prefix="STScalarMeshField", &
  & param=param)
END PROCEDURE aField_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Initiate1
CALL AbstractMeshFieldInitiate( &
  & obj=obj, &
  & prefix="STScalarMeshField", &
  & param=param, mesh=mesh)
END PROCEDURE aField_Initiate1

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Deallocate_Vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    CALL obj(ii)%DEALLOCATE()
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE aField_Deallocate_Vector

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE aField_Deallocate_Ptr_Vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE aField_Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
