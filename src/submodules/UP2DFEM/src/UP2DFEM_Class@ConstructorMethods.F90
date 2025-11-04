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

SUBMODULE(UP2DFEM_Class) ConstructorMethods

USE StringUtility, ONLY: UpperCase

USE GlobalData, ONLY: DOF_FMT, &
                      NONE

USE ReallocateUtility, ONLY: Reallocate

USE CSRMatrix_Method, ONLY: CSRMatrix_Initiate => Initiate, &
                            CSRMatrix_SetSparsity => SetSparsity

USE DOF_Method, ONLY: DOF_Initiate => Initiate, &
                      DOF_SIZE => Size

USE RealVector_Method, ONLY: RealVector_Initiate => Initiate, &
                             RealVector_Set => Set

USE HDF5File_Class, ONLY: HDF5File_
USE FPL
USE MatrixField_Class, ONLY: SetMatrixFieldParam
USE VectorField_Class, ONLY: SetVectorFieldParam
USE NeumannBC_Class, ONLY: NeumannBC_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                              -                     obj_ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

CALL FPL_Init
CALL obj%param%Initiate()

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                   obj_ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

obj%baseContinuityForSpace = "H1"
obj%baseInterpolationForSpace = "LAGR"
obj%verbosity = 0

CALL obj%param%DEALLOCATE()
CALL FPL_Finalize

IF (ALLOCATED(obj%cellcon)) DEALLOCATE (obj%cellcon)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateDomains
CHARACTER(*), PARAMETER :: myName = "obj_InitiateDomain()"
TYPE(HDF5File_) :: meshfile
INTEGER(I4B) :: tsize, ii

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

CALL meshfile%Initiate(filename=obj%meshfilename%chars(), &
                       mode="READ")
CALL meshfile%OPEN()
IF (debug) CALL Display("Read meshfile")

CALL obj%dom%Initiate(meshfile, '')
IF (debug) CALL Display("Initiate domain")

tsize = obj%dom%GetTotalEntities(nsd - 1)
CALL Display(tsize, "tsize ::")

ALLOCATE (obj%boundaries(tsize))

! NOTE: currently curve eintities is not read
! in initiate of fedomain so I load curve entities date
! independently here.
DO ii = 1, tsize
  CALL Display(ii, "id ::")
  ! WARN: it is necessary to allocate ptr
  ALLOCATE (obj%boundaries(ii)%ptr)
  CALL obj%boundaries(ii)%ptr%Initiate( &
    meshfile, '', dim=nsd - 1, entities=[ii])
END DO
IF (debug) CALL Display("Initiate Boundaries")

CALL meshfile%DEALLOCATE()

obj%cellmesh => obj%dom%GetMeshPointer(nsd)
obj%maxNNE = obj%cellmesh%GetMaxNNE()
obj%totalSpaceElements = obj%cellmesh%GetTotalElements()
! TODO: improve it
obj%spaceOrder = 1_I4B

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_InitiateDomains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFEDOF
CHARACTER(*), PARAMETER :: myName = "obj_InitiateFEDOF()"

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

CALL obj%fedof%Initiate(baseContinuity=obj%baseContinuityForSpace, &
                        baseInterpolation=obj%baseInterpolationForSpace, &
                        order=obj%spaceOrder, &
                        mesh=obj%cellmesh, &
                        ipType=obj%ipTypeForSpace)

obj%maxCON = obj%fedof%GetMaxTotalConnectivity()
CALL Reallocate(obj%cellcon, obj%maxcon)

! TODO: implement it
! IF (ALLOCATED(obj%nbc)) CALL InitiateFEDOF_NBC(obj)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_InitiateFEDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! SUBROUTINE InitiateFEDOF_NBC(obj)
!   CLASS(UP2DFEM_), INTENT(INOUT) :: obj
!   CHARACTER(*), PARAMETER :: myName = "InitiateFEDOF_NBC"
!   INTEGER(I4B) :: tnbc, ibc, imesh, tmesh, id, dim
!   INTEGER(I4B), ALLOCATABLE :: meshID(:)
!   LOGICAL(LGT) :: isok
!   CLASS(neumannBC_), POINTER :: nbc
!   CLASS(AbstractMesh_), POINTER :: meshptr
!
!   tnbc = SIZE(obj%nbc)
!   ALLOCATE (obj%fedofNBC(tnbc))
!
!   dim = nsd - 1_I4B
!
!   DO ibc = 1, tnbc
!     nbc => obj%nbc(ibc)%ptr
!     isok = ASSOCIATED(nbc)
!     CALL AssertError1(isok, myname, "NeumannBC is not associated")
!
!     CALL nbc%GetParam(isSelectionByMeshID=isok)
!     CALL AssertError1(isok, myname, &
!                       "currently only SelectionByMeshID is supported")
!
!     meshID = nbc%GetMeshID(dim=dim)
!     tmesh = SIZE(meshID)
!
!     DO imesh = 1, tmesh
!       id = meshID(imesh)
!       meshptr => obj%dom%GetMeshPointer(dim=dim, entityNum=id)
!
!       isok = meshptr%isEmpty()
!       IF (.NOT. isok) CYCLE
!     END DO
!   END DO
!
! END SUBROUTINE InitiateFEDOF_NBC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFields
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_InitiateFields()"
#endif

CHARACTER(LEN=1), PARAMETER :: names(1) = ["u"]
INTEGER(I4B) :: spaceCompo(1), timeCompo(1), aint
CHARACTER(*), PARAMETER :: engineName = "NATIVE_SERIAL"

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

timeCompo(1) = 1
spaceCompo(1) = nsd
aint = nsd * obj%maxNNE
CALL Reallocate(obj%ks, aint, aint)
CALL Reallocate(obj%ms, aint, aint)
CALL Reallocate(obj%cs, aint, aint)

CALL SetMatrixFieldParam(param=obj%param, name="T", matrixProp="UNSYM", &
                         engine=engineName, spaceCompo=nsd, timeCompo=1_I4B)

CALL obj%tanmat%Initiate(obj%param, fedof=obj%fedof)
IF (debug) CALL Display("Initiate tanmat")
IF (debug) CALL Display(obj%tanmat%SIZE(), 'size of tanmat:')
IF (debug) CALL Display(obj%tanmat%SHAPE(), 'shape of tanmat:')

CALL SetMatrixFieldParam(param=obj%param, name="M", matrixProp="UNSYM", &
                         engine=engineName, spaceCompo=nsd, timeCompo=1_I4B)

CALL obj%massMat%Initiate(obj%param, fedof=obj%fedof)
IF (debug) CALL Display("Initiate massMat")
IF (debug) CALL Display(obj%massMat%SIZE(), 'size of massMat:')
IF (debug) CALL Display(obj%massMat%SHAPE(), 'shape of massMat:')

CALL obj%massMat%Set(VALUE=zero)

CALL SetMatrixFieldParam(param=obj%param, name="K", matrixProp="UNSYM", &
                         engine=engineName, spaceCompo=nsd, timeCompo=1_I4B)

CALL obj%stiffMat%Initiate(obj%param, fedof=obj%fedof)
IF (debug) CALL Display("Initiate stiffMat")
IF (debug) CALL Display(obj%stiffMat%SIZE(), 'size of stiffMat:')
IF (debug) CALL Display(obj%stiffMat%SHAPE(), 'shape of stiffMat:')

CALL obj%stiffMat%Set(VALUE=zero)

CALL SetVectorFieldParam(param=obj%param, name="rhs", &
                         engine=engineName, spaceCompo=nsd)
CALL obj%rhs%Initiate(param=obj%param, fedof=obj%fedof)
IF (debug) CALL Display("Initiate rhs")

CALL SetVectorFieldParam(param=obj%param, name="sol", &
                         engine=engineName, spaceCompo=nsd)
CALL obj%sol%Initiate(param=obj%param, fedof=obj%fedof)
IF (debug) CALL Display("Initiate sol")

CALL SetVectorFieldParam(param=obj%param, name="rhs0", &
                         engine=engineName, spaceCompo=nsd)
CALL obj%rhs0%Initiate(param=obj%param, fedof=obj%fedof)
IF (debug) CALL Display("Initiate rhs0")

CALL SetVectorFieldParam(param=obj%param, name="u0", &
                         engine=engineName, spaceCompo=nsd)
CALL obj%u0%Initiate(param=obj%param, fedof=obj%fedof)
IF (debug) CALL Display("Initiate u0")

CALL SetVectorFieldParam(param=obj%param, name="v0", &
                         engine=engineName, spaceCompo=nsd)
CALL obj%v0%Initiate(param=obj%param, fedof=obj%fedof)
IF (debug) CALL Display("Initiate v0")

CALL SetVectorFieldParam(param=obj%param, name="a0", &
                         engine=engineName, spaceCompo=nsd)
CALL obj%a0%Initiate(param=obj%param, fedof=obj%fedof)
IF (debug) CALL Display("Initiate a0")

CALL SetVectorFieldParam(param=obj%param, name="force1", &
                         engine=engineName, spaceCompo=nsd)
CALL obj%force1%Initiate(param=obj%param, fedof=obj%fedof)
IF (debug) CALL Display("Initiate force1")

CALL SetVectorFieldParam(param=obj%param, name="force2", &
                         engine=engineName, spaceCompo=nsd)
CALL obj%force2%Initiate(param=obj%param, fedof=obj%fedof)
IF (debug) CALL Display("Initiate force2")

CALL SetVectorFieldParam(param=obj%param, name="tmp1", &
                         engine=engineName, spaceCompo=nsd)
CALL obj%tmp1%Initiate(param=obj%param, fedof=obj%fedof)
IF (debug) CALL Display("Initiate tmp1")

CALL obj%rhs%Set(VALUE=zero)
CALL obj%sol%Set(VALUE=zero)
CALL obj%rhs0%Set(VALUE=zero)
CALL obj%u0%Set(VALUE=zero)
CALL obj%v0%Set(VALUE=zero)
CALL obj%a0%Set(VALUE=zero)
CALL obj%force1%Set(VALUE=zero)
CALL obj%force2%Set(VALUE=zero)
CALL obj%tmp1%Set(VALUE=zero)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_InitiateFields

!----------------------------------------------------------------------------
!                                                           SetTotalDOFSpace
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
