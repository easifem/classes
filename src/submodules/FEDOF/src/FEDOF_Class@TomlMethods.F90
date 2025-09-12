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

SUBMODULE(FEDOF_Class) TomlMethods
USE GlobalData, ONLY: stdout, CHAR_LF
USE Display_Method, ONLY: Display, ToString
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value, &
                 toml_serialize
USE String_Class, ONLY: String
USE FEFactory_Method, ONLY: FEFactory
USE ReferenceElement_Method, ONLY: GetElementIndex
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

INTEGER(I4B), ALLOCATABLE :: order(:)
INTEGER(I4B) :: totalTopo, topoList(8), nsd, ii, jj, elemType, tsize
LOGICAL(LGT) :: islocal, isFound

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
obj%dom => dom
obj%mesh => obj%dom%GetMeshPointer()

islocal = .FALSE.
CALL ImportOrderFromToml(table=table, order=order, islocal=islocal, &
                         isFound=isFound)

IF (.NOT. isFound) THEN
  tsize = obj%mesh%GetTotalElements()
  CALL Reallocate(order, tsize)
  CALL obj%mesh%GetOrder_(order=order, tsize=tsize)
  islocal = .TRUE.
END IF

CALL ImportScaleForQuadOrderFromToml(obj=obj, table=table)
CALL obj%AllocateSizes()
CALL obj%SetCellOrder(order=order, islocal=islocal)
CALL obj%SetFaceOrder()
CALL obj%SetEdgeOrder()
CALL obj%SetOrdersFromCellOrder()

topoList = obj%mesh%GetElemTopology()
totalTopo = obj%mesh%GetTotalTopology()
nsd = obj%mesh%GetNSD()

DO ii = 1, totalTopo
  elemType = topoList(ii)
  jj = GetElementIndex(elemType)
  obj%fe(jj)%ptr => FEFactory(table=table)
  CALL obj%fe(jj)%ptr%ImportFromToml(elemType=elemType, table=table, nsd=nsd)
  obj%baseInterpolation = obj%fe(jj)%ptr%GetBaseInterpolation()
  obj%baseContinuity = obj%fe(jj)%ptr%GetBaseContinuity()
END DO

IF (obj%baseInterpolation .EQ. "LAGR") obj%isLagrange = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                         ImportOrderFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportOrderFromToml(table, order, islocal, isFound)
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: order(:)
  LOGICAL(LGT), INTENT(OUT) :: islocal
  LOGICAL(LGT), INTENT(OUT) :: isFound

  ! Internal variables
  INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportOrderFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading islocal...')
#endif

  ! islocal
  CALL GetValue(table=table, key="islocal", VALUE=islocal, &
                default_value=.FALSE., origin=origin, stat=stat, &
                isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading order...')
#endif

  ! order
  CALL GetValue(table=table, key="order", VALUE=order, &
                origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportOrderFromToml

!----------------------------------------------------------------------------
!                                            ImportScaleForQuadOrderFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportScaleForQuadOrderFromToml(obj, table)
  CLASS(FEDOF_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  ! internal variables
  INTEGER(I4B) :: origin, stat, scaleForQuadOrder
  LOGICAL(LGT) :: isFound
  INTEGER(I4B), PARAMETER :: default_value = 1

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportScaleForQuadOrderFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="scaleForQuadOrder", &
                VALUE=scaleForQuadOrder, &
                default_value=default_value, &
                origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  IF (.NOT. isFound) THEN
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'scaleForQuadOrder not found, using default value ')
  END IF
#endif

  obj%scaleForQuadOrder = INT(scaleForQuadOrder, kind=INT8)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportScaleForQuadOrderFromToml

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
#endif

TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  'following error occured while reading '// &
             'the toml file :: cannot find ['//tomlName//"] table in config.")
#endif

CALL obj%ImportFromToml(table=node, dom=dom)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF, &
               unitNo=stdout)
END IF
#endif

node => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods
