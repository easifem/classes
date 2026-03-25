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
SUBMODULE(GmshStructuredMesh2_Class) TomlMethods
USE tomlf, ONLY: toml_array
USE tomlf, ONLY: toml_get => get_value
USE tomlf, ONLY: toml_len => len
USE tomlf, ONLY: toml_serialize
USE TomlUtility, ONLY: GetValue
USE BaseType, ONLY: math => TypeMathOpt
USE ReallocateUtility, ONLY: Reallocate
USE StringUtility, ONLY: Uppercase

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: &
  modName = "GmshStructuredMesh2_Class@TomlMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

TYPE(String) :: filename
LOGICAL(LGT) :: recombineAll
REAL(DFP), ALLOCATABLE :: pointsOnAxis1(:, :), pointsOnAxis2(:, :)
INTEGER(I4B), ALLOCATABLE :: transfinitePointsOnAxis1(:)
INTEGER(I4B), ALLOCATABLE :: transfinitePointsOnAxis2(:)
INTEGER(I4B), ALLOCATABLE :: meshTypeOnAxis1(:)
INTEGER(I4B), ALLOCATABLE :: meshTypeOnAxis2(:)
REAL(DFP), ALLOCATABLE :: coeffOnAxis1(:)
REAL(DFP), ALLOCATABLE :: coeffOnAxis2(:)
INTEGER(I4B) :: defaultSize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL ImportFileNameFromToml(table=table, ans=filename)
CALL ImportRecombineAllFromToml(table=table, ans=recombineAll)
CALL ImportPointsOnAxis1FromToml(table=table, ans=pointsOnAxis1)
CALL ImportPointsOnAxis2FromToml(table=table, ans=pointsOnAxis2)
CALL ImportTransfinitePointsOnAxis1FromToml(table=table, &
                                            ans=transfinitePointsOnAxis1)
CALL ImportTransfinitePointsOnAxis2FromToml(table=table, &
                                            ans=transfinitePointsOnAxis2)
defaultSize = SIZE(pointsOnAxis1, 2) - 1
CALL ImportMeshTypeOnAxis1FromToml(table=table, &
                                   ans=meshTypeOnAxis1, &
                                   defaultSize=defaultSize)

CALL ImportCoeffOnAxis1FromToml(table=table, &
                                ans=coeffOnAxis1, &
                                defaultSize=defaultSize)

defaultSize = SIZE(pointsOnAxis2, 2) - 1
CALL ImportMeshTypeOnAxis2FromToml(table=table, &
                                   ans=meshTypeOnAxis2, &
                                   defaultSize=defaultSize)

CALL ImportCoeffOnAxis2FromToml(table=table, &
                                ans=coeffOnAxis2, &
                                defaultSize=defaultSize)

CALL obj%Initiate( &
  filename=filename%Chars(), &
  pointsOnAxis1=pointsOnAxis1, &
  pointsOnAxis2=pointsOnAxis2, &
  transfinitePointsOnAxis1=transfinitePointsOnAxis1, &
  transfinitePointsOnAxis2=transfinitePointsOnAxis2, &
  recombineAll=recombineAll, &
  meshTypeOnAxis1=meshTypeOnAxis1, &
  meshTypeOnAxis2=meshTypeOnAxis2, &
  coeffOnAxis1=coeffOnAxis1, &
  coeffOnAxis2=coeffOnAxis2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                     ImportFileNameFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportFileNameFromToml(table, ans)
  CLASS(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(INOUT) :: ans

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportFileNameFromToml()"
#endif
  CHARACTER(*), PARAMETER :: default_value = "GmshStructuredMesh2.msh"
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="filename", VALUE=ans, &
                origin=origin, stat=stat, isfound=isok, &
                default_value=default_value)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, "filename not found.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportFileNameFromToml

!----------------------------------------------------------------------------
!                                                ImportRecombineAllFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportRecombineAllFromToml(table, ans)
  CLASS(toml_table), INTENT(INOUT) :: table
  LOGICAL(LGT), INTENT(INOUT) :: ans

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportRecombineAllFromToml()"
#endif
  LOGICAL(LGT), PARAMETER :: default_value = math%yes
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="recombineAll", VALUE=ans, &
                origin=origin, stat=stat, isfound=isok, &
                default_value=default_value)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportRecombineAllFromToml

!----------------------------------------------------------------------------
!                                                ImportPointsOnAxis1FromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportPointsOnAxis1FromToml(table, ans)
  CLASS(toml_table), INTENT(INOUT) :: table
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportPointsOnAxis1FromToml()"
#endif
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  REAL(DFP), ALLOCATABLE :: temp(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="pointsOnAxis1", VALUE=temp, &
                origin=origin, stat=stat, isfound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, "pointsOnAxis1 not found.")
#endif

  ans = TRANSPOSE(temp)

  isok = ALLOCATED(temp)
  IF (isok) DEALLOCATE (temp)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportPointsOnAxis1FromToml

!----------------------------------------------------------------------------
!                                                ImportPointsOnAxis2FromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportPointsOnAxis2FromToml(table, ans)
  CLASS(toml_table), INTENT(INOUT) :: table
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportPointsOnAxis2FromToml()"
#endif
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  REAL(DFP), ALLOCATABLE :: temp(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="pointsOnAxis2", VALUE=temp, &
                origin=origin, stat=stat, isfound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, "pointsOnAxis2 not found.")
#endif

  ans = TRANSPOSE(temp)

  isok = ALLOCATED(temp)
  IF (isok) DEALLOCATE (temp)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportPointsOnAxis2FromToml

!----------------------------------------------------------------------------
!                                     ImportTransfinitePointsOnAxis1FromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportTransfinitePointsOnAxis1FromToml(table, ans)
  CLASS(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: ans(:)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: &
    myName = "ImportTransfinitePointsOnAxis1FromToml()"
#endif
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="transfinitePointsOnAxis1", VALUE=ans, &
                origin=origin, stat=stat, isfound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, "transfinitePointsOnAxis1 not found.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportTransfinitePointsOnAxis1FromToml

!----------------------------------------------------------------------------
!                                     ImportTransfinitePointsOnAxis2FromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportTransfinitePointsOnAxis2FromToml(table, ans)
  CLASS(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: ans(:)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: &
    myName = "ImportTransfinitePointsOnAxis2FromToml()"
#endif
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="transfinitePointsOnAxis2", VALUE=ans, &
                origin=origin, stat=stat, isfound=isok)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, "transfinitePointsOnAxis2 not found.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportTransfinitePointsOnAxis2FromToml

!----------------------------------------------------------------------------
!                                              ImportMeshTypeOnAxis1FromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportMeshTypeOnAxis1FromToml(table, ans, defaultSize)
  CLASS(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(IN) :: defaultSize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: &
    myName = "ImportMeshTypeOnAxis1FromToml()"
#endif
  INTEGER(I4B) :: origin, stat, tsize, ii, meshtype
  LOGICAL(LGT) :: isok
  TYPE(String), ALLOCATABLE :: strs(:)
  CHARACTER(1) :: oneChar

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="meshTypeOnAxis1", VALUE=strs, &
                origin=origin, stat=stat, isfound=isok)

  IF (isok) THEN

    tsize = SIZE(strs)

    IF (tsize .EQ. 1) THEN

      CALL Reallocate(ans, defaultSize)
      oneChar(1:1) = strs(1)%slice(1, 1)
      oneChar = UpperCase(oneChar)
      IF (oneChar .EQ. "P") THEN
        meshType = TypeGmshStructuredMesh2Opt%progression
      ELSE
        meshType = TypeGmshStructuredMesh2Opt%bump
      END IF
      ans = meshType

    ELSE

      CALL Reallocate(ans, tsize)

      DO ii = 1, tsize

        oneChar(1:1) = strs(ii)%slice(1, 1)
        oneChar = UpperCase(oneChar)
        IF (oneChar .EQ. "P") THEN
          meshType = TypeGmshStructuredMesh2Opt%progression
        ELSE
          meshType = TypeGmshStructuredMesh2Opt%bump
        END IF
        ans(ii) = meshType

      END DO

    END IF

  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportMeshTypeOnAxis1FromToml

!----------------------------------------------------------------------------
!                                              ImportMeshTypeOnAxis2FromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportMeshTypeOnAxis2FromToml(table, ans, defaultSize)
  CLASS(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(IN) :: defaultSize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: &
    myName = "ImportMeshTypeOnAxis2FromToml()"
#endif
  INTEGER(I4B) :: origin, stat, tsize, ii, meshtype
  LOGICAL(LGT) :: isok
  TYPE(String), ALLOCATABLE :: strs(:)
  CHARACTER(1) :: oneChar

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="meshTypeOnAxis2", VALUE=strs, &
                origin=origin, stat=stat, isfound=isok)

  IF (isok) THEN

    tsize = SIZE(strs)

    IF (tsize .EQ. 1) THEN

      CALL Reallocate(ans, defaultSize)
      oneChar(1:1) = strs(1)%slice(1, 1)
      oneChar = UpperCase(oneChar)
      IF (oneChar .EQ. "P") THEN
        meshType = TypeGmshStructuredMesh2Opt%progression
      ELSE
        meshType = TypeGmshStructuredMesh2Opt%bump
      END IF
      ans = meshType

    ELSE

      CALL Reallocate(ans, tsize)

      DO ii = 1, tsize

        oneChar(1:1) = strs(ii)%slice(1, 1)
        oneChar = UpperCase(oneChar)
        IF (oneChar .EQ. "P") THEN
          meshType = TypeGmshStructuredMesh2Opt%progression
        ELSE
          meshType = TypeGmshStructuredMesh2Opt%bump
        END IF
        ans(ii) = meshType

      END DO

    END IF

  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportMeshTypeOnAxis2FromToml

!----------------------------------------------------------------------------
!                                                 ImportCoeffOnAxis1FromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportCoeffOnAxis1FromToml(table, ans, defaultSize)
  CLASS(toml_table), INTENT(INOUT) :: table
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(IN) :: defaultSize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: &
    myName = "ImportCoeffOnAxis1FromToml()"
#endif
  REAL(DFP), ALLOCATABLE :: temp(:)
  INTEGER(I4B) :: origin, stat, tsize
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="coeffOnAxis1", VALUE=temp, &
                origin=origin, stat=stat, isfound=isok)

  IF (isok) THEN
    tsize = SIZE(temp)
    IF (tsize .EQ. 1) THEN
      CALL Reallocate(ans, defaultSize)
      ans = temp(1)
    ELSE
      CALL Reallocate(ans, tsize)
      ans(1:tsize) = temp(1:tsize)
    END IF
  ELSE
    CALL Reallocate(ans, defaultSize)
    ans(:) = math%one
  END IF

  isok = ALLOCATED(temp)
  IF (isok) DEALLOCATE (temp)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportCoeffOnAxis1FromToml

!----------------------------------------------------------------------------
!                                                 ImportCoeffOnAxis2FromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportCoeffOnAxis2FromToml(table, ans, defaultSize)
  CLASS(toml_table), INTENT(INOUT) :: table
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(IN) :: defaultSize

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: &
    myName = "ImportCoeffOnAxis2FromToml()"
#endif
  REAL(DFP), ALLOCATABLE :: temp(:)
  INTEGER(I4B) :: origin, stat, tsize
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue(table=table, key="coeffOnAxis2", VALUE=temp, &
                origin=origin, stat=stat, isfound=isok)

  IF (isok) THEN
    tsize = SIZE(temp)
    IF (tsize .EQ. 1) THEN
      CALL Reallocate(ans, defaultSize)
      ans = temp(1)
    ELSE
      CALL Reallocate(ans, tsize)
      ans(1:tsize) = temp(1:tsize)
    END IF
  ELSE
    CALL Reallocate(ans, defaultSize)
    ans(:) = math%one
  END IF

  isok = ALLOCATED(temp)
  IF (isok) DEALLOCATE (temp)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportCoeffOnAxis2FromToml

!----------------------------------------------------------------------------
!                                                             ImportFromToml
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
                        '[START] ')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

#ifdef DEBUG_VER
isok = ALLOCATED(table)
CALL AssertError1(isok, myName, "table is not allocated from GetValue")
#endif

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=math%no, &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  "cannot find "//tomlName//" table in config.")
#endif

CALL obj%ImportFromToml(table=node)

node => NULL()
DEALLOCATE (table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods
