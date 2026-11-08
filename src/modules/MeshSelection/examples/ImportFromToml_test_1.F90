!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Testing the toml methods of MeshSelection_

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: I4B
USE MeshSelection_Class, ONLY: MeshSelection_
USE MeshSelection_Class, ONLY: MeshSelectionImportFromToml
USE Test_Method, ONLY: OK
USE tomlf, ONLY: toml_get => get_value
USE tomlf, ONLY: toml_table
USE TomlUtility, ONLY: GetValue
IMPLICIT NONE
CHARACTER(*), PARAMETER :: filename = "./toml/MeshSelection.toml"
TYPE(MeshSelection_) :: obj
INTEGER(I4B) :: ans(16), tsize
CALL e%SetQuietMode(math%yes)
! Test 1: ImportFromToml reads every table of [region]
BLOCK
  CALL obj%ImportFromToml(tomlName="region", filename=filename)
  CALL OK(obj%IsInitiated(), "ImportFromToml: IsInitiated")
  CALL OK(obj%IsSelectionByMeshID(), "ImportFromToml: meshID flag")
  CALL OK(obj%IsSelectionByElemNum(), &
          "ImportFromToml: elemNum flag, table sets flag")
  CALL OK(obj%IsSelectionByNodeNum(), "ImportFromToml: nodeNum flag")
  CALL OK(obj%IsSelectionByBox(), "ImportFromToml: box flag")
  CALL OK(ALL(obj%GetMeshID(dim=0) .EQ. [1, 2, 3, 4, 5, 6]), "meshID point")
  CALL OK(ALL(obj%GetMeshID(dim=1) .EQ. [2, 5, 6, 7]), "meshID line")
  CALL OK(ALL(obj%GetMeshID(dim=2) .EQ. [1, 3, 5, 6, 8]), "meshID surface")
  CALL OK(ALL(obj%GetMeshID(dim=3) .EQ. [1, 2, 3]), "meshID from txt file")
  CALL obj%GetElemNum(dim=2, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 3, "elemNum: size")
  CALL OK(ALL(ans(1:tsize) .EQ. [1, 2, 3]), "elemNum: values")
  ! Set has collected the nodes of every dimension
  CALL obj%GetNodeNum(ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 11, "nodeNum: size")
  CALL OK(ALL(ans(1:tsize) .EQ. [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]), &
          "nodeNum: values")
  CALL obj%DEALLOCATE()
END BLOCK
END PROGRAM main
