!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Testing the toml methods of MeshSelection_

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: FPL_FINALIZE
USE FPL, ONLY: FPL_INIT
USE FPL, ONLY: ParameterList_
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
  CALL OK(obj%IsSelectionByElemNum(), "ImportFromToml: table sets flag")
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
END BLOCK

! Test 2: ImportParamFromToml reads only the flags
BLOCK
  TYPE(toml_table), ALLOCATABLE :: table
  TYPE(toml_table), POINTER :: node
  TYPE(ParameterList_) :: param
  TYPE(MeshSelection_) :: other
  INTEGER(I4B) :: origin, STAT

  CALL FPL_INIT()
  CALL param%Initiate()

  CALL GetValue(table=table, filename=filename)
  node => NULL()
  CALL toml_get(table, "region", node, origin=origin, STAT=STAT, &
                requested=math%no)
  CALL OK(ASSOCIATED(node), "region table is found")

  CALL other%ImportParamFromToml(param=param, table=node)
  CALL other%Initiate(param=param)
  CALL OK(other%IsSelectionByMeshID(), "ImportParamFromToml: meshID")
  CALL OK(.NOT. other%IsSelectionByElemNum(), &
          "ImportParamFromToml: elemNum")
  CALL OK(.NOT. other%IsMeshIDAllocated(dim=0), &
          "ImportParamFromToml: no data")

  ! Test 3: ImportFromToml from a table, it deallocates the old data
  CALL obj%ImportFromToml(table=node)
  CALL OK(ALL(obj%GetMeshID(dim=1) .EQ. [2, 5, 6, 7]), &
          "ImportFromToml(table): replaces old data")

  CALL other%DEALLOCATE()
  CALL param%DEALLOCATE()
  CALL FPL_FINALIZE()
  node => NULL()
  DEALLOCATE (table)
END BLOCK

! Test 4: module level generic interface
BLOCK
  TYPE(MeshSelection_) :: other

  CALL MeshSelectionImportFromToml(other, tomlName="region", &
                                   filename=filename)
  CALL OK(other%IsInitiated(), "MeshSelectionImportFromToml")
  CALL other%DEALLOCATE()
END BLOCK

CALL obj%DEALLOCATE()
END PROGRAM main
