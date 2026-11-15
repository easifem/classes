PROGRAM main
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractStfemAlgorithm2_Class, ONLY: AbstractStfemAlgorithm2_
USE StfemAlgorithm2FactoryUtility, ONLY: StfemAlgorithm2Factory
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_table
USE tomlf, ONLY: toml_get => get_value
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

CLASS(AbstractStfemAlgorithm2_), POINTER :: obj
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
CHARACTER(*), PARAMETER :: tomlName = "VStfemAlgorithm2"
CHARACTER(*), PARAMETER :: filename = "StfemAlgorithm2_test_1.toml"

obj => NULL()
node => NULL()

CALL GetValue(table=table, filename=filename)

CALL toml_get(table, tomlName, node, origin=origin, &
              requested=math%no, stat=stat)

obj => StfemAlgorithm2Factory(table)

CALL obj%ImportFromToml(tomlName=tomlName, filename=filename)

END PROGRAM main
