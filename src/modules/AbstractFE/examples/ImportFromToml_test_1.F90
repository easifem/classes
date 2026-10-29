!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise AbstractFE_ ImportFromToml operations
!
! Uses a concrete triangle finite element to exercise the public API.
!
PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE tomlf, ONLY: toml_table
USE tomlf, ONLY: set_value
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
IMPLICIT NONE
TYPE(TriangleH1LagrangeFE_) :: obj
TYPE(toml_table) :: table
INTEGER(I4B) :: nsd

CALL set_value(table, "baseContinuity", "H1")
CALL set_value(table, "baseInterpolation", "Lagrange")
CALL obj%ImportFromToml(table=table, elemType=TypeElemNameOpt%Triangle, &
                        nsd=2_I4B)
CALL obj%GetParam(nsd=nsd)
CALL OK(obj%isInit .AND. nsd .EQ. 2_I4B, "TOML table initialization")
CALL OK(obj%GetBaseContinuity() .EQ. "H1", "TOML continuity")
CALL obj%DEALLOCATE()
END PROGRAM main
