!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise ImportFromToml

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE tomlf, ONLY: toml_table
USE tomlf, ONLY: set_value
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
TYPE(toml_table) :: table
REAL(DFP) :: alpha(3)

CALL set_value(table, "baseContinuity", "H1")
CALL set_value(table, "baseInterpolation", "Lagrange")
CALL set_value(table, "alpha", 0.25_DFP)
CALL obj%ImportFromToml(table=table, elemType=TypeElemNameOpt%Triangle, &
                        nsd=2_I4B)
CALL obj%GetParam(alpha=alpha)
CALL OK(obj%GetNSD() .EQ. 2_I4B, "TOML dimension override")
CALL OK(obj%GetBaseInterpolation() .EQ. "LAGR", "TOML basis family")
CALL OK(ALL(ABS(alpha - 0.25_DFP) .LT. 1.0E-12_DFP), "Scalar expansion")
CALL obj%SetOrder(order=1_I4B)
CALL OK(obj%GetTotalDOF() .EQ. 3_I4B, "Set approximation order after import")
CALL obj%DEALLOCATE()
END PROGRAM main
