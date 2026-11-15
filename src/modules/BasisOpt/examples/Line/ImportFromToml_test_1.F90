!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Test for ImportFromToml method

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: math => TypeMathOpt
USE BasisOpt_Class, ONLY: BasisOpt_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
USE tomlf, ONLY: toml_table
USE tomlf, ONLY: set_value
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
TYPE(toml_table) :: table

CALL e%SetQuietMode(math%yes)
CALL set_value(table, "baseContinuity", "H1")
CALL set_value(table, "baseInterpolation", "Lagrange")
CALL set_value(table, "alpha", 0.25_DFP)
CALL obj%ImportFromToml(table=table, elemType=TypeElemNameOpt%line, &
                        nsd=1_I4B)
BLOCK
  REAL(DFP) :: alpha(3)
  CALL obj%GetParam(alpha=alpha)
  CALL OK(ALL(ABS(alpha - 0.25_DFP) .LT. 1.0E-12_DFP), "Scalar expansion")
END BLOCK

BLOCK
  LOGICAL(LGT) :: isBrokenSpace
  CALL obj%GetParam(isBrokenSpace=isBrokenSpace)
  CALL OK(.NOT. isBrokenSpace, &
          "Correct default value of brokenSpace is used")
END BLOCK

CALL OK(obj%GetNSD() .EQ. 1_I4B, "TOML dimension override:")
CALL OK(obj%GetBaseInterpolation() .EQ. "LAGR", "TOML basis family")
CALL obj%SetOrder(order=1_I4B)
CALL OK(obj%GetTotalDOF() .EQ. 2_I4B, "Set approximation order after import")

CALL set_value(table, "brokenSpace", math%yes)
CALL obj%ImportFromToml(table=table, elemType=TypeElemNameOpt%line, &
                        nsd=1_I4B)

BLOCK
  LOGICAL(LGT) :: isBrokenSpace
  CALL obj%GetParam(isBrokenSpace=isBrokenSpace)
  CALL OK(isBrokenSpace, &
          "Correct value of brokenSpace is used")
END BLOCK

CALL obj%DEALLOCATE()
END PROGRAM main
