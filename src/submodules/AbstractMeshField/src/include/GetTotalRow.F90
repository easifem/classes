!----------------------------------------------------------------------------
!                                                           GetTotalRow
!----------------------------------------------------------------------------

FUNCTION GetTotalRow(rank, varType) RESULT(nrow)
  INTEGER(I4B), INTENT(IN) :: rank, varType
  INTEGER(I4B) :: nrow

  SELECT CASE (rank)

  CASE (Scalar)
    SELECT CASE (varType)
    CASE (Constant, Space, Time)
      nrow = 1
      ! one dimension, single entry
      ! one dimension, multiple entries in space
      ! one dimension, multiple entries in time
    CASE (SpaceTime)
      ! two dimensions, multiple entries in space-time
      nrow = 2
    END SELECT

  CASE (Vector)
    SELECT CASE (varType)
    CASE (Constant)
      ! one dimension, only vector components
      nrow = 1
    CASE (Space, Time)
      nrow = 2
      ! two dimension, vector components and space values
      ! two dimension, vector components and time values
    CASE (SpaceTime)
      ! two dimension, vector components, space and time values
      nrow = 3
    END SELECT

  CASE (Matrix)
    SELECT CASE (varType)
    CASE (Constant)
      ! two dimensions, matrix components
      nrow = 2
    CASE (Space, Time)
      ! three dimensions, matrix components and space values
      ! three dimensions, matrix components and time values
      nrow = 3
    CASE (SpaceTime)
      ! four dimensions, matrix components, space and time values
      nrow = 4
    END SELECT
  END SELECT
END FUNCTION GetTotalRow
