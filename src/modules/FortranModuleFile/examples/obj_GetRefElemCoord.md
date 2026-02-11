---
date: 2025-10-13
summary: Get the reference element coordinates

---
 
# obj_GetRefElemCoord
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_GetRefElemCoord(obj, ans, nrow, ncol)
CLASS(BasisOpt_), INTENT(IN) :: obj

REAL(DFP), INTENT(INOUT) :: ans(:, :)

INTEGER(I4B), INTENT(OUT) :: nrow

INTEGER(I4B), INTENT(OUT) :: ncol

END SUBROUTINE obj_GetRefElemCoord
END INTERFACE
```
 
