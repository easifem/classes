---
date: 2025-07-12
summary:  Set the total degree of freedom from the order

---
 
# obj_SetTotalDOF
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_SetTotalDOF(obj, tdof)
CLASS(BasisOpt_), INTENT(INOUT) :: obj
!! Basis options
INTEGER(I4B), INTENT(IN) :: tdof
!! total degree of freedom
END SUBROUTINE obj_SetTotalDOF
END INTERFACE
```
 
