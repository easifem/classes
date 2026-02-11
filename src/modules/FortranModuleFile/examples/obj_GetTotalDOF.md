---
date: 2025-07-14
summary:  Get the total degree of freedom

---
 
# obj_GetTotalDOF
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE FUNCTION obj_GetTotalDOF(obj) RESULT(ans)
CLASS(BasisOpt_), INTENT(IN) :: obj

INTEGER(I4B) :: ans

END FUNCTION obj_GetTotalDOF
END INTERFACE
```
 
