---
date: 2025-07-15
summary:  Get total quadrature points for the basis functions

---
 
# Introduction
   You get the quadrature poitns based on the current state of the object
 You can change the state of the object by calling SetParam method

 
## Interface
 
 
```fortran
INTERFACE
MODULE FUNCTION obj_GetTotalQuadraturePoints(obj) RESULT(ans)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

INTEGER(I4B) :: ans

END FUNCTION obj_GetTotalQuadraturePoints
END INTERFACE
```
 
