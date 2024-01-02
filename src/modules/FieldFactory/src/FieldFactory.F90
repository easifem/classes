! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This modules is a factory for linear solver, vector and matrix

MODULE FieldFactory
USE GlobalData
USE String_Class
USE Field
USE Domain_Class, ONLY: Domain_, DomainPointer_
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "FieldFactory"

PUBLIC :: AbstractMatrixFieldFactory
PUBLIC :: MatrixFieldFactory
PUBLIC :: BlockMatrixFieldFactory
PUBLIC :: InitiateMatrixFields

PUBLIC :: NodeFieldFactory
PUBLIC :: BlockNodeFieldFactory
PUBLIC :: ScalarFieldFactory
PUBLIC :: VectorFieldFactory
PUBLIC :: STScalarFieldFactory
PUBLIC :: STVectorFieldFactory
PUBLIC :: InitiateScalarFields
PUBLIC :: InitiateVectorFields
PUBLIC :: InitiateSTScalarFields
PUBLIC :: InitiateSTVectorFields

PUBLIC :: MeshFieldFactory
PUBLIC :: ScalarMeshFieldFactory
PUBLIC :: VectorMeshFieldFactory
PUBLIC :: TensorMeshFieldFactory

!----------------------------------------------------------------------------
!                                                           MeshFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-09-14
! summary: This function returns child of AbstractMeshField

INTERFACE
  MODULE FUNCTION MeshFieldFactory(engine, name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: engine
    CHARACTER(*), INTENT(IN) :: name
    CLASS(AbstractMeshField_), POINTER :: ans
  END FUNCTION MeshFieldFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ScalarMeshFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-09-14
! summary: This function returns subclass of AbstractScalarMeshField

INTERFACE
  MODULE FUNCTION ScalarMeshFieldFactory(engine, name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: engine
    CHARACTER(*), INTENT(IN) :: name
    CLASS(AbstractScalarMeshField_), POINTER :: ans
  END FUNCTION ScalarMeshFieldFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                     VectorMeshFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-09-14
! summary: This function returns subclass of AbstractVectorMeshField

INTERFACE
  MODULE FUNCTION VectorMeshFieldFactory(engine, name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: engine
    CHARACTER(*), INTENT(IN) :: name
    CLASS(AbstractVectorMeshField_), POINTER :: ans
  END FUNCTION VectorMeshFieldFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                     TensorMeshFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-09-14
! summary: This function returns subclass of AbstractTensorMeshField

INTERFACE
  MODULE FUNCTION TensorMeshFieldFactory(engine, name) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: engine
    CHARACTER(*), INTENT(IN) :: name
    CLASS(AbstractTensorMeshField_), POINTER :: ans
  END FUNCTION TensorMeshFieldFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                        MatrixFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This function returns child of [[AbstractMatrixField_]]

INTERFACE
  MODULE FUNCTION AbstractMatrixFieldFactory(engine, name) RESULT(Ans)
    CLASS(AbstractMatrixField_), POINTER :: ans
    CHARACTER(*), INTENT(IN) :: engine
    CHARACTER(*), INTENT(IN) :: name
  END FUNCTION AbstractMatrixFieldFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                        MatrixFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This function returns child of [[AbstractMatrixField_]]

INTERFACE
  MODULE FUNCTION MatrixFieldFactory(engine) RESULT(Ans)
    CLASS(MatrixField_), POINTER :: ans
    CHARACTER(*), INTENT(IN) :: engine
  END FUNCTION MatrixFieldFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                   BlockMatrixFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This function returns child of [[AbstractMatrixField_]]

INTERFACE
  MODULE FUNCTION BlockMatrixFieldFactory(engine) RESULT(Ans)
    CLASS(BlockMatrixField_), POINTER :: ans
    CHARACTER(*), INTENT(IN) :: engine
  END FUNCTION BlockMatrixFieldFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                         NodeFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This function returns child of [[AbstractNodeField_]]

INTERFACE
  MODULE FUNCTION NodeFieldFactory(engine, datatype) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: engine
    CHARACTER(*), INTENT(IN) :: datatype
    CLASS(AbstractNodeField_), POINTER :: ans
  END FUNCTION NodeFieldFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                      BlockNodeFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: Returns child of [[BlockNodeField_]] based on engine

INTERFACE
  MODULE FUNCTION BlockNodeFieldFactory(engine) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: engine
    CLASS(BlockNodeField_), POINTER :: ans
  END FUNCTION BlockNodeFieldFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                         ScalarFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: Returns child of [[AbstractNodeField_]] based on engine

INTERFACE
  MODULE FUNCTION ScalarFieldFactory(engine) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: engine
    CLASS(ScalarField_), POINTER :: ans
  END FUNCTION ScalarFieldFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                         VectorFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This function returns child of [[AbstractNodeField_]]

INTERFACE
  MODULE FUNCTION VectorFieldFactory(engine) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: engine
    CLASS(VectorField_), POINTER :: ans
  END FUNCTION VectorFieldFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                      STScalarFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: Returns child of [[STScalarField_]] based on engine

INTERFACE
  MODULE FUNCTION STScalarFieldFactory(engine) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: engine
    CLASS(STScalarField_), POINTER :: ans
  END FUNCTION STScalarFieldFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                                      STVectorFieldFactory
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: Returns child of [[STVectorField_]] based on engine

INTERFACE
  MODULE FUNCTION STVectorFieldFactory(engine) RESULT(Ans)
    CHARACTER(*), INTENT(IN) :: engine
    CLASS(STVectorField_), POINTER :: ans
  END FUNCTION STVectorFieldFactory
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Initiate a vector of ScalarFieldPointer_
!
!# Introduction
!
! This routine initiates several vector of ScalarField and
! its subclasses.
!
! Many times we need to initiate the scalar field of same structures.
! Calling intiate methods on each field increases the
! code repeatition.
! Therefore, we can call this method  instead. This method
! will create ScalarField of same type. They just have different
! names.
!
! NOTE: This is a module routine not a Method to ScalarField_

INTERFACE InitiateScalarFields
  MODULE SUBROUTINE ScalarField_Initiate1(obj, names, fieldType, engine, dom)
    TYPE(ScalarFieldPointer_), INTENT(INOUT) :: obj(:)
    !! A vector of pointer to ScalarField or subclass
    !! NOTE: It should be allocated
    TYPE(String), INTENT(IN) :: names(:)
    !! names of vector field
    !! NOTE: The size of names should be at least the size of obj
    INTEGER(I4B), INTENT(IN) :: fieldType
    !! NOTE: Field type, for info see documentation of AbstractNodeField_
    CHARACTER(*), INTENT(IN) :: engine
    !! Engine, for info see documentation of AbstractNodeField_
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    !! pointer to the domain
  END SUBROUTINE ScalarField_Initiate1
END INTERFACE InitiateScalarFields

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Initiate a vector of ScalarFieldPointer_
!
!# Introduction
!
! This routine initiates several vector of ScalarField and
! its subclasses.
!
! Many times we need to initiate the scalar field of same structures.
! Calling intiate methods on each field increases the
! code repeatition.
! Therefore, we can call this method  instead. This method
! will create instances of ScalarField and its subclass.
!
! INFO: This routine is same as ScalarField_Initiate1 but
! here, we can set different properties to each vector field.
!
! NOTE: This is a module routine not a Method to ScalarField_

INTERFACE InitiateScalarFields
  MODULE SUBROUTINE ScalarField_Initiate2(obj, names, fieldType, engine,  &
    & dom)
    TYPE(ScalarFieldPointer_), INTENT(INOUT) :: obj(:)
    !! A vector of pointer to ScalarField or subclass
    !! NOTE: It should be allocated
    TYPE(String), INTENT(IN) :: names(:)
    !! names of vector field
    !! NOTE: The size of names should be at least the size of obj
    INTEGER(I4B), INTENT(IN) :: fieldType(:)
    !! NOTE: Field type, for info see documentation of AbstractNodeField_
    TYPE(String), INTENT(IN) :: engine(:)
    !! Engine, for info see documentation of AbstractNodeField_
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
    !! pointer to the domain
  END SUBROUTINE ScalarField_Initiate2
END INTERFACE InitiateScalarFields

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2023-12-16
! summary: Initiate a vector of STScalarFieldPointer_
!
!# Introduction
!
! This routine initiates several vector of STScalarField and
! its subclasses.
!
! NOTE: This is a module routine not a Method to STScalarField_

INTERFACE InitiateSTScalarFields
  MODULE SUBROUTINE STScalarField_Initiate1(obj, names, timeCompo, &
    & fieldType, engine, dom)
    TYPE(STScalarFieldPointer_), INTENT(INOUT) :: obj(:)
    !! A vector of pointer to STScalarField or subclass
    !! NOTE: It should be allocated
    TYPE(String), INTENT(IN) :: names(:)
    !! names of vector field
    !! NOTE: The size of names should be at least the size of obj
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! temporal components in STScalarField
    INTEGER(I4B), INTENT(IN) :: fieldType
    !! NOTE: Field type, for info see documentation of AbstractNodeField_
    CHARACTER(*), INTENT(IN) :: engine
    !! Engine, for info see documentation of AbstractNodeField_
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    !! pointer to the domain
  END SUBROUTINE STScalarField_Initiate1
END INTERFACE InitiateSTScalarFields

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2023-12-16
! summary: Initiate a vector of STScalarFieldPointer_
!
!# Introduction
!
! This routine initiates several vector of STScalarField and
! its subclasses.
!
! INFO: This routine is same as STScalarField_Initiate1 but
! here, we can set different properties to each vector field.
!
! NOTE: This is a module routine not a Method to ScalarField_

INTERFACE InitiateSTScalarFields
  MODULE SUBROUTINE STScalarField_Initiate2(obj, names, timeCompo, &
    & fieldType, engine, dom)
    TYPE(STScalarFieldPointer_), INTENT(INOUT) :: obj(:)
    !! A vector of pointer to STScalarField or subclass
    !! NOTE: It should be allocated
    TYPE(String), INTENT(IN) :: names(:)
    !! names of vector field
    !! NOTE: The size of names should be at least the size of obj
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! temporal components in STScalarField
    INTEGER(I4B), INTENT(IN) :: fieldType(:)
    !! NOTE: Field type, for info see documentation of AbstractNodeField_
    TYPE(String), INTENT(IN) :: engine(:)
    !! Engine, for info see documentation of AbstractNodeField_
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
    !! pointer to the domain
  END SUBROUTINE STScalarField_Initiate2
END INTERFACE InitiateSTScalarFields

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Initiate a vector of VectorFieldPointer_
!
!# Introduction
!
! This routine initiates several vector of VectorField and
! its subclasses.
!
! Many times we need to initiate the vector field of same structures.
! Calling intiate methods on each vector field increases the
! code repeatition.
! Therefore, we can call this method  instead. This method
! will create vectorfield of same type. They just have different
! names.
!
! NOTE: This is a module routine not a Method to VectorField_

INTERFACE InitiateVectorFields
  MODULE SUBROUTINE VectorField_Initiate1(obj, names, spaceCompo, &
    &  fieldType, engine, dom)
    TYPE(VectorFieldPointer_), INTENT(INOUT) :: obj(:)
    !! A vector of pointer to VectorField or subclass
    !! NOTE: It should be allocated
    TYPE(String), INTENT(IN) :: names(:)
    !! names of vector field
    !! NOTE: The size of names should be at least the size of obj
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! spatial components in vector field
    INTEGER(I4B), INTENT(IN) :: fieldType
    !! NOTE: Field type, for info see documentation of AbstractNodeField_
    CHARACTER(*), INTENT(IN) :: engine
    !! Engine, for info see documentation of AbstractNodeField_
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    !! pointer to the domain
  END SUBROUTINE VectorField_Initiate1
END INTERFACE InitiateVectorFields

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Initiate a vector of VectorFieldPointer_
!
!# Introduction
!
! This routine initiates several vector of VectorField and
! its subclasses.
!
! Many times we need to initiate the vector field of same structures.
! Calling intiate methods on each vector field increases the
! code repeatition.
! Therefore, we can call this method  instead. This method
! will create instances of vectorfield and its subclass.
!
! INFO: This routine is same as VectorField_Initiate1 but
! here, we can set different properties to each vector field.
!
! NOTE: This is a module routine not a Method to VectorField_

INTERFACE InitiateVectorFields
  MODULE SUBROUTINE VectorField_Initiate2(obj, names, spaceCompo,  &
     & fieldType, engine, dom)
    TYPE(VectorFieldPointer_), INTENT(INOUT) :: obj(:)
    !! A vector of pointer to VectorField or subclass
    !! NOTE: It should be allocated
    TYPE(String), INTENT(IN) :: names(:)
    !! names of vector field
    !! NOTE: The size of names should be at least the size of obj
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! spatial components in vector field
    INTEGER(I4B), INTENT(IN) :: fieldType(:)
    !! NOTE: Field type, for info see documentation of AbstractNodeField_
    TYPE(String), INTENT(IN) :: engine(:)
    !! Engine, for info see documentation of AbstractNodeField_
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
    !! pointer to the domain
  END SUBROUTINE VectorField_Initiate2
END INTERFACE InitiateVectorFields

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2023-12-16
! summary: Initiate a vector of STVectorFieldPointer_
!
!# Introduction
!
! This routine initiates several vector of STVectorField and
! its subclasses.
!
! NOTE: This is a module routine not a Method to STVectorField_

INTERFACE InitiateSTVectorFields
  MODULE SUBROUTINE STVectorField_Initiate1(obj, names, spaceCompo, &
    & timeCompo, fieldType, engine, dom)
    TYPE(STVectorFieldPointer_), INTENT(INOUT) :: obj(:)
    !! A vector of pointer to STVectorField or subclass
    !! NOTE: It should be allocated
    TYPE(String), INTENT(IN) :: names(:)
    !! names of vector field
    !! NOTE: The size of names should be at least the size of obj
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! spatial components in vector field
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! temporal components in vector field
    INTEGER(I4B), INTENT(IN) :: fieldType
    !! NOTE: Field type, for info see documentation of AbstractNodeField_
    CHARACTER(*), INTENT(IN) :: engine
    !! Engine, for info see documentation of AbstractNodeField_
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    !! pointer to the domain
  END SUBROUTINE STVectorField_Initiate1
END INTERFACE InitiateSTVectorFields

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2023-12-16
! summary: Initiate a vector of STVectorFieldPointer_
!
!# Introduction
!
! This routine initiates several vector of STVectorField and
! its subclasses.
!
! INFO: This routine is same as STVectorField_Initiate1 but
! here, we can set different properties to each vector field.
!
! NOTE: This is a module routine not a Method to STVectorField_

INTERFACE InitiateSTVectorFields
  MODULE SUBROUTINE STVectorField_Initiate2(obj, names, spaceCompo,  &
     & timeCompo, fieldType, engine, dom)
    TYPE(STVectorFieldPointer_), INTENT(INOUT) :: obj(:)
    !! A vector of pointer to VectorField or subclass
    !! NOTE: It should be allocated
    TYPE(String), INTENT(IN) :: names(:)
    !! names of vector field
    !! NOTE: The size of names should be at least the size of obj
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! spatial components in vector field
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! temporal components in vector field
    INTEGER(I4B), INTENT(IN) :: fieldType(:)
    !! NOTE: Field type, for info see documentation of AbstractNodeField_
    TYPE(String), INTENT(IN) :: engine(:)
    !! Engine, for info see documentation of AbstractNodeField_
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
    !! pointer to the domain
  END SUBROUTINE STVectorField_Initiate2
END INTERFACE InitiateSTVectorFields

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Initiate a vector of MatrixFieldPointer_

INTERFACE InitiateMatrixFields
MODULE SUBROUTINE MatrixField_Initiate1(obj, names, matrixProps, spaceCompo, &
                                         &  timeCompo, fieldType, engine, dom)
    TYPE(MatrixFieldPointer_), INTENT(INOUT) :: obj(:)
    !! A vector of pointer to MatrixField or subclass
    !! NOTE: It should be allocated
    TYPE(String), INTENT(IN) :: names(:)
    !! names of MatrixField
    !! NOTE: The size of names should be at least the size of obj
    CHARACTER(*), INTENT(IN) :: matrixProps
    !! properties of of MatrixField
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! spatial components in MatrixField
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! temporal components in MatrixField
    INTEGER(I4B), INTENT(IN) :: fieldType
    !! NOTE: Field type, for info see documentation of AbstractNodeField_
    CHARACTER(*), INTENT(IN) :: engine
    !! Engine, for info see documentation of AbstractNodeField_
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    !! pointer to the domain
  END SUBROUTINE MatrixField_Initiate1
END INTERFACE InitiateMatrixFields

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Initiate a vector of MatrixFieldPointer_

INTERFACE InitiateMatrixFields
  MODULE SUBROUTINE MatrixField_Initiate2(obj, names, matrixProps, spaceCompo,  &
     & timeCompo, fieldType, engine, dom)
    TYPE(MatrixFieldPointer_), INTENT(INOUT) :: obj(:)
    !! A vector of pointer to MatrixField or subclass
    !! NOTE: It should be allocated
    TYPE(String), INTENT(IN) :: names(:)
    !! names of MatrixField
    !! NOTE: The size of names should be at least the size of obj
    TYPE(String), INTENT(IN) :: matrixProps(:)
    !! properties of of MatrixField
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! spatial components in MatrixField
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! temporal components in MatrixField
    INTEGER(I4B), INTENT(IN) :: fieldType(:)
    !! NOTE: Field type, for info see documentation of AbstractNodeField_
    TYPE(String), INTENT(IN) :: engine(:)
    !! Engine, for info see documentation of AbstractNodeField_
    TYPE(DomainPointer_), TARGET, INTENT(IN) :: dom(:)
    !! pointer to the domain
  END SUBROUTINE MatrixField_Initiate2
END INTERFACE InitiateMatrixFields

END MODULE FieldFactory
