# Space time scalar field



## Introduction



`STScalarField_` is a child of `AbstractNodeField_`. It is defined for handling computation of scalar field in space-time finite element method.



## Getting started



### Construct by using `initiate()`



### Construct by using `import()`



**Template**



| Variable    | Data type |                          Value                          |                                                      Comment |
| ----------- | :-------: | :-----------------------------------------------------: | -----------------------------------------------------------: |
| `restart`*  |   Char    |                       `T` or `F`                        | The default value is False. If it is true then it represents that we are restarting the simulation, in this case more information are necessary. |
| `name`      |  String   |                                                         |                                     Name of the scalar field |
| `fieldType` |  String   | `NORMAL`, `CONSTANT`, `CONSTANT_SPACE`, `CONSTANT_TIME` |                                                              |
| `timeCompo` |  Integer  |                                                         |                                    Number of time components |
|             |           |                                                         |                                                              |

