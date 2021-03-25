title: Mesh

## Introduction

In **EASIFEM** `Mesh_` datatype is simply a collection of finite elements. To avoid working with linked list I have encapsulated a vector of `ElementPointer_`. This way, adding or removing of an element
to existing mesh becomes simple.
