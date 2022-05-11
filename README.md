# Haskell AST Pretty Printer

### Basic Understading

The AST is used to depict the syntactical structure of the language in a tree format. Since the
underlying data structure is the tree, the functions that we need to define (i.e., ppStencilDef,
ppArgDecl, etc.) are going to traverse over the entire tree (i.e., AST, FunctionSignature,
MainArgDecls, ASTInstance) and convert it into a string representation. During this process
the functions should only take into account parts of the defined premitive and higher-order
types and append it to the string representation. For example: A FVec in AST looks like:
FVec [(-1,92)] (Scalar VI DFloat "dz1_0") and the string representation should look like: FVec
[(-1,92)] Float. The 5 functions that we are asked to define convert the AST format into the
string representations for pretty printing.

### Method Explanations

##### ppStencilDef

This function takes in a StencilDefinition and it should convert it into a string presentation.
This would be straight forward as the datatype StencilDefinition consists of a tuple containing
a string and a list of integers. The function would just convert them into a string with an
equals sign between them.

##### ppArgDecl

This function would be similar to ppStencilDef and would also use ppArgDeclType to get the
type and ppArgName to get the name of the argument declaration. The string representation
would have < argName > :: < argType >.

##### ppFSig, ppLHSExpr, ppRHSExpr

These three function are support to perform the same task as that of the mentioned above
although they will be using recursion. The recursion is due to the fact that an Expr type consists of primitive types and higher-order types which can contain an Expr type. For example:
A Tuple is a list of Expr (i.e., Tuple [Expr]), therefore, it can contain multiple Expr types. The
recursion will come in handy here as it will traverse of all the Expr types in the AST.
