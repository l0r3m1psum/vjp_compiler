# Deriving "vector-Jacobian" products with matrix calculus

This repository contains my implementation of symbolic *matrix calculus*, in
particular the the special case of functions of matrix input and scalar output.

To understand why this family of functions is important consider the following
expression *G:F(X)* where *G* and *X* are matrices, *F* a function of matrix
value and *:* the Frobenius inner product. The derivative of that espression is
the "vector-Jacobian" needed by the back-propagation algorithm to make
the "gradient" pass through that function.

The matrix calculus used is the one by Magnus and Naudecker in "Matrix
Diﬀerential Calculus with Applications in Statistics and Econometrics". As far
as I know this is the first computer implementation of part of it. The only
other implementation of matrix calculus that I know of is the on the [Matrix
Calculus](https://www.matrixcalculus.org) website, which supports everything I
do and more, but using a more sofisticated underlying formalism and it is not
opensource.

Below there is an example output from the program:
```
vJp> G:(A X+A X)
Steps for the derivation of
G:(A X+A X)
Equivalent expression to check result on https://www.matrixcalculus.org
tr(G'*(A*X+A*X))
Step 1. Differential application;
G:(A dX+A dX)
#operand=5 #operator=6 sum=11
Step 2. Distribution;
	G:A dX+G:A dX
	G:A dX+G:A dX
G:A dX+G:A dX
#operand=6 #operator=7 sum=13
Step 3. Bring out differentials;
	A' G+A' G
A' G+A' G
#operand=4 #operator=5 sum=9
Step 4. Accumulation;
2.I A' G
#operand=3 #operator=3 sum=6
Step 5. Factorization;
	2.I A' G
2.I A' G
#operand=3 #operator=3 sum=6
2.I A' G
```
