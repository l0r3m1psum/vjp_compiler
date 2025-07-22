This repository contains the result of my master thesis: my implementation of
symbolic *matrix calculus*, in particular the the special case of functions of
matrix input and scalar output.

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
