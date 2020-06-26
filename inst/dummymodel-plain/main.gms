*' @title Dummy Model
*'
*' @description The *Dummy Model* (DumM) is a simple test model to test and
*' present the basic functionality of the gms and goxygen packages (@gms, @goxygen).
*'
*' It has the structure of a plain GAMS model and can be interpreted
*' by goxygen, but has no content and cannot be solved with GAMS.
*'
*' @limitations It is not really working as it is just an example.

$include "./equations.gms"

*' @stop The following code will be ignored by goxygen until the next identifier.

*' Even goxygen comments are ignored.

display v01_intern.l;

*' @code Let's add some standard code

display vm_exchange.l;

*' You can also link other gms files, e.g. the equation file of this model [equations]

