*' @title Dummy Model
*'
*' @description The *Dummy Model* (DumM) is a simple test model to test and
*' present the basic functionality of the gms and goxygen packages (@gms, @goxygen).
*'
*' It has the structure of a plain GAMS model and can be interpreted
*' by goxygen, but has no content and cannot be solved with GAMS.
*'
*' @authors Bruce Wayne, Max Mustermann
*'
*' @limitations It is not really working as it is just an example.

*' @code

*' Declaration of variables and equation.

variables
  v01_intern(i) internal variable (kg)
;

equations
  q01_equation1 Equation 1 (kg)
  q01_equation2 Equation 2 (kg)
;

*' @equations

*' Here we have some equations:

 q01_equation1 ..
	vm_exchange =e= sum(i,v01_intern(i));

 q01_equation2 ..
    sum(i,v01_intern(i)) =e= 12;

*' This was simple, wasn't it?

*' @stop The following code will be ignored by goxygen until the next identifier.

*' Even goxygen comments are ignored.

display v01_intern.l;

*' @code Let's add some standard code

display vm_exchange.l;


