*' @code

variables
  v02_intern(i) internal variable (kg)
;

equations
  q02_equation1 Equation 1 (kg)
  q02_equation2 Equation 2 (kg)
;


*' @equations

*' Here we have some equations:

 q02_equation1 ..
	vm_exchange =e= sum(i,v02_intern(i));

 q02_equation2 ..
    sum(i,v02_intern(i)) =e= 12;

*' This was simple, wasn't it?

*' @code Let's add some standard code

display vm_exchange.l;
