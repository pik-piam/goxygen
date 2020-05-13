*' @code

variables
  v02_complex internal variable (kg)
;

equations
  q02_equation1 Equation 1 (kg)
  q02_equation2 Equation 2 (kg)
;


*' @equations Here we have some equations:

 q02_equation1 ..
	vm_exchange =e= v02_complex;

 q02_equation2 ..
    v02_complex =e= (12*4*9*18.5)/v02_complex;

*' This was way to complex!

*' @code Let's add some standard code

display vm_exchange.l;
