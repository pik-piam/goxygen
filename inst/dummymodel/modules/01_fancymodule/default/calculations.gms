*' @code

variables
  v01_fancy internal variable (kg)
  vm_exchange exchange variable (kg)
;

equations
  q01_calcme Equation 1;
;


*' @equations

*' Here we have some equations:

 q01_calcme ..
	vm_exchange + 12 =e= v01_fancy;

*' This was fancy, wasn't it?

*' @code Let's add some standard code

pm_global = v01_fancy.l;
