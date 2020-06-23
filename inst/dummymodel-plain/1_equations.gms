*' @title Equations

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

*' This was simple, wasn't it? And we even have a picture. We include it using Markdown syntax
*'
*' ![Fancy cat](cat.png){ width=30% }
*'
*' ## Extra heading

*' Here we create an extra heading not using goxygen syntax but using Markdown syntax.
