*' @title Crazy Module

*' @description This module is crazy and therefore has two realizations
*' (crazy, right?). However, both are not working at all, so what's the point?

*' @authors Bruce Wayne, Max Mustermann

*###################### R SECTION START (MODULETYPES) ##########################
$Ifi "%crazymodule%" == "complex" $include "./modules/02_crazymodule/complex/realization.gms"
$Ifi "%crazymodule%" == "simple" $include "./modules/02_crazymodule/simple/realization.gms"
*###################### R SECTION END (MODULETYPES) ############################
