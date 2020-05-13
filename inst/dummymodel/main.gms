$title dummymodel

*' @title Dummy Model
*'
*' @description The *Dummy Model* (DumM) is a simple test model to test and
*' present the basic functionality of the goxygen package (@goxygen).
*'
*' It has the structure of a module GAMS model, and therefore can be interpreted
*' by goxygen, but has not content and cannot be solved with GAMS. It can serve as 
*' a template to build a modular GAMS model from scratch.
*'
*' The dummy model consists of two modules [01_fancymodule] and [02_crazymodule].

$setglobal fancymodule  default
$setglobal crazymodule  simple

$include "./core/sets.gms"
$include "./core/core.gms"
$batinclude "./modules/include.gms" calculations

