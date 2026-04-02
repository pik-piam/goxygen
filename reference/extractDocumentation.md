# extractDocumentation

Extracts doxygen-like GAMS documentation. Entries are introduced with an
@type at the beginning of the line. In case of @realization also GAMS
code is read and interpreted, in all other cases only the specific
documentation comment is evaluated.

## Usage

``` r
extractDocumentation(path, startType = NULL, comment = "*'")
```

## Arguments

- path:

  path to the file(s) which should be evaluated

- startType:

  set type for first line of code. This can be useful to extract
  documentation even if no documentation type has been set (e.g reading
  equations.gms as type realization)

- comment:

  comment chars used for documentation comments

## Value

a nested list of documentation pieces with type as name of each element.
Each element contains two lists \`content\` containing the actual
documentation and \`cfg\` containing optional attributes passed with the
type.

## See also

[`goxygen`](goxygen.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
mainfile <- paste0(system.file("dummymodel", package = "gms"), "/main.gms")
calcfile <- paste0(system.file("dummymodel", package = "gms"),
                           "/modules/02_crazymodule/complex/calculations.gms")
# extracting information from the main file of the model
extractDocumentation(mainfile)
#> $title
#> $title$content
#> [1] "Dummy Model"
#> 
#> $title$cfg
#> NULL
#> 
#> 
#> $description
#> $description$content
#>  [1] "The *Dummy Model* (DumM) is a simple test model to test and"                      
#>  [2] "present the basic functionality of the gms and goxygen packages (@gms, @goxygen)."
#>  [3] ""                                                                                 
#>  [4] "It has the structure of a module GAMS model, and therefore can be interpreted"    
#>  [5] "by goxygen, but has not content and cannot be solved with GAMS. It can serve as " 
#>  [6] "a template to build a modular GAMS model from scratch."                           
#>  [7] ""                                                                                 
#>  [8] "The dummy model consists of three modules [01_fancymodule], [02_crazymodule],"    
#>  [9] "and [03_Rmodule]."                                                                
#> [10] ""                                                                                 
#> 
#> $description$cfg
#> NULL
#> 
#> 
#> $title
#> $title$content
#> [1] "Settings"
#> 
#> $title$cfg
#> $title$cfg$extrapage
#> [1] "settings"
#> 
#> 
#> 
#> $description
#> $description$content
#> [1] "We might want to move some documentation to a separate page called Settings."
#> [2] ""                                                                            
#> 
#> $description$cfg
#> $description$cfg$extrapage
#> [1] "settings"
#> 
#> 
#> 
# extracting information from a file with some equations in it
extractDocumentation(calcfile)
#> $description
#> $description$content
#>  [1] "```"                                 
#>  [2] "variables"                           
#>  [3] "  v02_complex internal variable (kg)"
#>  [4] ";"                                   
#>  [5] "equations"                           
#>  [6] "  q02_equation1 Equation 1 (kg)"     
#>  [7] "  q02_equation2 Equation 2 (kg)"     
#>  [8] ";"                                   
#>  [9] "```"                                 
#> [10] ""                                    
#> 
#> $description$cfg
#> NULL
#> 
#> 
#> $description
#> $description$content
#> [1] "Here we have some equations:"                                                                                                       
#> [2] "\n\\begin{multline*}\n vm\\_exchange = v02\\_complex \n\\end{multline*}\n"                                                          
#> [3] ""                                                                                                                                   
#> [4] ""                                                                                                                                   
#> [5] "\n\\begin{multline*}\n v02\\_complex = \\frac{ \\left(12 \\cdot 4 \\cdot 9 \\cdot 18.5\\right)}{v02\\_complex} \n\\end{multline*}\n"
#> [6] ""                                                                                                                                   
#> [7] "This was way to complex!"                                                                                                           
#> [8] ""                                                                                                                                   
#> 
#> $description$cfg
#> NULL
#> 
#> 
#> $description
#> $description$content
#> [1] "Let's add some standard code" "```"                         
#> [3] "display vm_exchange.l;"       "```"                         
#> [5] ""                            
#> 
#> $description$cfg
#> NULL
#> 
#> 
```
