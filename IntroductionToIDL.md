# Overview #
IDL used by many researchers in astronomy, space physics, and medical physics for data analysis and exploration.  The core language borrows heavily from Fortran and C, with a rich set of libraries providing added functionality.  Commands may be entered interactively in an Eclipse based GUI or combined into compiled functions or procedures.  Key language features include
  * dynamic data type conversion
  * vector and array operations
  * a widget toolkit
  * an object oriented framework

IDL is a commercial product of [ExelisVis](http://www.exelisvis.com) (formerly ITTVIS, formerly RSI).  The University of Calgary Space Physics research group has a site license with 30(?) seats at a reasonable annual cost.  We have a large established code base and extensive experience using IDL for scientific data analysis. Free analysis packages (eg. R, numpy) do not currently provide the same functionality, although the gap does appear to be shrinking.

## On-line resources ##
  * http://www.exelisvis.com/language/en-us/productsservices/idl/idltutorials.aspx ExcelisVis tutorials]
    * [Part 1](http://www.exelisvis.com/portals/0/tutorials/idl/Programming_in_IDL.pdf)

  * http://www.astro.virginia.edu/class/oconnell/astr511/IDLguide.html
  * http://www.astro.virginia.edu/class/oconnell/astr511/IDLexercises
  * http://msi.umn.edu/software/idl/tutorial/

# Introduction #
**Note**: press "F1" to access the detailed built-in help.

## Scalar variables ##

Assign the value "3" to a variable called "x".  This dynamically creates a new variable
```
IDL> x=3
```
Print the value of "x"
```
IDL> print,x
       3
```
Get information about the type (integer) and value (3) of "x"
```
IDL> help,x
X               INT       =        3
```

Assign "x" to a new variable "y".  (This creates a copy of "x", not a pointer reference to it.)  Use the `&` symbol to put multiple commands on a single line.
```
IDL> y=x & help,y
Y               INT       =        3
```
Create a new variable "y" and assign "x" times 4 plus 9 to it
```
IDL> y=x*4+9 & help,y
Y               INT       =       21
```

### Dynamic types ###
IDL will dynamically "promote" variable types.  For example, multiplying two integers will always result in an integer
```
IDL> help,3*4
<Expression>    INT       =       12
```
but multiplying an integer with a floating point value will produce a floating result
```
IDL> help,3*4.0
<Expression>    FLOAT     =       12.0000
```

Similarly, multiplying two integers will produce an integer, but subsequent addition of a float will convert the result to floating point
```
IDL> y=x*4+9.0 & help,y
Y               FLOAT     =       21.0000
```

Double precision floating point is of even higher priority
```
IDL> y=x*4.0+9.0d0 & help,y
Y               DOUBLE    =        21.000000
```

Use explicit casing to force variables to specific types
```
IDL> help,double(3*4)
<Expression>    DOUBLE    =        12.000000
IDL> help,fix(3*4.0)
<Expression>    INT       =       12
```