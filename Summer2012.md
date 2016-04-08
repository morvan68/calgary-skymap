<a href='Hidden comment: 
#sidebar TableOfContents
<wiki:toc max_depth="1"/>
'></a>

# Introduction #
Starting in May 2012 we will have 16 weeks to focus on improvements to the SkyMap tools and apply them to scientific questions.

Software related tasks include
  * converting "old" IDL code into a unified object oriented framework while still maintaining computational efficiency
  * improving coverage of [unit tests](http://en.wikipedia.org/wiki/Unit_testing) to ensure valid results
  * documenting concepts, algorithms, and examples
  * starting development of a Python based toolkit

Scientific applications include
  * mapping very high resolution auroral all-sky images from the AuroraMax SLR
  * validating orientation of the new Norstar merdian scanning photometers
  * estimating cloud cover and viewing conditions
  * spectral calibration using stellar sources
  * automated determination of instrument orientation

is to use stars to carry out

http://www.asc-csa.gc.ca/eng/astronomy/auroramax/connect.asp



[University of Calgary](http://www.ucalgary.ca)
[Auroral Imaging Group](http://aurora.phys.ucalgary.ca)
http://svn.phys.ucalgary.ca/websvn/= Details =

http://calgary-skymap.googlecode.com/svn/trunk/Makefile



Brian Jackel will supervise John Anglo and Eric Davis,

  * overview of space physics and the aurora
  * introduction to computing tools
    * IDL
    * Subversion/Dropbox/Cygwin/LaTex
## Monday April 30 ##
Ensure that two Windows machines are functional, run updates, check NAS status.

## Tuesday May 01 ##
Meet at my office (SB637) around 09:30am so that we can
  * acquire keys for Division 10 (SB6??)
  * get Eric Davis info (SIN etc.) to Jeannette (SB6??) for payroll
  * create Windows accounts
  * create Google accounts for access to [calgary-skymap](https://code.google.com/p/calgary-skymap), add to [project committers](https://code.google.com/p/calgary-skymap/adminMembers)
  * set up Dropbox or Google Drive for backup
  * initial Subversion pull
start overview of space physics
  * 
start review of IDL
  * 


# Tasks #

## Vector operations in IDL ##

### Single dot product ###
```
function dot0 vec1,vec2
  result=0
  for indx=0,2 do result= result + vec1[indx] * vec2[indx]
  return,result
end
```


```
st= systime(1)
for indx=0,99999L do test= dot0([1,2,3],[4,5,6])
print,systime(1)-st
```

also try profiler

How to improve?

```
function dot1 vec1,vec2
  result= total(vec1* vec2)
  return,result
end
```

### Single cross product ###


## Array operations ##
what if `vec=dblarr(3,99)`?

what if `vec=dblarr(3,99)`?

## IDL objects ##
Discuss motivation for objectification
  * clear separation of functionality = less spaghetti
  * reduce input parameter screening eg. it's a "skymap\_vector" or can be turned into one, otherwise fail loudly

## skymap\_vector object ##
  * check general "rotation" against simple rotations about the x,y,z axes
  * check general "rotation" against notes in skymap/doc/generic\_rotation.pdf:
    * do results agree?
    * which is faster & by how much?
  * check Euler\_matrix method against Goldstein eqn 4-46
    * do results agree?
    * which is faster & by how much?
  * do we need other self-tests?

## skymap\_time object ##
Gather together all time related stuff into a single place.
Are we missing any self-tests?

## selftest assert method? ##
Could slightly reduce code in self-test methods by creating
a skymap\_object::assert method eg.
```
nfail=0

name="test example"
x=1 & y=-1
cmnd="x+y EQ 0"
nfail+= ~self.assert(cmnd,name)  ;# use execute: slow but safe?
```

<wiki:gadget url="http://mathml-gadget.googlecode.com/svn/trunk/mathml-gadget.xml" border="0" up\_content="root3x + x^phi + x\_1" height="15"/>