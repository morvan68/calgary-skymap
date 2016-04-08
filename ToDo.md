# License review #
I picked the GPL3 license almost at random.  Someone should google around ie.
  * ttp://google-opensource.blogspot.ca/2008/05/standing-against-license-proliferation.html
  * ttp://stackoverflow.com/questions/1082101/which-google-code-license-should-i-use
and see if one of the other options is better
  * Apache License 2.0
  * Artistic License/GPL
  * Eclipse Public License 1.0
  * GNU GPL v2
  * GNU GPL v3
  * GNU lesser GPL
  * MIT License
  * Mozilla Public License 1.1
  * New BSD License

My goal is to make our work freely available to everyone, while not exposing ourselves to any legal jeopardy.  It would be ideal if other researchers could easily contribute to this project.  Access for the broader public would be nice.


---

# IDL objects #
My existing code works well enough, but is growing increasingly tangled.  This is a problem for new users trying to modify or develop new applications.  Better documentation would help, and we will work on that over the summer.  It may also be helpful to take advantage of the relatively new support in IDL for [object oriented programming](http://en.wikipedia.org/wiki/Object-oriented_programming).  I have already factored
some of my existing code into several different object classes
  * skymap\_object: assorted utilities and debugging
  * skymap\_time: collection of time conversion routines
  * skymap\_vector: arbitrarily shaped 3-vectors and methods including cross, dot, magnitude etc.
  * skymap\_geographic: basic geodesy for ellipsoidal earth
  * skymap\_geospace: Hapgood/Russell transformations
  * skymap\_geo\_aim: convert between local, device, and astro coordinates
am part-way through others
  * skymap\_star\_catalog: interface to Yale Bright Star Catalog
  * skymap\_asi: generic all-sky imager
  * skymap\_msp: generic meridian scanning photometer
and hope to eventually get to
  * skymap\_igrf
  * skymap\_trixel
  * skymap\_htmesh


## myInteger ##
Create the simplest possible IDL object containing a single 16-bit integer.  Write the minimal set of methods: init, set, get, destroy.

## myIntegerWithUnits ##
Use inheritance to create a more complex object.  Add a "units" field (string) and make changes to allow
```
  obj->set,6.0,'metres'
  help,obj->get(),/structure
  struct {value:6.0, units:'metres'}
```

## skymap\_vector ##
This is a fundamental building block that many other objects depend upon.  It is essential to be able to use this object, and would be very useful to understand in detail how it works.

Review the code in [skymap\_vector\_\_define](http://code.google.com/p/calgary-skymap/source/browse/trunk/skymap/idl/skymap_vector__define.pro), focusing on the "selftest" method.  This should help you to understand some typical uses.

### Euler angles ###
A generic rotation can be expressed in terms of three Euler angles for three successive rotations about the z,x',z' axes.  This can be collapsed into a single 3x3 matrix eg. Goldstein equation 4-46.  Write IDL code to do rotations with the single matrix and compare to my 3-step implementation.  Which one is faster and by how much?

Also read Goldstein appendix B and google "gimbal lock".  Discuss...

### rotation ###
Take a look at the "rotation" method and read my [notes](http://code.google.com/p/calgary-skymap/source/browse/trunk/users/Brian.Jackel/tex/notes.pdf).  Check my derivation, then test simple cases of rotations about x/y/z axes.  Then code up the general 3x3 matrix and compare it to my approach.  Is it faster?


---

# Stellar calibration #
An ideal optical system would allow us to determine exactly
  * when each photon arrived
  * how much energy it had (wavelength)
  * the incident vector direction (orientation)
In practice the measurements will be incomplete.  Not every photon will produce a count, with sensitivity that may depend on wavelength and direction.

A calibration source of known intensity at all wavelengths can be used to determine the instrument response (transfer function).  Laboratory "dark-room" calibration of auroal instruments is surprisingly difficult, due in part to the requirement to simulate very dim extended targets at infinity.  Uncertainties on the order of 20% are common.  Differences between calibrations at different facilities may be even larger.

Even if perfect lab calibration was possible, instruments are usually then shipped under non-ideal conditions to remote field sites where they are often operated in a harsh environment (wide range of temperatures, voltages).  This can significantly alter the instrument response, either drifting gradually over time or in discrete jumps.

If an auroral instrument can detect stars with sufficient signal-to-noise levels, these data could be used for field calibration during every clear night.

## Yale bright stars ##
The [Yale Bright Star Catalog](http://tdc-www.harvard.edu/catalogs/bsc5.html) is a collection of key information about roughly 9000 astronommical objects.  You should read the documentation to get a sense of what information is provided.

## Star magnitude ##
Most (virtually all?) stars have a luminosity that is essentially constant over time. [Apparent magnitude](http://en.wikipedia.org/wiki/Apparent_magnitude) is a logarithmic scale that relates star brightness to Vega (apparent magnitude=1).

## Photometry and Radiometry ##
Consider a source that produces a flux of 100 558nm photons per second arriving at a detector surface area of 10cm-squared.  How many watts per metre is this?  An excellent overview of this topic can be found in this [FAQ](http://www.optics.arizona.edu/palmer/rpfaq/rpfaq.htm).  See also [here](http://www.coe.montana.edu/ee/jshaw/classes/EOSystems/F09/ClassResources/EE482_F09_RadiometryOverview_2pp.pdf); feel free to google for other references and add them to the Wiki.

## SkyCalc ##
We want to check our astronomical calculations against other reliable sources, and include these in the selftest methods.
Pull down the Java version of [SkyCalc](http://www.dartmouth.edu/~physics/faculty/skycalc/flyer.html) and figure out how to carry out simple calculations eg. solar position (azimuth&elevation) from Calgary at 18:00UTC May 22 2012.

## Miscellaneous ##
http://www.stsci.edu/hst/observatory/documents/isrs/scs8.rev.pdf
http://arxiv.org/ftp/astro-ph/papers/0604/0604339.pdf
http://www.am.ub.edu/~carrasco/documentsgaia/GAIA-C5-TN-UB-JMC-001-2.pdf