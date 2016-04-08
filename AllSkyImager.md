The medium-term (1-month?) goal has two elements
  * mapping between imager CCD and astronomical coordinates
  * fitting instrumental parameters (ie. orientation)

In the short term I want to start with a simple question: what information can we obtain automatically from a stack of image frames without any knowledge of the imager characteristics?

For example, averaging several hours will produce characteristic star tracks.  A visual examination can easily determine the location of the astronomical pole, as Polaris will be essentially stationary, while other stars will trace out arcs around that point.  How can we teach a computer to reach the same conclusion?  One possibility might be to calculate the variance for each pixel in the stack, with the expectation that it will be smallest around Polaris.

Start by testing this hypothesis, then try to come up with one (or more) of your own, and finally explore the usefulness of the Hough tansform.

## Hough transform ##
The [Hough transform](http://en.wikipedia.org/wiki/Hough_transform) might be used to automatically determine the radius and diameter of star tracks over several hours.  Note that the version in IDL seems to deal only with straight line features, so you may need to write a more general version.

Questions:
  1. what length of arc segment is required? (ie. do we need 24 hours?)
  1. are the tracks nearly circular? nearly elliptical? something else?
  1. what pre-processing (eg. edge detection) is required?

See also:
  * http://www.mathworks.com/matlabcentral/fileexchange/9833
  * http://www.cis.rit.edu/class/simg782/lectures/lecture_10/lec782_05_10.pdf
  * http://home.uchicago.edu/~borovicka/files/research/bristol/hough-report.pdf
  * http://www.smithbower.com/blog/2010/06/using-the-hough-transform-for-circle-detection/