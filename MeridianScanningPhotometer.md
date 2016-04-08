The medium-term goal is to use stars in the MSP data for directional and spectral calibration.

To begin, pick a single background channel and find a few clear examples with good signal-to-noise ratios (ie. lots of counts above background).  Use "vertical" slices to find the bin corresponding to the signal peak and then "horizontal" slices to determine the transit time (peak) and optical profile (is it Gaussian?).  Make plots and try using the IDL curvefit routine to determine the Gaussian parameters.

Repeat for other background channels.  Transit times and optical profile should be the same but peak count rate may be different.  Assemble a table with error estimates.

Repeat for signal channels.  Results should be similar, possibly with more noise.

## Step Tables ##


## Future work ##
Certain astronomical features (Andromeda) have relatively strong hydrogen features.  This could be useful for calibrating the H-beta channels.