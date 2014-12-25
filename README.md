smallvcm
========

Port of the [smallvcm renderer][1] to ocaml. I made it in order to verify my implementation of 
the mersenne twister random number generator, included in the gmaths library. Should be 
working now. I included the original rngs as well, and a few other command line options.
The report command line parameter is not tested at all (though the code is there).

The code is distributed under the GPL (since gmaths is GPLed). I'll probably provide a version following the MIT license
later on (it just requires me to strip the gmaths dependency out) - not sure how this would work out repository wise though.
 
[1]: https://github.com/SmallVCM/SmallVCM
