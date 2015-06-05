## Overview

This is a library for creating bi-level monochrome TIFF images.

#### Compression

The baseline image compression options are available:

* No Compression
* CCITT Group 3 1-Dimensional
* PackBits

#### Dithering

The following dithering options are available:

* Original Color Image

  ![Original Color Image](/images/dithering-original.png)

###### Threshold Dithering

* Fixed (50%)

  ![Threshold - Fixed (50%)](/images/dithering-threshold-fixed-127.png)

* Dispersed 2x2

  ![Threshold - Dispersed 2x2](/images/dithering-threshold-dispersed2x2.png)

* Dispersed 4x4

  ![Threshold - Dispersed 4x4](/images/dithering-threshold-dispersed4x4.png)

* Dispersed 8x8

  ![Threshold - Dispersed 8x8](/images/dithering-threshold-dispersed8x8.png)

* Clustered 2x2

  ![Threshold - Clustered 2x2](/images/dithering-threshold-clustered2x2.png)

* Clustered 4x4

  ![Threshold - Clustered 4x4](/images/dithering-threshold-clustered4x4.png)

* Clustered 6x6

  ![Threshold - Clustered 6x6](/images/dithering-threshold-clustered6x6.png)

* Clustered 8x8

  ![Threshold - Clustered 8x8](/images/dithering-threshold-clustered8x8.png)

###### Error Diffusion Dithering

* Basic

  ![Error Diffusion - Basic](/images/dithering-errorDiffusion-basic.png)

* False Floyd–Steinberg

  ![Error Diffusion - False Floyd–Steinberg](/images/dithering-errorDiffusion-falseFloydSteinberg.png)

* Floyd–Steinberg

  ![Error Diffusion - Floyd–Steinberg](/images/dithering-errorDiffusion-floydSteinberg.png)

* Jarvis-Judice-Ninke

  ![Error Diffusion - Jarvis-Judice-Ninke](/images/dithering-errorDiffusion-jarvisJudiceNinke.png)

* Stucki

  ![Error Diffusion - Stucki](/images/dithering-errorDiffusion-stucki.png)

* Burkes

  ![Error Diffusion - Burkes](/images/dithering-errorDiffusion-burkes.png)

* Sierra 3-Row

  ![Error Diffusion - Sierra 3-Row](/images/dithering-errorDiffusion-sierra3Row.png)

* Sierra 2-Row

  ![Error Diffusion - Sierra 2-Row](/images/dithering-errorDiffusion-sierra2Row.png)

* Sierra Lite

  ![Error Diffusion - Sierra Lite](/images/dithering-errorDiffusion-sierraLite.png)

* Atkinson

  ![Error Diffusion - Atkinson](/images/dithering-errorDiffusion-atkinson.png)

* Zhigang Fan

  ![Error Diffusion - Zhigang Fan](/images/dithering-errorDiffusion-zhigangFan.png)

* Shiau-Fan 1

  ![Error Diffusion - Shiau-Fan 1](/images/dithering-errorDiffusion-shiauFan1.png)

* Shiau-Fan 2

  ![Error Diffusion - Shiau-Fan 2](/images/dithering-errorDiffusion-shiauFan2.png)

###### Custom Dithering

Custom dithering options can also be applied by providing a user-defined threshold matrix or error diffusion filter definition.

## References

#### TIFF

* [Adobe TIFF 6.0 Specification](http://partners.adobe.com/public/developer/tiff/index.html)
* [AWare Systems TIFF File Format](http://www.awaresystems.be/imaging/tiff.html)

#### Compression

* [ITU-T Recommendation T.4 - Group 3 Compression](http://www.itu.int/rec/T-REC-T.4/en)
* [ITU-T Recommendation T.6 - Group 4 Compression](http://www.itu.int/rec/T-REC-T.6/en)
* [ITU-T Recommendation T.82 - JBIG Compression](http://www.itu.int/rec/T-REC-T.82/en)
* [Apple Technical Note TN2013 - PackBits Compression](http://web.archive.org/web/20080705155158/http://developer.apple.com/technotes/tn/tn1023.html)
* [Wolfgang Kowalk - CCITT Facsmile Compression Standards](http://einstein.informatik.uni-oldenburg.de/rechnernetze/fax.htm)
* [Mike Kohn - Huffman/CCITT Compression In TIFF](http://www.mikekohn.net/file_formats/tiff.php)

#### Dithering

* [Tanner Helland - Image Dithering](http://www.tannerhelland.com/4660/dithering-eleven-algorithms-source-code/)
* [Tanner Helland - Grayscale Conversion](http://www.tannerhelland.com/3643/grayscale-image-algorithm-vb6/)
* [Libcaca Study - Halftoning](http://caca.zoy.org/wiki/libcaca/study/2)
* [Libcaca Study - Error Diffusion](http://caca.zoy.org/wiki/libcaca/study/3)
* [Lee Crocker et al. - Digital Halftoning](http://www.efg2.com/Lab/Library/ImageProcessing/DHALF.TXT)
* [Michalis Zervos - Image Dithering in Matlab](http://michal.is/projects/image-dithering-in-matlab/)

#### Test Images

* [ITU-T Standard Test Images](https://www.itu.int/net/itu-t/sigdb/genimage/Tseries-g.htm)
* [USC-SIPI Image Database](http://sipi.usc.edu/database/database.php)
* [The Lenna Story](http://www.cs.cmu.edu/~chuck/lennapg/lenna.shtml)

## Notes

Here are some enhancements that can be made to this project:

* Improve performance of the compression module
* Parameterize the photometric interpretation
* Parameterize the resolution unit
* Parameterize the byte order
* Support multi-strip images
* Support multi-page documents
* Support CCITT Group 3 two dimensional compression
* Support CCITT Group 4 two dimensional compression
* Create NuGet package
