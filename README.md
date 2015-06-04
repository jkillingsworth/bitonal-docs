## Overview

A library for creating bi-level monochrome TIFF images.

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

* Consider eliminating the types module
* Improve performance of the compression module
* Parameterize the photometric interpretation
* Parameterize the resolution unit
* Parameterize the byte order
* Support multi-strip images
* Support multi-page documents
* Support CCITT Group 3 two dimensional compression
* Support CCITT Group 4 two dimensional compression
* Create NuGet package
