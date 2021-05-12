# similar-images
[![Build Status](https://api.cirrus-ci.com/github/shamazmazum/similar-images.svg)](https://cirrus-ci.com/github/shamazmazum/similar-images)
![CI](https://github.com/shamazmazum/similar-images/workflows/CI/badge.svg)

**similar-images** is a small library which can find similar images in
big datasets. Simply call `find-similar` function with the directory
which contains your images as its single argument:

~~~~
(find-similar "/path/to/my/pictures")
~~~~

This will scan the directory and its subdirectories for images and
return sets of similar images (if any) in a list. There are some
keyword arguments, the most important of them is `threshold` which is
a small integer value. The bigger this value is the more different
images are considered similar. When this value exceeds `1024` all the
images are considered similar. The default value is good enough to
find images with just tiny differences.

**Good news:** Since 12-05-2020 there is a new `find-similar-prob`
function which does all the same as `find-similar` but much much
faster!

Another good function is `similar-subset` which allows to compare a
small set of images with a bigger one and find images in the small set
which have similar pictures in the big one. Usage:

~~~~
(similar-subset "/path/to/small/set" "/path/to/bigger/set")
~~~~

It has the same keyword arguments as `find-similar`.

## Principle of work

When you call `find-similar` the directory is scanned for images. For
all found images 1024-bit aHash is calculated (more info
[here](http://www.hackerfactor.com/blog/?/archives/432-Looks-Like-It.html)).
aHash is a perceptual hash which means two hashes are "close" to each
other when two corresponding images are similar. The measure of
"proximity" of hashes is called Hamming distance. Because a hash is
1024-bit, the distance function can be in range from 0 (two pictures
are almost identical) to 1024 (two pictures are distinct). Hence, we
have 1024-dimensional space of hashes and need to find hashes which
are close enough. Here vantage point trees come to help us which are
designed exactly for that purpose. The tree is constructed on a set of
hashes with `O(n log n)` time complexity where `n` is a number of
hashes in the set. Then for each element in the set close elements are
found (search operation is `O(log n)`) which again gives us `O(n log
n)`. This is much faster than a naïve `O(n^2)` search. Also all
computed hashes are backed to SQLite database (`hashes.db` in your
directory) so only hashes to the new files are calculated.

## Miscellaneous packages

There are two miscellaneous packages in `similar-images/misc`
system. The first is `similar-images-remover` which holds
`remove-similar` function. It accepts a list of matches returned from
`find-similar` (or `find-similar-prob`) and removes all images but one
from all matches. The remaining image is chosen with `best-criterion`
optional argument. By default the image with the largest area is not
removed. An example: deduplicate images in a directory:
~~~~
(asdf:load-system :similar-images/misc)
(similar-images-remover:remove-similar
  (similar-images:find-similar-prob "~/my-images/"))
~~~~

You can also review similar images in GTK application (it's ugly ;):
~~~~
(asdf:load-system :similar-images/misc)
(similar-images-viewer:view
  (similar-images:find-similar-prob "~/my-images/"))
~~~~

Control keys:
* `Right arrow` — expand match
* `Left arrow` — collapse match
* `Delete` — delete an image

## Installation

You can install **similar-images** from [Ultralisp](https://ultralisp.org/)
repository. Add Ultralisp repository to quicklisp (if you haven't already):

~~~~
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
~~~~

and install **similar-images**:

~~~~
(ql:quickload :similar-images)
~~~~

CLI tool can be build like so:

~~~~
(ql:quickload :similar-images/cli)
(asdf:make :similar-images/cli)
~~~~

**similar-images** loads all image hashes into memory, so make sure you have a
big heap for that. If you are using `sbcl` load it supplying
`--dynamic-space-size 32gb` option before building CLI tool.

## CLI tool

You can run the following command to get command line interface tool for search
for similar images:
~~~~
(asdf:make :similar-images/cli)
~~~~

If you do not have GTK, add `:similar-images-no-gui` feature before
building. Usage:

~~~~
vasily@vonbraun:~/.quicklisp/local-projects/similar-images % ./cli/similar-images

Usage: similar-images [-m|--mode MODE] [-q|--quiet] [-r|--recursive]
                      [--threads THREADS] [-t|--threshold THRESHOLD] [--remove-errored]
                      [--big-set BIG-DIRECTORY] DIRECTORY

Available options:
  -m, --mode MODE           Mode of operation (view, print or remove)
  -q, --quiet               Be quiet
  -r, --recursive           Search for images recursively
  --threads THREADS         Number of threads
  -t, --threshold THRESHOLD
                            Sensitivity of the algorithm (0-1024). Lesser values
                           mean lower sensibility. Good values to try are (40-60).
  --remove-errored          Remove images which cannot be read (dangerous!)
  --big-set DIRECTORY       Specify the big set to match against
~~~~

There are 2 modes of operation. The first one is used when you do not specify
`--big-set` option. In this mode images in `DIRECTORY` are searched for
similarities. If you provide `--big-set` option then images in `DIRECTORY` are
compared not with each other but with images from `BIG-DIRECTORY` (useful when
you want to check if you already have a few pictures in your collection).

Also there are additional 3 modes based on what you want to do with
results. `view` is a mode used by default which launches simple GTK viewer with
found images separated into groups by similarity. `print` just prints found
images to console. `remove` removes all similar images in a group but one which
has the biggest dimensions.

You may wish to use `-r` to scan all sub directories of `DIRECTORY` or
`BIG-DIRECTORY`. `--remove-errored` removes all images which cannot be
read. `--threads` sets the number of threads which calculate image hashes.

## Caveats

The hash used in this library (ahash) is fast to calculate but can give false
positives (especially with high threshold value). Therefore you must use
`remove` mode in CLI tool with care.

Also `pngload` sometimes fails to load semi-broken png images which can be
loaded with another libraries, so `--remove-errored` option must be used with
care.

Do not forget to specify big dynamic space size (Despite that **similar-images**
will use only 1-2 Gb the bigger dynamic space you use the better).

Currently the hash database stores only file names and hashes. So if you change
that image later the hash will be the same as before changing.

## Dependencies

* GTK3 (for viewer)
* jpeg-turbo C library (via `jpeg-turbo`)
* SQLite

## TODO

* Some progress report (done in CLI tool).
* Multithreading (done with the help of `lparallel`).
* Timestamps for database entries.
