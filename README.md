# similar-images
[![Build Status](https://travis-ci.com/shamazmazum/similar-images.svg?branch=master)](https://travis-ci.com/shamazmazum/similar-images)

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
images are considered similar. When this value exceeds `4096` all the
images are considered similar. The default value is good enough to
find images with just tiny differences.

Another good function is `similar-subset` which allows to compare a
small set of images with a bigger one and find images in the small set
which have similar pictures in the big one. Usage:

~~~~
(similar-subset "/path/to/small/set" "/path/to/bigger/set")
~~~~

It has the same keyword arguments as `find-similar`.

## Principle of work

When you call `find-similar` the directory is scanned for images. For
all found images 4096-bit aHash is calculated (more info
[here](http://www.hackerfactor.com/blog/?/archives/432-Looks-Like-It.html)).
aHash is a perceptual hash which means two hashes are "close" to each
other when two corresponding images are similar. The measure of
"proximity" of hashes is called Hamming distance. Because a hash is
4096-bit, the distance function can be in range from 0 (two pictures
are almost identical) to 4096 (two pictures are distinct). Hence, we
have 4096-dimensional space of hashes and need to find hashes which
are close enough. Here vantage point trees come to help us which are
designed exactly for that purpose. The tree is constructed on a set of
hashes with `O(n log n)` time complexity where `n` is a number of
hashes in the set. Then for each element in the set close elements are
found (search operation is `O(log n)`) which again gives us `O(n log
n)`. This is much faster than a naïve `O(n^2)` search. Also all
computed hashes are backed to SQLite database (`hashes.db` in your
directory) so only hashes to the new files are calculated.

## TODO

* Some progress report
* Multithreading
* Timestamps for database entries.
