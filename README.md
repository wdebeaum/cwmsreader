# cwmsreader git mirror #

cwmsreader is the part of TRIPS/CWMS (the Collaborative World Modeling System) responsible for reading.

This git repo is a mirror of the TRIPS `cwmsreader` CVS module.

Note that the `src/config/lisp/defsystem/defsystem-3.6i/` directory contains a modified, non-standard, non-official version of [MK:DEFSYSTEM](http://www.cliki.net/mk-defsystem) 3.6i. See the comments near the top of `defsystem.lisp` in that directory for its copyright notice and license.

The rest of the repository is licensed using the [GPL 2+](http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html) (see `gpl-2.0.txt`):

TRIPS cwmsreader system  
Copyright (C) 2018  Institute for Human & Machine Cognition

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
 
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

## Download ##

To get the source code, run this command:

```bash
git clone https://github.com/wdebeaum/cwmsreader.git
```

This will create a `cwmsreader/` directory under your current directory. This is often called `$TRIPS_BASE`.

## Prerequisites ##

You will also need a Lisp implementation (we tend to use SBCL 1.2+) as well as Java (1.7+), Perl, Node.js/npm, GDAL/OGR, and ImageMagick, and some other relatively standard development tools. If you're on a Mac, I think XCode gets you much of this.

If you use SBCL, you need to make sure it has multithreading support (the pre-built binaries don't). On the Mac, an easy way to do this is through [MacPorts](http://www.macports.org/), using this command:

```bash
sudo port install sbcl +threads
```

MacPorts is also an easy way to get Node.js/npm, GDAL/OGR, and ImageMagick (all used by CWMS' Spaceman component):

```bash
sudo port install nodejs8 npm5 gdal ImageMagick
```

You'll also need to get these files from [WordNet](http://wordnet.princeton.edu/):

 - http://wordnetcode.princeton.edu/3.0/WordNet-3.0.tar.bz2
 - http://wordnetcode.princeton.edu/glosstag-files/WordNet-3.0-glosstag.tar.bz2

and unpack them in the same directory, `/usr/local/share/wordnet/` (create the directory if necessary). You should end up with these two directories:

    /usr/local/share/wordnet/WordNet-3.0/
    /usr/local/share/wordnet/WordNet-3.0/glosstag/

Also download this file:

 - [https://geonames.usgs.gov/docs/stategaz/NationalFile_20181001.zip](https://geonames.usgs.gov/docs/stategaz/NationalFile_20181001.zip)

And move/rename it to this (don't unpack it):

    /usr/local/share/geonames/2018-10-01/NationalFile.zip

(If the link is broken, try changing the date part to the first day of this or last month. If that doesn't work, see `$TRIPS_BASE/src/TextTagger/docs/README.xhtml` for more detailed instructions.)

Finally you will need [Stanford CoreNLP](http://stanfordnlp.github.io/CoreNLP/) 3.5.2+. The latest version as of this writing is [3.9.2](http://nlp.stanford.edu/software/stanford-corenlp-full-2018-10-05.zip). Download that zip file and unpack it in `/usr/local/share/stanford-corenlp/` (again, create the directory if necessary). Unpacking the zip file should create the directory `/usr/local/share/stanford-corenlp/stanford-corenlp-full-2018-10-05/`.

## Configuring, making, installing ##

Once you have the prerequisites, run these commands to configure, build, and install TRIPS:

```bash
cd $TRIPS_BASE/src/
./configure --with-lisp=sbcl
make
make install
```

The `--with-lisp` option lets you tell `configure` the command to use to run lisp. By default it uses `lisp`, but SBCL doesn't call itself that. There are many other options to `configure`, which you can see by giving it the `--help` option. But for the most part the defaults should be fine. (Doing `./configure --help` in `src/` doesn't show you absolutely everything, though; you can see more if you go into one of the `src/config/*/` directories first. For example, in `src/config/lisp/`, it describes `--with-lisp` and a few others.)

`make install` will install TRIPS into `$TRIPS_BASE/etc/` and `$TRIPS_BASE/bin/`. The latter is for executable programs, the former for everything else.

## Running ##

You can run the system using the following command:

```bash
$TRIPS_BASE/bin/trips-cwms -reader [ -data /path/to/data/folder ]
```

This will show up a GUI where you can navigate to the folder containing your data (if you use the `-data` option, you'll be taken there automatically), select one or more files, and run the system on them. The system creates a session folder where it keeps all the logs, as well as the results of the extraction process, which end up in EKB files (extension .ekb). EKB files use an XML format for representing the text inputs, annotated at paragraph and sentence level, and the extractions. 

Warning: The system doesn't do any character-level transformation of the input text, so, your text should only contain characters that are valid XML characters. Some pdf-to-text converters will output invalid characters!

It is possible to run the system without any user interface. For example:

```bash
$TRIPS_BASE/bin/trips-cwms -reader -nouser
```

This is particularly useful if you intend to run the system in batch mode. To be able to tell it to process data, you'd have to connect to it another TRIPS module which can send requests. You can create one yourself (see below on how). The system provides some simple clients that you can run from the terminal. For example:

```bash
$TRIPS_BASE/bin/trips_client
```

You can then type KQML messages into this client's STDIN, and the client will send them out. One of the first things you'd want to do is to give it a name and register it:

    (register :name test-client)
    (tell :content (module-status ready)) 

Now this client is a fully operational member of the TRIPS ecosystem, and can send and receive messages. Here is how you can use it to send requests to read text files:

    (request :receiver READER :content (run-file :folder "/path/to/data/folder/" :file "file_name.txt" :reply-when-done t :exit-when-done f) :reply-with R01)

When processing is finished, the client will get back a reply that includes the location of the resulting EKB file.

If you are running a reading process that takes a long time, you can check the status of the reader by sending:

    (request :receiver READER :content (get-status))

For further guidance on how to create new TRIPS components, look inside the [src/Hello](src/Hello) folder, which includes examples in several popular programming languages.

For information about table extraction from PDF files, see [src/PDFExtractor/README.html](src/PDFExtractor/README.html).

