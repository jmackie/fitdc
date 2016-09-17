# readfit

A pure `R` library for decoding Garmin FIT files. Written to circumvent the open-source incompatible [FIT SDK](https://www.thisisant.com/resources/fit), and hopefully serve as a reference for those interested in this (increasingly popular) file format.

## References

The FIT format is documented extensively within the [FIT SDK][fitsdk] materials.

In building this package, I relied heavily on [David Cooper's][dtcooper] [`python-fitparse`][pyfitparse] library. He did the grunt work, this is mostly just an `R` port of his code.

## Development notes

In order to decode FIT file messages, the data contained in the "Profile.xlsx" 
(provided with the [FIT SDK][fitsdk]) is essential. This data is wrangled from 
the spreadsheet into various R objects and saved as`R/sysdata.rda`, to be used
internally by this package. To update this file---e.g. to use a new profile---then execute the scripts as such:

```bash
$ cd fitdc/scripts
$ ./Types_sheet.py && ./Messages_sheet.py && ./make_data_dir.R
```

## TODO

There are a few holes in this implementation; some are in the interest of performance, others are due to my lack of interest. My motivation for putting this package together was to be able to get at the record messages of a FIT file. So if there's something missing, it's probably because I haven't found it to affect the actual records. Or it might be a record I'm not interested in. But here are some things I'm aware of:

+ I haven't made any effort to check the CRC during file reading, because that would slow things down considerably and I don't see a compelling reason for it.
+ Field components are currently ignored. I haven't encountered any files where components are used in a meaningful way.
+ Accumulated fields. Because, again, I haven't encountered any examples, and I haven't found any good documentation for them. 

If you need any of these holes filling, submit a PR. &#9786;

[fitsdk]: https://www.thisisant.com/resources/fit "FIT SDK"
[dtcooper]: https://github.com/dtcooper "David Cooper"
[pyfitparse]: https://github.com/dtcooper/python-fitparse/tree/ng "python-fitparse"
