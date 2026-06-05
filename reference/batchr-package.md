# batchr: Batch Process Files

Processes multiple files with a user-supplied function. The key design
principle is that only files which were last modified before the
directory was configured are processed. A hidden file stores the
configuration time and function etc while successfully processed files
are automatically touched to update their modification date. As a result
batch processing can be stopped and restarted and any files created (or
modified or deleted) during processing are ignored.

## See also

Useful links:

- <https://poissonconsulting.github.io/batchr/>

- Report bugs at <https://github.com/poissonconsulting/batchr/issues>

## Author

**Maintainer**: Joe Thorley <joe@poissonconsulting.ca>
([ORCID](https://orcid.org/0000-0002-7683-4592))

Other contributors:

- Audrey Beliveau <audrey.beliveau@uwaterloo.ca> \[contributor\]

- Ayla Pearson <ayla@poissonconsulting.ca>
  ([ORCID](https://orcid.org/0000-0001-7388-1222)) \[contributor\]

- Poisson Consulting \[copyright holder, funder\]
