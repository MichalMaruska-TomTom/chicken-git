# chicken-git

libgit2 bindings for Chicken Scheme.

## Install

Obviously, libgit2 is required: <http://github.com/libgit2/>.
Assuming you have that, installation should be straightforward:

    $ git clone git://github.com/evhan/chicken-git.git
    $ cd chicken-git
    $ chicken-install

## API

The library is split into two modules, `git` and `git-lolevel`:

* `git-lolevel` is essentially just the libgit2 API, thinly wrapped. Most of
  the function signatures remain the same, with a few exceptions (namely,
  structures that would go on the stack are automatically allocated and pointer
  arrays are converted to rest arguments).

* `git` is a higher-level interface around `git-lolevel`, providing
  record types for each libgit2 structure.

Documentation will be written shortly.

## Notes

The API is nowhere near final, and open for improvement as it is used and
common cases are identified. Some functionality is not yet provided, mainly
configuration parsing, revwalking and custom backends. Obviously, patches
are more than welcome.

## Contact

  * Evan Hanson <evan@evanhanson.com>

## License

BSD. See LICENSE for details.
