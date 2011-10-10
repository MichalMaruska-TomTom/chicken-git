# chicken-git

libgit2 bindings for Chicken Scheme.

## Install

Obviously, libgit2 is required: <http://github.com/libgit2/>.
Assuming you have that, installation should be straightforward:

    $ git clone git://github.com/evhan/chicken-git.git
    $ cd chicken-git
    $ chicken-install

Please note that the libgit library is currently a moving target, under heavy
development. This library has been written and tested against libgit2 0.15.0.
I'll do my best to keep things up to date, but if you're running into problems,
check your versions.

## API

The library is split into two modules, `git` and `git-lolevel`:

* `git-lolevel` is essentially just the libgit2 API, thinly wrapped. Most of
  the function signatures remain the same, with a few exceptions: 

  * Structures & pointers that would go on the stack are allocated
    automatically.
  * Return values are checked where appropriate, signaling an exception of type
    'git when negative.
  * Pointer arrays are converted to rest arguments.

* `git` is a higher-level interface around `git-lolevel`, providing
  record types for each libgit2 structure.

Limited documentation is available at <http://wiki.call-cc.org/eggref/4/git>.

## Notes

The API is nowhere near final, and open for improvement as it is used, common
cases are identified, and libgit2 itself changes. Some functionality is not yet
provided, mainly configuration parsing, custom backends and treebuilders.
Obviously, patches are more than welcome.

## Contact

  * Evan Hanson <evhan@thunktastic.com>

## License

BSD. See LICENSE for details.
