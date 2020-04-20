# fishpond

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/fishpond
* URL: https://github.com/mikelove/fishpond
* Date/Publication: 2019-10-29
* Number of recursive dependencies: 169

Run `revdep_details(,"fishpond")` for more info

</details>

## In both

*   checking whether package ‘fishpond’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/phgrosjean/Documents/Pgm/Github/SciViews/svMisc/revdep/checks.noindex/fishpond/new/fishpond.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fishpond’ ...
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/phgrosjean/Documents/Pgm/Github/SciViews/svMisc/revdep/library.noindex/fishpond/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/phgrosjean/Documents/Pgm/Github/SciViews/svMisc/revdep/library.noindex/fishpond/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c readEDS.cpp -o readEDS.o
In file included from readEDS.cpp:11:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: C++ requires a type specifier for all declarations
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from readEDS.cpp:11:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from readEDS.cpp:11:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from readEDS.cpp:11:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from readEDS.cpp:11:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:729:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
5 errors generated.
make: *** [readEDS.o] Error 1
ERROR: compilation failed for package ‘fishpond’
* removing ‘/Users/phgrosjean/Documents/Pgm/Github/SciViews/svMisc/revdep/checks.noindex/fishpond/new/fishpond.Rcheck/fishpond’

```
### CRAN

```
* installing *source* package ‘fishpond’ ...
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/phgrosjean/Documents/Pgm/Github/SciViews/svMisc/revdep/library.noindex/fishpond/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/phgrosjean/Documents/Pgm/Github/SciViews/svMisc/revdep/library.noindex/fishpond/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c readEDS.cpp -o readEDS.o
In file included from readEDS.cpp:11:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: C++ requires a type specifier for all declarations
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from readEDS.cpp:11:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from readEDS.cpp:11:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from readEDS.cpp:11:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from readEDS.cpp:11:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zlib.h:34:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/zconf.h:462:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:729:31: error: unknown type name 'uuid_t'; did you mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
5 errors generated.
make: *** [readEDS.o] Error 1
ERROR: compilation failed for package ‘fishpond’
* removing ‘/Users/phgrosjean/Documents/Pgm/Github/SciViews/svMisc/revdep/checks.noindex/fishpond/old/fishpond.Rcheck/fishpond’

```
