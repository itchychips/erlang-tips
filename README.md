For all the tips related to navigating the erlang ecosystem itself, go to the
wiki at: https://github.com/itchychips/erlang-tips/wiki

This file is mostly either very tiny tidbits, or how Windows (begrudgingly)
works.  Maybe other OSes, too, but unix-like environments tend to be paradise
compared to Windows.

# Compiling

If you're using erlc, you might want to use the following compiler command line (cite: https://stackoverflow.com/a/6967420):

    erlc +native module_name

# Windows 10

(note: If  you're not confident with Windows for dev work, I recommend using
FreeBSD.  Erlang is perfectly usable on that OS)

Some useful tips for running these on Windows 10:

First, install Erlang.  Ensure erl and escript are in your PATH.

Second, use PowerShell for all command line operations.

Next, get rebar3 from git via https://github.com/erlang/rebar3.git.  Then, run
the bootstrap.ps1 file.  Put the rebar3, rebar3.ps1, and rebar3.cmd files into
a folder of your PATH (I recommend making C:\bin and adding that to your PATH,
either through the environment variables panel, or in the file that is at the
$PROFILE variable (do something like `$env:PATH += ";C:\bin"` to do so, then
source the new profile with `. $PROFILE`).  Then you can do `rebar3 compile`
just fine.

However, `rebar shell` leaves a lot to be desired, since it uses the default
`erl` program in the PowerShell terminal, which isn't great (it doesn't have
good command line functionality, plus Ctrl+C will quit it easily, and Ctrl+G
doesn't work).

So, add the following lines to your $PROFILE:

    function Start-R3Shell {
        Param(
            [string]$ProgramName
        )

        if (Test-Path "env:\ESCRIPT_EMULATOR") {
            $oldShellVar = $env:ESCRIPT_EMULATOR
        }

        $env:ESCRIPT_EMULATOR = "werl"

        $args = @("shell")
        if ($ProgramName) {
            $args += $ProgramName
        }

        Start-Process rebar3.cmd -Args $args -WindowStyle Hidden

        if (-not $oldShellVar) {
            Remove-Item "env:\ESCRIPT_EMULATOR"
        }
        else {
            $env:ESCRIPT_EMULATOR = $oldShellVar
        }
    }

    Set-Alias -Name r3s -Value Start-R3Shell

This will allow you to input `r3s` from the PowerShell window and use `werl`
for your environment instead of the janky method via the normal Windows shell,
but not impact your use of rebar3 in other ways, since using `werl` for compile
will pop up and close a window before you can see any errors.

# Windows 10, but cygwin

If you want to use cygwin for Makefiles and such, there is one caveat I ran
into with using rebar3.  If you have your cygwin git binary in your PATH before
the Windows binary, rebar3 will fail to execute it in an odd way.  Presumably,
if you don't have git for cygwin, you won't run into any issues here, but my
development environment is a hot mess of way too many development environments
across difference languages.  Here is a demonstration (with irrelevancies left
out):

    cygwin> echo $PATH
    /cygdrive/c/bin
    /usr/local/bin
    /usr/bin
    /cygdrive/c/Users/ComputerUser/AppData/Local/Programs/Git/cmd
    /cygdrive/c/Program Files/erl-24.1/bin

(note: I typically use C:\bin on Windows as my usual place for standalone
programs, and C:\opt\* for programs that can't follow a unix-like hierarchy)

Here is what rebar3 sees from inside the Erlang environment (formatted for
readability):

    1> rebar_utils:sh("echo %PATH%", []).
    {ok,"C:\\Program Files\\erl-24.1\\erts-12.1\\bin
        ;C:\\bin
        ;C:\\Users\\ComputerUser\\cygwin64\\usr\\local\\bin
        ;C:\\Users\\ComputerUser\\cygwin64\\bin
        ;C:\\Users\\ComputerUser\\AppData\\Local\\Programs\\Git\\cmd
        ;C:\\Program Files\\erl-24.1\\bin\n"}

So, what's happening here is that, because cygwin has its own install of git
(in /usr/bin), it's doing some weird funky stuff with cmd (which is what
rebar_utils:sh/2 calls).  To fix this, we just have to pop our git path to the
front.  To make this more robust, I did the following:

    1. The rebar3 program is in C:\bin\rebar3-program
    2. I modified rebar3.cmd (called from cmd or PowerShell), and placed in C:\bin
    3. I made rebar3, callable from cygwin, and placed in C:\bin

C:\bin\rebar3 source:

    #!/bin/sh

    PATH="C:\\Users\\ComputerUser\\AppData\\Local\\Programs\\Git\\cmd:$PATH" /cygdrive/c/bin/rebar3.cmd $*

C:\bin\rebar3.cmd source:

    @echo off

    escript.exe "%~dpn0-program\%~n0" %*

I think this will suffice until I run headlong into yet another issue with the
environment, but thankfully erlang is very flexible in how it allows an
environment to look and work.  +1 for dynamic compilation of modules, I guess!

# Kerl and FreeBSD

g++ crashes when trying to compile any modern erlang on my system.  Instead of doing the responsible thing and report it, I just decided to use clang tools instead of gcc.

Demonstration of failure:

    [itchychips@freebsd-computer ~/src]$ ./kerl build 24.1.2
    Extracting source code
    Building Erlang/OTP 24.1.2 (24.1.2), please wait...
    APPLICATIONS INFORMATION (See: /home/itchychips/.kerl/builds/24.1.2/otp_build_24.1.2.log)
     * wx             : wxWidgets was not compiled with --enable-webview or wxWebView developer package is not installed, wxWebView will NOT be available
     *         wxWidgets must be installed on your system.
     *         Please check that wx-config is in path, the directory
     *         where wxWidgets libraries are installed (returned by
     *         'wx-config --libs' or 'wx-config --static --libs' command)
     *         is in LD_LIBRARY_PATH or equivalent variable and
     *         wxWidgets version is 3.0.2 or above.

    DOCUMENTATION INFORMATION (See: /home/itchychips/.kerl/builds/24.1.2/otp_build_24.1.2.log)
     * documentation  :
     *                  fop is missing.
     *                  Using fakefop to generate placeholder PDF files.

    Build failed.
    See <https://gcc.gnu.org/bugs/> for instructions.
    gmake[4]: *** [x86_64-unknown-freebsd12.2/Makefile:910: obj/x86_64-unknown-freebsd12.2/opt/jit/beam_asm.o] Error 4
    gmake[4]: Leaving directory '/usr/home/itchychips/.kerl/builds/24.1.2/otp_src_24.1.2/erts/emulator'
    gmake[3]: *** [/home/itchychips/.kerl/builds/24.1.2/otp_src_24.1.2/make/run_make.mk:35: opt] Error 2
    gmake[3]: Leaving directory '/usr/home/itchychips/.kerl/builds/24.1.2/otp_src_24.1.2/erts/emulator'
    gmake[2]: *** [Makefile:45: opt] Error 2
    gmake[2]: Leaving directory '/usr/home/itchychips/.kerl/builds/24.1.2/otp_src_24.1.2/erts'
    gmake[1]: *** [Makefile:54: jit] Error 2
    gmake[1]: Leaving directory '/usr/home/itchychips/.kerl/builds/24.1.2/otp_src_24.1.2/erts'
    gmake: *** [Makefile:483: emulator] Error 2

    Please see /home/itchychips/.kerl/builds/24.1.2/otp_build_24.1.2.log for full details.

And in the build log:

    g++: internal compiler error: Segmentation fault signal terminated program cc1plus
    Please submit a full bug report,
    with preprocessed source if appropriate.
    See <https://gcc.gnu.org/bugs/> for instructions.

Demonstration of success:

    [itchychips@freebsd-computer ~/src]$ CC=clang CXX=clang++ ./kerl build 24.1.2
    Extracting source code
    Building Erlang/OTP 24.1.2 (24.1.2), please wait...
    APPLICATIONS DISABLED (See: /home/itchychips/.kerl/builds/24.1.2/otp_build_24.1.2.log)
     * odbc           : ODBC library - link check failed

    APPLICATIONS INFORMATION (See: /home/itchychips/.kerl/builds/24.1.2/otp_build_24.1.2.log)
     * wx             : wxWidgets was not compiled with --enable-webview or wxWebView developer package is not installed, wxWebView will NOT be available
     *         wxWidgets must be installed on your system.
     *         Please check that wx-config is in path, the directory
     *         where wxWidgets libraries are installed (returned by
     *         'wx-config --libs' or 'wx-config --static --libs' command)
     *         is in LD_LIBRARY_PATH or equivalent variable and
     *         wxWidgets version is 3.0.2 or above.

    DOCUMENTATION INFORMATION (See: /home/itchychips/.kerl/builds/24.1.2/otp_build_24.1.2.log)
     * documentation  :
     *                  fop is missing.
     *                  Using fakefop to generate placeholder PDF files.

    Erlang/OTP 24.1.2 (24.1.2) has been successfully built

Not entirely sure what the odbc thing is on about, because it doesn't fail
every time.
