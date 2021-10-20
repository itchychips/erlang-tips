For all the tips related to navigating the erlang ecosystem itself, go to the
wiki at: https://github.com/itchychips/erlang-tips/wiki

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
