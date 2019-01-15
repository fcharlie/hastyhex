# HastyHex : a faster hex dumper

HastyHex is a ***blazing fast*** hex dump utility with optional ANSI
color output. It performs about *one to two orders of magnitude* faster
than your typical implementation of `hexdump` or `od`. It's is written
in plain old ANSI C, so you can run it literally anywhere.

![](https://i.imgur.com/xbr4aMj.png)

## Usage

HastyHex produces color output by default regardless of what's connected
to the output. The `-p` option turns off color.

    usage: hastyhex [-fhlp] [-o FILE]
      -h       print this help message
      -l       force output line-buffered
      -f       force output fully-buffered
      -o FILE  output to file instead of standard output
      -p       do not output color ("plain")

The `less` pager has a `-R` argument that understands ANSI color escape
sequences, making it a great candidate for accepting output from
HastyHex.

    $ hastyhex data.bin | less -FRX

The `-f` option increases the output buffer size which typically
improves performance. Since MSVC doesn't support line-buffering, `-l`
will be equivalent to `-f` on Windows.

## Our Fork

I used C++17 to rewrite this tool to support the specified length of data length, and support reading from a specific offset of the file.

```sh
OVERVIEW: hastyhex
Usage: hastyhex [options] <input>
OPTIONS:
  -h [--help]                      Print hastyhex usage information and exit
  -n [--length]                    Read only N bytes from the input.
  -s [--seek]                      Read from the specified offset
  -o [--out]                       Output to file instead of standard output
  -p [--plain]                     Do not output color ("plain")
  -l [--line]                      Force output line-buffered
  -f [--force]                     Force output fully-buffered

Example:
  hastyhex example.bin

```

Improvements to Windows 10 should use the version in this link: [https://github.com/fcharlie/Planck/tree/master/utils/hastyhex](https://github.com/fcharlie/Planck/tree/master/utils/hastyhex)
