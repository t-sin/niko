# Niko - Not-a-cat Slack bot

> "I'm not a cat!"

Niko is a Slack bot; Niko tells you are mentioned on GitHub.

Niko stores a mapping between GitHub users and Slack users. When the GitHub users are mentioned on issues, issue comments or pull requests, then Niko mentiones corresponding Slack users by a post on a Slack channel specified by the envorinment variable `SLACK_CHANNEL`.

By a managing console, the mapping between GitHub/Slack users can be edit on the browser. Run this app and access the path like `/`.

## Requirements

- libev
- PostgreSQL
    - database called `inventory`
- SBCL

## Installation

Clone this repository into ASDF registry and run in REPL like this:

```lisp
CL-USER> (ql:quickload :niko)
```

Or if you use [Roswell](https://github.com/roswell/roswell), simply do this in your shell:

```shell-session
$ ros install t-sin/niko
```

## Usage

In REPL, like this:

```lisp
CL-USER> (niko:start "localhost" 5000)
Hi, I'm Niko!
Woo server is started.
Listening on localhost:5000.
#S(CLACK.HANDLER::HANDLER :SERVER :WOO :ACCEPTOR #<SB-THREAD:THREAD "clack-handler-woo" RUNNING {1006D287C3}>)
```

Or you can run with Roswell script like this:

```shell-session
$ niko start localhost 5000
Hi, I'm Niko!
Woo server is started.
Listening on localhost:5000.
```

## Build

## Author

- TANAKA Shinichi (shinichi.tanaka45@gmail.com)

## License

*Niko* is licensed under the Lisp Lesser GNU General Public License (LLGPL). For more detail, see [LICENSE](LICENSE).
