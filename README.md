# 💡Niko - Not-a-cat Slack bot

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

### Configuration

Niko has some parameter for execution like DB password, auth tokens, etc. You can set them via environment variables described below:

- `DB_HOST`: DB hostname ex) `localhost`
- `DB_USER`: DB username
- `DB_PASS`: DB password
- `GITHUB_TOKEN`: GitHub auth token to get user info
- `SLACK_TOKEN`: Slack auth token to get user info and post message
- `SLACK_CHANNEL`: Slack channel name where Niko posts message

Don't forget to set them! ;3

### Running

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

For simplicity and easy deployment, You can create single executable binary of Niko. This requires building feature of Roswell, but you can build Niko executable file **on Docker container**.

### Simply build

If you already installed Roswell, just type it:

```shell
$ cd niko/
$ ros build ./roswell/niko.ros
```

### Building on Docker container

If you don't know about Roswell, try to bulid on Docker container.

```
$ sudo docker build .
$ sudo docker cp "$(sudo docker -q -f ancestor=niko:latest):/usr/bin/niko" .
```

## Author

- TANAKA Shinichi (shinichi.tanaka45@gmail.com)

## License

*Niko* is licensed under the Lisp Lesser GNU General Public License (LLGPL). For more detail, see [LICENSE](LICENSE).
