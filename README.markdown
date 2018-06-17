# niko

> "I'm not a cat!"

Niko tells you are mentioned at GitHub on Slack.

Niko holds the mapping between GitHub users and Slack users. When the GitHub users are mentioned on issues, issue comments or pull requests, then Niko mentiones corresponding Slack users by a post on specified Slack channel.

Because Niko has a managing console, the mapping between GitHub/Slack users can be edit on the browser. Run this app and access like `localhost:5000/`.

## Requirements

* [Roswell](https://github.com/roswell/roswell)
* [Qlot](https://github.com/fukamachi/qlot)
* [Lake](https://github.com/takagi/lake)
* NPM (Only for development)

## How to Use

### Installation

```
$ npm install
$ ros install qlot lake
$ qlot install

# setup DB
$ qlot exec lake db:migrate
```

### Run

#### Development

```
$ qlot exec lake server
```

#### Production

```
$ ros install clack  # for 'clackup' command

# set tokens into env
$ export GITHUB_TOKEN=xxxxxxx
$ export SLACK_TOKEN=yyyyyyy
$ export SLACK_CHANNEL=channel

# TCP localhost
$ APP_ENV=production qlot exec clackup app.lisp --server woo --debug nil --address 127.0.0.1 --port 8080 --worker-num 4
# UNIX domain socket
$ APP_ENV=production qlot exec clackup app.lisp --server woo --debug nil --listen /tmp/app.sock --worker-num 4
```

## See Also

* [Utopian](https://github.com/fukamachi/utopian): Web application framework
* [Mito](https://github.com/fukamachi/mito): An ORM library
