# Niko - Not-a-cat Slack bot

> "I'm not a cat!"

Niko is a Slack bot; Niko tells you are mentioned on GitHub.

Niko stores a mapping between GitHub users and Slack users. When the GitHub users are mentioned on issues, issue comments or pull requests, then Niko mentiones corresponding Slack users by a post on a Slack channel specified by the envorinment variable `SLACK_CHANNEL`.

By a managing console, the mapping between GitHub/Slack users can be edit on the browser. Run this app and access the path like `/`.

## Requirements

* [Roswell](https://github.com/roswell/roswell)
* [Qlot](https://github.com/fukamachi/qlot)
* PostgreSQL database called `inventory`

## Installation

## Build

## Usage

## License

*Niko* is licensed under the Lisp Lesser GNU General Public License (LLGPL). For more detail, see [LICENSE](LICENSE).
