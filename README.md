# account-container-picker

Firefox extension. Lets you choose an account container from omnibox and opens a new tab in it.

![typing "&", followed by a space, triggers suggestions; selecting a suggestion opens a new tab in selected container](https://github.com/gmunguia/account-container-picker/raw/master/demo.gif "Demo")

## Installation

Visit [the extension page](https://addons.mozilla.org/en-US/firefox/addon/account-container-picker/) to add it to Firefox.

## Usage

In omnibox, type "&", followed by a space, to start searching available account containers.

## Development

To build from source, install dependencies with `yarn` and run `yarn build`. Build artifacts are stored in `web-ext-artifacts`. See `package.json` for build dependencies. Tested on `macOS Mojave 10.14.3`, running `yarn 1.13.0` and `node 10.15.0`.
