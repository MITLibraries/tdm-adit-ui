# Web interface to Adit TDM data service

A SPA for exploring user interface designs. Not intended for production use.

## Requirements

Just install Elm - [installers for most platforms](https://guide.elm-lang.org/install.html)

## Build and Use

    elm-make src/*

will create an 'index.html' containing the entire application, wired up to the
TDM service. The first time you run make, you will be prompted to install dependencies:
agree to whatever is proposed. Then fire it up in a browser, recognizing that
many things will not work.

Pro-tip: open a javascript console to examine the queries being constructed by the app.
