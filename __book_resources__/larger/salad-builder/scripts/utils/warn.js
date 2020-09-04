/***
 * Excerpted from "Programming Elm",
 * published by The Pragmatic Bookshelf.
 * Copyrights apply to this code. It may not be used to create training material,
 * courses, books, articles, and the like. Contact us if you are in doubt.
 * We make no guarantees that this code is fit for any purpose.
 * Visit http://www.pragmaticprogrammer.com/titles/jfelm for more book information.
***/
const chalk = require('chalk');

module.exports = function warn(elmJsonPath) {
  const elmJson = require(elmJsonPath);
  if (elmJson.homepage || elmJson.proxy) {
    console.log();
    console.log(chalk.yellow('Warning:'));
    console.log();
    console.log(
      '  Using elm.json for configuring "homepage" and "proxy" is deprecated.'
    );
    console.log('  This feature will be removed in the future versions.');
    console.log();
  }
};
