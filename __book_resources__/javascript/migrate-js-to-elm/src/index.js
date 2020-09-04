/***
 * Excerpted from "Programming Elm",
 * published by The Pragmatic Bookshelf.
 * Copyrights apply to this code. It may not be used to create training material,
 * courses, books, articles, and the like. Contact us if you are in doubt.
 * We make no guarantees that this code is fit for any purpose.
 * Visit http://www.pragmaticprogrammer.com/titles/jfelm for more book information.
***/
import React from 'react';
import ReactDOM from 'react-dom';
import App from './App';
import './index.css';

function render() {
  const root = document.getElementById('root');
  ReactDOM.render(<App />, root);
}

render();
