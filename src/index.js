'use struct'

import 'elm-canvas'
const { Elm } = require('./Elm/Main.elm');
const main = document.getElementById('main');

const app = Elm.Main.init({
    node: main
});
