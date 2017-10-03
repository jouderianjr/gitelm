// pull in desired CSS/SASS files
require( './styles/main.scss' );
require( './styles/font-awesome/scss/font-awesome.scss' );

// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
Elm.Main.embed( document.getElementById( 'main' ) );
