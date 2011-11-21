/*
 * OpaTetris
 *
 * @author Matthieu Guffroy
 * @author Ida Swarczewskaja (improving html structure)
 *
 */

import mattgu74.tetris

conf()=
 myTetris = Tetris(25, 10, 20, 250, Color.rgb(28, 28, 28))
 myTetris.init(#game)

@client Load()=
    conf()

start()=
  <a href="http://github.com/mattgu74/OpaTetris">
     <img style="position: absolute; top: 0; right: 0; border: 0;" 
     src="https://a248.e.akamai.net/assets.github.com/img/4c7dc970b89fd04b81c8e221ba88ff99a06c6b61/687474703a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f77686974655f6666666666662e706e67" alt="Fork me on GitHub"/>
  </a>     
  <div id=#game >
      <div class="logo"></>
      <div class="author">by mattgu74</>
      <div id=#info onready={_ -> Load()}>Loading...</>
  </>
  <div class="footer">
      <span><a href="http://blog.opalang.org/2011/11/spotlight-on-opa-app-opatetris-by.html">About the app</a></span> • 
      <span><a href="http://github.com/mattgu74/OpaTetris">Fork on GitHub</a></span> • 
      <span><a href="https://opalang.org">Built with <img src="resources/opa-logo-small.png" alt="Opa"/></a></span>
  </>
  <script src="http://opalang.org/google_analytics.js" />

server = Server.one_page_bundle("M@ttgu74 - Game - Tetris",
       [@static_resource_directory("resources")],
       ["resources/resources.css"], start)
