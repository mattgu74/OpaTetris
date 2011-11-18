/*
 * OpaTetris
 *
 * @author Matthieu Guffroy
 *
 */

import mattgu74.tetris

conf()=
 myTetris = Tetris(25, 10, 20, 250, Color.rgb(31,31,31))
 myTetris.init(#game)

@client Load()=
    conf()

start()=
  <div id=#game >
    <div class="logo"></div>
    <div id=#info onready={_ -> Load()}>Load</>
  </>


server = Server.one_page_bundle("M@ttgu74 - Game - Tetris",
       [@static_resource_directory("resources")],
       ["resources/resources.css"], start)
