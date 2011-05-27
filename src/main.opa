/*
 * OpaTetris
 *
 * @author Matthieu Guffroy
 *
 */

import mattgu74.tetris

conf()=
 myTetris = Tetris(25, 10, 20, 250, Color.rgb(255,255,255))
 myTetris.init(#game)

@client Load()=
    conf()

start()=
  <div id=#game >
    <h1>M@ttgu74 - OpaTetris</>
    <div id=#info onready={_ -> Load()}>Load</>
  </>


server = Server.one_page_bundle("M@ttgu74 - Game - Tetris",
       [@static_resource_directory("resources")],
       ["resources/resources.css"], start)
