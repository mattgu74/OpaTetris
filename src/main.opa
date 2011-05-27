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
  <h1>M@ttgu74 - OpaTetris</>
  <div id=#load onclick={_ -> Load()}>Load</>
  <div id=#game />
server = Server.one_page_bundle("M@ttgu74 - Game - Tetris",
       [@static_resource_directory("resources")],
       ["resources/css.css"], start)
