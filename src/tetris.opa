/*
 * OpaTetris
 *
 * @author Matthieu Guffroy
 * @author Adam Koprowski (improving control logic & session management;
 *                         no more lost key events)
 * @author Ida Swarczewskaja (improving html structure)
 */

/*
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

import stdlib.web.canvas

package mattgu74.tetris

import stdlib.web.canvas

type Tetris.conf = {
  size : int;      // square side in pixels
  nbcol : int;     // horizontal width in squares
  nbline : int;    // vertical width in squares
  width : int;     // horizontal in pixels (must be size*nbcol)
  height : int;    // vertical in pixels (must be size*nbline)
  speed : int;     // game loop speed.
  bgcolor : Color.color
}

type Tetris.gradient = {c0: Color.color; c1: Color.color; c2: Color.color}

type Tetris.object = {
  color : Tetris.gradient
  cases : list({x:int ; y:int})
}

type Tetris.keyboard_configuration = {
  move_right : Dom.key_code
  move_down  : Dom.key_code
  move_left  : Dom.key_code
  rotate     : Dom.key_code
}

type Tetris.game_stage = {starting} / {in_progress} / {paused}

type Tetris.game_event = {left} / {right} / {down} / {rotate}

type Tetris.game_state = {
  stage : Tetris.game_stage ;
  score : int ;
  event : option(Tetris.game_event);
  next_event : option(Tetris.game_event);
  map : intmap(intmap( { color : Tetris.gradient
                         state : {empty} / {fixed} / {mobile}
                        })) ;
  object : {x : int ; y : int; object : Tetris.object} ;
  nextobject : Tetris.object ;
  kb_conf : Tetris.keyboard_configuration ;
}

Tetris(size, nbcol, nbline, speed, color) = {{

/////////////////////////////////
// Configuration
/////////////////////////////////

  conf = {
           size = size ;
           nbcol = nbcol ;
           nbline = nbline ;
           width = (nbcol * size) ;
           height = (nbline * size) ;
           speed = speed ;
           bgcolor = color
         }

  kb_conf_default = {
           move_right = Dom.Key.RIGHT
           move_down = Dom.Key.DOWN
           move_left = Dom.Key.LEFT
           rotate = Dom.Key.UP
  }


//////////////////////////////////
// DEFAULT VALUE
//////////////////////////////////

  add_element(n, a, map) =
    match n with
      | 1 -> Map.add(n-1,a,map)
      | _ -> add_element(n-1, a, Map.add(n-1,a,map))

  // empty grid
  default_case = {color = {c0=color c1=color c2=color}; state = {empty}}
  default_line = add_element(nbcol, default_case, Map.empty)
  default_map = add_element(nbline, default_line, Map.empty)

  initial_game_state = {
    stage = {starting} ;
    score = 0 ;
    event = {none} ;
    next_event = {none} ;
    map = default_map ;
    object = { x=4 ; y=-10 ; object=object_get()} ;
    nextobject = object_get()
    kb_conf = kb_conf_default
  } : Tetris.game_state

  mk_col(c0, c1, c2) =
    rgb((r, g, b)) = Color.rgb(r, g, b)
    { c0 = rgb(c0)
    ; c1 = rgb(c1)
    ; c2 = rgb(c2)
    }

  // List of objects
  objects = [
    { // CUBE
      color = mk_col((207, 35, 42), (236, 28, 36), (243, 126, 95))
      cases = [{x=0 ; y=0}, {x=0 ; y=1}, {x=1 ; y=0}, {x=1; y=1}]
    },{ // L
      color = mk_col((157, 36, 142), (199, 21, 140), (212,113,173))
      cases = [{x=0;y=-1}, {x=0;y=0}, {x=0;y=1}, {x=1;y=1}]
    },{ // L REVERSED
      color = mk_col((71, 67, 116), (46, 48, 146), (91,87,165))
      cases = [{x=0;y=-1}, {x=0;y=0}, {x=0;y=1}, {x=-1;y=1}]
    },{ // S
      color = mk_col((0, 149, 218), (0, 174, 238), (43, 196, 243))
      cases = [{x=0;y=-1}, {x=0;y=0}, {x=1;y=0}, {x=1;y=1}]
    },{ // Z
      color = mk_col((0, 145, 76), (0, 165, 80), (100, 192, 138))
      cases = [{x=0;y=-1}, {x=0;y=0}, {x=-1;y=0}, {x=-1;y=1}]
    },{ // T
      color = mk_col((255, 215, 0), (255, 241, 0), (255, 247, 169))
      cases = [{x=-1;y=0}, {x=0;y=0}, {x=1;y=0}, {x=0;y=1}]
    },{ // |
      color = mk_col((212, 98, 42), (243, 117, 33), (246, 148, 83))
      cases = [{x=0;y=-1}, {x=0;y=0}, {x=0;y=1}, {x=0;y=2}]
    }]

///////////////////////////////
// SESSION
///////////////////////////////

  start_game(gs : Tetris.game_state) =
    do Dom.transform([#info <- <>Use arrow keys to play</>])
    { initial_game_state with
        object = { x=4 y=-4 object=gs.nextobject }
        stage = {in_progress}
    }

  game_key_pressed(gs : Tetris.game_state, key_code : option(Dom.key_code)) =
    match key_code with
    | {none} -> gs
    | {some=key} ->
        event =
          if key == gs.kb_conf.move_right     then some({right})
          else if key == gs.kb_conf.move_left then some({left})
          else if key == gs.kb_conf.move_down then some({down})
          else if key == gs.kb_conf.rotate    then some({rotate})
          else none
        next_event =
           // pressing rotate doesn't fire the even more than once
          if event == some({rotate}) then
            none
          else
            event
        match event with
        | {none} -> gs
        | {some=_} ->
//            do Log.info("Tetris", "event={event}, next_event={next_event}")
            {gs with ~event ~next_event}

  process_dom_event(gs : Tetris.game_state, event : Dom.event) =
    match event.kind with
    | {keyup} ->
        {gs with next_event=none}
    | {keydown} ->
        do Log.info("Tetris", "keydown")
        (match gs.stage with
        | {starting} ->
             // key pressed while we're waiting for the game to start
            start_game(gs)
        | {in_progress} ->
             // key pressed while we're playing -- let's handle it
            game_key_pressed(gs, event.key_code)
        | {paused} ->
             // key pressed while game paused -- let's ignore it
            gs
        )
    | _ -> gs

  process_user_tick(gs : Tetris.game_state) =
    new_gs =
      match gs.event with
      | {none} -> gs
      | {some=event} ->
//          do Log.info("Tetris", "Event: {event}")
          match event with
          | {right} -> event_right(gs)
          | {left} -> event_left(gs)
          | {rotate} -> event_rotate(gs)
          | {down} -> event_down(gs)
    {new_gs with event=new_gs.next_event}

  board2str(gs) =
    mk_cell(i)(j) =
      match Map.get(i, gs.map) with
      | {none} -> "? "
      | {some=row} ->
        match Map.get(j, row) with
        | {none} -> "?"
        | {some=cell} ->
          match cell.state with
          | {empty} -> " "
          | {fixed} -> "X"
          | {mobile} -> "o"
    mk_line(i) = List.init(mk_cell(i), conf.nbcol)
    s = List.init(mk_line, conf.nbline)
    board = List.list_to_string(List.to_string_using("[", "]", "", _), s)
    "{gs.stage}: {board}"

    // re-draw everything on the screen
  redraw(gs : Tetris.game_state, ctx_board, ctx_next) =
    do TetrisCanvas.draw_next(conf, color, ctx_next, gs.nextobject)
    do TetrisCanvas.draw_map(conf, ctx_board, object_add_to_map(gs.object, gs.map))
    do TetrisCanvas.draw_grid(conf, ctx_board)
    do Dom.transform([#tetris_score_value <- gs.score])
    void

    // let pieces fall & check whether the game is finished
  process_game_tick(gs : Tetris.game_state) =
    match gs.stage with
    | {in_progress} -> piece_falls(gs) |> detect_gameover(_)
    | _ -> gs

  switch_pause(gs : Tetris.game_state) =
    new_stage =
      match gs.stage with
       // game paused -- let's continue
      | {paused} ->
          do Dom.transform([#pause_button <- "Pause"])
          {in_progress}
       // game in progress -- let's pause it
      | {in_progress} ->
          do Dom.transform([#pause_button <- "Resume"])
          {paused}
       // game either finished or not started yet -- let's ignore the pause
      | stage -> stage
    { gs with stage=new_stage }

  process_message(game, message) =
    gs = game.state
    new_gs =
      match message with
       // We process a DOM event (key press)
      | ~{dom_event} -> process_dom_event(gs, dom_event)
       // User clock progress -- the user has a chance to make a move
      | {user_tick} -> process_user_tick(gs)
       // Game clock progress -- the piece goes one level down
      | {game_tick} -> process_game_tick(gs)
       // Pause button was clicked
      | {switch_pause} -> switch_pause(gs)
    do redraw(new_gs, game.ctx_board, game.ctx_next_preview)
    {set={game with state=new_gs}}

////////////////////////////////
/// GRID FUNCTIONS
////////////////////////////////

  get_line_offset(map, offset) =
    match Map.get(offset, map) with
      | {some=line} -> line
      | _ -> default_line

  get_case_offset(line, offset) =
    match Map.get(offset, line)
     | {some=case} -> case
     | _ -> default_case

  piece_falls(gs : Tetris.game_state) =
    is_line_complete(offset)(_, case, (line,b)) =
      match b with
        | 0 -> (line,0)
        | 1 -> match case.state with
                   | {fixed} -> (get_line_offset(gs.map, offset-1),1)
                   | _ -> (get_line_offset(gs.map, offset),0)
                  end
        | _ -> (line,0)
    clean_line(key, line, (map, nb)) =
      (line, b) = Map.fold(is_line_complete(key-nb), line, (Map.empty, 1))
      (Map.add(key, line, map),nb+b)
    (new_map, n) = Map.rev_fold(clean_line,gs.map,(Map.empty, 0))
    new_gs =
      { gs with
         score = gs.score + n
         map = new_map
      }
    event_down(new_gs)

  detect_gameover(gs : Tetris.game_state) =
    last_line = get_line_offset(gs.map, 0)
    is_fixed(_, case, st) =
      match st with
        | {true} -> true
        | {false} ->
          match case.state with
          | {fixed} -> true
          | _ -> false
    if Map.fold(is_fixed, last_line, false) then
      do Dom.transform([#info <- <>You lost... Press any key to start a new game </>])
      {gs with stage={starting}}
    else
      gs

////////////////////////////////
// OBJECT PLACEMENT FUNCTIONS
////////////////////////////////

  object_get()=
    Option.get(List.get(Random.int(List.length(objects)), objects))

  object_to_map(object, map, st) =
    case_edit(y)(x, v) =
      compare(a) =
        match Int.compare(x,(a.x + object.x)) with
         | {eq} -> match Int.compare(y,(a.y + object.y)) with
                   | {eq} -> true
                   | _ -> false
                  end
         | _ -> false
        end
      match List.find(compare, object.object.cases) with
       | {none} -> v
       | {some = _} -> { color = object.object.color ; state = st }
    col_edit(key, value) =
      Map.mapi(case_edit(key), value)
    Map.mapi(col_edit,map)

  object_add_to_map(object, map) =
    object_to_map(object, map, {mobile})

  object_glued_to_map(object, map) =
    object_to_map(object, map, {fixed})


  event_rotate(state : Tetris.game_state) =
    rotate(~{x ; y}, acc)=
      List.add({x=-1*y;y=x},acc)
    newobject=List.fold(rotate,state.object.object.cases,List.empty)
    { state with
       object = { state.object with
                    object={ color = state.object.object.color
                             cases =newobject
                           }
                }
    }

  event_down(state : Tetris.game_state) =
    check(~{x ; y}, acc)=
      match Int.compare(y+1+state.object.y,conf.nbline) with
       | {eq} -> acc + 1
       | _ ->
              rha = Map.get(y+1+state.object.y,state.map)
              match rha with
               | {some = zoui} ->
                 w = Map.get(x+state.object.x,zoui)
                 match w with
                  | {some = s} ->
                             match s.state with
                              | {fixed} -> acc + 1
                              | _ -> acc
                             end
                  | _ -> acc
                 end
               | _ -> acc
              end
      end
    nb=List.fold(check,state.object.object.cases,0)
    match nb with
     | 0 -> { state with
                  object = { state.object with
                                 y = state.object.y + 1
                           }
            }
     | _ -> { state with
                  map = object_glued_to_map(state.object, state.map) ;
                  object = {x=4;y=-4;object=state.nextobject} ;
                  nextobject = object_get()
            }

  event_right(state) =
    check(~{x:int ; y:int}, acc)=
      match Int.compare(x+1+state.object.x,conf.nbcol) with
       | {eq} -> acc + 1
       | _ ->
              rha = Map.get(state.object.y+y , state.map)
              match rha with
               | {some = zoui} ->
                 w = Map.get(x+1+state.object.x,zoui)
                 match w with
                  | {some = s} ->
                             match s.state with
                              | {fixed} -> acc + 1
                              | _ -> acc
                             end
                  | _ -> acc
                 end
               | _ -> acc
              end
      end
     nb=List.fold(check,state.object.object.cases,0)
     match nb with
      | 0 ->
         { state with
               object = { state.object with
                           x = state.object.x + 1
                        }
         }
      | _ -> state

  event_left(state) =
    check(~{x ; y}, acc)=
      match Int.compare(x+state.object.x,0) with
       | {eq} -> acc + 1
       | _ ->
              rha = Map.get(y+state.object.y+0,state.map)
              match rha with
               | {some = zoui} ->
                 w = Map.get(x-1+state.object.x,zoui)
                 match w with
                  | {some = s} ->
                             match s.state with
                              | {fixed} -> acc + 1
                              | _ -> acc
                             end
                  | _ -> acc
                 end
               | _ -> acc
              end
      end
     nb=List.fold(check,state.object.object.cases,0)
     match nb with
      | 0 ->
         { state with
               object = { state.object with
                              x = state.object.x - 1
                        }
         }
      | _ -> state


////////////////////////////////
// INIT
////////////////////////////////
//
// init() prepare the game
//

  init(div) =
    get_ctx(id) =
      canvas = Canvas.get(#{id})
      Option.get(Canvas.get_context_2d(Option.get(canvas)))
    // Prepare the canvas element
    xhtml =
      <canvas id=#tetris_board height={conf.height} width={conf.width}>
        Your browser doesn't support canvas element (part of html5)
      </>
      <div class="tetris_board"></div>
      <div class="next"></div>
      <canvas id=#tetris_next_preview height={6*conf.size} width={6*conf.size} />
      <div class="tetris_next_preview"></div> 
      <div id=#tetris_score>
        <div class="score"></div>
        <div class="board" id=#tetris_score_value />
      </div>
      <div id=#control_div>
        <button type=button id=#pause_button>Pause</button>
      </>
    do Dom.transform([{div} +<- xhtml])
    ctx_board = get_ctx("tetris_board")
    ctx_next_preview = get_ctx("tetris_next_preview")
    game = Session.make({state=initial_game_state ~ctx_board ~ctx_next_preview}, process_message)
     // Key bindings
    bind_event(event_type : Dom.event.kind) =
      process(dom_event) = Session.send(game, ~{dom_event})
      _ = Dom.bind(Dom.select_document(), event_type, process)
      void
    do bind_event({keyup})
    do bind_event({keydown})
    _ = Dom.bind(#pause_button, {click}, (_-> Session.send(game, {switch_pause})))
     // Timer events
    action(freq, event) =
      go() = Session.send(game, event)
      Scheduler.timer(freq, go)
    // A timer to handle user controls
    do action(180, {user_tick})
    // A timer to handle progress of the game
    do action(speed, {game_tick})
    Dom.transform([#info <- <>Press a key to start</>])

}}

TetrisCanvas = {{

  draw_case(conf, ctx, x, y, color) =
    x1 = x*conf.size
    x2 = x1 + conf.size
    y1 = y*conf.size
    y2 = y1 + conf.size
    gradient = Canvas.create_linear_gradient(ctx, x1, y1, x2, y2)
    do Canvas.add_color_stop(gradient, 0.0, color.c2)
    do Canvas.add_color_stop(gradient, 0.5, color.c1)
    do Canvas.add_color_stop(gradient, 1.0, color.c0)
    do Canvas.set_fill_style(ctx,{gradient = gradient})
    do Canvas.fill_rect(ctx, x1, y1, conf.size, conf.size)
    void

  // Draw the map of square
  draw_map(conf, ctx, map) =
    do Canvas.clear_rect(ctx,0,0,conf.width, conf.height)
    do draw_bg(conf,conf.height, conf.width,ctx)
    func(y,xmap,_) =
            Map.fold((x,case,_ -> draw_case(conf, ctx, x,y,case.color)), xmap, void)
    do Map.fold(func, map, void)
    void

  draw_vertical_lines(conf, ctx, i) =
    do Canvas.begin_path(ctx)
    do Canvas.move_to(ctx, i*conf.size, 0)
    do Canvas.line_to(ctx, i*conf.size, conf.height)
    do Canvas.stroke(ctx)
    do Canvas.close_path(ctx)
    match i with
      | 1 -> void
      | _ -> draw_vertical_lines(conf, ctx, i-1)

  draw_horizontal_lines(conf, ctx, i) =
    do Canvas.begin_path(ctx)
    do Canvas.move_to(ctx, 0, i*conf.size)
    do Canvas.line_to(ctx, conf.width, i*conf.size)
    do Canvas.stroke(ctx)
    do Canvas.close_path(ctx)
    match i with
      | 1 -> void
      | _ -> draw_horizontal_lines(conf, ctx, i-1)

  // Set canvas background color
  draw_bg(conf,height,width,ctx)=
    do Canvas.set_fill_style(ctx, {color = conf.bgcolor})
    do Canvas.fill_rect(ctx,0,0,width,height)
    void

  // Draw lines of the grid
  draw_grid(conf, ctx) =
    gray = Color.rgb(11, 11, 11)
    do Canvas.set_stroke_style(ctx, {color = gray})
    do Canvas.set_line_width(ctx, 0.5)
    do draw_vertical_lines(conf, ctx, conf.nbcol-1)
    draw_horizontal_lines(conf, ctx, conf.nbline-1)

  //Draw the next object in other canvas
  draw_next(conf, color, ctx, object) =
    do Canvas.clear_rect(ctx, 0, 0, 6*conf.size, 6*conf.size)
    do Canvas.set_fill_style(ctx, {color = color})
    do draw_bg(conf, 6*conf.size, 6*conf.size, ctx)
    do List.fold((case, _ -> draw_case(conf, ctx, case.x+2, case.y+2,object.color)), object.cases, void)
    draw_grid(conf, ctx)
}}
