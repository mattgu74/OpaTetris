/*
 * OpaTetris
 *
 * @author Matthieu Guffroy
 */

/*
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

package mattgu74.tetris

type Tetris.conf = {
  size : int;      // square side in pixels
  nbcol : int;     // horizontal width in squares
  nbline : int;    // vertical width in squares
  width : int;     // horizontal in pixels (must be size*nbcol)
  height : int;    // vertical in pixels (must be size*nbline)
  speed : int;     // game speed in the loop.
  dropspeed : int; // game speed when dropping objects.
  origspeed : int; // original game speed
  bgcolor : Color.color
}

type Tetris.object = {
  color : Color.color ;
  cases : list({x:int ; y:int})
}

type Tetris.session = {
  etat : {paused} / {stopped} / {started} / {game_over} ;
  event : {none} / {event : Dom.event} ;
  map : intmap(intmap( { color : Color.color ; 
                         state : {empty} / {fixed} / {mobile} 
                        })) ;
  object : {x : int ; y : int; object : Tetris.object} ;
  nextobject : Tetris.object
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
           dropspeed = 50 ;
           origspeed = speed ;
           bgcolor = color
         }

//////////////////////////////////
// DEFAULT VALUE
//////////////////////////////////

  add_element(n, a, map) =
    match n with
      | 1 -> Map.add(n-1,a,map)
      | _ -> add_element(n-1, a, Map.add(n-1,a,map))

  // empty grid
  default_case = {color = Color.rgb(200,200,200); state = {empty}}
  default_line = add_element(nbcol, default_case, Map.empty)
  default_map = add_element(nbline, default_line, Map.empty)

  default_session = {
    etat = {stopped} ;
    event = {none} ;
    map = default_map ;
    object = { x=4 ; y=0 ; object=object_get()} ;
    nextobject = object_get()
  } : Tetris.session

  // List of objects
  objects = [
    { // CUBE
      color = Color.rgb(125,250,125) ;
      cases = [{x=0 ; y=0}, {x=0 ; y=1}, {x=1 ; y=0}, {x=1; y=1}]
    },{ // L
      color = Color.rgb(250,125,125) ;
      cases = [{x=0;y=-1}, {x=0;y=0}, {x=0;y=1}, {x=1;y=1}]
    },{ // L REVERSED
      color = Color.rgb(125,125,250) ;
      cases = [{x=0;y=-1}, {x=0;y=0}, {x=0;y=1}, {x=-1;y=1}]
    },{ // S
      color = Color.rgb(250,250,125) ;
      cases = [{x=0;y=-1}, {x=0;y=0}, {x=1;y=0}, {x=1;y=1}]
    },{ // Z
      color = Color.rgb(125,250,250) ;
      cases = [{x=0;y=-1}, {x=0;y=0}, {x=-1;y=0}, {x=-1;y=1}]
    },{ // T
      color = Color.rgb(250,125,250) ;
      cases = [{x=-1;y=0}, {x=0;y=0}, {x=1;y=0}, {x=0;y=1}]
    },{ // |
      color = Color.rgb(125,125,125) ;
      cases = [{x=0;y=-1}, {x=0;y=0}, {x=0;y=1}, {x=0;y=2}]
    }]

///////////////////////////////
// SESSION
///////////////////////////////

  modify_event(session:Tetris.session, event) =
    { etat = session.etat ; event = event ; map = session.map ; object = session.object; nextobject = session.nextobject}

  modify_state(session ,state) =
    { etat = state ; event = session.event ; map = session.map; object = session.object; nextobject = session.nextobject}   
 
  modify_add_object(session) =
    object = object_get()
    { etat = session.etat ; 
      event = session.event ; 
      map = session.map ; 
      object = {x=4;y=-4;object=session.nextobject} ; 
      nextobject = object}

  get_message(session : Tetris.session, message) =
    match message with 
     | {session = m} -> {return = m ; instruction = {set = m}}
     | {event = e} -> a = modify_event(session,{event = e})
                      {return = a ; instruction = {set = a}}
     | {timer} -> a = modify_event(session, {none})
                  {return = session ; instruction = {set = a}}     
     | {action} -> {return = session ; instruction = {unchanged}}
     // We take the default session, in order to have a clean map
     | {start} -> 
                   a = modify_event(default_session, {none}) 
                   b = modify_state(a, {started})
                   {return = b ; instruction = {set = b}}
     | {game_over} -> b = modify_state(session, {game_over})
                      {return = b ; instruction = {set = b}}
     | {addobject} -> 
                      a = modify_add_object(session)
                      {return = a ; instruction = {set = a}}
     | {stop} ->  a = modify_event(session, {none})
                  b = modify_state(a,{stopped})
                  {return = b ; instruction = {set = b}}
     | {pause} ->  a = modify_event(session, {none})
                   b = modify_state(a, {paused})
                   {return = b ; instruction = {set = b}}
     | {down} -> object_session_down(session)
     | {right} -> object_session_right(session)
     | {left} -> object_session_left(session)
     | {rotate} -> object_session_rotate(session)
     | {down_and_remove} -> object_session_down_and_remove(session)

  mySession = Cell.make(default_session, get_message)

////////////////////////////////
// EVENT
////////////////////////////////

  bind_event(event : Dom.event) =
    _ = Cell.call(mySession, {event = event})
    void

  do_event(event) = 
    match event.kind with 
      | {keydown} -> 
        key = Option.get(event.key_code)
        match key == Dom.Key.RIGHT with
          | {true} -> objet_right()
          | {false} -> match Int.compare(key, Dom.Key.LEFT) with
                  | {eq} -> objet_left()
                  | _ -> match Int.compare(key, Dom.Key.DOWN) with
                          | {eq} -> objet_down()
                          | _ -> match Int.compare(key, Dom.Key.UP) with
                                  | {eq} -> objet_rotate()
                                  | _ -> match Int.compare(key, 82) with
                                          | {eq} -> objet_rotate()
                                          | _ -> void
                                         end
                                 end
                          end
                 end
          end
      | _ -> void


////////////////////////////////
// DRAWING
////////////////////////////////

  draw_case(ctx,x,y,color) =
    do Canvas.set_fill_style(ctx,{color = color})
    do Canvas.fill_rect(ctx,x*conf.size,y*conf.size,(x+1)*conf.size,(y+1)*conf.size)
    void

  // Draw the map of square
  draw_map(ctx, map) =
    do Canvas.clear_rect(ctx,0,0,conf.width, conf.height)
    func(y,xmap,_) = Map.fold((x,case,_ -> draw_case(ctx, x,y,case.color)), xmap, void)
    do Map.fold(func, map, void)  
    void

  draw_vertical_lines(ctx, i) =   
    do Canvas.begin_path(ctx)
    do Canvas.move_to(ctx, i*conf.size, 0)
    do Canvas.line_to(ctx, i*conf.size, conf.height)
    do Canvas.stroke(ctx)
    do Canvas.close_path(ctx)
    match i with
      | 1 -> void
      | _ -> draw_vertical_lines(ctx, i-1)

  draw_horizontal_lines(ctx, i) =
    do Canvas.begin_path(ctx)
    do Canvas.move_to(ctx, 0, i*conf.size)
    do Canvas.line_to(ctx, conf.width, i*conf.size)
    do Canvas.stroke(ctx)
    do Canvas.close_path(ctx)
    match i with
      | 1 -> void
      | _ -> draw_horizontal_lines(ctx, i-1)

  // Draw lines of the grid
  draw_grid(ctx) =   
    do Canvas.set_stroke_style(ctx, {color = Color.rgb(100,100,100)})
    do Canvas.set_line_width(ctx,0.5)
    do draw_vertical_lines(ctx, conf.nbcol-1)
    draw_horizontal_lines(ctx, conf.nbline-1)

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

  object_session_down_and_remove(session) = 
    is_line_complete(offset)(_, case, (line,b)) = 
      match b with
        | 0 -> (line,0)
        | 1 -> match case.state with
                   | {fixed} -> (get_line_offset(session.map, offset-1),1)
                   | _ -> (get_line_offset(session.map, offset),0)
                  end
        | _ -> (line,0)
    
    clean_line(key, line, (map, nb)) =
      (line, b) = Map.fold(is_line_complete(key-nb), line, (Map.empty, 1))
      (Map.add(key, line, map),nb+b)
    (map, _) = Map.rev_fold(clean_line,session.map,(Map.empty, 0))
    sess = { etat = session.etat ; 
           event = session.event ; 
           map = map ; 
           object = session.object ; 
           nextobject = session.nextobject}
    object_session_down(sess : Tetris.session)

  detect_gameover(session) =
    last_line = get_line_offset(session.map, 0)
    is_fixed(_, case, st) =
      match st with
        | {true} -> {true}
        | {false} -> match case.state with
                      | {fixed} -> {true}
                      | _ -> {false}
                     end
       end 
    match Map.fold(is_fixed, last_line, {false}) with
      | {false} -> void
      | {true} -> _ = Cell.call(mySession, {game_over})
                  do Debug.jlog("Game Over")
                  void


////////////////////////////////
// OBJECT DEPLACEMENT FUNCTIONS
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


  objet_rotate() =
    _ = Cell.call(mySession, {rotate})
    void  
 
  object_session_rotate(session : Tetris.session) =
    rotate(~{x ; y}, acc)=
      List.add({x=-1*y;y=x},acc)
    newobject=List.fold(rotate,session.object.object.cases,List.empty)
    sess = 
       { etat = session.etat ; 
         event = session.event ; 
         map = session.map ; 
         object = { x=session.object.x; y=session.object.y ; object={color = session.object.object.color ; cases =newobject} } ; 
         nextobject = session.nextobject}
    {return = sess ; instruction = {set = sess}}

  objet_down() =
    _ = Cell.call(mySession, {down})
    void 

  object_session_down(session : Tetris.session) =
    check(~{x ; y}, acc)=
      match Int.compare(y+1+session.object.y,conf.nbline) with
       | {eq} -> acc + 1
       | _ ->
              rha = Map.get(y+1+session.object.y,session.map)
              match rha with
               | {some = zoui} -> 
                 w = Map.get(x+session.object.x,zoui)
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
    nb=List.fold(check,session.object.object.cases,0)
    match nb with
     | 0 -> sess = 
            { etat = session.etat ; 
              event = session.event ; 
              map = session.map ; 
              object = { x=session.object.x; y=session.object.y+1 ; object=session.object.object} ; 
              nextobject = session.nextobject}
            { return = sess ; instruction = {set = sess}}
     | _ -> sess =
            { etat = session.etat ; 
              event = session.event ; 
              map = object_glued_to_map(session.object, session.map) ; 
              object = {x=4;y=-4;object=session.nextobject} ; 
              nextobject = object_get()}
            { return = sess ; instruction = {set = sess}}

  objet_right() =
    _ = Cell.call(mySession, {right})
    void

  object_session_right(session) =   
    check(~{x:int ; y:int}, acc)=
      match Int.compare(x+1+session.object.x,conf.nbcol) with
       | {eq} -> acc + 1
       | _ ->
              rha = Map.get(session.object.y+y , session.map)
              match rha with
               | {some = zoui} -> 
                 w = Map.get(x+1+session.object.x,zoui)
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
     nb=List.fold(check,session.object.object.cases,0)
     match nb with 
      | 0 -> 
        sess = 
         { etat = session.etat ; 
           event = session.event ; 
           map = session.map ; 
           object = { x=session.object.x+1; y=session.object.y ; object=session.object.object} ; 
           nextobject = session.nextobject}
         { return = sess ; instruction = {set = sess}}
      | _ ->
        sess = 
         { etat = session.etat ; 
           event = session.event ; 
           map = session.map ; 
           object = { x=session.object.x; y=session.object.y ; object=session.object.object} ; 
           nextobject = session.nextobject}
         { return = sess ; instruction = {set = sess}}


  objet_left() =  
    _ = Cell.call(mySession, {left})
    void
  
  object_session_left(session) =
    check(~{x ; y}, acc)= 
      match Int.compare(x+session.object.x,0) with
       | {eq} -> acc + 1
       | _ ->
              rha = Map.get(y+session.object.y+0,session.map)
              match rha with
               | {some = zoui} -> 
                 w = Map.get(x-1+session.object.x,zoui)
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
     nb=List.fold(check,session.object.object.cases,0)
     match nb with 
      | 0 -> 
        sess = 
         { etat = session.etat ; 
           event = session.event ; 
           map = session.map ; 
           object = { x=session.object.x-1; y=session.object.y ; object=session.object.object} ; 
           nextobject = session.nextobject}
         { return = sess ; instruction = {set = sess}}
      | _ ->
        sess = 
         { etat = session.etat ; 
           event = session.event ; 
           map = session.map ; 
           object = { x=session.object.x; y=session.object.y ; object=session.object.object} ; 
           nextobject = session.nextobject}
         { return = sess ; instruction = {set = sess}}


////////////////////////////////
// TIMER FUNCTION
////////////////////////////////
//
// These function are called by timer
//

   // This function is called really often, and get the event action
   event_timer() =
    now = Cell.call(mySession, {timer})
    match now.etat with
      | {started} -> keyboard_action(now)
      | _ -> wait_start(now) 

   refresh_timer(ctx)() =
     now = Cell.call(mySession, {timer}) 
     do draw_map(ctx, object_add_to_map(now.object, now.map))
     draw_grid(ctx)


  // In the case of the game is stopped or paused
  // Any key trigerred will resume or start the game
  wait_start(now : Tetris.session) =
    match now.event with
      | {event = e} -> 
         match e.kind with
           | {keyup} ->  _ = Cell.call(mySession, {start}) // Indicate in the session that game is launched 
                         _ = Cell.call(mySession, {addobject}) // Push an object in the current session
                         Dom.transform([#load <- <>Use arrows keys to play</>]) // How to play indication
           | _ -> void 
         end
      | _ -> void

  // In the case of the game is launched, we must do action when key are trigered
  keyboard_action(now : Tetris.session) =
    match now.event with 
      | {event = e} -> do_event(e)
      | _ -> void


  // When this function is called we must down
  turn_timer(ctx)() =
    now = Cell.call(mySession, {action})
    match now.etat with
      | {started} -> a = Cell.call(mySession, {down_and_remove})
                     detect_gameover(a)
      | _ -> void




////////////////////////////////
// INIT
////////////////////////////////
//
// init() prepare the game
//
  init(div) =
    // Prepare the canvas element
    do Dom.transform([{div} <- <canvas id=#tetris height={conf.height} width={conf.width}>Your browser doesn't support canvas element (part of html5)</canvas>])
    canvas = Canvas.get(#tetris)
    match canvas with
     | {none} -> Dom.transform([#load <- <>An error as occured... Sorry</>])
     | {some = c} -> 
    ctx = Option.get(Canvas.get_context_2d(c)) 
    // Prepare the key binding
    _ = Dom.bind(#Body, {keydown}, bind_event)
    _ = Dom.bind(#Body, {keyup}, bind_event)
    _ = Dom.bind(#Body, {keypress}, bind_event)
    // A really short timer, to handle event action (like move or rotate)
    do Scheduler.timer(5, event_timer)
    // a timer to refresh screen
    do Scheduler.timer(10, refresh_timer(ctx))
    // A timer based on the speed variable
    do Scheduler.timer(speed, turn_timer(ctx))
    // Show we are ready to play
    Dom.transform([#load <- <>Press a touch to start</>])

}}
