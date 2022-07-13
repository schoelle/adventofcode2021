
let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l

let range n =
  let rec aux i acc = if i > 0 then aux (i-1) (i-1 :: acc) else acc
  in aux n []

(* --------------------------------------------------------------------------- *)
(* Amphipod Definitions *)

type amphipod = Amber | Bronze | Copper | Desert

let step_cost = function
  | Amber -> 1;
  | Bronze -> 10;
  | Copper -> 100;
  | Desert -> 1000

let amphipod_to_string = function
  | Some Amber -> "A"
  | Some Bronze -> "B";
  | Some Copper -> "C";
  | Some Desert -> "D";
  | None -> "_"

(* --------------------------------------------------------------------------- *)
(* Room Stuff *)
type room_slots = amphipod option list

type room = {
    owner: amphipod;
    slots: room_slots;
  }

let room_complete room =
  List.for_all (fun e -> e = Some room.owner) room.slots

let rec can_leave owner slots =
  match slots with
  | Some x :: r -> (x <> owner) || can_leave owner r
  | None :: r -> can_leave owner r
  | [] -> false

let rec remove_one_from_slots = function
  | [] -> None
  | None :: r -> 
     (let sub = remove_one_from_slots r in
      match sub with
      | None -> None
      | Some (l,c,a) -> Some (None :: l, (c + step_cost a), a)
     )
  | Some a :: r -> Some (None :: r, 0, a)

let remove_one room =
  if not (can_leave room.owner room.slots) then
    None
  else
    (match remove_one_from_slots room.slots with
     | None -> None
     | Some (s, c, a) -> Some ({owner = room.owner; slots = s}, c, a))

let rec can_add a slots =
  match slots with
  | [] -> true
  | None :: r -> can_add a r
  | Some b :: r -> (a == b) && can_add a r

let rec add_one_to_slots a slots =
  match slots with
  | [] -> None
  | None :: r -> Some (Some a :: r, 0)
  | Some b :: r ->
     (let sub = add_one_to_slots b r in
      match sub with
      | None -> None
      | Some (l,c) -> Some (Some a :: l, (c + step_cost b))
     )

let add_one a room =
  if a == room.owner && can_add a room.slots then
    (match add_one_to_slots a room.slots with
     | None -> None
     | Some (s, c) -> Some ({owner = room.owner; slots = s}, c))
  else None

let room_to_string room =
  let r = List.map amphipod_to_string room.slots in
  String.concat "" r

(* --------------------------------------------------------------------------- *)
(* State Stuff *)

type space = amphipod option

type connection = {
    room_index: int;
    space_index: int;
    distance: int;
    empty_required: int list;
  }

type state = {
    rooms: room list;
    spaces: space list;
    connections: connection list;
    costs: int;
  }

let is_final state =
  List.for_all (fun r -> room_complete r) state.rooms

let find_connection state rno sno =
  List.find (fun c -> c.room_index = rno && c.space_index == sno) state.connections

let state_cmp s1 s2 =
  s1.costs - s2.costs

let state_to_string state =
  Printf.sprintf "%8d - Rooms: %s Spaces: %s"
    (state.costs)
    (String.concat " " (List.map room_to_string state.rooms))
    (String.concat " " (List.map amphipod_to_string state.spaces))

let same_state s1 s2 =
  s1.rooms = s2.rooms && s1.spaces = s2.spaces

let rec clean_states = function
  | x :: r -> x :: List.filter (fun e -> not (same_state x e)) r
  | [] -> []

(* --------------------------------------------------------------------------- *)
(* Movement *)
  
let step_out state rno sno =
  let con = find_connection state rno sno in
  let room = List.nth state.rooms rno in
  let target_free = List.nth state.spaces sno = None in
  let path_free = List.for_all (fun idx -> (List.nth state.spaces idx) = None) con.empty_required in
  if path_free && target_free then
    (match remove_one room with
     | Some (r,c,a) ->
        let new_rooms = replace state.rooms rno r in
        let new_spaces = replace state.spaces sno (Some a) in
        Some {rooms=new_rooms;
         spaces=new_spaces;
         costs=state.costs + c + (step_cost a) * con.distance;
         connections=state.connections}
     | None -> None
    )
  else
    None

let step_in state sno rno =
  let con = find_connection state rno sno in
  let room = List.nth state.rooms rno in
  let path_free = List.for_all (fun idx -> (List.nth state.spaces idx) = None) con.empty_required in
  if path_free then
    match List.nth state.spaces sno with
    | Some a -> (match add_one a room with
                 | Some (r,c) ->
                    let new_rooms = replace state.rooms rno r in
                    let new_spaces = replace state.spaces sno None in
                    Some {rooms=new_rooms;
                          spaces=new_spaces;
                          costs=state.costs + c + (step_cost a) * con.distance;
                          connections=state.connections}
                 | None -> None
                )
    | None -> None
  else
    None

(* --------------------------------------------------------------------------- *)  
(* Search minimum *)

exception OutOfStates of state list

let rec compute_min states =
  match states with
  | x :: r ->
     if is_final x then
       [x]
     else
       let next_step_out_states =
         List.flatten (List.map (fun sno ->
                           List.map (fun rno -> step_out x rno sno) (range 4)           
                         ) (range 7)) in
       let next_step_in_states =
         List.flatten (List.map (fun sno ->
                           List.map (fun rno -> step_in x sno rno) (range 4)           
                         ) (range 7)) in
       let next_states = List.append next_step_out_states next_step_in_states in
       let valid_states = List.filter_map (fun x -> x) next_states in
       let all_new_states = List.sort state_cmp valid_states in
       let all_states = List.merge state_cmp all_new_states r in
       let uniq_states = clean_states all_states in
       compute_min uniq_states
  | [] -> raise (OutOfStates states)

(* --------------------------------------------------------------------------- *)
(* Actual Problem *)

let map_configuration = [
    { room_index = 0; space_index = 0; distance = 3; empty_required = [1] };
    { room_index = 0; space_index = 1; distance = 2; empty_required = [] };
    { room_index = 0; space_index = 2; distance = 2; empty_required = [] };
    { room_index = 0; space_index = 3; distance = 4; empty_required = [2] };
    { room_index = 0; space_index = 4; distance = 6; empty_required = [2;3] };
    { room_index = 0; space_index = 5; distance = 8; empty_required = [2;3;4] };
    { room_index = 0; space_index = 6; distance = 9; empty_required = [2;3;4;5] };
    
    { room_index = 1; space_index = 0; distance = 5; empty_required = [1;2] };
    { room_index = 1; space_index = 1; distance = 4; empty_required = [2] };
    { room_index = 1; space_index = 2; distance = 2; empty_required = [] };
    { room_index = 1; space_index = 3; distance = 2; empty_required = [] };
    { room_index = 1; space_index = 4; distance = 4; empty_required = [3] };
    { room_index = 1; space_index = 5; distance = 6; empty_required = [3;4] };
    { room_index = 1; space_index = 6; distance = 7; empty_required = [3;4;5] };
    
    { room_index = 2; space_index = 0; distance = 7; empty_required = [1;2;3] };
    { room_index = 2; space_index = 1; distance = 6; empty_required = [2;3] };
    { room_index = 2; space_index = 2; distance = 4; empty_required = [3] };
    { room_index = 2; space_index = 3; distance = 2; empty_required = [] };
    { room_index = 2; space_index = 4; distance = 2; empty_required = [] };
    { room_index = 2; space_index = 5; distance = 4; empty_required = [4] };
    { room_index = 2; space_index = 6; distance = 5; empty_required = [4;5] };
    
    { room_index = 3; space_index = 0; distance = 9; empty_required = [1;2;3;4] };
    { room_index = 3; space_index = 1; distance = 8; empty_required = [2;3;4] };
    { room_index = 3; space_index = 2; distance = 6; empty_required = [3;4] };
    { room_index = 3; space_index = 3; distance = 4; empty_required = [4] };
    { room_index = 3; space_index = 4; distance = 2; empty_required = [] };
    { room_index = 3; space_index = 5; distance = 2; empty_required = [] };
    { room_index = 3; space_index = 6; distance = 3; empty_required = [5] }
  ]

let example_start_state: state = {
    rooms = [
      {owner = Amber; slots = [Some Bronze; Some Amber]};
      {owner = Bronze; slots = [Some Copper; Some Desert]};
      {owner = Copper; slots = [Some Bronze; Some Copper]};
      {owner = Desert; slots = [Some Desert; Some Amber]}
    ];
    spaces = [None; None; None; None; None; None; None];
    connections = map_configuration;
    costs=0;
  }

let my_start_state1: state = {
    rooms = [
      {owner = Amber; slots = [Some Desert; Some Copper]};
      {owner = Bronze; slots = [Some Amber; Some Amber]};
      {owner = Copper; slots = [Some Desert; Some Bronze]};
      {owner = Desert; slots = [Some Copper; Some Bronze]}
    ];
    spaces = [None; None; None; None; None; None; None];
    connections = map_configuration;
    costs=0;
  }

let my_start_state2: state = {
    rooms = [
      {owner = Amber; slots = [Some Desert; Some Desert; Some Desert; Some Copper]};
      {owner = Bronze; slots = [Some Amber; Some Copper; Some Bronze; Some Amber]};
      {owner = Copper; slots = [Some Desert; Some Bronze; Some Amber; Some Bronze]};
      {owner = Desert; slots = [Some Copper; Some Amber; Some Copper; Some Bronze]}
    ];
    spaces = [None; None; None; None; None; None; None];
    connections = map_configuration;
    costs=0;
  }

;;

let s = my_start_state1 in
Printf.printf "First:  %s\n" (String.concat "\n" (List.map state_to_string (compute_min [s])))

;;

let s = my_start_state2 in
Printf.printf "Second: %s\n" (String.concat "\n" (List.map state_to_string (compute_min [s])))
