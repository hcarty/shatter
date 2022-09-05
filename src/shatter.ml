module Runtime = struct
  let section = "Runtime"

  module Block_spawner = struct
    let key = "BlockSpawnerGUID"

    let set spawner =
      Orx.Config.(set set_guid) ~section ~key (Orx.Object.to_guid spawner)

    let no_more_blocks =
      let spawner_finished = ref false in
      fun () ->
        let all_blocks_spawned =
          if !spawner_finished then true
          else
            let block_spawner =
              Orx.Config.(get get_guid) ~section ~key |> Orx.Object.of_guid
            in
            spawner_finished := Option.is_none block_spawner;
            !spawner_finished
        in
        if all_blocks_spawned then
          match (Orx.Object.get_group (Group "blocks")) () with
          | Nil -> true
          | Cons _ -> false
        else false
  end

  module Entity = struct
    type t = Paddle | Ball

    let to_key : t -> string = function
      | Paddle -> "PlayerPaddle"
      | Ball -> "Ball"

    let get_by_guid key =
      Orx.Config.(get get_guid) ~section ~key
      |> Orx.Object.of_guid |> Option.get

    let get what = get_by_guid (to_key what)

    let get_speed what =
      match what with
      | Paddle -> Orx.Config.(get get_float) ~section:"Paddle" ~key:"Speed"
      | Ball -> Orx.Config.(get get_float) ~section:"Ball" ~key:"TargetSpeed"
  end

  module Game_over = struct
    let key = "GameOver"
    let set () = Orx.Config.(set set_bool) ~section ~key:"GameOver" true
    let is_game_over () = Orx.Config.(get get_bool) ~section ~key:"GameOver"
  end

  module Score = struct
    let key = "Score"
    let get () = Orx.Config.(get get_int) ~section ~key

    let set score =
      if not (Game_over.is_game_over ()) then
        Orx.Config.(set set_int) ~section ~key score

    let increment_score () = set (get () + 1)
    let bottom_wall_hit () = set (get () - 5)
  end
end

module Input = struct
  let check_player () =
    let paddle = Runtime.Entity.get Paddle in
    let paddle_speed = Runtime.Entity.get_speed Paddle in
    let right_speed = Orx.Vector.make ~x:paddle_speed ~y:0.0 ~z:0.0 in
    let left_speed = Orx.Vector.mulf right_speed ~-.1.0 in
    if Orx.Input.is_active "Left" then Orx.Object.set_speed paddle left_speed;
    if Orx.Input.is_active "Right" then Orx.Object.set_speed paddle right_speed
end

module Stabilize = struct
  module Paddle = struct
    let stabilize_rotation obj target =
      let rotation = Orx.Object.get_rotation obj in
      let diff = rotation -. target in
      if Float.abs diff < 0.1 then ()
      else
        let torque = 5.0 *. if diff < 0.0 then 1.0 else ~-.1.0 in
        Orx.Object.apply_torque obj torque

    let stabilize_y_position obj target =
      let y = Orx.Object.get_world_position obj |> Orx.Vector.get_y in
      let diff = target -. y in
      if Float.abs diff < 10.0 then ()
      else
        let force_y = diff in
        Orx.Object.apply_force obj (Orx.Vector.make ~x:0.0 ~y:force_y ~z:0.0)

    let stabilize () =
      let paddle = Runtime.Entity.get Paddle in
      let original_position_y =
        Orx.Config.(get get_vector) ~section:"PaddleObject" ~key:"Position"
        |> Orx.Vector.get_y
      in
      stabilize_rotation paddle 0.0;
      stabilize_y_position paddle original_position_y
  end

  module Ball = struct
    let get_stabilized_ball_direction ball =
      let speed = Orx.Object.get_speed ball in
      let direction = Orx.Vector.normalize speed in
      let x = Orx.Vector.get_x direction in
      let y = Orx.Vector.get_y direction in
      let () =
        if x = 0.0 || y >= x then ()
        else
          let y' = if y = 0.0 then 0.5 else Float.abs x *. (y /. Float.abs y) in
          Orx.Vector.set_y direction y';
          Orx.Vector.normalize' ~target:direction direction
      in
      direction

    let stabilize () =
      let ball = Runtime.Entity.get Ball in
      let direction = get_stabilized_ball_direction ball in
      let target_magnitude = Runtime.Entity.get_speed Ball in
      let corrected_speed = Orx.Vector.mulf direction target_magnitude in
      Orx.Object.set_speed ball corrected_speed
  end

  let run () =
    Paddle.stabilize ();
    Ball.stabilize ()
end

module Sound = struct
  let boop (o : Orx.Object.t) = Orx.Object.add_sound_exn o "Boop"
end

module Collision = struct
  let find_colliders name event payload =
    let sender_name =
      Orx.Physics_event.get_sender_part payload |> Orx.Body_part.get_name
    in
    let recipient_name =
      Orx.Physics_event.get_recipient_part payload |> Orx.Body_part.get_name
    in
    let is_sender = String.equal sender_name name in
    let is_recipient = String.equal recipient_name name in
    let sender = Orx.Event.get_sender_object event in
    let recipient = Orx.Event.get_recipient_object event in
    if is_sender then (sender, recipient)
    else if is_recipient then (recipient, sender)
    else (None, None)

  let find_spark event (payload : Orx.Physics_event.payload) =
    let name = "SparkBodyPart" in
    fst (find_colliders name event payload)

  let find_block event (payload : Orx.Physics_event.payload) =
    let name = "BlockBodyPart" in
    fst (find_colliders name event payload)

  let find_ball event (payload : Orx.Physics_event.payload) =
    let name = "BallBodyPart" in
    fst (find_colliders name event payload)

  let find_bottom_wall event (payload : Orx.Physics_event.payload) =
    let name = "WallBodyPart" in
    match find_colliders name event payload with
    | None, _ -> None
    | Some _, None -> None
    | (Some wall as o), Some collider ->
        let wall_is_bottom =
          String.equal (Orx.Object.get_name wall) "BottomWallObject"
        in
        let collider_is_ball =
          String.equal (Orx.Object.get_name collider) "BallObject"
        in
        if wall_is_bottom && collider_is_ball then o else None

  let add_sparks payload =
    let contact_position = Orx.Physics_event.get_position payload in
    let sparks = Orx.Object.create_from_config_exn "CollisionSparksObject" in
    Orx.Object.set_position sparks contact_position

  let disable_block event payload =
    match (find_block event payload, find_ball event payload) with
    | None, _ | _, None -> ()
    | Some block, Some _ball ->
        Runtime.Score.increment_score ();
        (* Disable collisions with the block so that it can't have any more
           influence on the ball or other world objects *)
        let body = Orx.Object.get_structure block Body |> Option.get in
        Seq.iter
          (fun part -> Orx.Body_part.set_self_flags part 0)
          (Orx.Body.get_parts body);
        (* Gravity has power over the blocks now *)
        let custom_gravity =
          Orx.Config.(get get_vector)
            ~section:"BlockDisappearance" ~key:"CustomGravity"
        in
        Orx.Object.set_custom_gravity block (Some custom_gravity);
        (* Make the block disappear *)
        let life_time =
          Orx.Config.(get get_float)
            ~section:"BlockDisappearance" ~key:"LifeTime"
        in
        Orx.Object.set_life_time block life_time;
        Orx.Object.add_unique_fx_exn block "DisappearFX"

  let check_bottom_wall event payload =
    match find_bottom_wall event payload with
    | None -> ()
    | Some _wall -> Runtime.Score.bottom_wall_hit ()

  let play_boop event event_payload =
    match find_ball event event_payload with
    | None -> ()
    | Some ball -> Sound.boop ball

  let callback (event : Orx.Event.t) (physics_event : Orx.Physics_event.t)
      (payload : Orx.Physics_event.payload) =
    match physics_event with
    | Contact_remove -> Ok ()
    | Contact_add ->
        disable_block event payload;
        check_bottom_wall event payload;
        let spark = find_spark event payload in
        if Option.is_some spark then
          (* Friends don't let sparks create sparks *)
          Ok ()
        else (
          add_sparks payload;
          play_boop event payload;
          Ok ()
        )
end

let spawner_key = "BlockSpawnerGUID"

let init () =
  (* Setup the scene *)
  let _viewport = Orx.Viewport.create_from_config_exn "Viewport" in
  let _player = Orx.Object.create_from_config_exn "PaddleObject" in
  let _ball = Orx.Object.create_from_config_exn "BallObject" in
  let _walls = Orx.Object.create_from_config_exn "WallsSceneObject" in
  let _score = Orx.Object.create_from_config_exn "ScoreObject" in
  let block_spawner = Orx.Object.create_from_config_exn "BlockSpawner" in

  Runtime.Block_spawner.set block_spawner;

  Orx.Config.(set set_int) ~section:"Runtime" ~key:"Score" 0;

  (* Setup callbacks *)
  Orx.Event.add_handler Physics Collision.callback;

  Ok ()

let run () =
  if Orx.Input.is_active "Quit" then
    (* Return an error to indicate that it's time to quit the engine *)
    Orx.Status.error
  else (
    (* Just keep going *)
    Input.check_player ();
    Stabilize.run ();
    let game_over = Runtime.Game_over.is_game_over () in
    let no_more_blocks = Runtime.Block_spawner.no_more_blocks () in
    ( if (not game_over) && no_more_blocks then
      let (_ : Orx.Object.t) = Orx.Object.create_from_config_exn "EndText" in
      Runtime.Game_over.set ()
    );
    Orx.Status.ok
  )

let () =
  (* Start the main game engine loop *)
  Orx.Main.start ~config_dir:"data/config" ~init ~run "shatter"
