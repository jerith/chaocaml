module User = struct
  type t = {
    id : string;
    mutable username : string;
    mutable discriminator : string;
    mutable avatar : string option;
  }

  let make_user id username discriminator avatar =
    { id; username; discriminator; avatar }

  let from_json user_json = {
    id = Json_utils.get_string "id" user_json;
    username = Json_utils.get_string "username" user_json;
    discriminator = Json_utils.get_string "discriminator" user_json;
    avatar = Json_utils.opt_string "avatar" user_json;
  }
end

module Guild = struct
  type member = {
    user_id : string;
    (* TODO: More stuff? *)
  }

  type t = {
    id : string;
    mutable name : string;
    mutable members : member list;
    mutable channels : string list; (* Channel ids *)
    (* TODO: More stuff? *)
  }

  let make_guild id name members channels =
    { id; name; members; channels }

  let member_from_json member_json = {
    user_id = Json_utils.member "user" member_json |>
              Json_utils.get_string "id";
  }

  let channel_from_json channel_json =
    Json_utils.get_string "id" channel_json

  let from_json guild_json = {
    id = Json_utils.get_string "id" guild_json;
    name = Json_utils.get_string "name" guild_json;
    members = Json_utils.get_list "members" guild_json |>
              List.map member_from_json;
    channels = Json_utils.get_list "channels" guild_json |>
               List.map (fun ch -> Json_utils.get_string "id" ch);
  }
end

module Channel = struct
  type channel_type =
    | Text
    | Voice

  type t = {
    id : string;
    guild_id : string;          (* Implicit *)
    channel_type : channel_type;
    mutable name : string;
    mutable topic : string option;
    (* TODO: More stuff? *)
  }

  let from_json guild_id channel_json = {
    id = Json_utils.get_string "id" channel_json;
    guild_id = guild_id;
    channel_type = (match Json_utils.get_string "type" channel_json with
        | "text" -> Text
        | "voice" -> Voice
        | bad -> failwith @@ "Unexpected channel type: " ^ bad);
    name = Json_utils.get_string "name" channel_json;
    topic = Json_utils.opt_string "topic" channel_json;
  }
end

module All_the_things = struct
  type t = {
    users : (string, User.t) Hashtbl.t;
    guilds : (string, Guild.t) Hashtbl.t;
    channels : (string, Channel.t) Hashtbl.t;
  }

  let create () = {
    users = Hashtbl.create 10;
    guilds = Hashtbl.create 1;
    channels = Hashtbl.create 10;
  }


  let add_user things user =
    let user_id = user.User.id in
    Hashtbl.replace things.users user_id user

  let add_guild things guild =
    let guild_id = guild.Guild.id in
    Hashtbl.replace things.guilds guild_id guild

  let add_channel things channel =
    let channel_id = channel.Channel.id in
    Hashtbl.replace things.channels channel_id channel

  let users_to_string things =
    let u2s id user acc = acc ^ " " ^ id ^ ":" ^ user.User.username in
    String.trim @@ Hashtbl.fold u2s things.users ""

  let guilds_to_string things =
    let g2s id guild acc = acc ^ " " ^ id ^ ":" ^ guild.Guild.name in
    String.trim @@ Hashtbl.fold g2s things.guilds ""

  let channels_to_string things =
    let g2s id channel acc = acc ^ " " ^ id ^ ":" ^ channel.Channel.guild_id ^ ":" ^ channel.Channel.name in
    String.trim @@ Hashtbl.fold g2s things.channels ""

  let get_user things user_id =
    Hashtbl.find things.users user_id

end
