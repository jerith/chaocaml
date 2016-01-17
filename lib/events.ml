open Entities

module Create_message = struct
  type t = {
    id : string;
    timestamp : string;
    channel_id : string;
    author : string;
    content : string;
    tts : bool;
    mentions : string list;
    mention_everyone : bool;
  }

  let from_json event_json = {
    id = Json_utils.get_string "id" event_json;
    timestamp = Json_utils.get_string "timestamp" event_json;
    channel_id = Json_utils.get_string "channel_id" event_json;
    author = Json_utils.member "author" event_json |>
             Json_utils.get_string "id";
    content = Json_utils.get_string "content" event_json;
    tts = Json_utils.get_bool "tts" event_json;
    mentions = Json_utils.get_list "mentions" event_json |>
               List.map Yojson.Basic.to_string;
    mention_everyone = Json_utils.get_bool "mention_everyone" event_json;
  }

  let to_string things event =
    let author = All_the_things.get_user things event.author in
    String.concat "" ["<"; author.User.username; "> "; event.content]
end
