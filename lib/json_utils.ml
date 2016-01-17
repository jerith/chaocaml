let prettify_or_escaped str =
  try Yojson.Basic.prettify str with
  | _ -> String.escaped str

let member name json = Yojson.Basic.Util.member name json

let get_int name json =
  member name json |> Yojson.Basic.Util.to_int

let get_float name json =
  member name json |> Yojson.Basic.Util.to_number

let get_string name json =
  member name json |> Yojson.Basic.Util.to_string

let opt_string name json =
  member name json |> Yojson.Basic.Util.to_string_option

let get_list name json =
  member name json |> Yojson.Basic.Util.to_list

let get_bool name json =
  member name json |> Yojson.Basic.Util.to_bool
