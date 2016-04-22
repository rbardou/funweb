open Funweb
open Property.Symbols
open Html

let input = Property.(single URL string "1")
let unit = Property.(group "unit" URL int 0)

let make_result () =
  let input =
    try
      float_of_string !!input
    with _ ->
      0.
  in
  match !!unit with
    | 0 ->
        let result = input /. 1.609344 in
        string_of_float result ^ " miles"
    | 1 ->
        let result = input *. 1.609344 in
        string_of_float result ^ " km"
    | _ ->
        "(error)"

let () =
  run @@ fun () ->
  div [
    div [
      input_text input;
      input_radio unit 0;
      text "km";
      input_radio unit 1;
      text "miles";
    ];
    div [
      text "Result: ";
      dynamic @@ fun () ->
      text (make_result ())
    ];
    button ~on_click: Property.reset_all [ text "Reset" ]
  ]
