open Funweb
open Property.Symbols
open Html

let volatile = Property.(single Volatile string "")
let cookie = Property.(single (cookie ~expires: 3600. "data") string "")
let url = Property.(single URL string "")

let () =
  run @@ fun () ->
  div [
    div [
      text "The following field is volatile: it is not saved.";
      div [ input_text volatile ];
    ];
    div [
      text "The following field is saved using a cookie. This means that \
            the HTTP server will also receive it. It lasts for one hour.";
      div [ input_text cookie ];
    ];
    div [
      text "The following field is saved after the # in the URL. This means \
            that the user can bookmark it and use the Back button.";
      div [ input_text url ];
    ];
  ]
