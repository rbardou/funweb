open Funweb
open Property.Symbols
open Html

let registering = Property.(single Volatile bool false)
let session = Property.(single Volatile string "")
let username = Property.(single Volatile string "")
let password = Property.(single Volatile string "")
let confirm = Property.(single Volatile string "")
let show_password = Property.(single Volatile bool false)
let error = Property.(single Volatile string "")

let accounts = Hashtbl.create 8

let register () =
  if !!password = !!confirm then (
    match Hashtbl.find accounts !!username with
      | exception Not_found ->
          Hashtbl.add accounts !!username !!password;
          session <-- "1234";
          error <-- ""
      | _ ->
          error <-- "Error: an account with this username already exists."
  )
  else
    error <-- "Error: passwords do not match."

let login () =
  match Hashtbl.find accounts !!username with
    | exception Not_found ->
        error <-- "Error: no account with this username exists."
    | expected ->
        if !!password = expected then (
          session <-- "1234";
          error <-- ""
        )
        else
          error <-- "Error: invalid password."

let submit () =
  if !!registering then register () else login ()

let set_registering value () =
  registering <-- value;
  error <-- "";
  (* Stop the event to prevent the form from being submitted. *)
  raise Stop

let input_password x =
  if !!show_password then
    input_text x
  else
    input_password x

let () =
  run ~focus: username @@ fun () ->
  dynamic @@ fun () ->
  div [
    if !!session = "" then
      (* The first button of the form (Register or Login) calls ~on_submit.
         It is also executed if the user presses ENTER. *)
      form ~on_submit: submit [
        div [
          text "Username: ";
          input_text username;
        ];
        div [
          text "Password: ";
          input_password password;
          input_checkbox show_password;
          text " Show password";
        ];
        if !!registering then
          div [
            div [
              text "Confirm password: ";
              input_password confirm;
            ];
            button [ text "Register" ];
            button ~on_click: (set_registering false) [
              text "Already registered? Log in";
            ];
          ]
        else
          div [
            button [ text "Login" ];
            button ~on_click: (set_registering true) [
              text "No account? Create one";
            ];
          ];
      ]
    else
      div [
        div [
          text "Logged in as ";
          text !!username;
        ];
        button ~on_click: Property.reset_all [ text "Log Out" ];
      ];
    div [
      text !!error;
    ];
  ]
