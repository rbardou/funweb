open Funweb
open Property.Symbols
open Html

let input = Property.(single URL string "")
let canvas_state = Property.volatile ()

let draw c =
  let open Funweb.Canvas in

  (* Draw a yellow rectangle. *)
  set_fill_color c 255 255 0;
  fill_rect c ~x: 10. ~y: 10. ~w: 100. ~h: 40.;

  (* Draw boundaries of this rectangle in red. *)
  set_stroke_color c 255 0 0;
  set_line_width c 4.;
  stroke_rect c ~x: 10. ~y: 10. ~w: 100. ~h: 40.;

  (* Draw text at the center of this rectangle. *)
  set_text_align c Center;
  set_text_baseline c Middle;
  set_fill_color c 255 0 255;
  fill_text c !!input ~x: 60. ~y: 30.;

  (* Draw a path. *)
  set_line_cap c Round;
  set_line_join c Round;
  begin_path c;
  move_to c 20. 50.;
  line_to c 20. 60.;
  arc_to c 20. 70. 30. 70. 10.;
  bezier_curve_to c 50. 70. 70. 150. 100. 50.;
  stroke c;

  ()

let () =
  run @@ fun () ->
  dynamic @@ fun () ->
  div [
    text "Input:";
    div [ input_text input ];
    text "Output (should be a rectangle containing your input):";
    div [ canvas ~state: canvas_state draw ];
  ]
