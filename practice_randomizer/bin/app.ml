open Incr_dom
open Base

module Model = struct
  type t = { spot_items : string list; spot_input_text : string }
  [@@deriving sexp, equal]

  let cutoff t1 t2 = equal t1 t2
end

module Action = struct
  type t = Update_spot_input of string | Submit_spot_input [@@deriving sexp]
end

module State = struct
  type t = unit
end

let apply_action (model : Model.t) (action : Action.t) (_ : State.t)
    ~schedule_action:_ =
  match action with
  | Update_spot_input text -> { model with spot_input_text = text }
  | Submit_spot_input ->
      {
        (* model with *)
        spot_items = model.spot_input_text :: model.spot_items;
        spot_input_text = "";
      }

let view_spot text =
  let open Vdom in
  Node.tr
    ~attrs:[ Attr.class_ "row" ]
    [
      Node.div
        ~attrs:[ Attr.class_ "col" ]
        [ Node.text text ];
      Node.button
        ~attrs:[ Attr.class_ "col-1" ]
        [ Node.text "x" ]
    ]


let view (m : Model.t) ~(inject : Action.t -> unit Vdom.Effect.t) =
  let open Vdom in
  Node.body
    [
      Node.div
        ~attrs:
          [
            Attr.style (Css_gen.flex_container ~direction:`Column ());
            Attr.class_ "container is-full-screen";
          ]
        [
          (* header *)
          Node.a
            ~attrs:[ Attr.href "https://dmitrivolkov.com" ]
            [ Node.text "dmitrivolkov.com" ];
          (* name and play *)
          Node.div
            ~attrs:[ Attr.class_ "row" ]
            [
              Node.h1
                ~attrs:[ Attr.class_ "col" ]
                [ Node.text "Practice Randomizer" ];
              Node.button
                ~attrs:[ Attr.class_ "col-1 button primary" ]
                [ Node.text ">" ];
            ];
          (* spot input *)
          Node.div
            ~attrs:[ Attr.class_ "row" ]
            [
              Node.input
                ~attrs:
                  [
                    Attr.class_ "col";
                    Attr.type_ "text";
                    Attr.string_property "value" m.spot_input_text;
                    Attr.on_input (fun _ev text ->
                        inject (Action.Update_spot_input text));
                  ]
                ();
              Node.button
                ~attrs:
                  [
                    Attr.class_ "col-1 button outline";
                    Attr.on_click (fun _ev -> inject Action.Submit_spot_input);
                  ]
                [ Node.text "+" ];
            ];
          (* spot list *)
          Node.tbody
            ~attrs:
              [
                Attr.style (Css_gen.flex_item ~grow:1.0 ());
                Attr.style (Css_gen.overflow `Scroll);
                Attr.class_ "striped";
              ]
              List.map ~f:view_spot m.spot_items;
          Node.div [ Node.text "test" ];
        ];
    ]

let on_startup ~schedule_action:_ _ = Async_kernel.return ()

let create (model : Model.t Incr.t) ~old_model:(_old_model : Model.t Incr.t)
    ~(inject : Action.t -> unit Vdom.Effect.t) =
  let open Incr.Let_syntax in
  let%map model = model in
  let apply_action = apply_action model in
  let view = view model ~inject in
  Component.create ~apply_action model view

let initial_model : Model.t = { spot_items = []; spot_input_text = "" }
