open Main

let toString ival = match ival with
    Some x -> Main.stringIVal x
  | None -> ""

module Javascript =
struct
  let () =
    Js.export_all
      (object%js
        method run src = toString (Main.run (Lexing.from_string (Js.to_string src)) 1 (fun d -> d))
      end)
end
