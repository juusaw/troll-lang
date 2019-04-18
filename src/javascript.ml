open Main

module Javascript =
struct
  let () =
    Js.export_all
      (object%js
        method run src = Main.main (Lexing.from_string (Js.to_string src)) 1 (Some 1)
      end)
end
