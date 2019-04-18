open Core
open Main

module Cli =
struct
  let spec =
    let open Command.Spec in
    empty
    +> anon (maybe ("filename" %: string))
    +> flag "--times" (optional_with_default 1 int) ~doc:"int Number of rolls"
    +> flag "--seed" (optional int) ~doc:"int Seed value for dice"

  let get_lex_stream filename =
    let lb = Lexing.from_channel
        (match filename with
           Some (filename) -> (In_channel.create filename)
         | None -> In_channel.stdin) in lb

  let command =
    Command.basic_spec
      ~summary:"Simulate dice rolling based on a domain-specific syntax"
      ~readme:(fun () -> "Command-line options")
      spec
      (fun filename count seed () -> Main.main (get_lex_stream filename) count seed)

  let () =
    Command.run ~version:"0.0.1" command
end
