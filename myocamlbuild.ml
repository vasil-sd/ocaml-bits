open Ocamlbuild_plugin
open Command

let dir_contents dir =
  let rec loop result = function
      | f::fs when Sys.is_directory f ->
            Sys.readdir f
            |> Array.to_list
            |> List.map (Filename.concat f)
            |> List.append fs
            |> loop result
      | f::fs -> loop (f::result) fs
      | []    -> result
  in
    loop [] [dir]

let is_header name =
  let len = String.length name in
    if len > 2 then
      String.sub name (len - 2) 2
      |> String.lowercase_ascii
      |> String.equal ".h"
    else
      false

let headers =
  dir_contents "src"
  |> List.filter is_header

let () =
  dispatch begin function
  | After_rules -> dep  ["compile"; "c"] headers;
  | _ -> ()
  end
