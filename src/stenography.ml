open Core

(* You need to transform the image to only read the last 2 bits of every
   color pixel and output the hidden image within the bowl of fruit *)
let transform image =
  Image.map image ~f:(fun (r, g, b) -> r % 4 * 64, g % 4 * 64, b % 4 * 64)
;;

let command =
  Command.basic
    ~summary:"Find the hidden image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "mystery.ppm")]
;;
