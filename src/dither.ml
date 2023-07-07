open Core

let distribute_error
  ~(x : int)
  ~(y : int)
  ~(error : float)
  ~(image : Image.t)
  ~(fill : bool)
  =
  let width = Image.width image in
  let height = Image.height image in
  let bounds = x < width - 2 && x > 0 && y < height - 2 && y > 0 in
  let pixels = [ x + 1, y; x, y + 1; x + 1, y + 1; x - 1, y + 1 ] in
  let errors = [ 7 // 16; 5 // 16; 1 // 16; 3 // 16 ] in
  (* whether to fill the pixel with black or white *)
  if fill
  then (
    let max = Image.max_val image in
    Image.set image ~x ~y (Pixel.of_int max))
  else Image.set image ~x ~y Pixel.zero;
  let count = List.range ~stride:1 ~start:`inclusive ~stop:`exclusive 0 4 in
  List.iter count ~f:(fun idx ->
    if bounds
    then (
      let error_fraction = List.nth_exn errors idx in
      let dir_error =
        Float.to_int
          Float.O.(
            error * error_fraction *. Int.to_float (Image.max_val image))
      in
      let x_adjusted, y_adjusted = List.nth_exn pixels idx in
      let new_val =
        Pixel.red (Image.get image ~x:x_adjusted ~y:y_adjusted) + dir_error
      in
      Image.set image ~x:x_adjusted ~y:y_adjusted (Pixel.of_int new_val)));
  image
;;

(* This should look familiar by now! *)
let transform image =
  let gray = Grayscale.transform image in
  Image.foldi gray ~init:gray ~f:(fun ~x ~y img p ->
    let percent = Pixel.green p // Image.max_val img in
    let fill = Float.O.(percent >. 0.5) in
    let error = if fill then percent -. 1.0 else percent in
    distribute_error ~x ~y ~error ~image:gray ~fill)
;;

let command =
  Command.basic
    ~summary:"Dither an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
