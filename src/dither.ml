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
  if fill
  then (
    let max = Image.max_val image in
    Image.set image ~x ~y (max, max, max))
  else Image.set image ~x ~y (0, 0, 0);
  if x < width - 1
  then (
    let right_error =
      Float.to_int
        Float.O.(error * (7 // 16) *. Int.to_float (Image.max_val image))
    in
    let right_pixel = Image.get image ~x:(x + 1) ~y in
    let new_val = Pixel.red right_pixel + right_error in
    Image.set image ~x:(x + 1) ~y (new_val, new_val, new_val));
  if y < height - 1
  then (
    let down_error =
      Float.to_int
        Float.O.(error * (5 // 16) *. Int.to_float (Image.max_val image))
    in
    let down_pixel = Image.get image ~x ~y:(y + 1) in
    let new_val = Pixel.red down_pixel + down_error in
    Image.set image ~x ~y:(y + 1) (new_val, new_val, new_val));
  if x < width - 1 && y < height - 1
  then (
    let down_right_error =
      Float.to_int
        Float.O.(error * (1 // 16) *. Int.to_float (Image.max_val image))
    in
    let down_right_pixel = Image.get image ~x:(x + 1) ~y:(y + 1) in
    let new_val = Pixel.red down_right_pixel + down_right_error in
    Image.set image ~x:(x + 1) ~y:(y + 1) (new_val, new_val, new_val));
  if x - 1 > 0 && y < height - 1
  then (
    let down_left_error =
      Float.to_int
        Float.O.(error * (3 // 16) *. Int.to_float (Image.max_val image))
    in
    let down_left_pixel = Image.get image ~x:(x - 1) ~y:(y + 1) in
    let new_val = Pixel.red down_left_pixel + down_left_error in
    Image.set image ~x:(x - 1) ~y:(y + 1) (new_val, new_val, new_val));
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
