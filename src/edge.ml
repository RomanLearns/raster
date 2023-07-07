open Core

(* You need to transform the image to gray scale and then blur it before
   performing a convolution to apply a Sobel operator kernel *)
let transform image =
  let threshold = Int.to_float (2 * Image.max_val image / 5) in
  let gradient_x = [ -1; 0; 1; -2; 0; 2; -1; 0; 1 ] in
  let gradient_y = [ -1; -2; -1; 0; 0; 0; 1; 2; 1 ] in
  let gray_blur = Grayscale.transform image |> Blur.transform ~radius:2 in
  Image.mapi gray_blur ~f:(fun ~x ~y _ ->
    if x > 0
       && y > 0
       && x < Image.width image - 1
       && y < Image.height image - 1
    then (
      let sliced =
        Image.slice
          gray_blur
          ~x_start:(x - 1)
          ~x_end:(x + 2)
          ~y_start:(y - 1)
          ~y_end:(y + 2)
      in
      let sum_x =
        Image.foldi sliced ~init:0.0 ~f:(fun ~x ~y acc pix ->
          let idx = (3 * y) + x in
          let gradient_x_value = List.nth_exn gradient_x idx in
          acc
          +. (Int.to_float (Pixel.red pix) *. Int.to_float gradient_x_value))
      in
      let sum_y =
        Image.foldi sliced ~init:0.0 ~f:(fun ~x ~y acc pix ->
          let idx = (3 * y) + x in
          let gradient_y_value = List.nth_exn gradient_y idx in
          acc
          +. (Int.to_float (Pixel.red pix) *. Int.to_float gradient_y_value))
      in
      let convolution = sqrt ((sum_x *. sum_x) +. (sum_y *. sum_y)) in
      if Float.compare convolution threshold = -1
      then Pixel.of_int 0
      else Pixel.of_int (Image.max_val image))
    else Pixel.of_int (Image.max_val image))
;;

let command =
  Command.basic
    ~summary:"Output the edges"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edge.ppm")]
;;
