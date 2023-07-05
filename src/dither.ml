open Core




let distribute_error ~(x:int) ~(y:int) ~(error:float) ~(image:Image.t) = 
  let width = Image.width image in
  let height = Image.height image in
  
  let this_pixel = Image.get image ~x ~y in if error >. 0.5 then (
    Pixel.red this_pixel <- Image.max_val image;
    Pixel.blue this_pixel <- Image.max_val image;
    Pixel.green this_pixel <- Image.max_val image;
  ) else (
    Pixel.red this_pixel <- 0;
    Pixel.blue this_pixel <- 0;
    Pixel.green this_pixel <- 0;
  )

  if x < width - 1 then (
  let right_error = error *= (7//16 *. Image.max_val image) in
  let right_difference = Float.to_int right_error in
  let right_pixel = Image.get image ~x:(x + 1) ~y in
  Pixel.red right_pixel <- Pixel.red right_pixel + right_difference;
  Pixel.blue right_pixel <- Pixel.blue right_pixel + right_difference;
  Pixel.green right_pixel <- Pixel.green right_pixel + right_difference;
  Image.set image ~x ~y right_pixel
  )
  if y < height - 1 then (
    let down_error = error *= (5//16 *. Image.max_val image) in
    let down_difference = Float.to_int down_error in
    let down_pixel = Image.get image ~x ~y:(y+1) in
    Pixel.red down_pixel <- Pixel.red down_pixel + down_difference;
    Pixel.blue down_pixel <- Pixel.blue down_pixel + down_difference;
    Pixel.green down_pixel <- Pixel.green down_pixel + down_difference;
    Image.set image ~x ~y down_pixel
    )
    if x < width - 1 && y < height - 1 then (
      let down_right_error = error *= (1//16 *. Image.max_val image) in
      let down_right_difference = Float.to_int down_right_error in
      let down_right_pixel = Image.get image ~x:(x + 1) ~y in
      Pixel.red down_right_pixel <- Pixel.red down_right_pixel + down_right_difference;
      Pixel.blue down_right_pixel <- Pixel.blue down_right_pixel + down_right_difference;
      Pixel.green down_right_pixel <- Pixel.green down_right_pixel + down_right_difference;
      Image.set image ~x ~y down_right_pixel
      )
      if x > 0  && y < height - 1 then (
        let down_left_error = error *= (3//16 *. Image.max_val image) in
        let down_left_difference = Float.to_int down_left_error in
        let down_left_pixel = Image.get image ~x:(x + 1) ~y in
        Pixel.red down_left_pixel <- Pixel.red down_left_pixel + down_left_difference;
        Pixel.blue down_left_pixel <- Pixel.blue down_left_pixel + down_left_difference;
        Pixel.green down_left_pixel <- Pixel.green down_left_pixel + down_left_difference;
        Image.set image ~x ~y down_left_pixel
        )

        (* This should look familiar by now! *)
let transform image = 
  let gray = Grayscale.transform image in Image.foldi gray ~init:gray ~f:(fun ~x ~y img p -> 
      (let error = 1.0 -. (Pixel.green p // (Image.max_val img)) in distribute_error x y error))
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
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
