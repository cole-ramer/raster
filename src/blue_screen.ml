open Core

let is_pixel_mostly_blue pixel =
  Pixel.blue pixel > Pixel.red pixel + Pixel.green pixel
;;

(* let are_surrounding_pixels_mostly_blue ~x ~y foreground_image =
   let is_left_blue_or_edge =
   match x > 0 with
   | true -> is_pixel_mostly_blue (Image.get ~x ~y foreground_image)
   | false -> true
   in
   let is_right_blue_or_edge =
   match x < Image.width foreground_image - 1 with
   | true -> is_pixel_mostly_blue (Image.get ~x ~y foreground_image)
   | false -> true
   in
   let is_up_blue_or_edge =
   match y > 0 with
   | true -> is_pixel_mostly_blue (Image.get ~x ~y foreground_image)
   | false -> true
   in
   let is_down_blue_or_edge =
   match y < Image.height foreground_image - 1 with
   | true -> is_pixel_mostly_blue (Image.get ~x ~y foreground_image)
   | false -> true
   in
   let is_surrounding_blue_list =
   [ is_left_blue_or_edge ; is_right_blue_or_edge ; is_up_blue_or_edge ; is_down_blue_or_edge ]
   in
   let total_blue_surrounding =
   List.fold is_surrounding_blue_list ~init:0 ~f:(fun acc is_blue_or_edge ->
   match is_blue_or_edge with true -> acc + 1 | false -> acc)
   in
   total_blue_surrounding = 4
   ;; *)

(* You need to change the implementation of this function so that it
   replaces the "blue" pixels of the foreground image with pixels from
   the corresponding position in the background image instead of
   just ignoring the background image and returning the foreground image.
*)
let transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y foreground_pixel ->
    let background_pixel = Image.get background ~x ~y in
    match is_pixel_mostly_blue foreground_pixel with
    | true -> background_pixel
    | false -> foreground_pixel)
;;

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;

let%expect_test "blue screen" =
  let our_image =
    transform
      ~foreground:(Image.load_ppm ~filename:"../images/oz_bluescreen.ppm")
      ~background:(Image.load_ppm ~filename:"../images/meadow.ppm")
  in
  let reference_image =
    Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx.ppm"
  in
  Image.compare ~output_image:our_image ~reference_image;
  [%expect {||}]
;;
