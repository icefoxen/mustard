(* Does all the setup and command line parsing and such before firing up
 * the main loop.
 *)


let _ = 
   let port = int_of_string Sys.argv.(1)
   and bufferLength = 2048 in
   let cs = Net.newConnectionSet port in
   Core.mainloop cs bufferLength
;;
