(* This file does core processing... the main loop, essentially.
 *)


let sendToAll cs message =
   let rec loop conns =
      match conns with 
         [] -> ()
       | hd :: tl -> 
             ignore (Unix.send hd message 0 (String.length message) []);
             loop tl; 
   in
   let _,conns = cs in
   loop conns
;;


let mainloop cs bufferLength =
   let rec loop cs =
      Printf.printf "Looping\n";
      let newcs, inputs = Net.doWonderfulThings cs bufferLength in
      List.iter (fun r -> let _, msg = r in sendToAll newcs msg) inputs;
      loop newcs
   in
   loop cs
;;
