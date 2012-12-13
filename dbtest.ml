(* Okay, this is a program to create a database, insert a lot of things,
 * search for a lot of things, remove a lot of things, and iterate over
 * the entire database
 *
 * Insertions take the most time.  Then removals, then searches.  This makes
 * sense.  Iterations involve very little overhead, and it appears that dumps
 * and reads are reasonably quick, taking 0.3 seconds for a 23 meg db file.
 *)
(* ocamlc unix.cma dbtest.ml *)

module Db = Map.Make (
   struct
      type t = int
      let compare = (-)
   end
);;

(* You will be tempted to add another zero to this number.  Doing so will grind
 * phoenix's swap into the ground. *)

let numThings = 1000000;;

let rec createDB db size =
   if size = 0 then
      db
   else
      createDB (Db.add size (string_of_int size) db) (size - 1);
;;

let rec searchDB db times =
   if times = 0 then
      ()
   else (
      ignore (Db.find times db);
      searchDB db (times - 1);
   ) 
;;

let rec randomSearch db times =
   if times = 0 then
      ()
   else (
      Db.find ((Random.int (numThings - 2)) + 1) db;
      randomSearch db (times - 1)
   )

let rec removeDB db times =
   if times = 0 then
      ()
   else (
      ignore (Db.remove times db);
      removeDB db (times - 1);
   )
;;

let rec dumpDB db filename =
   let out = open_out filename in
   Marshal.to_channel out db [];
   close_out out;
;;

let rec loadDB filename =
   let inn = open_in filename in
   let i = Marshal.from_channel inn in
   close_in inn;
   i
;;


let printProcessTimes () =
   let t = Unix.times () in
   Printf.printf "User: %f System: %f\n" t.Unix.tms_utime t.Unix.tms_stime;
;;

let main () =
   print_endline "Starting...";
   printProcessTimes ();

   let d = createDB Db.empty numThings in
   print_endline "Created:";
   printProcessTimes ();

   searchDB d numThings;
   print_endline "Searched:";
   printProcessTimes ();

   randomSearch d numThings;
   print_endline "Searched randomly:";
   printProcessTimes ();

   removeDB d numThings;
   print_endline "Removed:";
   printProcessTimes ();

   Db.iter (fun _ _ -> ()) d;
   print_endline "Iterated:";
   printProcessTimes ();

   dumpDB d "fop.db";
   print_endline "Dumped:";
   printProcessTimes ();

   ignore (loadDB "fop.db");
   print_endline "read:";
   printProcessTimes ();

;;

let _ = main ();;
