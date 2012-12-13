(* Entirely functional... cannot be altered, merely re-created. 
 * In GC we trust, I suppose *)

(* Types.  We COULD have database types be entirely orthogonal, but... there
 * are a few things that are starting to suggest against that.  Programs don't
 * need to exist in a location, but should hopefully have dedicated fields to
 * documentation.  That sorta thing.
 *
 * Data types:
    * Room/area
    * Player
    * Program
    * Exits (bi-directional links)
 *
 * Okay.  While ACL's go on the item being accessed, caps go on the accessor.
 * Objects will probably have a "default capability" though, which gets used
 * when you want to allow access by default... any specific capabilities that
 * people have will override the default.
 *
 * So, players and programs at least need caps.  What about links?  I dunno.
 * Yes, because they have to hold references to other objects.  Hah.
 * Or, a link IS a capability?  It is implicit permission for a set of people
 * (those in point A) to have the capability to move to point B...
 * But, why would you want to?  I think that a capability to create links
 * connecting to a room would be enough, which would be a capability of the
 * room.  
 *
 * Capabilities: Alter default cap, read/write/alter/delete property (general or
 * specific)
 * delete item, change item name, change item location, change contents,
 * create/remove link to room, alter the values of a link, get contents of room,
 * pass through a link, move a player, edit a program, delete a program,
 *
 * Other caps for programs to do resource limitations?  It seems the right way
 * to do it.  That involves capabilities with arguments then; you can't just say
 * "I have access to X", you have to say "I have access to Y amount of X".  Ah
 * well.
 *
 * XXX: Can objects have caps on each other?  For instance, can a program hold
 * capabilities, or a room?  Well, it doesn't make sense for a room, but does
 * seem to make sense for a program...  Ugh.
 * 
 *)

(************************************************************************)
(* Fundamental types and their most primative operations.
 *)

type id = int;;

let nullID = 0;;


(* XXX: Freelist? *)
let nextID d = d + 1;;

type capid = int;;

let nullCapID = 0;;

let nextCapID d = d + 1;;

type capItem =
 (* Generic capabilities *)
   AlterDefaultCap
 | SetName
 | ReadProp of string
 | SetProp of string
 | RemoveProp of string
 | ReadAnyProp
 | SetAnyProp
 | RemoveAnyProp
 | Delete

 (* Area capabilities *)
 (* XXX: Clarify the distinction between object, area and player, if any.
  * So far it seems that they all have a location, and can all have child
  * objects...
  *)
 | AddPlayer
 | AddSubarea
 | AddLink
 | RemoveLink 

(* Player capabilities *)
 | ChangeLocation
 | CheckPassword
 | SetPassword

(* Program capabilities *)
 (* Might you need a cap to read source code???
  * No.
  *)
 | EditProg
 | EditDoc
 | Execute

(* Link capabilities *)
 | PassThrough
;;


(* object, Parent, children, items *)
type capability = {
   cid: capid;
   cobj: obj;
   cparent: capid;
   cchildren: capid list;
   cflags: capItem list
};;


type dbtype = 
   (* Parent, children, players, links *)
   Area of id * id list * id list * id list
   (* Connection, hashed password, location *)
 | Player of Unix.file_descr option * string * id
   (* Program text, documentation *)
 | Program of string * string
   (* Area1, area2 *)
 | Link of id * id
;;

let string_of_dbtype = function
   Area( _ ) -> "area"
 | Player( _ ) -> "player"
 | Program( _ ) -> "program"
 | Link( _ ) -> "link"
;;

type dbentry = {
    dbID : id;
    dbName : string;
    dbType : dbtype;
    dbDefaultCap : capItem list;
    dbProperties : (string * string) list;
};;

   

module DBMap = Map.Make ( 
   struct
   type t = id
   let compare = (-)
   end
   )
;;

module CapMap = Map.Make (
   struct
      type t = capid
      let compare = (-)
   end


type db = {
    db : dbentry DBMap.t;
    caps : capability CapMap.t;
    dbNext : id;
    capNext : capid;
};;


(************************************************************************)
(* Error handling and such.
 * XXX: Note the function that raised it?
 *)

(* Capability, flag needed *)
exception EPermissionDenied of capability * capItem list;;
(* Object, flag *)
exception EPropertyNotSet of capability * string;;
(* Expected, got *)
exception ETypeMismatch of string * string;;
exception EImpossible of string



(************************************************************************)
(* Primative database functions.
 *)


let nullDB = {
   db = DBMap.empty;
   caps = DBMap.empty;
   dbNext = 1;
   capNext = 1;
}
;;

let alterDB olddb ?(db=olddb.db) ?(dbNext=olddb.dbNext) 
                  ?(caps=olddb.caps) ?(capNext=olddb.capNext) () =
   {db=db; caps=caps; dbNext=dbNext; capNext=capNext}
;;

(* Fundamental operations on the db type *)
let make db name t props =
  let obj = {
      dbID = db.dbNext;
      dbName = name;
      dbType = t;
      dbDefaultCap = [];  (* Default: Deny everything!  Muahahaha! *)
      dbProperties = props;
    } in
  let next = nextID db.dbNext in
  let newdb = DBMap.add next obj db.db in
  alterDB db ~db: newdb ~dbNext: next ()
;;

let add db obj =
   alterDB db ~db: (DBMap.add obj.dbID obj db.db) ()
;;

let remove db idx = 
   alterDB db ~db: (DBMap.remove idx db.db) ()
;;

let addCap db cap =
   alterDB db ~caps: (CapMap.add cap.cid cap db.caps) ()
;;

let removeCap db cid =
   alterDB db ~caps: (CapMap.remove cid db.caps) ()
;;

let alter db idx newobj =
   add (remove db idx) newobj
;; 

let alterCap db cid newcap =
   add (remove db cid) newcap
;;

let find db idx =
   DBMap.find idx db.db
;;

let findCap db cid =
   CapMap.find cid db.caps
;;

let string_of_dbobj obj =
   Printf.sprintf "%s <%s, #%d>" obj.dbName 
      (string_of_dbtype obj.dbType) obj.dbID
;;

let string_of_cap obj =
   Printf.sprintf "#C%d(%d)" obj.cid obj.cobj
;;


let serialize filename db = 
  let f = open_out_bin filename in
    Marshal.to_channel f db [];
    close_out f;
;;

let unserialize (filename : string) : db = 
  let f = open_in_bin filename in
  let d = Marshal.from_channel f in
  close_in f;
  d
;;



let alterEntry old ?(id=old.dbID) ?(name=old.dbName) 
       ?(typ=old.dbType) ?(cap=old.dbDefaultCap) ?(props=old.dbProperties) () =
   { dbID = id;
     dbName = name;
     dbType = typ;
     dbDefaultCap = cap;
     dbProperties=props;
   }
;;

(************************************************************************)
(* Capabilities, and functions to do things with them.
 * Capabilities: Alter default cap, read/write/alter/delete property (general or
 * specific)
 * delete item, change item name, change item location, change contents,
 * create/remove link to room, alter the values of a link, get contents of room,
 * pass through a link, move a player, edit a program, delete a program,
 *
 * Other caps for programs to do resource limitations?  It seems the right way
 * to do it.  That involves capabilities with arguments then; you can't just say
 * "I have access to X", you have to say "I have access to Y amount of X".  Ah
 * well.
 *
 * NOTE: These functions should keep the same API, even if the implementation of
 * capabilities change.
 *
 * NOTE: Functions marked "dangerous" are fundamental functions that alter
 * capabilities and database objects without necessarily keeping them
 * consistent.  They should only be used by higher-lefel functions that do keep
 * consistency.
 *
 * XXX: For fuck's sake.  What if two different people grant different caps on
 * the same item?
 *)

(* Dangerous *)
let alterCap cap ?(parent=cap.cparent) ?(children=cap.cchildren) 
       ?(flags=cap.cflags) () = { 
      cid = cap.cid;
      cparent = parent;
      cchildren = children;
      cflags = flags;
};;
             

(* XXX: type checking!  Not capabilities are valid on all types of objects *)
(* Dangerous *)
let addCap cap capitem =
   if List.mem capitem cap.cflags then
      cap
   else
      alterCap cap ~flags: (capitem :: cap.cflags) ()
;;

(* Dangerous *)
let removeCap cap capitem =
   alterCap cap ~flags: (List.filter ((<>) capitem) cap.cflags) ()
;;

let capParent cap = cap.cparent;;

let hasCap cap capitem =
   List.mem capitem cap.cflags 
;;

(* Dangerous *)
let addChildCap cap childcap =
   if List.mem childcap cap.cchildren then cap
   else alterCap cap ~children: (childcap :: cap.cchildren) ()
;;

(* Dangerous *)
let removeChildCap cap childcap =
   alterCap cap ~children: (List.filter ((<>) childcap) cap.cchildren);;
;;

(* XXX: Make this revoke from all children, as well *)
let revokeCap mycap othercap flag =
   match capParent othercap with
      None -> othercap
    | Some( mycap ) -> 
       if hasCap mycap flag then
          let o = removeCap othercap flag in
          if o.cflags = [] then
             (None, removeChildCap mycap othercap)
          else
             (Some( o ), removeChildCap mycap othercap)
       else
          (Some( othercap ), mycap)
    | x -> (Some( othercap ), mycap)
;;


let grantCap mycap othercap flags =
   match othercap with
   None -> 
      if hasAllCaps mycap flags then
         let other =  {
            cid = mycap.cid; cparent = mycap;
            cchildren = []; cflags: flags 
          }
         in let mine = 



;;

let withCap db cap capItem f =
   if hasCap cap capItem then
      f db cap.cid
   else
      raise (EPermissionDenied( cap, [capItem] ))
;;

let hasAllCaps cap capItems =
   let trues = List.map (fun x -> List.mem x capItems) cap.cflags in
   List.fold_left (fun x y -> x && y) true trues
;;

let hasAnyCap cap capItems =
   let trues = List.map (fun x -> List.mem x capItems) cap.cflags in
   List.fold_left (fun x y -> x || y) false trues
;;

let withAllCaps db cap capItems f =
   if hasAllCaps cap capItems then
      f db cap.cid
   else
      raise (EPermissionDenied( cap, capItems ))
;;

let withAnyCap db cap capItems f =
   if hasAnyCap cap capItems then
      f db cap.cid
   else
      raise (EPermissionDenied( cap, capItems ))
;;


let grantCapToPlayer db cap flagToGrant playerid =
   ()
;;

let revokeCapFromPlayer db cap flagToRevoke playerid =
   ()
;;


(************************************************************************)
(* Hokay, here we're going to start writing library-y functions, more 
 * high-level things to do on the database...  
 * Create and remove objects, players in areas
 * Move objects and players, keeping "location" pointer in sync
 * find exits/exit destinations
 * Update players to open and close connections.
 * Search database for X
 * Walk up tree of areas looking for X
 * Link/unlink areas
 * Create/remove areas
 * get and set object properties, name, type (get only), 
 * Destroy entries, and go through the database setting all references to it
 * to a null reference
 * Get the location of objects, players and areas
 * Create and remove and update programs and exits
 * Hash and compare passwords, and change password on a user
 *
 *
 * Make a real API out of it, with error handling and such! 
 *)

let getProperty db cap prop =
   let f db id =
      try
         let itm = find db id in
         List.assoc prop itm.dbProperties
      with
         Not_found -> raise (EPropertyNotSet( cap, prop ))
   in
   withAnyCap db cap [ReadAnyProp; ReadProp( prop )] f
;;

let setProperty db cap prop vl =
   let f db id =
      let itm = find db id in
      let nprop = (prop, vl) :: (List.remove_assoc prop itm.dbProperties) in
      let nobj = alterEntry itm ~props: nprop () in
      alter db id nobj 
   in
   withAnyCap db cap [SetAnyProp; SetProp( prop )] f
;;

let delProperty db cap prop =
   let f db id =
      let itm = find db id in
      let nprop = List.remove_assoc prop itm.dbProperties in
      let nobj = alterEntry itm ~props: nprop () in
      alter db id nobj 
   in
   withAnyCap db cap [SetAnyProp; SetProp( prop )] f
;;

let setDefaultCap db cap newcaplist =
   let f db idx =
      let old = find db idx in
      let nw = alterEntry old ~cap: newcaplist () in
      alter db idx nw
   in
   withCap db cap AlterDefaultCap f 
;;


let setName db cap newname =
   let f db idx =
      let old = find db idx in
      let nw = alterEntry old ~name: newname () in
      alter db idx nw
   in
   withCap db cap SetName f 
;;

(* TODO Some way to differentiate between low-level functions like this, and the
 * capability-based ones these are used in, would be nice...  *)
(* Dangerous *)
let addToArea db areaobj childobj =
   ()
;;

(* TODO *)
(* Dangerous *)
let removeFromArea db areaobj childobj =
   ()
;;

(* Hmmmm, I wonder if I can template this sort of code somehow? *)
(*
let addPlayerToArea db playerid locid =
   let a = find db locid in
   match a.dbType with
      Area( parent, children, players, links ) ->
         let na = alterEntry a ~typ: (Area( parent, children, playerid ::
                                            players, links)) () in
         alter db locid na 
    | x -> raise (ETypeMismatch( "area", string_of_dbtype x ))
;;

let removePlayerFromArea db playerid locid =
   let a = find db locid in
   match a.dbType with
      Area( parent, children, players, links ) ->
         let newplayers = List.filter ((<>) playerid) players in
         let na = alterEntry a ~typ: (Area( parent, children, 
                                            newplayers, links)) () in
         alter db locid na 
    | x -> raise (ETypeMismatch( "area", string_of_dbtype x ))
;;
*)

(* Dangerous *)
let setPlayerLoc db playerid locid =
   let p = find db playerid in
   match p.dbType with
      Player( conn, pass, _ ) ->
         let np = alterEntry p ~typ: (Player( conn, pass, locid )) () in
         alter db playerid np
    | x -> raise (ETypeMismatch( "player", string_of_dbtype x ))
;;

let getPlayerLoc db playerid =
   let p = find db playerid in
   match p.dbType with
      Player( _, _, loc ) ->
         loc
    | x -> raise (ETypeMismatch( "player", string_of_dbtype x ))
;;

(* Dangerous *)
let setAreaParent db areaid locid =
   ()
;;

let getAreaParent db areaid =
   ()
;;

(* Nothing can prevent a player from leaving a certain area.
 * Otherwise, it would be trivial to create a room that nobody could
 * leave.  Literally nobody, as there are no ways to get around it.
 * Which is unacceptable.
 *
 * Yeah.  This becomes much clearer when objects are not involved.
 *)
let movePlayerTo db playercap destcap =
   if hasCap playercap ChangeLocation then
      if hasCap destcap AddPlayer then 
         let oldloc = getPlayerArea db playercap.cid in
         let db1 = removePlayerFromArea db playercap.cid oldloc in
         addPlayerToArea db1 playercap.cid destcap.cid
      else
         raise (EPermissionDenied( destcap, [AddPlayer] ))
   else
      raise (EPermissionDenied( playercap, [ChangeLocation] ))
;;

let moveAreaTo db areacap parentcap =
   if hasCap areacap ChangeLocation then
      if hasCap parentcap AddSubarea then
      else
         raise (EPermissionDenied( destcap, [AddSubarea] ))
   else
      raise (EPermissionDenied( playercap, [ChangeLocation] ))
;;


(* See how insecure MD5 actually is, replace it with something
 * better if necessary (ugh).
 * Well, MD5 is has been slightly broken, but it is suspected that SHA-1 is
 * possibly weak as well, so.  We'll just use the bloody MD5
 *) 
let setPassword db cap str =
   let f db id =
      let p = find db id 
      and d = Digest.string str in
      let newp = match p.dbType with
         Player( sock, _, loc ) ->
            Player( sock, d, loc )
       | x -> raise (ETypeMismatch( "player", string_of_dbtype x ))
      in
      let obj = alterEntry p ~typ:newp () in
      alter db id obj 
   in
   withCap db cap SetPassword f
;;

let checkPassword db cap str =
   let f db id =
      let p = find db id in
      match p.dbType with
         Player( _, pass, _ ) ->
            pass = (Digest.string str) 
       | _ -> false
   in
   withCap db cap CheckPassword f
;;

let setProgramText db cap str =
   ()
;;

let getProgramText db cap str =
   ()
;;

let setProgramComment db cap str =
   ()
;;

let getProgramComment db cap str =
   ()
;;

let executeProgram db cap = 
   ()
;;

let passThroughLink db playercap linkcap =
   ()
;;

let linkAreas db areacap1 areacap2 =
   ()
;;

let unlinkAreas db areacap1 areacap2 =
   ()
;;

let deleteLink db linkcap =
   ()
;;

let deletePlayer db playercap =
   ()
;;

let deleteArea db areacap =
   ()
;;

let deleteProgram db programcap =
   ()
;;
