# MUUD

A simple talker-like program, made entirely to play around with networking in F#.

# Structure & such

Basically there are three (or four) parts:

* Network/communication system
* Database engine
* Scripting engine

Then there are lesser parts like the security system and the command parser.

## Networking

## Database

An embedded key-value datastore would match well to what existing MUCK's do.

However, a relational store might be nice as well.

Options include, roughly in order of popularity,

* SQLite
* LMDB/LightningDB
* LevelDB
* LightningDB
* BerkeleyDB
* db4o
* Unqlite

Plus anything that offers an OBDC or ADO.NET interface, though those
are generally bigger and more heavyweight.  The NHibernate ORM might
also be useful maybe, though I don't anticipate needing anything that
can't be handled by ISerializable.

## Scripting

I've looked into making .NET code sandboxed using AppDomain's and it
would totally work, but there are a couple gotchas:

* AppDomains cannot communicate directly with each other, they
  have to serialize and deserialize, which has
  some overhead.  (Maybe not enough to worry about, but, talking
  between AppDomains still gets squirrelly.)
* To add something to an AppDomain you have to compile it to an
  Assembly and load it... but you can't *unload* or replace an
  Assembly without nuking the entire AppDomain, which might be awkward
  to combine with rapid/easy development.

There are things like the Managed AddIn Framework and Managed
Extensibility Framework which *might* make life easier for
these... but probably won't.

The *easier* solution appears to be, more or less, to use a full
script engine.  Good options appear to be:

* MoonScript (Lua in pure C#)
* NLua (Interface to C Lua)
* Maybe IronScheme
* CSScript
* Scheme.Net?  May be a place to start on my own Scheme at least.
* ...F# has an interactive mode, dude

## Security

Capabilities

## Command interpreter

Hmmmm.  It'd be nice to have a "normal ass command" mode and an
"execute code interactively" mode.
