/* -*-Prolog-* -*/

/**
 * This is a quick and dirty prolog version of the set code, It will
 * be used as a refrence version with erlog to test the normal version
  * against */

sdata(_,_).

add_to_dataset(Key, Value) :-
    assert(sdata(Key, Value)).

item_in_dataset(Key,Value,false) :-
    \+  sdata(Key,Value),false.
item_in_dataset(Key, Value, true) :-
    sdata(Key, Value).
