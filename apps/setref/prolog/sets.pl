/* -*-Prolog-* -*/

/**
 * This is a quick and dirty prolog version of the set code,
 * It will be used as a refrence version with erlog to test
 * the normal version against
 *
*/
set(a,b).

add_to_set(Key, Value) :-
    assert(set(Key, Value)).

item_in_set(Key, Value) :-
    set(Key, Value).

